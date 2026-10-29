// ============================================================================
// www/js/shiny-bridge.js
// Protocol v1 bridge between custom HTML/JS UI and R-Shiny session
// Registers handler before first request; handles debounce and reconnect sync
// ============================================================================

export class ShinyBridge {
  constructor(store) {
    this.store = store;
    this.protocolVersion = 1;
    this.debounceTimers = {};
    this.initialized = false;
    this.handlerRegistered = false;
    this.bootstrapSent = false;
    this.outgoingQueue = [];
  }

  generateRequestId() {
    if (typeof crypto !== 'undefined' && typeof crypto.randomUUID === 'function') {
      return crypto.randomUUID();
    }
    return 'req_' + Date.now() + '_' + Math.random().toString(36).substring(2, 9);
  }

  flushQueue() {
    if (typeof window.Shiny !== 'undefined' && typeof window.Shiny.setInputValue === 'function') {
      while (this.outgoingQueue.length > 0) {
        const env = this.outgoingQueue.shift();
        window.Shiny.setInputValue('app_command', env, { priority: 'event' });
      }
    }
  }

  init() {
    if (this.initialized) return;
    this.initialized = true;

    const registerHandler = () => {
      if (typeof window.Shiny !== 'undefined' && typeof window.Shiny.addCustomMessageHandler === 'function') {
        if (!this.handlerRegistered) {
          this.handlerRegistered = true;
          try {
            window.Shiny.addCustomMessageHandler('app_event', message => {
              this.store.applyServerEnvelope(message);
            });
          } catch (e) {
            // Handler might already be registered
          }
        }
      }
      // Drain any buffered messages received prior to module load
      if (Array.isArray(window.__SHINY_MSG_BUFFER__) && window.__SHINY_MSG_BUFFER__.length > 0) {
        while (window.__SHINY_MSG_BUFFER__.length > 0) {
          const msg = window.__SHINY_MSG_BUFFER__.shift();
          this.store.applyServerEnvelope(msg);
        }
      }
    };

    const onConnected = () => {
      registerHandler();
      const wasReconnecting = this.store.state.connection.reconnecting;
      this.store.state.connection.connected = true;
      this.store.state.connection.reconnecting = false;
      this.store.notify('connection');
      this.flushQueue();

      if ((!wasReconnecting && !this.store.state.dataset) || (!this.store.state.dataset && !this.bootstrapSent)) {
        this.bootstrapSent = true;
        this.sendCommand('app.bootstrap', {
          base_currency: this.store.state.base_currency,
          model_id: this.store.state.active_model_id,
          portfolio_id: this.store.state.active_portfolio_id,
          custom_portfolios: Object.values(this.store.state.custom_portfolios || {})
        });
      } else if (wasReconnecting && this.store.state.dirty_flags.has_unapplied_changes) {
        // Reconcile draft changes on reconnect without triggering heavy background jobs
        this.sendCommand('portfolio.evaluate', {
          portfolio_id: this.store.state.active_portfolio_id,
          weights: this.store.state.draft_weights,
          hedge_config: this.store.state.draft_hedge_config,
          cost_bps_pa: this.store.state.cost_bps_pa
        });
      }
    };

    const onDisconnected = () => {
      this.store.state.connection.connected = false;
      this.store.state.connection.reconnecting = true;
      this.store.setCalcState('unavailable', 'Verbindung zum R-Shiny-Server unterbrochen.');
      this.store.notify('connection');
    };

    // Register handler immediately
    registerHandler();

    // Shiny fires shiny:connected and shiny:disconnected via jQuery on document
    if (typeof window.jQuery !== 'undefined') {
      window.jQuery(document).on('shiny:connected', onConnected);
      window.jQuery(document).on('shiny:disconnected', onDisconnected);
    } else {
      document.addEventListener('shiny:connected', onConnected);
      document.addEventListener('shiny:disconnected', onDisconnected);
    }

    // Handle case where ES module executes after shiny:connected has already fired
    const checkAlreadyConnected = () => {
      registerHandler();
      this.flushQueue();
      const shinyReady = (
        typeof window.Shiny !== 'undefined' &&
        typeof window.Shiny.setInputValue === 'function' &&
        window.Shiny.shinyapp &&
        typeof window.Shiny.shinyapp.isConnected === 'function' &&
        window.Shiny.shinyapp.isConnected()
      );

      if (shinyReady) {
        if (!this.store.state.connection.connected) {
          onConnected();
        }
        if (!this.store.state.dataset && !this.bootstrapSent) {
          this.bootstrapSent = true;
          this.sendCommand('app.bootstrap', {
            base_currency: this.store.state.base_currency,
            model_id: this.store.state.active_model_id,
            portfolio_id: this.store.state.active_portfolio_id,
            custom_portfolios: Object.values(this.store.state.custom_portfolios || {})
          });
        }
        return true;
      }
      return false;
    };

    // Poll until connection is established and initial dataset is loaded
    let attempts = 0;
    const pollTimer = setInterval(() => {
      attempts += 1;
      registerHandler();
      this.flushQueue();
      if (checkAlreadyConnected() && this.store.state.dataset) {
        clearInterval(pollTimer);
      } else if (attempts >= 60 && this.store.state.connection.connected) {
        clearInterval(pollTimer);
      }
    }, 200);
  }

  sendCommand(type, payload = {}, options = {}) {
    const requestId = this.generateRequestId();
    const st = this.store.state;

    const envelope = {
      protocol_version: this.protocolVersion,
      request_id: requestId,
      type: type,
      context: {
        dataset_id: st.dataset ? st.dataset.dataset_id : null,
        basis_id: st.covariance_models.active ? st.covariance_models.active.basis_id : null,
        model_id: st.active_model_id,
        base_currency: st.base_currency,
        return_mode: st.return_mode,
        state_version: st.state_version
      },
      payload: payload
    };

    st.jobs.pending_requests[requestId] = {
      type,
      sent_at: Date.now()
    };
    st.jobs.latest_request_by_type[type] = requestId;

    if (!options.silentRunningState) {
      this.store.setCalcState('running');
    }

    if (typeof window.Shiny !== 'undefined' && typeof window.Shiny.setInputValue === 'function') {
      window.Shiny.setInputValue('app_command', envelope, { priority: 'event' });
    } else {
      console.warn('[ShinyBridge] Shiny.setInputValue not yet ready, queueing:', type);
      this.outgoingQueue.push(envelope);
    }
    return requestId;
  }

  sendDebounced(type, payload = {}, delayMs = 200) {
    if (this.debounceTimers[type]) {
      clearTimeout(this.debounceTimers[type]);
    }
    // Immediate client-side preview while debouncing
    this.store.computeClientSidePreview();
    this.store.state.dirty_flags.has_unapplied_changes = true;
    this.store.setCalcState('dirty');

    this.debounceTimers[type] = setTimeout(() => {
      delete this.debounceTimers[type];
      this.sendCommand(type, payload);
    }, delayMs);
  }
}
