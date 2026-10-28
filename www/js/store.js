// ============================================================================
// www/js/store.js
// Central Frontend State Store + Immediate Client-Side Matrix Preview Engine
// Separates draft inputs from authoritative R state; tracks job/connection status
// ============================================================================

export class AppStore {
  constructor() {
    this.state = {
      active_tab: 'portfolio',
      dataset: null,
      metadata: [],
      universe: [],
      base_currency: 'CHF',
      return_mode: 'TOTAL',
      active_model_id: 'shrinkage_long',
      active_portfolio_id: 'saa_50',
      active_scenario_id: 'base_5y',
      rebalancing_rule: 'ANNUAL_REBALANCED',
      cost_bps_pa: 0,
      risk_settings: {},
      covariance_models: {},
      portfolios: {},
      custom_portfolios: {},
      active_cma_set_id: 'cma_base',
      custom_cma_sets: {},
      carry_overrides: {},
      hedge_mode: 'ASSET_CLASS',
      hedge_configs: {
        ASSET_CLASS: { mode: 'ASSET_CLASS', class_ratios: {}, asset_overrides: {} },
        CURRENCY_OVERLAY: { mode: 'CURRENCY_OVERLAY', currency_ratios: {} }
      },
      draft_weights: {},
      draft_hedge_config: null,
      comparison_slots: ['', '', '', '', '', ''],
      expectations: null,
      results: {
        portfolio_evaluation: null,
        scenario_evaluation: null,
        comparison: null,
        validation: null,
        preview_evaluation: null
      },
      cache_stats: null,
      jobs: {
        pending_requests: {},
        latest_request_by_type: {},
        validation_running: false
      },
      connection: {
        connected: false,
        reconnecting: false,
        last_sync_at: null
      },
      dirty_flags: {
        calc_state: 'uncalculated', // uncalculated | current | dirty | running | stale | failed | unavailable
        has_unapplied_changes: false,
        showing_previous_valid_result: false,
        last_error_message: null
      },
      diagnostics: {
        errors: [],
        warnings: [],
        info: []
      },
      ui_filters: {
        coverage_search: '',
        coverage_type: 'ALL',
        matrix_view_mode: 'HEATMAP', // HEATMAP | TABLE_CORR | TABLE_COV
        risk_decomp_level: 'ASSET'   // ASSET | COMPONENT | CLASS
      },
      state_version: 0
    };

    // Load custom portfolios from localStorage if present
    try {
      const rawCustom = localStorage.getItem('MAPS_CUSTOM_PORTFOLIOS');
      if (rawCustom) {
        const parsed = JSON.parse(rawCustom);
        if (parsed && typeof parsed === 'object') {
          this.state.custom_portfolios = parsed;
          this.state.portfolios = { ...(this.state.portfolios || {}), ...parsed };
        }
      }
    } catch (e) {}

    // Load custom CMA sets from localStorage if present
    try {
      const rawCma = localStorage.getItem('MAPS_CUSTOM_CMA_SETS');
      if (rawCma) {
        const parsedCma = JSON.parse(rawCma);
        if (parsedCma && typeof parsedCma === 'object') {
          this.state.custom_cma_sets = parsedCma;
        }
      }
    } catch (e) {}

    this.listeners = new Set();
  }

  saveCustomPortfolio(portId, name, weights, hedgeConfig = null) {
    if (!this.state.custom_portfolios) this.state.custom_portfolios = {};
    const pObj = {
      id: portId,
      name: name || portId,
      weights: { ...(weights || {}) },
      hedge_config: hedgeConfig ? JSON.parse(JSON.stringify(hedgeConfig)) : null,
      is_custom: true
    };
    this.state.custom_portfolios[portId] = pObj;
    this.state.portfolios[portId] = pObj;
    try {
      localStorage.setItem('MAPS_CUSTOM_PORTFOLIOS', JSON.stringify(this.state.custom_portfolios));
    } catch (e) {}
  }

  selectPortfolio(portId, bridge) {
    this.state.active_portfolio_id = portId;
    const tpl = this.state.portfolios?.[portId];
    if (tpl) {
      if (tpl.weights) {
        this.state.draft_weights = { ...tpl.weights };
      }
      if (tpl.hedge_config) {
        const hc = tpl.hedge_config;
        if (hc.mode) {
          this.state.hedge_mode = hc.mode;
        }
        if (hc.configs) {
          if (hc.configs.ASSET_CLASS) {
            this.state.hedge_configs.ASSET_CLASS = JSON.parse(JSON.stringify(hc.configs.ASSET_CLASS));
          }
          if (hc.configs.CURRENCY_OVERLAY) {
            this.state.hedge_configs.CURRENCY_OVERLAY = JSON.parse(JSON.stringify(hc.configs.CURRENCY_OVERLAY));
          }
        }
        this.state.draft_hedge_config = JSON.parse(JSON.stringify(hc));
      } else {
        this.state.draft_hedge_config = JSON.parse(
          JSON.stringify(this.state.hedge_configs[this.state.hedge_mode] || {})
        );
      }
    }
    bridge.sendCommand('portfolio.evaluate', {
      portfolio_id: portId,
      weights: this.state.draft_weights,
      hedge_config: this.state.draft_hedge_config,
      cost_bps_pa: this.state.cost_bps_pa
    });
  }

  deleteCustomPortfolio(portId) {
    if (this.state.custom_portfolios && this.state.custom_portfolios[portId]) {
      delete this.state.custom_portfolios[portId];
      delete this.state.portfolios[portId];
      try {
        localStorage.setItem('MAPS_CUSTOM_PORTFOLIOS', JSON.stringify(this.state.custom_portfolios));
      } catch (e) {}
    }
    if (Array.isArray(this.state.comparison_slots)) {
      this.state.comparison_slots = this.state.comparison_slots.map(s => {
        if (s === portId || s === `${portId}_unhedged`) return '';
        return s;
      });
      try {
        localStorage.setItem('lukb_comparison_slots', JSON.stringify(this.state.comparison_slots));
      } catch (_) {}
    }
  }

  saveCustomCmaSet(setId, name, assetReturns, carryOverrides, scenarioId = 'base_5y') {
    if (!setId) setId = `cma_custom_${Date.now()}`;
    if (!this.state.custom_cma_sets) this.state.custom_cma_sets = {};
    const setObj = {
      id: setId,
      name: name || setId,
      scenario_id: scenarioId,
      asset_returns: assetReturns || {},
      carry_overrides: carryOverrides || {}
    };
    this.state.custom_cma_sets[setId] = setObj;
    this.state.active_cma_set_id = setId;
    try {
      localStorage.setItem('MAPS_CUSTOM_CMA_SETS', JSON.stringify(this.state.custom_cma_sets));
    } catch (_) {}
    return setObj;
  }

  deleteCustomCmaSet(setId) {
    if (this.state.custom_cma_sets && this.state.custom_cma_sets[setId]) {
      delete this.state.custom_cma_sets[setId];
      try {
        localStorage.setItem('MAPS_CUSTOM_CMA_SETS', JSON.stringify(this.state.custom_cma_sets));
      } catch (_) {}
      if (this.state.active_cma_set_id === setId) {
        this.state.active_cma_set_id = 'cma_base';
      }
    }
  }

  selectCmaSet(setId, bridge) {
    this.state.active_cma_set_id = setId;
    const customSet = this.state.custom_cma_sets && this.state.custom_cma_sets[setId];
    if (customSet) {
      this.state.active_scenario_id = customSet.scenario_id || 'base_5y';
      this.state.carry_overrides = customSet.carry_overrides ? JSON.parse(JSON.stringify(customSet.carry_overrides)) : {};
      bridge.sendCommand('scenarios.evaluate', {
        scenario_id: this.state.active_scenario_id,
        rebalancing_rule: this.state.rebalancing_rule,
        carry_overrides: this.state.carry_overrides,
        asset_return_overrides: customSet.asset_returns || {}
      });
    } else {
      let scId = 'base_5y';
      if (setId === 'cma_stagflation') scId = 'stagflation_5y';
      if (setId === 'cma_disinflation') scId = 'disinflation_boom_5y';
      this.state.active_scenario_id = scId;
      this.state.carry_overrides = {};
      bridge.sendCommand('scenarios.evaluate', {
        scenario_id: scId,
        rebalancing_rule: this.state.rebalancing_rule,
        carry_overrides: {}
      });
    }
  }

  setComparisonSlot(slotIndex, portfolioId) {
    if (slotIndex < 0 || slotIndex >= 6) return;
    const slots = [...(this.state.comparison_slots || ['', '', '', '', '', ''])];
    while (slots.length < 6) slots.push('');

    // Disallow duplicate selection: if portfolioId is selected in another slot, clear that slot
    if (portfolioId) {
      for (let i = 0; i < 6; i++) {
        if (i !== slotIndex && slots[i] === portfolioId) {
          slots[i] = '';
        }
      }
    }

    slots[slotIndex] = portfolioId || '';
    this.state.comparison_slots = slots;
    try {
      localStorage.setItem('lukb_comparison_slots', JSON.stringify(slots));
    } catch (_) {}
  }

  initComparisonSlotsIfNeeded() {
    const comp = this.state.results.comparison;
    if (!comp || !Array.isArray(comp.summary_table)) return;

    let slots = this.state.comparison_slots;
    if (!Array.isArray(slots) || slots.length !== 6 || slots.every(s => !s)) {
      try {
        const saved = JSON.parse(localStorage.getItem('lukb_comparison_slots') || '[]');
        if (Array.isArray(saved) && saved.length > 0) {
          slots = saved.slice(0, 6);
          while (slots.length < 6) slots.push('');
        }
      } catch (_) {}
    }

    if (!Array.isArray(slots) || slots.length !== 6 || slots.every(s => !s)) {
      slots = ['', '', '', '', '', ''];
      const activeId = this.state.active_portfolio_id || 'saa_50';
      const available = comp.summary_table.map(x => x.portfolio_id);

      if (available.includes(activeId)) slots[0] = activeId;
      const activeUnh = `${activeId}_unhedged`;
      if (available.includes(activeUnh)) slots[1] = activeUnh;

      let slotIdx = 2;
      for (const pid of available) {
        if (slotIdx >= 6) break;
        if (!slots.includes(pid) && !pid.endsWith('_unhedged')) {
          slots[slotIdx++] = pid;
        }
      }
    }

    this.state.comparison_slots = slots;
  }

  subscribe(fn) {
    this.listeners.add(fn);
    return () => this.listeners.delete(fn);
  }

  notify(reason = 'update') {
    for (const fn of this.listeners) {
      try {
        fn(this.state, reason);
      } catch (err) {
        console.error('[AppStore listener error]', err);
      }
    }
  }

  setCalcState(calcState, errorMsg = null) {
    this.state.dirty_flags.calc_state = calcState;
    if (errorMsg !== null) {
      this.state.dirty_flags.last_error_message = errorMsg;
    }
    this.notify('status');
  }

  applyServerEnvelope(envelope) {
    if (!envelope || typeof envelope !== 'object') return false;

    // Receiving any valid message from the server proves the session is connected
    this.state.connection.connected = true;
    this.state.connection.reconnecting = false;

    const reqId = envelope.request_id;
    const reqType = envelope.type;

    // Stale-job protection: verify that this response matches the latest request for its command family
    const latestForType = this.state.jobs.latest_request_by_type[reqType];
    if (
      latestForType &&
      reqId &&
      reqId !== 'srv_initial_bootstrap' &&
      latestForType !== reqId &&
      reqType !== 'validation.run'
    ) {
      delete this.state.jobs.pending_requests[reqId];
      return false;
    }

    if (reqId && this.state.jobs.pending_requests[reqId]) {
      delete this.state.jobs.pending_requests[reqId];
    }

    this.state.diagnostics.errors = Array.isArray(envelope.errors) ? envelope.errors : [];
    this.state.diagnostics.warnings = Array.isArray(envelope.warnings) ? envelope.warnings : [];
    this.state.diagnostics.info = Array.isArray(envelope.info) ? envelope.info : [];

    if (envelope.status === 'running') {
      if (reqType === 'validation.run') {
        this.state.jobs.validation_running = true;
      }
      this.state.dirty_flags.calc_state = 'running';
      this.notify('running');
      return true;
    }

    if (envelope.status === 'error') {
      if (reqType === 'validation.run') {
        this.state.jobs.validation_running = false;
      }
      this.state.dirty_flags.calc_state = 'failed';
      this.state.dirty_flags.showing_previous_valid_result = Boolean(this.state.results.portfolio_evaluation);
      const firstErr = this.state.diagnostics.errors[0];
      this.state.dirty_flags.last_error_message = firstErr ? firstErr.message : 'Berechnung abgelehnt.';
      if (envelope.result && envelope.result.cache_stats) {
        this.state.cache_stats = envelope.result.cache_stats;
      }
      this.notify('error');
      return true;
    }

    const res = envelope.result || {};
    if (envelope.context && envelope.context.state_version) {
      this.state.state_version = envelope.context.state_version;
    }

    if (reqType === 'validation.run') {
      this.state.jobs.validation_running = false;
      this.state.results.validation = res.validation || res;
      this.state.dirty_flags.calc_state = 'current';
      this.notify('validation');
      return true;
    }

    if (res.dataset) {
      this.state.dataset = res.dataset;
      this.state.metadata = Array.isArray(res.dataset.metadata) ? res.dataset.metadata : [];
      this.state.universe = Array.isArray(res.dataset.active_assets) ? res.dataset.active_assets : [];
    }

    if (res.config) {
      const cfg = res.config;
      this.state.base_currency = cfg.base_currency || this.state.base_currency;
      this.state.return_mode = cfg.return_mode || this.state.return_mode;
      this.state.active_model_id = cfg.active_model_id || this.state.active_model_id;
      this.state.active_portfolio_id = cfg.active_portfolio_id || this.state.active_portfolio_id;
      this.state.active_scenario_id = cfg.active_scenario_id || this.state.active_scenario_id;
      this.state.rebalancing_rule = cfg.rebalancing_rule || this.state.rebalancing_rule;
      this.state.cost_bps_pa = typeof cfg.cost_bps_pa === 'number' ? cfg.cost_bps_pa : 0;
      this.state.risk_settings = cfg.risk_models || this.state.risk_settings;
      this.state.portfolios = { ...(cfg.portfolios || {}), ...(this.state.custom_portfolios || {}) };
      this.state.hedge_mode = cfg.hedge_mode || this.state.hedge_mode;
      if (cfg.hedge_configs) {
        this.state.hedge_configs = cfg.hedge_configs;
      }
      if (cfg.expectations) {
        this.state.expectations = cfg.expectations;
      }
    }

    if (res.covariance_model) {
      const cm = res.covariance_model;
      const cacheKey = `${cm.base_currency}_${cm.return_mode}_${cm.model_id}`;
      this.state.covariance_models[cacheKey] = cm;
      this.state.covariance_models.active = cm;
    }

    if (res.portfolio_evaluation && res.portfolio_evaluation.valid) {
      this.state.results.portfolio_evaluation = res.portfolio_evaluation;
      this.state.results.preview_evaluation = null;
      this.state.draft_weights = { ...(res.portfolio_evaluation.weights || {}) };
      const curPort = this.state.portfolios?.[this.state.active_portfolio_id];
      if (curPort && curPort.hedge_config) {
        this.state.draft_hedge_config = JSON.parse(JSON.stringify(curPort.hedge_config));
      } else {
        this.state.draft_hedge_config = JSON.parse(
          JSON.stringify(this.state.hedge_configs[this.state.hedge_mode] || {})
        );
      }
    }

    if (res.scenario_evaluation) {
      this.state.results.scenario_evaluation = res.scenario_evaluation;
      if (res.scenario_evaluation.carry_overrides) {
        this.state.carry_overrides = res.scenario_evaluation.carry_overrides;
      }
    }

    if (res.custom_cma_sets && typeof res.custom_cma_sets === 'object') {
      this.state.custom_cma_sets = { ...(this.state.custom_cma_sets || {}), ...res.custom_cma_sets };
    }

    if (res.comparison) {
      this.state.results.comparison = res.comparison;
      this.initComparisonSlotsIfNeeded();
    }

    if (res.cache_stats) {
      this.state.cache_stats = res.cache_stats;
    }

    this.state.dirty_flags.has_unapplied_changes = false;
    this.state.dirty_flags.showing_previous_valid_result = false;
    this.state.dirty_flags.last_error_message = null;
    this.state.dirty_flags.calc_state = 'current';
    this.state.connection.last_sync_at = new Date().toLocaleTimeString('de-CH');

    this.notify('server_update');
    return true;
  }

  // Immediate client-side preview from authoritative Sigma_B already in browser
  computeClientSidePreview() {
    const model = this.state.covariance_models.active;
    if (!model || !model.sigma_monthly || !Array.isArray(model.sigma_monthly.rows)) {
      return null;
    }

    const compIds = model.component_ids || [];
    const assetIds = model.asset_ids || [];
    const availHedgeCcys = new Set(model.available_hedge_currencies || []);
    const baseCcy = this.state.base_currency;
    const weights = this.state.draft_weights || {};
    const hc = this.state.draft_hedge_config || this.state.hedge_configs[this.state.hedge_mode] || {};
    const mode = hc.mode || this.state.hedge_mode || 'ASSET_CLASS';

    const metaMap = {};
    for (const m of this.state.metadata) {
      if (m && m.Code) metaMap[m.Code] = m;
    }

    let weightSum = 0;
    const gByCcy = {};
    for (const aid of assetIds) {
      const w = Number(weights[aid] || 0);
      weightSum += w;
      const m = metaMap[aid] || {};
      const ccy = m.FX_Exposure || m.Return_Currency || baseCcy;
      const isDom = ccy === baseCcy;
      const isEmeq = ccy === 'EMEQ';
      const fwdOk = isDom ? true : isEmeq ? false : availHedgeCcys.has(ccy);
      const eligible = !isDom && !isEmeq && Boolean(m.Hedge_Allowed) && fwdOk;
      if (!eligible) continue;

      let h = 0;
      if (mode === 'ASSET_CLASS') {
        const ovr = hc.asset_overrides && hc.asset_overrides[aid];
        if (ovr !== undefined && ovr !== null && ovr !== '') {
          h = Number(ovr);
        } else if (hc.class_ratios && hc.class_ratios[m.Asset_Class] !== undefined) {
          h = Number(hc.class_ratios[m.Asset_Class]);
        }
      } else {
        if (hc.currency_ratios && hc.currency_ratios[ccy] !== undefined) {
          h = Number(hc.currency_ratios[ccy]);
        }
      }
      h = Math.max(0, Math.min(1, isNaN(h) ? 0 : h));
      gByCcy[ccy] = (gByCcy[ccy] || 0) + w * h;
    }

    const aVec = compIds.map(cid => {
      if (cid.startsWith('HEDGE_')) {
        const ccy = cid.replace(/^HEDGE_/, '');
        return Number(gByCcy[ccy] || 0);
      }
      return Number(weights[cid] || 0);
    });

    const aUnhedged = compIds.map(cid => (cid.startsWith('HEDGE_') ? 0 : Number(weights[cid] || 0)));
    const rows = model.sigma_monthly.rows;
    const n = compIds.length;

    let varM = 0;
    let varUnhM = 0;
    for (let i = 0; i < n; i++) {
      let sAi = 0;
      let sUnhI = 0;
      const row = rows[i];
      for (let j = 0; j < n; j++) {
        const sij = Number(row[j] || 0);
        sAi += sij * aVec[j];
        sUnhI += sij * aUnhedged[j];
      }
      varM += aVec[i] * sAi;
      varUnhM += aUnhedged[i] * sUnhI;
    }

    const volAnn = Math.sqrt(Math.max(0, 12.0 * varM));
    const volUnhAnn = Math.sqrt(Math.max(0, 12.0 * varUnhM));
    const totalHedgeNotional = Object.values(gByCcy).reduce((a, b) => a + b, 0);

    let equityShare = 0;
    let foreignExpBefore = 0;
    for (const aid of assetIds) {
      const w = Number(weights[aid] || 0);
      const m = metaMap[aid] || {};
      if (m.Asset_Class === 'EQUITY') {
        equityShare += w;
      }
      const ccy = m.FX_Exposure || m.Return_Currency || baseCcy;
      if (ccy !== baseCcy) {
        foreignExpBefore += w;
      }
    }
    const foreignExpAfter = Math.max(0, foreignExpBefore - totalHedgeNotional);

    const preview = {
      is_preview: true,
      weight_sum: weightSum,
      vol_annualized: volAnn,
      vol_unhedged_annualized: volUnhAnn,
      hedge_vol_effect_annualized: volAnn - volUnhAnn,
      total_hedge_notional: totalHedgeNotional,
      equity_share: equityShare,
      foreign_exposure_after: foreignExpAfter
    };
    this.state.results.preview_evaluation = preview;
    return preview;
  }
}
