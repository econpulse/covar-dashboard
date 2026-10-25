// ============================================================================
// www/js/app.js
// Main Frontend Controller for Multi-Asset Portfolio Studio
// Connects AppStore, ShinyBridge, EChartsManager, and Workspace Components
// ============================================================================

import { AppStore } from './store.js';
import { ShinyBridge } from './shiny-bridge.js';
import { EChartsManager } from './charts/echarts-manager.js';
import {
  el,
  clearAndAppend,
  fmtPct,
  renderPortfolioTriggers,
  renderCurrenciesWorkspace,
  renderRiskWorkspace,
  renderScenariosWorkspace,
  renderComparisonWorkspace,
  renderDataWorkspace
} from './components/workspaces.js';

const store = new AppStore();
const bridge = new ShinyBridge(store);
const charts = new EChartsManager();

// Expose for browser console inspection / integration tests
window.__MAPS_APP__ = { store, bridge, charts };

const STATE_LABELS = {
  uncalculated: { label: 'Nicht berechnet', cls: 'state-uncalculated' },
  current:      { label: 'Aktuell', cls: 'state-current' },
  dirty:        { label: 'Änderungen nicht angewendet (Vorschau)', cls: 'state-dirty' },
  running:      { label: 'Berechnung läuft …', cls: 'state-running' },
  stale:        { label: 'Veraltet', cls: 'state-stale' },
  failed:       { label: 'Fehlgeschlagen (Vorheriger Stand)', cls: 'state-failed' },
  unavailable:  { label: 'Nicht verfügbar (Offline)', cls: 'state-unavailable' }
};

function updateContextHeader() {
  const st = store.state;
  const ev = st.results.portfolio_evaluation;
  const cm = st.covariance_models.active;
  const sc = st.results.scenario_evaluation;

  // Connection status in sidebar
  const connDot = document.getElementById('conn_dot');
  const connText = document.getElementById('conn_text');
  if (connDot && connText) {
    connDot.classList.toggle('disconnected', !st.connection.connected);
    connText.textContent = st.connection.connected
      ? `Verbunden (${st.connection.last_sync_at || 'bereit'})`
      : 'Verbindung getrennt';
  }

  // Context bar values
  const setTxt = (id, v) => {
    const n = document.getElementById(id);
    if (n) n.textContent = v;
  };

  const portObj = st.portfolios[st.active_portfolio_id];
  setTxt('ctx_portfolio_name', portObj ? portObj.name : st.active_portfolio_id);
  setTxt('ctx_risk_model', cm ? cm.label : st.active_model_id);
  setTxt('ctx_scenario_name', sc ? sc.scenario_label : st.active_scenario_id);
  setTxt('ctx_as_of', cm ? `${cm.end_month} (N=${cm.n_obs})` : '-');
  setTxt('ctx_config_hash', ev ? ev.config_hash : '-');

  // Base currency segmented buttons
  document.querySelectorAll('[data-base-ccy]').forEach(btn => {
    const ccy = btn.getAttribute('data-base-ccy');
    btn.classList.toggle('active', ccy === st.base_currency);
    btn.setAttribute('aria-pressed', ccy === st.base_currency ? 'true' : 'false');
  });

  // Status badge
  const badge = document.getElementById('ctx_state_badge');
  if (badge) {
    const info = STATE_LABELS[st.dirty_flags.calc_state] || STATE_LABELS.uncalculated;
    badge.className = `state-badge ${info.cls}`;
    badge.textContent = info.label;
  }

  // Sync top selectors
  const selPort = document.getElementById('select_active_portfolio');
  if (selPort && selPort.value !== st.active_portfolio_id) {
    selPort.value = st.active_portfolio_id;
  }
  const selModel = document.getElementById('select_risk_model');
  if (selModel && selModel.value !== st.active_model_id) {
    selModel.value = st.active_model_id;
  }
  const selRetMode = document.getElementById('select_return_mode');
  if (selRetMode && selRetMode.value !== st.return_mode) {
    selRetMode.value = st.return_mode;
  }
  const selScen = document.getElementById('select_scenario_id');
  if (selScen && selScen.value !== st.active_scenario_id) {
    selScen.value = st.active_scenario_id;
  }
  const selRebal = document.getElementById('select_rebalancing_rule');
  if (selRebal && selRebal.value !== st.rebalancing_rule) {
    selRebal.value = st.rebalancing_rule;
  }

  // Diagnostics / Alert Strip
  const strip = document.getElementById('alert_strip');
  if (strip) {
    const errs = st.diagnostics.errors || [];
    const warns = st.diagnostics.warnings || [];
    const allAlerts = [...errs, ...warns];

    if (allAlerts.length === 0) {
      strip.className = 'alert-strip';
      strip.replaceChildren();
    } else {
      strip.className = `alert-strip visible ${errs.length > 0 ? 'has-error' : ''}`;
      const nodes = allAlerts.slice(0, 6).map(item =>
        el('div', { className: 'alert-item' }, [
          el('span', { className: `alert-tag ${item.severity || 'WARNING'}`, textContent: item.code || item.severity }),
          el('span', { textContent: item.message || '' })
        ])
      );
      clearAndAppend(strip, nodes);
    }
  }
}

function renderActiveTabCharts() {
  const st = store.state;
  const tab = st.active_tab;
  const ev = st.results.portfolio_evaluation;
  const cm = st.covariance_models.active;
  const sc = st.results.scenario_evaluation;
  const comp = st.results.comparison;

  if (tab === 'portfolio' && ev) {
    charts.renderPortfolioWeightsChart('chart_portfolio_weights', ev.exposures?.asset_table || []);
    if (ev.historical_path) {
      charts.renderHistoricalPathChart('chart_historical_path', ev.historical_path, st.base_currency);
    }
  } else if (tab === 'currencies' && ev) {
    charts.renderCurrencyExposureChart('chart_currency_exposures', ev.exposures?.currency_table || []);
  } else if (tab === 'risk' && cm && ev) {
    // Only render heatmap if HEATMAP view is active
    const heatContainer = document.getElementById('chart_correlation_heatmap');
    const tableContainer = document.getElementById('matrix_table_wrapper');
    const mode = st.ui_filters.matrix_view_mode || 'HEATMAP';
    if (heatContainer && tableContainer) {
      heatContainer.style.display = mode === 'HEATMAP' ? 'block' : 'none';
      tableContainer.style.display = mode === 'HEATMAP' ? 'none' : 'block';
    }
    if (mode === 'HEATMAP') {
      charts.renderCorrelationHeatmap('chart_correlation_heatmap', cm.correlation);
    }
    const decompItems = st.ui_filters.risk_decomp_level === 'COMPONENT'
      ? ev.component_contributions
      : ev.asset_contributions;
    const idField = st.ui_filters.risk_decomp_level === 'COMPONENT' ? 'component_id' : 'asset_id';
    charts.renderRiskContributionsChart('chart_risk_contributions', decompItems, idField);
  } else if (tab === 'returns' && sc) {
    charts.renderScenarioPathChart('chart_scenario_paths', sc.yearly_path || [], st.base_currency);
  } else if (tab === 'comparison' && comp) {
    charts.renderComparisonChart('chart_comparison_models', comp.cross_model_comparison || []);
  }
}

function renderAllWorkspaces() {
  updateContextHeader();
  renderPortfolioTriggers(store, bridge);
  renderCurrenciesWorkspace(store, bridge);
  renderRiskWorkspace(store);
  renderScenariosWorkspace(store, bridge);
  renderComparisonWorkspace(store);
  renderDataWorkspace(store);
  renderActiveTabCharts();
}

function switchTab(tabId) {
  store.state.active_tab = tabId;
  document.querySelectorAll('.nav-btn').forEach(btn => {
    const isAct = btn.getAttribute('data-tab') === tabId;
    btn.classList.toggle('active', isAct);
    btn.setAttribute('aria-current', isAct ? 'page' : 'false');
  });
  document.querySelectorAll('.workspace-panel').forEach(panel => {
    panel.classList.toggle('active', panel.id === `panel_${tabId}`);
  });
  // Render charts on-demand when switching to a tab
  requestAnimationFrame(() => {
    renderActiveTabCharts();
    charts.resizeAllVisible();
  });
}

function bindUIEvents() {
  // Navigation tabs
  document.querySelectorAll('.nav-btn').forEach(btn => {
    btn.addEventListener('click', () => {
      const tabId = btn.getAttribute('data-tab');
      if (tabId) switchTab(tabId);
    });
  });

  // Base currency switcher
  document.querySelectorAll('[data-base-ccy]').forEach(btn => {
    btn.addEventListener('click', () => {
      const newCcy = btn.getAttribute('data-base-ccy');
      if (!newCcy || newCcy === store.state.base_currency) return;
      store.state.base_currency = newCcy;
      bridge.sendCommand('risk.estimate', {
        base_currency: newCcy,
        return_mode: store.state.return_mode,
        model_id: store.state.active_model_id,
        portfolio_id: store.state.active_portfolio_id,
        weights: store.state.draft_weights,
        hedge_config: store.state.draft_hedge_config
      });
    });
  });

  // Portfolio selector & 25/50/75 profile quick buttons
  const selPort = document.getElementById('select_active_portfolio');
  if (selPort) {
    selPort.addEventListener('change', e => {
      const pid = e.target.value;
      store.state.active_portfolio_id = pid;
      const tpl = store.state.portfolios[pid];
      if (tpl && tpl.weights) {
        store.state.draft_weights = { ...tpl.weights };
      }
      bridge.sendCommand('portfolio.evaluate', {
        portfolio_id: pid,
        weights: store.state.draft_weights,
        hedge_config: store.state.draft_hedge_config,
        cost_bps_pa: store.state.cost_bps_pa
      });
    });
  }

  document.querySelectorAll('[data-profile-saa]').forEach(btn => {
    btn.addEventListener('click', () => {
      const pid = btn.getAttribute('data-profile-saa');
      store.state.active_portfolio_id = pid;
      const tpl = store.state.portfolios[pid];
      if (tpl && tpl.weights) {
        store.state.draft_weights = { ...tpl.weights };
      }
      bridge.sendCommand('portfolio.evaluate', {
        portfolio_id: pid,
        weights: store.state.draft_weights,
        hedge_config: store.state.draft_hedge_config,
        cost_bps_pa: store.state.cost_bps_pa
      });
    });
  });

  // Normalize weights to 100% helper button
  const btnNorm = document.getElementById('btn_normalize_weights');
  if (btnNorm) {
    btnNorm.addEventListener('click', () => {
      const dw = store.state.draft_weights || {};
      const sum = Object.values(dw).reduce((a, b) => a + Number(b || 0), 0);
      if (sum > 1e-6) {
        for (const k of Object.keys(dw)) {
          dw[k] = Number((Number(dw[k] || 0) / sum).toFixed(6));
        }
        bridge.sendCommand('portfolio.evaluate', {
          portfolio_id: store.state.active_portfolio_id,
          weights: dw,
          hedge_config: store.state.draft_hedge_config,
          cost_bps_pa: store.state.cost_bps_pa
        });
      }
    });
  }

  // Exclusive Hedge Mode Toggle (ASSET_CLASS vs CURRENCY_OVERLAY)
  const btnModeClass = document.getElementById('btn_hedge_mode_class');
  const btnModeOverlay = document.getElementById('btn_hedge_mode_overlay');
  const selFallback = document.getElementById('select_hedge_return_policy');

  if (btnModeClass) {
    btnModeClass.addEventListener('click', () => {
      if (store.state.hedge_mode === 'ASSET_CLASS') return;
      const fallbackPol = selFallback ? selFallback.value : 'RESTORE_SAVED';
      bridge.sendCommand('hedge.convert_mode', {
        target_mode: 'ASSET_CLASS',
        fallback_policy: fallbackPol,
        weights: store.state.draft_weights,
        hedge_config: store.state.draft_hedge_config
      });
    });
  }

  if (btnModeOverlay) {
    btnModeOverlay.addEventListener('click', () => {
      if (store.state.hedge_mode === 'CURRENCY_OVERLAY') return;
      bridge.sendCommand('hedge.convert_mode', {
        target_mode: 'CURRENCY_OVERLAY',
        weights: store.state.draft_weights,
        hedge_config: store.state.draft_hedge_config
      });
    });
  }

  // Hedge transaction cost assumption (bps p.a.)
  const inpCost = document.getElementById('input_hedge_cost_bps');
  if (inpCost) {
    inpCost.addEventListener('change', e => {
      const bps = Math.max(0, Math.min(200, Number(e.target.value || 0)));
      store.state.cost_bps_pa = bps;
      bridge.sendCommand('portfolio.evaluate', {
        portfolio_id: store.state.active_portfolio_id,
        weights: store.state.draft_weights,
        hedge_config: store.state.draft_hedge_config,
        cost_bps_pa: bps
      });
    });
  }

  // Risk Model & Return Mode (Total vs Excess Return) selectors
  const selModel = document.getElementById('select_risk_model');
  if (selModel) {
    selModel.addEventListener('change', e => {
      const mid = e.target.value;
      store.state.active_model_id = mid;
      bridge.sendCommand('risk.estimate', {
        base_currency: store.state.base_currency,
        return_mode: store.state.return_mode,
        model_id: mid,
        portfolio_id: store.state.active_portfolio_id,
        weights: store.state.draft_weights,
        hedge_config: store.state.draft_hedge_config
      });
    });
  }

  const selRetMode = document.getElementById('select_return_mode');
  if (selRetMode) {
    selRetMode.addEventListener('change', e => {
      const rmode = e.target.value;
      store.state.return_mode = rmode;
      bridge.sendCommand('risk.estimate', {
        base_currency: store.state.base_currency,
        return_mode: rmode,
        model_id: store.state.active_model_id,
        portfolio_id: store.state.active_portfolio_id,
        weights: store.state.draft_weights,
        hedge_config: store.state.draft_hedge_config
      });
    });
  }

  // Matrix View Mode (Heatmap vs Correlation Table vs Covariance Table) - Pure UI change, no R call!
  const selMatView = document.getElementById('select_matrix_view_mode');
  if (selMatView) {
    selMatView.addEventListener('change', e => {
      store.state.ui_filters.matrix_view_mode = e.target.value;
      renderRiskWorkspace(store);
      renderActiveTabCharts();
    });
  }

  // Risk decomposition level (Asset vs Component) - Pure UI change, no R call!
  const selDecomp = document.getElementById('select_risk_decomp_level');
  if (selDecomp) {
    selDecomp.addEventListener('change', e => {
      store.state.ui_filters.risk_decomp_level = e.target.value;
      renderRiskWorkspace(store);
      renderActiveTabCharts();
    });
  }

  // Scenario & Rebalancing selectors
  const selScen = document.getElementById('select_scenario_id');
  if (selScen) {
    selScen.addEventListener('change', e => {
      store.state.active_scenario_id = e.target.value;
      bridge.sendCommand('scenarios.evaluate', {
        scenario_id: store.state.active_scenario_id,
        rebalancing_rule: store.state.rebalancing_rule
      });
    });
  }

  const selRebal = document.getElementById('select_rebalancing_rule');
  if (selRebal) {
    selRebal.addEventListener('change', e => {
      store.state.rebalancing_rule = e.target.value;
      bridge.sendCommand('scenarios.evaluate', {
        scenario_id: store.state.active_scenario_id,
        rebalancing_rule: store.state.rebalancing_rule
      });
    });
  }

  // Coverage table search/type filter - Pure client-side filter, no R recalculation!
  const inpSearch = document.getElementById('input_coverage_search');
  if (inpSearch) {
    inpSearch.addEventListener('input', e => {
      store.state.ui_filters.coverage_search = e.target.value;
      renderDataWorkspace(store);
    });
  }

  const selType = document.getElementById('select_coverage_type');
  if (selType) {
    selType.addEventListener('change', e => {
      store.state.ui_filters.coverage_type = e.target.value;
      renderDataWorkspace(store);
    });
  }

  // EUR Pre-1999 Cash Proxy toggle
  const chkEurProxy = document.getElementById('chk_allow_eur_proxy');
  if (chkEurProxy) {
    chkEurProxy.addEventListener('change', e => {
      bridge.sendCommand('risk.estimate', {
        base_currency: store.state.base_currency,
        return_mode: store.state.return_mode,
        model_id: store.state.active_model_id,
        allow_eur_cash_proxy: Boolean(e.target.checked)
      });
    });
  }

  // Background Validation Job button (Walk-Forward + Block Bootstrap)
  const btnRunVal = document.getElementById('btn_run_validation');
  if (btnRunVal) {
    btnRunVal.addEventListener('click', () => {
      bridge.sendCommand('validation.run', {
        train_months: 120,
        test_months: 12,
        n_boot: 200,
        block_length: 6,
        seed: 42
      });
    });
  }

  // Save / Load versioned JSON configuration locally
  const btnSaveCfg = document.getElementById('btn_save_json_config');
  if (btnSaveCfg) {
    btnSaveCfg.addEventListener('click', () => {
      const st = store.state;
      const cfgBlob = {
        schema_version: 1,
        saved_at: new Date().toISOString(),
        dataset_id: st.dataset?.dataset_id,
        base_currency: st.base_currency,
        return_mode: st.return_mode,
        active_model_id: st.active_model_id,
        active_portfolio_id: st.active_portfolio_id,
        weights: st.draft_weights,
        hedge_mode: st.hedge_mode,
        hedge_config: st.draft_hedge_config,
        cost_bps_pa: st.cost_bps_pa,
        active_scenario_id: st.active_scenario_id,
        rebalancing_rule: st.rebalancing_rule,
        portfolios: st.portfolios
      };
      const blob = new Blob([JSON.stringify(cfgBlob, null, 2)], { type: 'application/json' });
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = `maps_config_${st.base_currency}_${st.active_portfolio_id}.json`;
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
    });
  }

  const inpLoadCfg = document.getElementById('input_load_json_config');
  if (inpLoadCfg) {
    inpLoadCfg.addEventListener('change', e => {
      const file = e.target.files && e.target.files[0];
      if (!file) return;
      const reader = new FileReader();
      reader.onload = evt => {
        try {
          const parsed = JSON.parse(String(evt.target.result));
          bridge.sendCommand('config.import', { config_data: parsed });
        } catch (err) {
          store.setCalcState('failed', 'JSON-Konfigurationsdatei konnte nicht gelesen werden.');
        }
      };
      reader.readAsText(file);
    });
  }
}

document.addEventListener('DOMContentLoaded', () => {
  bindUIEvents();
  store.subscribe((state, reason) => {
    if (reason === 'status' || reason === 'running' || reason === 'connection') {
      updateContextHeader();
      if (state.results.preview_evaluation) {
        renderPortfolioTriggers(store, bridge);
      }
    } else {
      renderAllWorkspaces();
    }
  });
  bridge.init();
});
