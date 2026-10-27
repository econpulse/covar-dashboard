// ============================================================================
// www/js/components/workspaces.js
// Safe DOM component builders (using textContent, no raw innerHTML from data)
// for the 6 workspaces: Portfolio, Currencies, Risk, Returns, Comparison, Data
// ============================================================================

export function el(tag, attrs = {}, children = []) {
  const node = document.createElement(tag);
  for (const [k, v] of Object.entries(attrs)) {
    if (v === null || v === undefined) continue;
    if (k === 'className') {
      node.className = v;
    } else if (k === 'textContent') {
      node.textContent = String(v);
    } else if (k === 'style' && typeof v === 'object') {
      Object.assign(node.style, v);
    } else if (k.startsWith('on') && typeof v === 'function') {
      node.addEventListener(k.slice(2).toLowerCase(), v);
    } else if (k === 'disabled' || k === 'checked' || k === 'selected' || k === 'readOnly') {
      node[k] = Boolean(v);
    } else {
      node.setAttribute(k, String(v));
    }
  }
  const childArr = Array.isArray(children) ? children : [children];
  for (const ch of childArr) {
    if (ch === null || ch === undefined || ch === false) continue;
    if (typeof ch === 'string' || typeof ch === 'number') {
      node.appendChild(document.createTextNode(String(ch)));
    } else if (ch instanceof Node) {
      node.appendChild(ch);
    }
  }
  return node;
}

export function clearAndAppend(container, children) {
  if (!container) return;
  container.replaceChildren();
  const arr = Array.isArray(children) ? children : [children];
  for (const ch of arr) {
    if (ch instanceof Node) container.appendChild(ch);
  }
}

export function fmtPct(val, digits = 2, naStr = 'n/a') {
  if (val === null || val === undefined || Number.isNaN(Number(val))) return naStr;
  return (Number(val) * 100).toFixed(digits) + ' %';
}

export function fmtNum(val, digits = 2, naStr = 'n/a') {
  if (val === null || val === undefined || Number.isNaN(Number(val))) return naStr;
  return Number(val).toFixed(digits);
}

export function getAssetClassClass(assetClass) {
  return String(assetClass || '').toLowerCase().replace(/[^a-z0-9]+/g, '-');
}

export function updateAllPortfolioKpis(store) {
  const st = store.state;
  const ev = st.results.portfolio_evaluation;
  const prev = st.results.preview_evaluation;
  const activeMetrics = prev || (ev ? ev.metrics : null);

  if (!activeMetrics) return;

  const isPrev = Boolean(prev && prev.is_preview);
  const volText = fmtPct(activeMetrics.vol_annualized, 2);
  const volSubText = isPrev ? 'Live-Vorschau aus Matrix' : `Autoritativ (${st.base_currency}, 12×Sigma)`;

  const unhVolText = fmtPct(activeMetrics.vol_unhedged_annualized, 2);
  const unhVolSubText = `Hedge-Effekt: ${fmtPct(activeMetrics.hedge_vol_effect_annualized, 2)}`;

  const wSum = activeMetrics.weight_sum !== undefined
    ? Number(activeMetrics.weight_sum)
    : Object.values(st.draft_weights || {}).reduce((a, b) => a + Number(b || 0), 0);
  const wSumText = fmtPct(wSum, 1);
  const wSumSubText = Math.abs((wSum || 0) - 1) <= 1e-4 ? 'Voll investiert (Summe = 100%)' : 'Abweichung von 100%!';

  const eqShare = activeMetrics.equity_share !== undefined
    ? activeMetrics.equity_share
    : (ev && ev.metrics ? ev.metrics.equity_share : null);
  const eqText = fmtPct(eqShare, 1);

  const foreignAfter = activeMetrics.foreign_exposure_after !== undefined
    ? activeMetrics.foreign_exposure_after
    : (ev && ev.metrics ? ev.metrics.foreign_exposure_after : null);
  const eqSubText = `Fremdwährung nach Hedge: ${fmtPct(foreignAfter, 1)}`;

  const kpis = [
    {
      ids: ['kpi_port_vol', 'kpi_curr_port_vol', 'kpi_risk_port_vol', 'kpi_ret_port_vol'],
      dataKpi: 'port_vol',
      val: volText,
      sub: volSubText
    },
    {
      ids: ['kpi_port_unh_vol', 'kpi_curr_port_unh_vol', 'kpi_risk_port_unh_vol', 'kpi_ret_port_unh_vol'],
      dataKpi: 'port_unh_vol',
      val: unhVolText,
      sub: unhVolSubText
    },
    {
      ids: ['kpi_port_wsum', 'kpi_curr_port_wsum', 'kpi_risk_port_wsum', 'kpi_ret_port_wsum'],
      dataKpi: 'port_wsum',
      val: wSumText,
      sub: wSumSubText
    },
    {
      ids: ['kpi_port_equity', 'kpi_curr_port_equity', 'kpi_risk_port_equity', 'kpi_ret_port_equity'],
      dataKpi: 'port_equity',
      val: eqText,
      sub: eqSubText
    }
  ];

  for (const item of kpis) {
    for (const id of item.ids) {
      const el = document.getElementById(id);
      if (el) el.textContent = item.val;
      const subEl = document.getElementById(id + '_sub');
      if (subEl && item.sub !== null && item.sub !== undefined) {
        subEl.textContent = item.sub;
      }
    }
    document.querySelectorAll(`[data-kpi="${item.dataKpi}"]`).forEach(el => {
      el.textContent = item.val;
    });
    if (item.sub !== null && item.sub !== undefined) {
      document.querySelectorAll(`[data-kpi-sub="${item.dataKpi}"]`).forEach(el => {
        el.textContent = item.sub;
      });
    }
  }
}

// ----------------------------------------------------------------------------
// 1. Portfolio Workspace
// ----------------------------------------------------------------------------
export function renderPortfolioTriggers(store, bridge) {
  const st = store.state;
  const ev = st.results.portfolio_evaluation;

  // Update KPI summary cards across all workspaces with user inputs
  updateAllPortfolioKpis(store);

  // Render SAA Portfolio Editor Table
  const tbody = document.getElementById('portfolio_weights_tbody');
  const tfoot = document.getElementById('portfolio_weights_tfoot');
  if (!tbody || !ev || !ev.exposures || !Array.isArray(ev.exposures.asset_table)) return;

  const assetRows = ev.exposures.asset_table;
  const rcMap = {};
  if (Array.isArray(ev.asset_contributions)) {
    for (const r of ev.asset_contributions) {
      rcMap[r.asset_id] = r;
    }
  }

  // Fast in-place DOM update if table rows already exist (avoids destroying active sliders)
  if (tbody.children.length === assetRows.length) {
    let canUpdateInPlace = true;
    for (let i = 0; i < assetRows.length; i++) {
      if (tbody.children[i].getAttribute('data-asset-id') !== assetRows[i].asset_id) {
        canUpdateInPlace = false;
        break;
      }
    }

    if (canUpdateInPlace) {
      for (let i = 0; i < assetRows.length; i++) {
        const row = assetRows[i];
        const aid = row.asset_id;
        const draftW = st.draft_weights[aid] !== undefined ? Number(st.draft_weights[aid]) : Number(row.weight || 0);
        const wPct = Number((draftW * 100).toFixed(2));
        const rc = rcMap[aid] || {};
        const tr = tbody.children[i];

        const sliderInput = tr.querySelector('input.range-slider');
        const numInput = tr.querySelector('input.num-input');
        if (sliderInput && document.activeElement !== sliderInput) {
          sliderInput.value = wPct.toFixed(1);
        }
        if (numInput && document.activeElement !== numInput) {
          numInput.value = wPct.toFixed(1);
        }

        if (tr.children[5]) {
          tr.children[5].textContent = row.is_domestic ? 'Inland' : (row.is_emeq ? 'n/a' : fmtPct(row.applied_h, 0));
        }
        if (tr.children[6]) tr.children[6].textContent = fmtPct(row.hedge_notional, 1);
        if (tr.children[7]) tr.children[7].textContent = fmtPct(rc.hedged_vol_ann, 2);
        if (tr.children[8]) tr.children[8].textContent = fmtPct(rc.risk_contribution_ann, 2);
        if (tr.children[9]) tr.children[9].textContent = fmtPct(rc.pct_risk_contribution, 1);
      }
      updateWeightFooter(st, tfoot, ev);
      return;
    }
  }

  const trList = assetRows.map(row => {
    const aid = row.asset_id;
    const draftW = st.draft_weights[aid] !== undefined ? Number(st.draft_weights[aid]) : Number(row.weight || 0);
    const wPct = Number((draftW * 100).toFixed(2));
    const rc = rcMap[aid] || {};

    const numInput = el('input', {
      type: 'number',
      className: 'form-control num-input',
      min: '0',
      max: '100',
      step: '0.5',
      value: wPct.toFixed(1),
      'aria-label': `Gewicht für ${row.name} in Prozent`,
      onInput: e => {
        const v = Math.max(0, Math.min(100, Number(e.target.value || 0))) / 100.0;
        st.draft_weights[aid] = v;
        sliderInput.value = (v * 100).toFixed(1);
        updateWeightFooter(st);
        bridge.sendDebounced('portfolio.evaluate', {
          portfolio_id: st.active_portfolio_id,
          weights: st.draft_weights,
          hedge_config: st.draft_hedge_config,
          cost_bps_pa: st.cost_bps_pa
        });
      }
    });

    const sliderInput = el('input', {
      type: 'range',
      className: 'range-slider',
      min: '0',
      max: '60',
      step: '0.5',
      value: wPct.toFixed(1),
      'aria-label': `Slider Gewicht ${row.name}`,
      onInput: e => {
        const v = Number(e.target.value || 0) / 100.0;
        st.draft_weights[aid] = v;
        numInput.value = (v * 100).toFixed(1);
        updateWeightFooter(st);
        bridge.sendDebounced('portfolio.evaluate', {
          portfolio_id: st.active_portfolio_id,
          weights: st.draft_weights,
          hedge_config: st.draft_hedge_config,
          cost_bps_pa: st.cost_bps_pa
        });
      }
    });

    const acClass = getAssetClassClass(row.asset_class);
    const ccyTagClass = row.fx_exposure === 'EMEQ' ? 'tag tag-emeq' : (row.is_domestic ? 'tag tag-domestic' : 'tag tag-ccy');

    return el('tr', { className: `asset-row asset-row-${acClass}`, 'data-asset-id': aid }, [
      el('td', { className: 'sticky-col', style: { fontFamily: 'var(--font-mono)', fontWeight: '600' }, textContent: aid }),
      el('td', { textContent: row.name }),
      el('td', {}, el('span', { className: `tag tag-asset tag-asset-${acClass}`, textContent: row.asset_class })),
      el('td', {}, el('span', { className: ccyTagClass, textContent: row.fx_exposure })),
      el('td', { className: 'num' }, [sliderInput, ' ', numInput]),
      el('td', { className: 'num', textContent: row.is_domestic ? 'Inland' : (row.is_emeq ? 'n/a' : fmtPct(row.applied_h, 0)) }),
      el('td', { className: 'num', textContent: fmtPct(row.hedge_notional, 1) }),
      el('td', { className: 'num', textContent: fmtPct(rc.hedged_vol_ann, 2) }),
      el('td', { className: 'num', textContent: fmtPct(rc.risk_contribution_ann, 2) }),
      el('td', { className: 'num', textContent: fmtPct(rc.pct_risk_contribution, 1) })
    ]);
  });

  clearAndAppend(tbody, trList);
  updateWeightFooter(st, tfoot, ev);
}

export function updateWeightFooter(st, tfootNode = document.getElementById('portfolio_weights_tfoot'), ev = st.results.portfolio_evaluation) {
  if (!tfootNode) return;
  const wSum = Object.values(st.draft_weights || {}).reduce((a, b) => a + Number(b || 0), 0);
  const wSumText = fmtPct(wSum, 1);
  const subText = Math.abs(wSum - 1.0) <= 1e-4 ? 'Voll investiert (Summe = 100%)' : 'Abweichung von 100%!';

  const idList = ['kpi_port_wsum', 'kpi_curr_port_wsum', 'kpi_risk_port_wsum', 'kpi_ret_port_wsum'];
  for (const id of idList) {
    const node = document.getElementById(id);
    if (node) node.textContent = wSumText;
    const subNode = document.getElementById(id + '_sub');
    if (subNode) subNode.textContent = subText;
  }
  document.querySelectorAll('[data-kpi="port_wsum"]').forEach(el => {
    el.textContent = wSumText;
  });
  document.querySelectorAll('[data-kpi-sub="port_wsum"]').forEach(el => {
    el.textContent = subText;
  });

  const tr = el('tr', {}, [
    el('td', { className: 'sticky-col', textContent: 'SUMME' }),
    el('td', { textContent: 'Gesamtportfolio (Summe(w) = 100%, Hedge g separat)' }),
    el('td', { textContent: '' }),
    el('td', { textContent: '' }),
    el('td', {
      className: 'num',
      style: { color: Math.abs(wSum - 1.0) > 1e-4 ? 'var(--status-dirty-text)' : 'var(--status-ok-text)' },
      textContent: fmtPct(wSum, 2)
    }),
    el('td', { className: 'num', textContent: '' }),
    el('td', { className: 'num', textContent: fmtPct(ev?.exposures?.total_hedge_notional, 1) }),
    el('td', { className: 'num', textContent: fmtPct(ev?.metrics?.vol_annualized, 2) }),
    el('td', { className: 'num', textContent: fmtPct(ev?.metrics?.vol_annualized, 2) }),
    el('td', { className: 'num', textContent: '100.0 %' })
  ]);
  clearAndAppend(tfootNode, tr);
}

// ----------------------------------------------------------------------------
// 2. Currencies & Hedge Engine Workspace
// ----------------------------------------------------------------------------
export function renderCurrenciesWorkspace(store, bridge) {
  updateAllPortfolioKpis(store);
  const st = store.state;
  const ev = st.results.portfolio_evaluation;
  if (!ev || !ev.exposures) return;

  const exp = ev.exposures;
  const mode = st.hedge_mode || exp.active_mode || 'ASSET_CLASS';

  const modeBtnClass = document.getElementById('btn_hedge_mode_class');
  const modeBtnOverlay = document.getElementById('btn_hedge_mode_overlay');
  if (modeBtnClass) modeBtnClass.classList.toggle('active', mode === 'ASSET_CLASS');
  if (modeBtnOverlay) modeBtnOverlay.classList.toggle('active', mode === 'CURRENCY_OVERLAY');

  const noteEl = document.getElementById('hedge_attribution_note');
  if (noteEl) {
    noteEl.textContent = exp.allocation_attribution_note || '';
  }

  // Central Currency Exposure Table (Section 7 requirement)
  const ccyTbody = document.getElementById('currency_exposure_tbody');
  if (ccyTbody && Array.isArray(exp.currency_table)) {
    const ccyRows = exp.currency_table.map(row => {
      const ccy = row.fx_exposure;
      const isDom = Boolean(row.is_domestic);
      const isEmeq = Boolean(row.is_emeq);
      const fwdOk = Boolean(row.forward_available);
      const hedgeable = Number(row.hedgeable_exposure || 0);

      let statusBadge;
      if (isDom) {
        statusBadge = el('span', { className: 'tag tag-domestic', textContent: `Inland (${st.base_currency})` });
      } else if (isEmeq) {
        statusBadge = el('span', { className: 'tag tag-emeq', textContent: 'EMEQ (Nicht hedgefähig)' });
      } else if (!fwdOk) {
        statusBadge = el('span', { className: 'tag tag-locked', textContent: '1M-Forward fehlt (Gesperrt)' });
      } else {
        statusBadge = el('span', { className: 'tag tag-ok', textContent: '1M-Forward verfügbar' });
      }

      let ratioCell;
      if (mode === 'CURRENCY_OVERLAY' && !isDom && !isEmeq && fwdOk && hedgeable > 1e-6) {
        const currVal = Number(
          st.draft_hedge_config?.currency_ratios?.[ccy] ??
          (row.effective_hedge_ratio !== null ? row.effective_hedge_ratio : 0)
        );
        const inp = el('input', {
          type: 'number',
          className: 'form-control num-input',
          min: '0',
          max: '100',
          step: '5',
          value: (currVal * 100).toFixed(0),
          'aria-label': `Overlay-Hedgequote für ${ccy}`,
          onInput: e => {
            const hVal = Math.max(0, Math.min(100, Number(e.target.value || 0))) / 100.0;
            if (!st.draft_hedge_config || st.draft_hedge_config.mode !== 'CURRENCY_OVERLAY') {
              st.draft_hedge_config = { mode: 'CURRENCY_OVERLAY', currency_ratios: { ...(exp.derived_currency_ratios || {}) } };
            }
            st.draft_hedge_config.currency_ratios[ccy] = hVal;
            bridge.sendDebounced('portfolio.evaluate', {
              portfolio_id: st.active_portfolio_id,
              weights: st.draft_weights,
              hedge_config: st.draft_hedge_config,
              cost_bps_pa: st.cost_bps_pa
            });
          }
        });
        ratioCell = el('td', { className: 'num' }, [inp, ' %']);
      } else {
        const txt = isDom ? 'Inland (0%)' : (isEmeq || hedgeable <= 1e-9 ? 'n/a' : fmtPct(row.effective_hedge_ratio, 1));
        ratioCell = el('td', { className: 'num', textContent: txt });
      }

      const overlayNote = Number(row.contractual_overlay_exposure || 0) > 0
        ? el('span', { className: 'tag tag-overlay', style: { marginLeft: '6px' }, textContent: `inkl. ${fmtPct(row.contractual_overlay_exposure, 1)} Futures-Nominal` })
        : null;

      return el('tr', {}, [
        el('td', { className: 'sticky-col', style: { fontWeight: '700', fontFamily: 'var(--font-mono)' } }, [ccy, overlayNote]),
        el('td', { className: 'num', textContent: fmtPct(row.exposure_before_hedge, 2) }),
        el('td', { className: 'num', textContent: fmtPct(row.hedgeable_exposure, 2) }),
        el('td', { className: 'num', textContent: fmtPct(row.non_hedgeable_exposure, 2) }),
        ratioCell,
        el('td', { className: 'num', textContent: fmtPct(row.hedge_notional, 2) }),
        el('td', { className: 'num', style: { fontWeight: '600' }, textContent: fmtPct(row.exposure_after_hedge, 2) }),
        el('td', {}, statusBadge)
      ]);
    });
    clearAndAppend(ccyTbody, ccyRows);
  }

  // Asset-Class Hedge Controls Table
  const classTbody = document.getElementById('asset_class_hedge_tbody');
  if (classTbody && Array.isArray(exp.derived_class_table)) {
    const classRows = exp.derived_class_table.map(row => {
      const ac = row.asset_class;
      const hWeight = Number(row.hedgeable_weight || 0);
      let controlCell;

      if (mode === 'ASSET_CLASS') {
        const currRatio = Number(
          st.draft_hedge_config?.class_ratios?.[ac] ??
          st.hedge_configs?.ASSET_CLASS?.class_ratios?.[ac] ?? 0
        );
        const inp = el('input', {
          type: 'number',
          className: 'form-control num-input',
          min: '0',
          max: '100',
          step: '5',
          value: (currRatio * 100).toFixed(0),
          'aria-label': `Klassen-Hedgequote für ${ac}`,
          onInput: e => {
            const hVal = Math.max(0, Math.min(100, Number(e.target.value || 0))) / 100.0;
            if (!st.draft_hedge_config || st.draft_hedge_config.mode !== 'ASSET_CLASS') {
              st.draft_hedge_config = {
                mode: 'ASSET_CLASS',
                class_ratios: { ...(st.hedge_configs?.ASSET_CLASS?.class_ratios || {}) },
                asset_overrides: { ...(st.hedge_configs?.ASSET_CLASS?.asset_overrides || {}) }
              };
            }
            st.draft_hedge_config.class_ratios[ac] = hVal;
            bridge.sendDebounced('portfolio.evaluate', {
              portfolio_id: st.active_portfolio_id,
              weights: st.draft_weights,
              hedge_config: st.draft_hedge_config,
              cost_bps_pa: st.cost_bps_pa
            });
          }
        });
        controlCell = el('td', { className: 'num' }, [inp, ' %']);
      } else {
        controlCell = el('td', {
          className: 'num',
          textContent: hWeight > 1e-9 ? `${fmtPct(row.effective_class_ratio, 1)} (abgeleitet)` : 'n/a'
        });
      }

      const acClass = getAssetClassClass(ac);
      return el('tr', { className: `asset-row asset-row-${acClass}` }, [
        el('td', { className: 'sticky-col' }, el('span', { className: `tag tag-asset tag-asset-${acClass}`, style: { fontWeight: '700' }, textContent: ac })),
        el('td', { className: 'num', textContent: fmtPct(row.total_weight, 2) }),
        el('td', { className: 'num', textContent: fmtPct(row.hedgeable_weight, 2) }),
        controlCell,
        el('td', { className: 'num', textContent: fmtPct(row.hedge_notional, 2) })
      ]);
    });
    clearAndAppend(classTbody, classRows);
  }
}

// ----------------------------------------------------------------------------
// 3. Risk & Covariance Diagnostics Workspace
// ----------------------------------------------------------------------------
export function renderRiskWorkspace(store) {
  updateAllPortfolioKpis(store);
  const st = store.state;
  const cm = st.covariance_models.active;
  const ev = st.results.portfolio_evaluation;
  if (!cm) return;

  const setDiag = (id, val) => {
    const n = document.getElementById(id);
    if (n) n.textContent = val;
  };

  setDiag('risk_diag_estimator', `${cm.estimator} (${cm.model_id})`);
  setDiag('risk_diag_sample', `${cm.n_obs} Monate (${cm.start_month} bis ${cm.end_month})`);
  setDiag('risk_diag_shrinkage', `${fmtPct(cm.shrinkage_intensity, 1)} | Target: ${cm.target}`);
  setDiag(
    'risk_diag_psd',
    `${cm.diagnostics?.is_psd ? 'PSD Ja' : 'Repariert'} | Kondition: ${fmtNum(cm.diagnostics?.condition_number, 1)} | Min EW: ${Number(cm.diagnostics?.min_eigenvalue || 0).toExponential(2)}`
  );

  // Render Component / Asset Risk Contributions Table
  const rcTbody = document.getElementById('risk_contributions_tbody');
  if (rcTbody && ev) {
    const level = st.ui_filters.risk_decomp_level || 'ASSET';
    if (level === 'COMPONENT' && Array.isArray(ev.component_contributions)) {
      const rows = ev.component_contributions.map(r =>
        el('tr', {}, [
          el('td', { className: 'sticky-col', style: { fontFamily: 'var(--font-mono)', fontWeight: '600' }, textContent: r.component_id }),
          el('td', { textContent: r.component_type }),
          el('td', { className: 'num', textContent: fmtPct(r.exposure, 2) }),
          el('td', { className: 'num', textContent: fmtPct(r.standalone_vol_ann, 2) }),
          el('td', { className: 'num', textContent: fmtPct(r.marginal_risk_ann, 2) }),
          el('td', {
            className: 'num',
            style: { color: Number(r.risk_contribution_ann) < 0 ? 'var(--accent-secondary)' : 'inherit', fontWeight: '600' },
            textContent: fmtPct(r.risk_contribution_ann, 3)
          }),
          el('td', { className: 'num', textContent: fmtPct(r.pct_risk_contribution, 1) })
        ])
      );
      clearAndAppend(rcTbody, rows);
    } else if (Array.isArray(ev.asset_contributions)) {
      const rows = ev.asset_contributions.map(r => {
        const acClass = getAssetClassClass(r.asset_class);
        return el('tr', { className: `asset-row asset-row-${acClass}` }, [
          el('td', { className: 'sticky-col', style: { fontFamily: 'var(--font-mono)', fontWeight: '600' }, textContent: r.asset_id }),
          el('td', {}, [
            el('span', { textContent: `${r.name} ` }),
            r.asset_class ? el('span', { className: `tag tag-asset tag-asset-${acClass}`, textContent: r.asset_class }) : null,
            ' ',
            r.fx_exposure ? el('span', { className: 'tag tag-ccy', textContent: r.fx_exposure }) : null
          ].filter(Boolean)),
          el('td', { className: 'num', textContent: fmtPct(r.weight, 2) }),
          el('td', { className: 'num', textContent: `${fmtPct(r.hedged_vol_ann, 2)} (unges.: ${fmtPct(r.unhedged_vol_ann, 2)})` }),
          el('td', { className: 'num', textContent: fmtPct(r.marginal_risk_ann, 2) }),
          el('td', {
            className: 'num',
            style: { color: Number(r.risk_contribution_ann) < 0 ? 'var(--accent-secondary)' : 'inherit', fontWeight: '600' },
            textContent: fmtPct(r.risk_contribution_ann, 3)
          }),
          el('td', { className: 'num', textContent: fmtPct(r.pct_risk_contribution, 1) })
        ]);
      });
      clearAndAppend(rcTbody, rows);
    }
  }

  // Render Accessible Numeric Matrix Table (Correlation or Annualized Covariance)
  const matThead = document.getElementById('matrix_numeric_thead');
  const matTbody = document.getElementById('matrix_numeric_tbody');
  if (matThead && matTbody && cm.correlation && cm.sigma_annualized) {
    const viewMode = st.ui_filters.matrix_view_mode || 'HEATMAP';
    if (viewMode === 'HEATMAP') {
      // Skip constructing 225 DOM table cells while Heatmap mode is active
      if (matThead.children.length > 0) matThead.replaceChildren();
      if (matTbody.children.length > 0) matTbody.replaceChildren();
      return;
    }

    const isCov = viewMode === 'TABLE_COV';
    const matObj = isCov ? cm.sigma_annualized : cm.correlation;
    const ids = matObj.component_ids || [];

    const headRow = el('tr', {}, [
      el('th', { className: 'sticky-col', textContent: isCov ? 'Kovarianz p.a. (%²)' : 'Korrelation' }),
      ...ids.map(id => el('th', { className: 'num', textContent: id }))
    ]);
    clearAndAppend(matThead, headRow);

    const bodyRows = ids.map((rowId, i) => {
      const rVals = matObj.rows[i] || [];
      return el('tr', {}, [
        el('td', { className: 'sticky-col', style: { fontFamily: 'var(--font-mono)', fontWeight: '600' }, textContent: rowId }),
        ...rVals.map(v => el('td', {
          className: 'num',
          textContent: isCov ? fmtNum(Number(v) * 10000, 2) : fmtNum(v, 2)
        }))
      ]);
    });
    clearAndAppend(matTbody, bodyRows);
  }
}

// ----------------------------------------------------------------------------
// 4. Forward-Looking 5Y Return Scenarios Workspace
// ----------------------------------------------------------------------------
export function renderScenariosWorkspace(store, bridge) {
  updateAllPortfolioKpis(store);
  const st = store.state;
  const scEval = st.results.scenario_evaluation;
  if (!scEval) return;

  const setTxt = (id, val) => {
    const n = document.getElementById(id);
    if (n) n.textContent = val;
  };

  setTxt('scen_kpi_cagr_hedged', fmtPct(scEval.metrics?.scenario_cagr_hedged_5y, 2));
  setTxt('scen_kpi_cagr_unhedged', fmtPct(scEval.metrics?.scenario_cagr_unhedged_5y, 2));
  setTxt('scen_kpi_cagr_local', fmtPct(scEval.metrics?.scenario_cagr_local_5y, 2));
  setTxt('scen_kpi_hedge_effect', fmtPct(scEval.metrics?.scenario_hedge_effect_pa, 2));
  setTxt('scen_uncertainty_note', scEval.uncertainty_description || '');

  const tbody = document.getElementById('scenario_assets_tbody');
  if (tbody && Array.isArray(scEval.asset_table)) {
    const rows = scEval.asset_table.map(row => {
      const aid = row.asset_id;
      const locPct = Number((Number(row.cagr_local_5y || 0) * 100).toFixed(2));

      const editInput = el('input', {
        type: 'number',
        className: 'form-control num-input',
        step: '0.1',
        min: '-25',
        max: '35',
        value: locPct.toFixed(2),
        'aria-label': `Lokale 5J-Renditeannahme p.a. für ${row.name}`,
        onChange: e => {
          const newRate = Number(e.target.value || 0) / 100.0;
          bridge.sendCommand('scenarios.evaluate', {
            scenario_id: st.active_scenario_id,
            rebalancing_rule: st.rebalancing_rule,
            asset_return_overrides: { [aid]: newRate }
          });
        }
      });

      const acClass = getAssetClassClass(row.asset_class);
      return el('tr', { className: `asset-row asset-row-${acClass}` }, [
        el('td', { className: 'sticky-col', style: { fontFamily: 'var(--font-mono)', fontWeight: '600' }, textContent: aid }),
        el('td', {}, [
          el('span', { textContent: `${row.name} ` }),
          row.asset_class ? el('span', { className: `tag tag-asset tag-asset-${acClass}`, textContent: row.asset_class }) : null
        ].filter(Boolean)),
        el('td', {}, el('span', { className: 'tag tag-ccy', textContent: `${row.return_currency} / ${row.fx_exposure}` })),
        el('td', { className: 'num', textContent: fmtPct(row.weight, 1) }),
        el('td', { className: 'num' }, [editInput, ' %']),
        el('td', { className: 'num', textContent: fmtPct(row.cagr_unhedged_base_5y, 2) }),
        el('td', { className: 'num', textContent: fmtPct(row.avg_forward_carry_pa, 2) }),
        el('td', { className: 'num', style: { fontWeight: '700' }, textContent: fmtPct(row.cagr_hedged_base_5y, 2) })
      ]);
    });
    clearAndAppend(tbody, rows);
  }
}

// ----------------------------------------------------------------------------
// 5. Multi-SAA & Multi-Model Comparison Workspace
// ----------------------------------------------------------------------------
export function renderComparisonWorkspace(store) {
  const st = store.state;
  const comp = st.results.comparison;
  if (!comp) return;

  const sumTbody = document.getElementById('comparison_summary_tbody');
  if (sumTbody && Array.isArray(comp.summary_table)) {
    const rows = comp.summary_table.map(r =>
      el('tr', {}, [
        el('td', { className: 'sticky-col', style: { fontWeight: '600' }, textContent: r.portfolio_name }),
        el('td', { className: 'num', style: { fontFamily: 'var(--font-mono)' }, textContent: r.config_hash }),
        el('td', { className: 'num', textContent: fmtPct(r.equity_share, 1) }),
        el('td', { className: 'num', textContent: fmtPct(r.foreign_before, 1) }),
        el('td', { className: 'num', textContent: fmtPct(r.hedge_notional, 1) }),
        el('td', { className: 'num', textContent: fmtPct(r.foreign_after, 1) }),
        el('td', { className: 'num', textContent: fmtPct(r.vol_unhedged_ann, 2) }),
        el('td', { className: 'num', style: { fontWeight: '700' }, textContent: fmtPct(r.vol_hedged_ann, 2) }),
        el('td', { className: 'num', textContent: fmtPct(r.hedge_vol_effect_ann, 2) }),
        el('td', { className: 'num', textContent: fmtNum(r.diversification_ratio, 2) }),
        el('td', { className: 'num', style: { fontWeight: '600' }, textContent: fmtPct(r.tracking_error_vs_bench_ann, 2) })
      ])
    );
    clearAndAppend(sumTbody, rows);
  }

  // Pairwise Tracking Error Matrix
  const teThead = document.getElementById('comparison_te_thead');
  const teTbody = document.getElementById('comparison_te_tbody');
  if (teThead && teTbody && comp.pairwise_tracking_error) {
    const pIds = comp.pairwise_tracking_error.portfolio_ids || [];
    const pRows = comp.pairwise_tracking_error.rows || [];

    clearAndAppend(teThead, el('tr', {}, [
      el('th', { className: 'sticky-col', textContent: 'Tracking Error p.a.' }),
      ...pIds.map(id => el('th', { className: 'num', textContent: id }))
    ]));

    const body = pIds.map((pid, i) =>
      el('tr', {}, [
        el('td', { className: 'sticky-col', style: { fontFamily: 'var(--font-mono)', fontWeight: '600' }, textContent: pid }),
        ...(pRows[i] || []).map(v => el('td', { className: 'num', textContent: fmtPct(v, 2) }))
      ])
    );
    clearAndAppend(teTbody, body);
  }
}

// ----------------------------------------------------------------------------
// 6. Data, Coverage, Factor/Forward Audit & Methodology Workspace
// ----------------------------------------------------------------------------
export function renderDataWorkspace(store) {
  const st = store.state;
  const ds = st.dataset;
  if (!ds) return;

  const setTxt = (id, val) => {
    const n = document.getElementById(id);
    if (n) n.textContent = val;
  };

  setTxt('data_info_source', `${ds.source_path} (${ds.is_private_upload ? 'Privater Upload' : 'Hausdatensatz'})`);
  setTxt('data_info_hashes', `Dataset-ID: ${ds.dataset_id} | Datei-Hash: ${ds.file_hash}`);
  if (st.cache_stats) {
    setTxt(
      'data_info_cache',
      `Kovarianz-Schätzungen: ${st.cache_stats.covariance_estimations} | Cache-Hits: ${st.cache_stats.cache_hits} | Misses: ${st.cache_stats.cache_misses}`
    );
  }

  // Coverage & Metadata Table with client-side filter (never triggers R calculation!)
  const covTbody = document.getElementById('data_coverage_tbody');
  if (covTbody && Array.isArray(ds.metadata)) {
    const q = (st.ui_filters.coverage_search || '').toLowerCase();
    const tFilter = st.ui_filters.coverage_type || 'ALL';

    const filtered = ds.metadata.filter(m => {
      if (tFilter !== 'ALL' && m.Series_Type !== tFilter) return false;
      if (q && !(`${m.Code} ${m.Name} ${m.Series_Type}`.toLowerCase().includes(q))) return false;
      return true;
    });

    const rows = filtered.map(m => {
      const gaps = Number(m.Interior_Gaps || 0);
      return el('tr', {}, [
        el('td', { className: 'sticky-col', style: { fontFamily: 'var(--font-mono)', fontWeight: '600' }, textContent: m.Code }),
        el('td', { textContent: m.Name }),
        el('td', {}, el('span', { className: 'tag', textContent: m.Series_Type })),
        el('td', {}, el('span', { className: m.Active ? 'tag tag-ok' : 'tag tag-domestic', textContent: m.Active ? 'Aktiv' : 'Inaktiv' })),
        el('td', { textContent: m.Return_Currency || m.Foreign_Currency || m.Cash_Currency || '-' }),
        el('td', { textContent: m.FX_Exposure || m.Quote_Direction || m.Proxy_Type || '-' }),
        el('td', { className: 'num', textContent: m.Start_Month || '-' }),
        el('td', { className: 'num', textContent: m.End_Month || '-' }),
        el('td', { className: 'num', textContent: String(m.Obs_Count ?? 0) }),
        el('td', {
          className: 'num',
          style: { color: gaps > 0 ? 'var(--status-error-text)' : 'var(--status-ok-text)', fontWeight: '600' },
          textContent: String(gaps)
        }),
        el('td', { textContent: m.Review_Status || '-' })
      ]);
    });
    clearAndAppend(covTbody, rows);
  }

  // FX Forward Carry vs Spot Offset Audit Table
  const fxAuditTbody = document.getElementById('fx_audit_tbody');
  if (fxAuditTbody && Array.isArray(ds.fx_audit_summary)) {
    const rows = ds.fx_audit_summary.map(r =>
      el('tr', {}, [
        el('td', { className: 'sticky-col', style: { fontFamily: 'var(--font-mono)', fontWeight: '600' }, textContent: r.component_id }),
        el('td', { textContent: `${r.currency} -> ${r.base_currency}` }),
        el('td', { className: 'num', textContent: `${r.obs} (${r.start_month}..${r.end_month})` }),
        el('td', { className: 'num', textContent: fmtPct(r.mean_carry_pa, 2) }),
        el('td', { className: 'num', textContent: fmtPct(r.mean_spot_offset_pa, 2) }),
        el('td', { className: 'num', style: { fontWeight: '600' }, textContent: fmtPct(r.mean_z_net_pa, 2) }),
        el('td', { className: 'num', textContent: fmtPct(r.vol_z_ann, 2) })
      ])
    );
    clearAndAppend(fxAuditTbody, rows);
  }

  // Factor Moment Reconstruction Audit Table (if available)
  const facAuditTbody = document.getElementById('factor_audit_tbody');
  const cm = st.covariance_models.active;
  if (facAuditTbody && cm) {
    const fRows = Array.isArray(cm.factor_audit) ? cm.factor_audit : [];
    if (fRows.length === 0) {
      clearAndAppend(facAuditTbody, el('tr', {}, [
        el('td', { colSpan: '7', style: { color: 'var(--text-muted)' }, textContent: 'Wählen Sie das Modell "Faktor-Momentenmodell (Kurze Historien)", um die Donor-Regressionen anzuzeigen.' })
      ]));
    } else {
      clearAndAppend(facAuditTbody, fRows.map(r =>
        el('tr', {}, [
          el('td', { className: 'sticky-col', style: { fontFamily: 'var(--font-mono)', fontWeight: '600' }, textContent: r.series_code }),
          el('td', { className: 'num', textContent: `${r.true_obs} (echt) / ${r.donor_obs} (Donor)` }),
          el('td', { className: 'num', textContent: `${r.overlap_start}..${r.overlap_end}` }),
          el('td', { textContent: r.donors }),
          el('td', { style: { fontFamily: 'var(--font-mono)', fontSize: '11px' }, textContent: r.betas }),
          el('td', { className: 'num', textContent: fmtPct(r.r_squared, 1) }),
          el('td', { className: 'num', textContent: fmtPct(r.residual_vol_ann, 2) })
        ])
      ));
    }
  }

  // Walk-Forward & Bootstrap Validation Results (if run)
  const valBox = document.getElementById('validation_results_tbody');
  if (valBox && st.results.validation) {
    const v = st.results.validation;
    const wf = Array.isArray(v.walk_forward?.summary) ? v.walk_forward.summary : [];
    const bootQ = v.bootstrap?.vol_quantiles;

    const rows = wf.map(r =>
      el('tr', {}, [
        el('td', { className: 'sticky-col', style: { fontWeight: '600' }, textContent: r.method }),
        el('td', { className: 'num', textContent: String(r.n_windows) }),
        el('td', { className: 'num', textContent: fmtPct(r.mean_predicted_vol, 2) }),
        el('td', { className: 'num', textContent: fmtPct(r.mean_realized_vol, 2) }),
        el('td', { className: 'num', textContent: fmtPct(r.bias, 2) }),
        el('td', { className: 'num', textContent: fmtPct(r.mae, 2) }),
        el('td', { className: 'num', style: { fontWeight: '600' }, textContent: fmtPct(r.rmse, 2) })
      ])
    );
    clearAndAppend(valBox, rows);

    const bootNote = document.getElementById('bootstrap_band_summary');
    if (bootNote && bootQ) {
      bootNote.textContent = `Block-Bootstrap (${v.bootstrap.n_boot} Replikationen, Blocklänge ${v.bootstrap.block_length}M, Seed ${v.bootstrap.seed}): 5%-Quantil ${fmtPct(bootQ.p05, 2)} | Median ${fmtPct(bootQ.p50, 2)} | 95%-Quantil ${fmtPct(bootQ.p95, 2)}`;
    }
  }
}
