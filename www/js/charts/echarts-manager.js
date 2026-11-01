// ============================================================================
// www/js/charts/echarts-manager.js
// Apache ECharts instance manager with ResizeObserver, on-demand heatmap
// rendering, SVG renderer (Citrix VDI optimized), and zero-animation tooltips
// ============================================================================

function citrixSafeTooltip(overrides = {}) {
  return {
    transitionDuration: 0,
    confine: true,
    renderMode: 'html',
    enterable: false,
    hideDelay: 40,
    ...overrides
  };
}

export class EChartsManager {
  constructor() {
    this.instances = new Map();
    this.resizeObserver = null;
    this.rafResize = null;

    if (typeof ResizeObserver !== 'undefined') {
      this.resizeObserver = new ResizeObserver(entries => {
        if (this.rafResize) cancelAnimationFrame(this.rafResize);
        this.rafResize = requestAnimationFrame(() => {
          for (const entry of entries) {
            const id = entry.target.id;
            const inst = this.instances.get(id);
            if (inst && entry.contentRect.width > 0 && entry.contentRect.height > 0) {
              inst.resize();
            }
          }
        });
      });
    }
  }

  getOrCreate(containerId) {
    const el = document.getElementById(containerId);
    if (!el || typeof window.echarts === 'undefined') return null;

    // Only initialize when container is actually visible in layout
    if (el.offsetParent === null && el.clientWidth === 0) {
      return null;
    }

    let inst = this.instances.get(containerId);
    if (inst && inst.getDom() !== el) {
      inst.dispose();
      this.instances.delete(containerId);
      inst = null;
    }

    if (!inst) {
      // Citrix VDI optimization: use SVG renderer to avoid host CPU software rasterization
      inst = window.echarts.init(el, null, { renderer: 'svg' });
      this.instances.set(containerId, inst);
      if (this.resizeObserver) {
        this.resizeObserver.observe(el);
      }
    }
    return inst;
  }

  resizeAllVisible() {
    for (const [id, inst] of this.instances.entries()) {
      const el = document.getElementById(id);
      if (el && el.offsetParent !== null) {
        inst.resize();
      }
    }
  }

  disposeAll() {
    if (this.rafResize) {
      cancelAnimationFrame(this.rafResize);
      this.rafResize = null;
    }
    if (this.resizeObserver) {
      this.resizeObserver.disconnect();
    }
    for (const inst of this.instances.values()) {
      inst.dispose();
    }
    this.instances.clear();
  }

  renderPortfolioWeightsChart(containerId, assetTable) {
    const inst = this.getOrCreate(containerId);
    if (!inst || !Array.isArray(assetTable)) return;

    const sorted = [...assetTable].filter(r => Number(r.weight || 0) > 0.0001);
    const labels = sorted.map(r => r.asset_id);
    const weightsPct = sorted.map(r => Number((Number(r.weight || 0) * 100).toFixed(2)));
    const postHedgePct = sorted.map(r => Number((Number(r.post_hedge_exposure || 0) * 100).toFixed(2)));

    inst.setOption({
      animation: false,
      tooltip: citrixSafeTooltip({ trigger: 'axis', axisPointer: { type: 'shadow' } }),
      legend: { data: ['Kapitalgewicht (%)', 'Netto-Exposure nach Hedge (%)'], top: 4, textStyle: { fontSize: 11 } },
      grid: { left: 105, right: 24, top: 34, bottom: 26 },
      xAxis: { type: 'value', axisLabel: { formatter: '{value}%', fontSize: 11 } },
      yAxis: { type: 'category', data: labels, inverse: true, axisLabel: { fontSize: 11, fontFamily: 'monospace' } },
      series: [
        {
          name: 'Kapitalgewicht (%)',
          type: 'bar',
          data: weightsPct,
          itemStyle: { color: '#1d4ed8' }
        },
        {
          name: 'Netto-Exposure nach Hedge (%)',
          type: 'bar',
          data: postHedgePct,
          itemStyle: { color: '#0f766e' }
        }
      ]
    }, false);
  }

  renderHistoricalPathChart(containerId, histPath, baseCcy) {
    const inst = this.getOrCreate(containerId);
    if (!inst || !histPath) return;

    let months, wHedged, wUnhedged;
    if (Array.isArray(histPath.month_keys)) {
      months = histPath.month_keys;
      wHedged = Array.isArray(histPath.wealth_hedged) ? histPath.wealth_hedged : [];
      wUnhedged = Array.isArray(histPath.wealth_unhedged) ? histPath.wealth_unhedged : [];
    } else if (Array.isArray(histPath.series)) {
      months = histPath.series.map(d => d.month_key);
      wHedged = histPath.series.map(d => Number(Number(d.wealth_hedged || 100).toFixed(2)));
      wUnhedged = histPath.series.map(d => Number(Number(d.wealth_unhedged || 100).toFixed(2)));
    } else {
      return;
    }

    inst.setOption({
      animation: false,
      tooltip: citrixSafeTooltip({ trigger: 'axis' }),
      legend: {
        data: [`Mit Hedge (${baseCcy})`, `Ungesichert (${baseCcy})`],
        top: 4,
        textStyle: { fontSize: 11 }
      },
      grid: { left: 52, right: 20, top: 34, bottom: 30 },
      xAxis: { type: 'category', data: months, axisLabel: { fontSize: 11 } },
      yAxis: { type: 'value', scale: true, axisLabel: { fontSize: 11 } },
      series: [
        {
          name: `Mit Hedge (${baseCcy})`,
          type: 'line',
          showSymbol: false,
          sampling: 'lttb',
          data: wHedged,
          lineStyle: { width: 2, color: '#1d4ed8' },
          itemStyle: { color: '#1d4ed8' }
        },
        {
          name: `Ungesichert (${baseCcy})`,
          type: 'line',
          showSymbol: false,
          sampling: 'lttb',
          data: wUnhedged,
          lineStyle: { width: 1.5, type: 'dashed', color: '#64748b' },
          itemStyle: { color: '#64748b' }
        }
      ]
    }, false);
  }

  renderCurrencyExposureChart(containerId, currencyTable) {
    const inst = this.getOrCreate(containerId);
    if (!inst || !Array.isArray(currencyTable)) return;

    const ccys = currencyTable.map(r => r.fx_exposure);
    const beforeVals = currencyTable.map(r => Number((Number(r.exposure_before_hedge || 0) * 100).toFixed(2)));
    const hedgeVals = currencyTable.map(r => Number((Number(r.hedge_notional || 0) * 100).toFixed(2)));
    const afterVals = currencyTable.map(r => Number((Number(r.exposure_after_hedge || 0) * 100).toFixed(2)));

    inst.setOption({
      animation: false,
      tooltip: citrixSafeTooltip({ trigger: 'axis', axisPointer: { type: 'shadow' } }),
      legend: {
        data: ['Exposure vor Hedge (%)', 'Hedge-Nominal g (%)', 'Exposure nach Hedge (%)'],
        top: 4,
        textStyle: { fontSize: 11 }
      },
      grid: { left: 50, right: 20, top: 36, bottom: 28 },
      xAxis: { type: 'category', data: ccys, axisLabel: { fontSize: 11, fontWeight: 'bold' } },
      yAxis: { type: 'value', axisLabel: { formatter: '{value}%', fontSize: 11 } },
      series: [
        { name: 'Exposure vor Hedge (%)', type: 'bar', data: beforeVals, itemStyle: { color: '#3b82f6' } },
        { name: 'Hedge-Nominal g (%)', type: 'bar', data: hedgeVals, itemStyle: { color: '#f59e0b' } },
        { name: 'Exposure nach Hedge (%)', type: 'bar', data: afterVals, itemStyle: { color: '#0f766e' } }
      ]
    }, false);
  }

  renderCorrelationHeatmap(containerId, corrObj) {
    const inst = this.getOrCreate(containerId);
    if (!inst || !corrObj || !Array.isArray(corrObj.rows)) return;

    const ids = corrObj.component_ids || [];
    const data = [];
    for (let i = 0; i < ids.length; i++) {
      const row = corrObj.rows[i] || [];
      for (let j = 0; j < ids.length; j++) {
        const val = row[j] !== null && row[j] !== undefined ? Number(Number(row[j]).toFixed(2)) : 0;
        data.push([j, i, val]);
      }
    }

    inst.setOption({
      animation: false,
      tooltip: citrixSafeTooltip({
        position: 'top',
        formatter: params => {
          const xLabel = ids[params.value[0]] || '';
          const yLabel = ids[params.value[1]] || '';
          return `<b>${yLabel} × ${xLabel}</b><br/>Korrelation: ${params.value[2].toFixed(2)}`;
        }
      }),
      grid: { left: 108, right: 56, top: 16, bottom: 78 },
      xAxis: {
        type: 'category',
        data: ids,
        splitArea: { show: true },
        axisLabel: { rotate: 40, fontSize: 10, fontFamily: 'monospace' }
      },
      yAxis: {
        type: 'category',
        data: ids,
        inverse: true,
        splitArea: { show: true },
        axisLabel: { fontSize: 10, fontFamily: 'monospace' }
      },
      visualMap: {
        min: -1,
        max: 1,
        calculable: false,
        orient: 'vertical',
        right: 4,
        top: 'center',
        itemHeight: 160,
        inRange: {
          color: ['#1e3a8a', '#93c5fd', '#f8fafc', '#fca5a5', '#991b1b']
        },
        textStyle: { fontSize: 10 }
      },
      series: [
        {
          name: 'Korrelation',
          type: 'heatmap',
          data: data,
          label: {
            show: ids.length <= 16,
            fontSize: 9,
            formatter: p => p.value[2].toFixed(2)
          }
        }
      ]
    }, true);
  }

  renderRiskContributionsChart(containerId, items, idField = 'component_id') {
    const inst = this.getOrCreate(containerId);
    if (!inst || !Array.isArray(items)) return;

    const labels = items.map(r => r[idField]);
    const rcPct = items.map(r => Number((Number(r.risk_contribution_ann || 0) * 100).toFixed(3)));
    const sharePct = items.map(r => Number((Number(r.pct_risk_contribution || 0) * 100).toFixed(1)));

    inst.setOption({
      animation: false,
      tooltip: citrixSafeTooltip({ trigger: 'axis', axisPointer: { type: 'shadow' } }),
      legend: { data: ['Risikobeitrag (% p.a.)', 'Relativer Anteil (%)'], top: 4, textStyle: { fontSize: 11 } },
      grid: { left: 105, right: 24, top: 34, bottom: 26 },
      xAxis: { type: 'value', axisLabel: { formatter: '{value}%', fontSize: 11 } },
      yAxis: { type: 'category', data: labels, inverse: true, axisLabel: { fontSize: 10, fontFamily: 'monospace' } },
      series: [
        {
          name: 'Risikobeitrag (% p.a.)',
          type: 'bar',
          data: rcPct.map(v => ({
            value: v,
            itemStyle: { color: v < 0 ? '#0f766e' : '#1d4ed8' }
          }))
        },
        {
          name: 'Relativer Anteil (%)',
          type: 'bar',
          data: sharePct,
          itemStyle: { color: '#64748b' }
        }
      ]
    }, true);
  }

  renderScenarioPathChart(containerId, yearlyPath, baseCcy) {
    const inst = this.getOrCreate(containerId);
    if (!inst || !Array.isArray(yearlyPath)) return;

    const labels = yearlyPath.map(r => r.label);
    const wHedged = yearlyPath.map(r => Number(Number(r.wealth_hedged_base || 100).toFixed(2)));
    const wUnhedged = yearlyPath.map(r => Number(Number(r.wealth_unhedged_base || 100).toFixed(2)));
    const wLocal = yearlyPath.map(r => Number(Number(r.wealth_local || 100).toFixed(2)));

    inst.setOption({
      animation: false,
      tooltip: citrixSafeTooltip({ trigger: 'axis' }),
      legend: {
        data: [`Portfolio mit Hedge (${baseCcy})`, `Portfolio ungesichert (${baseCcy})`, 'Lokale Währungsbasis'],
        top: 4,
        textStyle: { fontSize: 11 }
      },
      grid: { left: 52, right: 20, top: 36, bottom: 28 },
      xAxis: { type: 'category', data: labels, axisLabel: { fontSize: 11 } },
      yAxis: { type: 'value', scale: true, axisLabel: { fontSize: 11 } },
      series: [
        {
          name: `Portfolio mit Hedge (${baseCcy})`,
          type: 'line',
          data: wHedged,
          lineStyle: { width: 2.5, color: '#1d4ed8' },
          itemStyle: { color: '#1d4ed8' }
        },
        {
          name: `Portfolio ungesichert (${baseCcy})`,
          type: 'line',
          data: wUnhedged,
          lineStyle: { width: 2, type: 'dashed', color: '#0f766e' },
          itemStyle: { color: '#0f766e' }
        },
        {
          name: 'Lokale Währungsbasis',
          type: 'line',
          data: wLocal,
          lineStyle: { width: 1.5, type: 'dotted', color: '#64748b' },
          itemStyle: { color: '#64748b' }
        }
      ]
    }, true);
  }

  renderComparisonChart(containerId, crossModelData, activeSelectedIds = null) {
    const inst = this.getOrCreate(containerId);
    if (!inst) return;
    if (!Array.isArray(crossModelData) || crossModelData.length === 0) {
      inst.clear();
      return;
    }

    let filteredData = crossModelData;
    if (Array.isArray(activeSelectedIds)) {
      if (activeSelectedIds.length === 0) {
        inst.clear();
        return;
      }
      filteredData = crossModelData.filter(d => activeSelectedIds.includes(d.portfolio_id));
    }

    const models = [...new Set(filteredData.map(d => d.model_label || d.model_id))];
    let portfolios = [];
    if (Array.isArray(activeSelectedIds) && activeSelectedIds.length > 0) {
      for (const pid of activeSelectedIds) {
        const match = filteredData.find(d => d.portfolio_id === pid);
        if (match) {
          portfolios.push({
            id: pid,
            name: match.portfolio_name || match.portfolio_id
          });
        }
      }
    } else {
      const seen = new Set();
      for (const d of filteredData) {
        const id = d.portfolio_id;
        if (!seen.has(id)) {
          seen.add(id);
          portfolios.push({
            id: id,
            name: d.portfolio_name || id
          });
        }
      }
    }

    if (portfolios.length === 0 || models.length === 0) {
      inst.clear();
      return;
    }

    const colors = ['#1d4ed8', '#0f766e', '#b45309', '#7c3aed', '#be123c', '#0284c7'];

    const series = portfolios.map((pObj, idx) => ({
      name: pObj.name,
      type: 'bar',
      data: models.map(mLabel => {
        const match = filteredData.find(
          d => d.portfolio_id === pObj.id && (d.model_label || d.model_id) === mLabel
        );
        return match ? Number((Number(match.vol_annualized || 0) * 100).toFixed(2)) : 0;
      }),
      itemStyle: { color: colors[idx % colors.length] }
    }));

    inst.setOption({
      animation: false,
      tooltip: citrixSafeTooltip({ trigger: 'axis', axisPointer: { type: 'shadow' } }),
      legend: { data: portfolios.map(p => p.name), top: 4, textStyle: { fontSize: 11 } },
      grid: { left: 50, right: 20, top: 38, bottom: 52 },
      xAxis: { type: 'category', data: models, axisLabel: { fontSize: 10, interval: 0, rotate: 12 } },
      yAxis: { type: 'value', axisLabel: { formatter: '{value}%', fontSize: 11 } },
      series: series
    }, { replaceMerge: ['series'] });
  }

  renderMonteCarloFanChart(containerId, fanData, baseCcy) {
    const inst = this.getOrCreate(containerId);
    if (!inst || !Array.isArray(fanData) || fanData.length === 0) return;

    const labels = fanData.map(r => r.label);
    const p05 = fanData.map(r => Number(r.hedged_p05));
    const p25 = fanData.map(r => Number(r.hedged_p25));
    const p50 = fanData.map(r => Number(r.hedged_p50));
    const p75 = fanData.map(r => Number(r.hedged_p75));
    const p95 = fanData.map(r => Number(r.hedged_p95));
    const detH = fanData.map(r => Number(r.deterministic_hedged));
    const unh50 = fanData.map(r => Number(r.unhedged_p50));

    // Stacking diffs for bands
    const diff90 = p95.map((v, i) => Number((v - p05[i]).toFixed(2)));
    const diff50 = p75.map((v, i) => Number((v - p25[i]).toFixed(2)));

    inst.setOption({
      animation: false,
      tooltip: citrixSafeTooltip({
        trigger: 'axis',
        formatter: params => {
          if (!params || params.length === 0) return '';
          const idx = params[0].dataIndex;
          const row = fanData[idx];
          if (!row) return '';
          return `
            <div style="font-size: 11px; line-height: 1.5;">
              <div style="font-weight: 700; margin-bottom: 4px; border-bottom: 1px solid rgba(0,0,0,0.1); padding-bottom: 2px;">
                ${row.label} (${row.year} Jahre)
              </div>
              <div><span style="color: #60a5fa;">■</span> P95 (Optimistisch): <b>${Number(row.hedged_p95).toFixed(1)}</b></div>
              <div><span style="color: #2563eb;">■</span> P75 (Kern oben): <b>${Number(row.hedged_p75).toFixed(1)}</b></div>
              <div><span style="color: #1d4ed8;">●</span> <b>Median P50: ${Number(row.hedged_p50).toFixed(1)}</b></div>
              <div><span style="color: #2563eb;">■</span> P25 (Kern unten): <b>${Number(row.hedged_p25).toFixed(1)}</b></div>
              <div><span style="color: #60a5fa;">■</span> P05 (95%-Stress): <b>${Number(row.hedged_p05).toFixed(1)}</b></div>
              <div style="margin-top: 4px; padding-top: 3px; border-top: 1px dashed rgba(0,0,0,0.1);">
                <span style="color: #059669;">┄</span> Deterministisch: <b>${Number(row.deterministic_hedged).toFixed(1)}</b><br/>
                <span style="color: #dc2626;">┈</span> Ungesichert P50: <b>${Number(row.unhedged_p50).toFixed(1)}</b>
              </div>
            </div>
          `;
        }
      }),
      legend: {
        data: ['P5 – P95 Bandbreite (90%)', 'P25 – P75 Kernbereich (50%)', `Median P50 (${baseCcy})`, 'Deterministischer CMA-Pfad', `Median ungesichert (${baseCcy})`],
        top: 4,
        textStyle: { fontSize: 11 }
      },
      grid: { left: 52, right: 24, top: 40, bottom: 28 },
      xAxis: {
        type: 'category',
        data: labels,
        axisLabel: {
          fontSize: 10,
          interval: (idx, val) => idx === 0 || idx % 12 === 0 || idx === fanData.length - 1
        }
      },
      yAxis: {
        type: 'value',
        scale: true,
        axisLabel: { fontSize: 11, formatter: '{value}' }
      },
      series: [
        // Band 90 base (invisible)
        {
          name: 'band90_base',
          type: 'line',
          stack: 'band90',
          data: p05,
          lineStyle: { opacity: 0 },
          itemStyle: { opacity: 0 },
          showSymbol: false,
          tooltip: { show: false }
        },
        // Band 90 fill
        {
          name: 'P5 – P95 Bandbreite (90%)',
          type: 'line',
          stack: 'band90',
          data: diff90,
          lineStyle: { opacity: 0 },
          areaStyle: { color: 'rgba(59, 130, 246, 0.16)' },
          showSymbol: false
        },
        // Band 50 base (invisible)
        {
          name: 'band50_base',
          type: 'line',
          stack: 'band50',
          data: p25,
          lineStyle: { opacity: 0 },
          itemStyle: { opacity: 0 },
          showSymbol: false,
          tooltip: { show: false }
        },
        // Band 50 fill
        {
          name: 'P25 – P75 Kernbereich (50%)',
          type: 'line',
          stack: 'band50',
          data: diff50,
          lineStyle: { opacity: 0 },
          areaStyle: { color: 'rgba(37, 99, 235, 0.28)' },
          showSymbol: false
        },
        // Median P50 line
        {
          name: `Median P50 (${baseCcy})`,
          type: 'line',
          data: p50,
          lineStyle: { width: 2.8, color: '#1d4ed8' },
          itemStyle: { color: '#1d4ed8' },
          showSymbol: false,
          markLine: {
            silent: true,
            symbol: 'none',
            lineStyle: { color: '#94a3b8', type: 'dashed' },
            data: [{ yAxis: 100, label: { formatter: 'Kapitalerhalt (100)', position: 'insideEndTop', fontSize: 10 } }]
          }
        },
        // Deterministic CMA scenario line
        {
          name: 'Deterministischer CMA-Pfad',
          type: 'line',
          data: detH,
          lineStyle: { width: 2.0, type: 'dashed', color: '#059669' },
          itemStyle: { color: '#059669' },
          showSymbol: false
        },
        // Unhedged median line
        {
          name: `Median ungesichert (${baseCcy})`,
          type: 'line',
          data: unh50,
          lineStyle: { width: 1.6, type: 'dotted', color: '#dc2626' },
          itemStyle: { color: '#dc2626' },
          showSymbol: false
        }
      ]
    }, true);
  }

  renderMonteCarloDistributionChart(containerId, histData, metrics, baseCcy) {
    const inst = this.getOrCreate(containerId);
    if (!inst || !histData || !Array.isArray(histData) || histData.length === 0) return;

    const xLabels = histData.map(r => `${(Number(r.mid) * 100).toFixed(1)}%`);

    inst.setOption({
      animation: false,
      tooltip: citrixSafeTooltip({
        trigger: 'axis',
        axisPointer: { type: 'shadow' },
        formatter: params => {
          if (!params || params.length === 0) return '';
          const idx = params[0].dataIndex;
          const row = histData[idx];
          return `
            <div style="font-size: 11px;">
              <b>5J-CAGR: ${(Number(row.mid) * 100).toFixed(2)}% p.a.</b><br/>
              Häufigkeit: <b>${row.pct}%</b> (${row.count} Pfade)
            </div>
          `;
        }
      }),
      grid: { left: 45, right: 20, top: 32, bottom: 42 },
      xAxis: {
        type: 'category',
        data: xLabels,
        axisLabel: { fontSize: 9, rotate: 30, interval: 2 }
      },
      yAxis: {
        type: 'value',
        axisLabel: { formatter: '{value}%', fontSize: 10 }
      },
      series: [
        {
          name: 'Häufigkeit',
          type: 'bar',
          data: histData.map(r => ({
            value: Number(r.pct || 0),
            itemStyle: {
              color: Number(r.mid) < 0 ? '#f87171' : '#3b82f6'
            }
          })),
          markLine: {
            silent: true,
            symbol: 'none',
            data: [
              {
                name: 'Nullrendite',
                xAxis: xLabels.reduce((closest, curr, i) => {
                  const val = parseFloat(curr);
                  const closestVal = parseFloat(xLabels[closest]);
                  return Math.abs(val) < Math.abs(closestVal) ? i : closest;
                }, 0),
                lineStyle: { color: '#ef4444', width: 2, type: 'solid' },
                label: { formatter: '0% p.a.', position: 'end', fontSize: 10, color: '#ef4444' }
              }
            ]
          }
        }
      ]
    }, true);
  }
}
