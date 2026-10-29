// ============================================================================
// tests/js/test_store_preview.mjs
// Frontend Unit & Protocol Tests:
// - Stale request rejection (older request_id cannot overwrite newer state)
// - Immediate client-side preview matrix math matches R Sigma_B projection
// - Invalid server response preserves previous valid evaluation (showing_previous_valid_result)
// ============================================================================

import assert from 'node:assert/strict';
import { AppStore } from '../../www/js/store.js';

const store = new AppStore();

// 1. Simulate registering two sequential requests of the same type ('risk.estimate')
store.state.jobs.latest_request_by_type['risk.estimate'] = 'req_new_2';
store.state.jobs.pending_requests['req_old_1'] = { type: 'risk.estimate', sent_at: 100 };
store.state.jobs.pending_requests['req_new_2'] = { type: 'risk.estimate', sent_at: 200 };

// Applying the newer response succeeds
const appliedNew = store.applyServerEnvelope({
  protocol_version: 1,
  request_id: 'req_new_2',
  type: 'risk.estimate',
  status: 'ok',
  context: { state_version: 2, base_currency: 'EUR' },
  result: {
    dataset: {
      dataset_id: 'ds1',
      active_assets: ['A1', 'A2'],
      metadata: [
        { Code: 'A1', Name: 'Swiss Eq', Asset_Class: 'EQUITY', Return_Currency: 'CHF', FX_Exposure: 'CHF', Hedge_Allowed: true },
        { Code: 'A2', Name: 'Euro Bond', Asset_Class: 'BOND', Return_Currency: 'EUR', FX_Exposure: 'EUR', Hedge_Allowed: true }
      ]
    },
    config: {
      base_currency: 'EUR',
      return_mode: 'TOTAL',
      active_model_id: 'shrinkage_long',
      active_portfolio_id: 'saa_50',
      hedge_mode: 'ASSET_CLASS',
      hedge_configs: {
        ASSET_CLASS: { mode: 'ASSET_CLASS', class_ratios: { EQUITY: 0.5, BOND: 1.0 }, asset_overrides: {} }
      }
    },
    covariance_model: {
      model_id: 'shrinkage_long',
      base_currency: 'EUR',
      return_mode: 'TOTAL',
      asset_ids: ['A1', 'A2'],
      hedge_ids: ['HEDGE_CHF'],
      component_ids: ['A1', 'A2', 'HEDGE_CHF'],
      available_hedge_currencies: ['CHF'],
      sigma_monthly: {
        component_ids: ['A1', 'A2', 'HEDGE_CHF'],
        dimension: 3,
        rows: [
          [0.0020, 0.0005, -0.0008],
          [0.0005, 0.0004,  0.0000],
          [-0.0008, 0.0000, 0.0006]
        ]
      }
    },
    portfolio_evaluation: {
      valid: true,
      config_hash: 'hash_eur_2',
      weights: { A1: 0.6, A2: 0.4 },
      metrics: {
        vol_annualized: 0.11,
        vol_unhedged_annualized: 0.12,
        hedge_vol_effect_annualized: -0.01,
        weight_sum: 1.0
      }
    }
  }
});

assert.equal(appliedNew, true);
assert.equal(store.state.base_currency, 'EUR');
assert.equal(store.state.results.portfolio_evaluation.config_hash, 'hash_eur_2');

// Applying the older response ('req_old_1' for 'CHF') MUST be rejected and NOT overwrite 'EUR'
const appliedStale = store.applyServerEnvelope({
  protocol_version: 1,
  request_id: 'req_old_1',
  type: 'risk.estimate',
  status: 'ok',
  context: { state_version: 1, base_currency: 'CHF' },
  result: {
    config: { base_currency: 'CHF' },
    portfolio_evaluation: { valid: true, config_hash: 'hash_chf_1', weights: { A1: 0.5, A2: 0.5 } }
  }
});

assert.equal(appliedStale, false);
assert.equal(store.state.base_currency, 'EUR');
assert.equal(store.state.results.portfolio_evaluation.config_hash, 'hash_eur_2');

// 2. Verify client-side matrix preview math a' Sigma_B a
// w = [0.6, 0.4], base = EUR -> A1 (CHF) is foreign with EQUITY hedge ratio 0.5 -> g_CHF = 0.6 * 0.5 = 0.3
// A2 (EUR) is domestic -> g_EUR = 0
// a = [0.6, 0.4, 0.3]
const preview = store.computeClientSidePreview();
assert.ok(preview);
assert.equal(preview.is_preview, true);
assert.ok(Math.abs(preview.weight_sum - 1.0) < 1e-12);
assert.ok(Math.abs(preview.total_hedge_notional - 0.3) < 1e-12);
assert.ok(Math.abs(preview.equity_share - 0.6) < 1e-12);
assert.ok(Math.abs(preview.foreign_exposure_after - 0.3) < 1e-12);

// Manual calculation of a' Sigma_m a:
const a = [0.6, 0.4, 0.3];
const S = [
  [0.0020, 0.0005, -0.0008],
  [0.0005, 0.0004,  0.0000],
  [-0.0008, 0.0000, 0.0006]
];
let expectedVarM = 0;
for (let i = 0; i < 3; i++) {
  for (let j = 0; j < 3; j++) {
    expectedVarM += a[i] * S[i][j] * a[j];
  }
}
const expectedVolAnn = Math.sqrt(12.0 * expectedVarM);
assert.ok(Math.abs(preview.vol_annualized - expectedVolAnn) < 1e-12);

// 3. Verify failed validation preserves previous valid result on true ERROR
store.state.jobs.latest_request_by_type['portfolio.evaluate'] = 'req_err_3';
store.applyServerEnvelope({
  protocol_version: 1,
  request_id: 'req_err_3',
  type: 'portfolio.evaluate',
  status: 'error',
  errors: [{ code: 'INVALID_WEIGHT_NUMBERS', severity: 'ERROR', message: 'Ungültige Zahlen' }]
});

assert.equal(store.state.dirty_flags.calc_state, 'failed');
assert.equal(store.state.dirty_flags.showing_previous_valid_result, true);
assert.equal(store.state.results.portfolio_evaluation.config_hash, 'hash_eur_2');

// 4. Verify WEIGHT_SUM_NOT_ONE warning does NOT block evaluation
store.state.jobs.latest_request_by_type['portfolio.evaluate'] = 'req_warn_4';
store.applyServerEnvelope({
  protocol_version: 1,
  request_id: 'req_warn_4',
  type: 'portfolio.evaluate',
  status: 'ok',
  result: {
    portfolio_evaluation: {
      valid: true,
      config_hash: 'hash_eur_105',
      metrics: {
        weight_sum: 1.05,
        vol_annualized: 0.0825,
        vol_unhedged_annualized: 0.0846,
        hedge_vol_effect_annualized: -0.0021,
        equity_share: 0.525,
        foreign_exposure_after: 0.38
      }
    }
  },
  warnings: [{ code: 'WEIGHT_SUM_NOT_ONE', severity: 'WARNING', message: 'Summe der Kapitalgewichte muss 100.0% ergeben (aktuell: 105.00%).' }]
});

assert.equal(store.state.dirty_flags.calc_state, 'current');
assert.equal(store.state.dirty_flags.showing_previous_valid_result, false);
assert.equal(store.state.results.portfolio_evaluation.config_hash, 'hash_eur_105');
assert.equal(store.state.diagnostics.warnings.length, 1);
assert.equal(store.state.diagnostics.warnings[0].code, 'WEIGHT_SUM_NOT_ONE');

// 5. Test Comparison Slots in Tab 5:
// - Max 6 slots
// - Disallow duplicate selection (selecting an ID clears it from another slot)
// - Supports blank ("") slot
// - Custom portfolio deletion clears slot
store.applyServerEnvelope({
  status: 'ok',
  result: {
    comparison: {
      summary_table: [
        { portfolio_id: 'saa_50', portfolio_name: 'SAA 50' },
        { portfolio_id: 'saa_50_unhedged', portfolio_name: 'SAA 50 (Ungesichert 0%)' },
        { portfolio_id: 'saa_25', portfolio_name: 'SAA 25' },
        { portfolio_id: 'saa_25_unhedged', portfolio_name: 'SAA 25 (Ungesichert 0%)' },
        { portfolio_id: 'saa_75', portfolio_name: 'SAA 75' },
        { portfolio_id: 'custom_growth', portfolio_name: 'Growth SAA' },
        { portfolio_id: 'custom_growth_unhedged', portfolio_name: 'Growth SAA (Ungesichert 0%)' }
      ],
      pairwise_tracking_error: {
        portfolio_ids: ['saa_50', 'saa_50_unhedged', 'saa_25', 'saa_75'],
        matrix: [[0, 0.01, 0.02, 0.03], [0.01, 0, 0.02, 0.03], [0.02, 0.02, 0, 0.04], [0.03, 0.03, 0.04, 0]]
      }
    }
  }
});

assert.equal(Array.isArray(store.state.comparison_slots), true);
assert.equal(store.state.comparison_slots.length, 6);
assert.equal(store.state.comparison_slots[0], 'saa_50');
assert.equal(store.state.comparison_slots[1], 'saa_50_unhedged');

// Set slot 2 to custom_growth
store.setComparisonSlot(2, 'custom_growth');
assert.equal(store.state.comparison_slots[2], 'custom_growth');

// Set slot 3 to custom_growth (duplicate test):
// Slot 2 is cleared and slots below shift up so no empty gaps remain above filled rows!
// Initial slots were ['saa_50', 'saa_50_unhedged', 'custom_growth', 'saa_75', '', '']
// Setting slot 3 to 'custom_growth' clears slot 2 -> ['saa_50', 'saa_50_unhedged', '', 'custom_growth', '', '']
// Which compacts to ['saa_50', 'saa_50_unhedged', 'custom_growth', '', '', '']
store.setComparisonSlot(3, 'custom_growth');
assert.equal(store.state.comparison_slots[2], 'custom_growth');
assert.equal(store.state.comparison_slots[3], '');

// Add saa_75 to slot 3 -> ['saa_50', 'saa_50_unhedged', 'custom_growth', 'saa_75', '', '']
store.setComparisonSlot(3, 'saa_75');
assert.equal(store.state.comparison_slots[3], 'saa_75');

// Blank slot 1 ('saa_50_unhedged') -> rows below ('custom_growth', 'saa_75') MUST shift up to slots 1 and 2!
store.setComparisonSlot(1, '');
assert.equal(store.state.comparison_slots[0], 'saa_50');
assert.equal(store.state.comparison_slots[1], 'custom_growth');
assert.equal(store.state.comparison_slots[2], 'saa_75');
assert.equal(store.state.comparison_slots[3], '');

// Delete custom portfolio ('custom_growth' at slot 1) clears it and shifts 'saa_75' up to slot 1!
store.state.custom_portfolios = { custom_growth: { name: 'Growth SAA' } };
store.deleteCustomPortfolio('custom_growth');
assert.equal(store.state.comparison_slots[0], 'saa_50');
assert.equal(store.state.comparison_slots[1], 'saa_75');
assert.equal(store.state.comparison_slots[2], '');

// 6. Test CMA Sets Management:
// - Save custom CMA set
// - Select CMA set
// - Delete custom CMA set
const savedSet = store.saveCustomCmaSet('cma_test_1', 'CMA Test Optimistic', { A1: 0.08, A2: 0.04 }, { CHF: -0.01 }, 'base_5y');
assert.equal(savedSet.name, 'CMA Test Optimistic');
assert.equal(store.state.active_cma_set_id, 'cma_test_1');
assert.equal(store.state.custom_cma_sets['cma_test_1'].asset_returns['A1'], 0.08);

// Mock bridge for selectCmaSet test
const sentCmds = [];
const mockBridge = {
  sendCommand: (type, payload) => sentCmds.push({ type, payload })
};

store.selectCmaSet('cma_test_1', mockBridge);
assert.equal(sentCmds.length, 1);
assert.equal(sentCmds[0].type, 'scenarios.evaluate');
assert.equal(sentCmds[0].payload.asset_return_overrides['A1'], 0.08);

store.deleteCustomCmaSet('cma_test_1');
assert.equal(store.state.custom_cma_sets['cma_test_1'], undefined);
assert.equal(store.state.active_cma_set_id, 'cma_base');

// 7. Verify draft hedge config preservation in CURRENCY_OVERLAY mode
store.state.active_portfolio_id = 'custom_ausgewogen';
store.state.portfolios['custom_ausgewogen'] = {
  id: 'custom_ausgewogen',
  name: 'Ausgewogen',
  weights: { A1: 0.5, A2: 0.5 },
  hedge_config: { mode: 'CURRENCY_OVERLAY', currency_ratios: { USD: 0.5, EUR: 0.5 } }
};
store.state.hedge_mode = 'CURRENCY_OVERLAY';
store.state.draft_hedge_config = { mode: 'CURRENCY_OVERLAY', currency_ratios: { USD: 0.8, EUR: 0.3 } };

// Simulate portfolio.evaluate response from server
store.state.jobs.latest_request_by_type['portfolio.evaluate'] = 'req_eval_hedge';
store.applyServerEnvelope({
  protocol_version: 1,
  request_id: 'req_eval_hedge',
  type: 'portfolio.evaluate',
  status: 'ok',
  result: {
    config: {
      hedge_mode: 'CURRENCY_OVERLAY',
      hedge_configs: {
        CURRENCY_OVERLAY: { mode: 'CURRENCY_OVERLAY', currency_ratios: { USD: 0.5, EUR: 0.5 } }
      }
    },
    portfolio_evaluation: {
      valid: true,
      weights: { A1: 0.5, A2: 0.5 },
      exposures: { active_mode: 'CURRENCY_OVERLAY' }
    }
  }
});

// Verify draft hedge config is NOT overwritten with 50%
assert.equal(store.state.draft_hedge_config.currency_ratios.USD, 0.8, 'USD hedge ratio must remain 80% and not jump back to 50%');
assert.equal(store.state.draft_hedge_config.currency_ratios.EUR, 0.3, 'EUR hedge ratio must remain 30%');

console.log('[PASS] All Frontend Store & Matrix Preview tests succeeded.');
