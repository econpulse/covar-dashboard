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

// 3. Verify failed validation preserves previous valid result
store.state.jobs.latest_request_by_type['portfolio.evaluate'] = 'req_err_3';
store.applyServerEnvelope({
  protocol_version: 1,
  request_id: 'req_err_3',
  type: 'portfolio.evaluate',
  status: 'error',
  errors: [{ code: 'WEIGHT_SUM_NOT_ONE', severity: 'ERROR', message: 'Summe != 100%' }]
});

assert.equal(store.state.dirty_flags.calc_state, 'failed');
assert.equal(store.state.dirty_flags.showing_previous_valid_result, true);
assert.equal(store.state.results.portfolio_evaluation.config_hash, 'hash_eur_2');

console.log('[PASS] All Frontend Store & Matrix Preview tests succeeded.');
