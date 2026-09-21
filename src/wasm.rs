//! WASM bindings for skillratings.
//!
//! Exposes all rating systems to JavaScript via wasm-bindgen.
//! Each system gets: `{system}_rate`, `{system}_expected`, and where applicable
//! `{system}_rate_team`, `{system}_rate_multi_team`.
//!
//! All ratings are passed as JS objects `{ rating, uncertainty? }` and returned as such.

#![cfg_attr(feature = "wasm", allow(missing_docs))]

use wasm_bindgen::prelude::*;

// ── Helpers ──────────────────────────────────────────────────────────

/// Generic rating result as a JS-friendly struct.
#[wasm_bindgen]
pub struct WasmRating {
    /// Numeric rating value.
    pub rating: f64,
    /// Uncertainty / deviation value.
    pub uncertainty: f64,
}

#[wasm_bindgen]
impl WasmRating {
    /// Create a new WasmRating.
    #[wasm_bindgen(constructor)]
    pub fn new(rating: f64, uncertainty: f64) -> Self {
        Self {
            rating,
            uncertainty,
        }
    }
}

/// A pair of ratings returned from a 1v1 match.
#[wasm_bindgen]
pub struct RatingPair {
    /// Player 1 resulting rating.
    pub p1_rating: f64,
    /// Player 1 resulting uncertainty.
    pub p1_uncertainty: f64,
    /// Player 2 resulting rating.
    pub p2_rating: f64,
    /// Player 2 resulting uncertainty.
    pub p2_uncertainty: f64,
}

/// Expected score pair.
#[wasm_bindgen]
pub struct ExpectedPair {
    /// Expected probability for player 1.
    pub p1: f64,
    /// Expected probability for player 2.
    pub p2: f64,
}

// ── Elo ──────────────────────────────────────────────────────────────

#[wasm_bindgen]
pub fn elo_rate(
    p1_rating: f64,
    p2_rating: f64,
    outcome: u8, // 0 = WIN, 1 = LOSS, 2 = DRAW
    k: Option<f64>,
) -> RatingPair {
    use crate::elo::*;
    let o = match outcome {
        1 => crate::Outcomes::LOSS,
        2 => crate::Outcomes::DRAW,
        _ => crate::Outcomes::WIN,
    };
    let config = match k {
        Some(k) => EloConfig { k },
        None => EloConfig::new(),
    };
    let (a, b) = elo(
        &EloRating { rating: p1_rating },
        &EloRating { rating: p2_rating },
        &o,
        &config,
    );
    RatingPair {
        p1_rating: a.rating,
        p1_uncertainty: 0.0,
        p2_rating: b.rating,
        p2_uncertainty: 0.0,
    }
}

#[wasm_bindgen]
pub fn elo_expected(p1_rating: f64, p2_rating: f64) -> ExpectedPair {
    use crate::elo::*;
    let (a, b) = expected_score(
        &EloRating { rating: p1_rating },
        &EloRating { rating: p2_rating },
    );
    ExpectedPair { p1: a, p2: b }
}

// ── Glicko ───────────────────────────────────────────────────────────

#[wasm_bindgen]
pub fn glicko_rate(
    p1_rating: f64,
    p1_deviation: f64,
    p2_rating: f64,
    p2_deviation: f64,
    outcome: u8,
) -> RatingPair {
    use crate::glicko::*;
    let o = match outcome {
        1 => crate::Outcomes::LOSS,
        2 => crate::Outcomes::DRAW,
        _ => crate::Outcomes::WIN,
    };
    let (a, b) = glicko(
        &GlickoRating {
            rating: p1_rating,
            deviation: p1_deviation,
        },
        &GlickoRating {
            rating: p2_rating,
            deviation: p2_deviation,
        },
        &o,
        &GlickoConfig::new(),
    );
    RatingPair {
        p1_rating: a.rating,
        p1_uncertainty: a.deviation,
        p2_rating: b.rating,
        p2_uncertainty: b.deviation,
    }
}

// ── Glicko-2 ─────────────────────────────────────────────────────────

#[wasm_bindgen]
pub fn glicko2_rate(
    p1_rating: f64,
    p1_deviation: f64,
    p1_volatility: f64,
    p2_rating: f64,
    p2_deviation: f64,
    p2_volatility: f64,
    outcome: u8,
    tau: Option<f64>,
) -> js_sys::Float64Array {
    use crate::glicko2::*;
    let o = match outcome {
        1 => crate::Outcomes::LOSS,
        2 => crate::Outcomes::DRAW,
        _ => crate::Outcomes::WIN,
    };
    let config = Glicko2Config {
        tau: tau.unwrap_or(0.5),
        ..Glicko2Config::new()
    };
    let (a, b) = glicko2(
        &Glicko2Rating {
            rating: p1_rating,
            deviation: p1_deviation,
            volatility: p1_volatility,
        },
        &Glicko2Rating {
            rating: p2_rating,
            deviation: p2_deviation,
            volatility: p2_volatility,
        },
        &o,
        &config,
    );
    // Return [p1.rating, p1.rd, p1.vol, p2.rating, p2.rd, p2.vol]
    let arr = js_sys::Float64Array::new_with_length(6);
    arr.set_index(0, a.rating);
    arr.set_index(1, a.deviation);
    arr.set_index(2, a.volatility);
    arr.set_index(3, b.rating);
    arr.set_index(4, b.deviation);
    arr.set_index(5, b.volatility);
    arr
}

#[wasm_bindgen]
pub fn glicko2_expected(
    p1_rating: f64,
    p1_deviation: f64,
    p1_volatility: f64,
    p2_rating: f64,
    p2_deviation: f64,
    p2_volatility: f64,
) -> ExpectedPair {
    use crate::glicko2::*;
    let (a, b) = expected_score(
        &Glicko2Rating {
            rating: p1_rating,
            deviation: p1_deviation,
            volatility: p1_volatility,
        },
        &Glicko2Rating {
            rating: p2_rating,
            deviation: p2_deviation,
            volatility: p2_volatility,
        },
    );
    ExpectedPair { p1: a, p2: b }
}

// ── TrueSkill ────────────────────────────────────────────────────────

#[wasm_bindgen]
pub fn trueskill_rate(
    p1_rating: f64,
    p1_uncertainty: f64,
    p2_rating: f64,
    p2_uncertainty: f64,
    outcome: u8,
) -> RatingPair {
    use crate::trueskill::*;
    let o = match outcome {
        1 => crate::Outcomes::LOSS,
        2 => crate::Outcomes::DRAW,
        _ => crate::Outcomes::WIN,
    };
    let (a, b) = trueskill(
        &TrueSkillRating {
            rating: p1_rating,
            uncertainty: p1_uncertainty,
        },
        &TrueSkillRating {
            rating: p2_rating,
            uncertainty: p2_uncertainty,
        },
        &o,
        &TrueSkillConfig::new(),
    );
    RatingPair {
        p1_rating: a.rating,
        p1_uncertainty: a.uncertainty,
        p2_rating: b.rating,
        p2_uncertainty: b.uncertainty,
    }
}

#[wasm_bindgen]
pub fn trueskill_rate_teams(
    team1_ratings: &[f64],
    team1_uncertainties: &[f64],
    team2_ratings: &[f64],
    team2_uncertainties: &[f64],
    outcome: u8,
) -> js_sys::Float64Array {
    use crate::trueskill::*;
    let o = match outcome {
        1 => crate::Outcomes::LOSS,
        2 => crate::Outcomes::DRAW,
        _ => crate::Outcomes::WIN,
    };
    let t1: Vec<TrueSkillRating> = team1_ratings
        .iter()
        .zip(team1_uncertainties)
        .map(|(&r, &u)| TrueSkillRating {
            rating: r,
            uncertainty: u,
        })
        .collect();
    let t2: Vec<TrueSkillRating> = team2_ratings
        .iter()
        .zip(team2_uncertainties)
        .map(|(&r, &u)| TrueSkillRating {
            rating: r,
            uncertainty: u,
        })
        .collect();
    let (r1, r2) = trueskill_two_teams(&t1, &t2, &o, &TrueSkillConfig::new());
    let total = r1.len() + r2.len();
    let arr = js_sys::Float64Array::new_with_length((total * 2) as u32);
    let mut idx = 0;
    for r in &r1 {
        arr.set_index(idx, r.rating);
        idx += 1;
        arr.set_index(idx, r.uncertainty);
        idx += 1;
    }
    for r in &r2 {
        arr.set_index(idx, r.rating);
        idx += 1;
        arr.set_index(idx, r.uncertainty);
        idx += 1;
    }
    arr
}

/// Multi-team TrueSkill: flat arrays + team_sizes + ranks.
/// ratings/uncertainties are flat (all players concatenated).
/// team_sizes = [5, 5, 5, 4] for a 5v5v5v4.
/// ranks = [1, 2, 3, 4] (1-indexed).
/// Returns flat Float64Array of [rating, uncertainty] pairs for each player.
#[wasm_bindgen]
pub fn trueskill_rate_multi_team(
    ratings: &[f64],
    uncertainties: &[f64],
    team_sizes: &[u32],
    ranks: &[u32],
) -> js_sys::Float64Array {
    use crate::trueskill::*;
    let mut teams: Vec<Vec<TrueSkillRating>> = Vec::new();
    let mut offset = 0usize;
    for &size in team_sizes {
        let s = size as usize;
        let team: Vec<TrueSkillRating> = (0..s)
            .map(|i| TrueSkillRating {
                rating: ratings[offset + i],
                uncertainty: uncertainties[offset + i],
            })
            .collect();
        teams.push(team);
        offset += s;
    }

    let input: Vec<(&[TrueSkillRating], crate::MultiTeamOutcome)> = teams
        .iter()
        .zip(ranks.iter())
        .map(|(t, &r)| (t.as_slice(), crate::MultiTeamOutcome::new(r as usize)))
        .collect();

    let result = match trueskill_multi_team(&input, &TrueSkillConfig::new(), None) {
        Ok(r) => r,
        Err(_) => {
            // Return the input unchanged on error
            let arr = js_sys::Float64Array::new_with_length((ratings.len() * 2) as u32);
            for (i, (&r, &u)) in ratings.iter().zip(uncertainties).enumerate() {
                arr.set_index((i * 2) as u32, r);
                arr.set_index((i * 2 + 1) as u32, u);
            }
            return arr;
        }
    };

    let total: usize = team_sizes.iter().map(|&s| s as usize).sum();
    let arr = js_sys::Float64Array::new_with_length((total * 2) as u32);
    let mut idx = 0u32;
    for team in &result {
        for r in team {
            arr.set_index(idx, r.rating);
            idx += 1;
            arr.set_index(idx, r.uncertainty);
            idx += 1;
        }
    }
    arr
}

// ── Weng-Lin (OpenSkill) ─────────────────────────────────────────────

#[wasm_bindgen]
pub fn weng_lin_rate(
    p1_rating: f64,
    p1_uncertainty: f64,
    p2_rating: f64,
    p2_uncertainty: f64,
    outcome: u8,
) -> RatingPair {
    use crate::weng_lin::*;
    let o = match outcome {
        1 => crate::Outcomes::LOSS,
        2 => crate::Outcomes::DRAW,
        _ => crate::Outcomes::WIN,
    };
    let (a, b) = weng_lin(
        &WengLinRating {
            rating: p1_rating,
            uncertainty: p1_uncertainty,
        },
        &WengLinRating {
            rating: p2_rating,
            uncertainty: p2_uncertainty,
        },
        &o,
        &WengLinConfig::new(),
    );
    RatingPair {
        p1_rating: a.rating,
        p1_uncertainty: a.uncertainty,
        p2_rating: b.rating,
        p2_uncertainty: b.uncertainty,
    }
}

#[wasm_bindgen]
pub fn weng_lin_rate_multi_team(
    ratings: &[f64],
    uncertainties: &[f64],
    team_sizes: &[u32],
    ranks: &[u32],
) -> js_sys::Float64Array {
    use crate::weng_lin::*;
    let mut teams: Vec<Vec<WengLinRating>> = Vec::new();
    let mut offset = 0usize;
    for &size in team_sizes {
        let s = size as usize;
        let team: Vec<WengLinRating> = (0..s)
            .map(|i| WengLinRating {
                rating: ratings[offset + i],
                uncertainty: uncertainties[offset + i],
            })
            .collect();
        teams.push(team);
        offset += s;
    }

    let input: Vec<(&[WengLinRating], crate::MultiTeamOutcome)> = teams
        .iter()
        .zip(ranks.iter())
        .map(|(t, &r)| (t.as_slice(), crate::MultiTeamOutcome::new(r as usize)))
        .collect();

    let result = weng_lin_multi_team(&input, &WengLinConfig::new());
    let total: usize = team_sizes.iter().map(|&s| s as usize).sum();
    let arr = js_sys::Float64Array::new_with_length((total * 2) as u32);
    let mut idx = 0u32;
    for team in &result {
        for r in team {
            arr.set_index(idx, r.rating);
            idx += 1;
            arr.set_index(idx, r.uncertainty);
            idx += 1;
        }
    }
    arr
}

#[wasm_bindgen]
pub fn weng_lin_expected(
    p1_rating: f64,
    p1_uncertainty: f64,
    p2_rating: f64,
    p2_uncertainty: f64,
) -> ExpectedPair {
    use crate::weng_lin::*;
    let (a, b) = expected_score(
        &WengLinRating {
            rating: p1_rating,
            uncertainty: p1_uncertainty,
        },
        &WengLinRating {
            rating: p2_rating,
            uncertainty: p2_uncertainty,
        },
        &WengLinConfig::new(),
    );
    ExpectedPair { p1: a, p2: b }
}

// ── Sticko ───────────────────────────────────────────────────────────

#[wasm_bindgen]
pub fn sticko_rate(
    p1_rating: f64,
    p1_deviation: f64,
    p2_rating: f64,
    p2_deviation: f64,
    outcome: u8,
) -> RatingPair {
    use crate::sticko::*;
    let o = match outcome {
        1 => crate::Outcomes::LOSS,
        2 => crate::Outcomes::DRAW,
        _ => crate::Outcomes::WIN,
    };
    let (a, b) = sticko(
        &StickoRating {
            rating: p1_rating,
            deviation: p1_deviation,
            ..StickoRating::new()
        },
        &StickoRating {
            rating: p2_rating,
            deviation: p2_deviation,
            ..StickoRating::new()
        },
        &o,
        &StickoConfig::new(),
    );
    RatingPair {
        p1_rating: a.rating,
        p1_uncertainty: a.deviation,
        p2_rating: b.rating,
        p2_uncertainty: b.deviation,
    }
}

// ── Glicko-Boost ─────────────────────────────────────────────────────

#[wasm_bindgen]
pub fn glicko_boost_rate(
    p1_rating: f64,
    p1_deviation: f64,
    p2_rating: f64,
    p2_deviation: f64,
    outcome: u8,
) -> RatingPair {
    use crate::glicko_boost::*;
    let o = match outcome {
        1 => crate::Outcomes::LOSS,
        2 => crate::Outcomes::DRAW,
        _ => crate::Outcomes::WIN,
    };
    let (a, b) = glicko_boost(
        &GlickoBoostRating {
            rating: p1_rating,
            deviation: p1_deviation,
        },
        &GlickoBoostRating {
            rating: p2_rating,
            deviation: p2_deviation,
        },
        &o,
        &GlickoBoostConfig::new(),
    );
    RatingPair {
        p1_rating: a.rating,
        p1_uncertainty: a.deviation,
        p2_rating: b.rating,
        p2_uncertainty: b.deviation,
    }
}

// ── DWZ ──────────────────────────────────────────────────────────────

#[wasm_bindgen]
pub fn dwz_rate(
    p1_rating: f64,
    p1_index: u32,
    p1_age: u32,
    p2_rating: f64,
    p2_index: u32,
    p2_age: u32,
    outcome: u8,
) -> RatingPair {
    use crate::dwz::*;
    let o = match outcome {
        1 => crate::Outcomes::LOSS,
        2 => crate::Outcomes::DRAW,
        _ => crate::Outcomes::WIN,
    };
    let (a, b) = dwz(
        &DWZRating {
            rating: p1_rating,
            index: p1_index as usize,
            age: p1_age as usize,
        },
        &DWZRating {
            rating: p2_rating,
            index: p2_index as usize,
            age: p2_age as usize,
        },
        &o,
    );
    RatingPair {
        p1_rating: a.rating,
        p1_uncertainty: a.index as f64,
        p2_rating: b.rating,
        p2_uncertainty: b.index as f64,
    }
}

// ── FIFA ─────────────────────────────────────────────────────────────

#[wasm_bindgen]
pub fn fifa_rate(p1_rating: f64, p2_rating: f64, outcome: u8) -> RatingPair {
    use crate::fifa::*;
    let o = match outcome {
        1 => crate::Outcomes::LOSS,
        2 => crate::Outcomes::DRAW,
        _ => crate::Outcomes::WIN,
    };
    let (a, b) = fifa(
        &FifaRating { rating: p1_rating },
        &FifaRating { rating: p2_rating },
        &o,
        &FifaConfig::new(),
    );
    RatingPair {
        p1_rating: a.rating,
        p1_uncertainty: 0.0,
        p2_rating: b.rating,
        p2_uncertainty: 0.0,
    }
}

// ── EGF ──────────────────────────────────────────────────────────────

#[wasm_bindgen]
pub fn egf_rate(p1_rating: f64, p2_rating: f64, outcome: u8) -> RatingPair {
    use crate::egf::*;
    let o = match outcome {
        1 => crate::Outcomes::LOSS,
        2 => crate::Outcomes::DRAW,
        _ => crate::Outcomes::WIN,
    };
    let (a, b) = egf(
        &EGFRating { rating: p1_rating },
        &EGFRating { rating: p2_rating },
        &o,
        &EGFConfig::new(),
    );
    RatingPair {
        p1_rating: a.rating,
        p1_uncertainty: 0.0,
        p2_rating: b.rating,
        p2_uncertainty: 0.0,
    }
}

// ── USCF ─────────────────────────────────────────────────────────────

#[wasm_bindgen]
pub fn uscf_rate(p1_rating: f64, p2_rating: f64, outcome: u8) -> RatingPair {
    use crate::uscf::*;
    let o = match outcome {
        1 => crate::Outcomes::LOSS,
        2 => crate::Outcomes::DRAW,
        _ => crate::Outcomes::WIN,
    };
    let (a, b) = uscf(
        &USCFRating {
            rating: p1_rating,
            games: 20,
        },
        &USCFRating {
            rating: p2_rating,
            games: 20,
        },
        &o,
        &USCFConfig::new(),
    );
    RatingPair {
        p1_rating: a.rating,
        p1_uncertainty: 0.0,
        p2_rating: b.rating,
        p2_uncertainty: 0.0,
    }
}

// ── Ingo ─────────────────────────────────────────────────────────────

#[wasm_bindgen]
pub fn ingo_rate(p1_rating: f64, p2_rating: f64, outcome: u8) -> RatingPair {
    use crate::ingo::*;
    let o = match outcome {
        1 => crate::Outcomes::LOSS,
        2 => crate::Outcomes::DRAW,
        _ => crate::Outcomes::WIN,
    };
    let (a, b) = ingo(
        &IngoRating {
            rating: p1_rating,
            age: 25,
        },
        &IngoRating {
            rating: p2_rating,
            age: 25,
        },
        &o,
    );
    RatingPair {
        p1_rating: a.rating,
        p1_uncertainty: 0.0,
        p2_rating: b.rating,
        p2_uncertainty: 0.0,
    }
}

// ── Yuksel ───────────────────────────────────────────────────────────

/// Zero-sum 1v1 rating update (Algorithm 2).
/// Returns Float64Array of [p1.rating, p1.d, p1.w_sum, p1.r_mean, p1.v,
///                           p2.rating, p2.d, p2.w_sum, p2.r_mean, p2.v].
#[wasm_bindgen]
pub fn yuksel_rate(
    p1_rating: f64,
    p1_d: f64,
    p1_w_sum: f64,
    p1_r_mean: f64,
    p1_v: f64,
    p2_rating: f64,
    p2_d: f64,
    p2_w_sum: f64,
    p2_r_mean: f64,
    p2_v: f64,
    outcome: u8,
    alpha: Option<f64>,
    dr_max: Option<f64>,
    s_factor: Option<f64>,
) -> js_sys::Float64Array {
    use crate::yuksel::*;
    let o = match outcome {
        1 => crate::Outcomes::LOSS,
        2 => crate::Outcomes::DRAW,
        _ => crate::Outcomes::WIN,
    };
    let config = YukselConfig {
        alpha: alpha.unwrap_or(2.0),
        dr_max: dr_max.unwrap_or(350.0),
        s_factor: s_factor.unwrap_or(1.0),
    };
    let (a, b) = yuksel(
        &YukselRating {
            rating: p1_rating,
            d: p1_d,
            w_sum: p1_w_sum,
            r_mean: p1_r_mean,
            v: p1_v,
        },
        &YukselRating {
            rating: p2_rating,
            d: p2_d,
            w_sum: p2_w_sum,
            r_mean: p2_r_mean,
            v: p2_v,
        },
        &o,
        &config,
    );
    let arr = js_sys::Float64Array::new_with_length(10);
    arr.set_index(0, a.rating);
    arr.set_index(1, a.d);
    arr.set_index(2, a.w_sum);
    arr.set_index(3, a.r_mean);
    arr.set_index(4, a.v);
    arr.set_index(5, b.rating);
    arr.set_index(6, b.d);
    arr.set_index(7, b.w_sum);
    arr.set_index(8, b.r_mean);
    arr.set_index(9, b.v);
    arr
}

#[wasm_bindgen]
pub fn yuksel_expected(p1_rating: f64, p2_rating: f64) -> ExpectedPair {
    use crate::yuksel::*;
    let (a, b) = expected_score(
        &YukselRating {
            rating: p1_rating,
            ..YukselRating::new()
        },
        &YukselRating {
            rating: p2_rating,
            ..YukselRating::new()
        },
    );
    ExpectedPair { p1: a, p2: b }
}

/// Rating period (batch) update using incremental non-zero-sum updates (Algorithm 1).
/// Opponents are passed as flat rating values (other fields default to new-player values).
/// Returns Float64Array of [rating, d, w_sum, r_mean, v].
#[wasm_bindgen]
pub fn yuksel_rate_period(
    p_rating: f64,
    p_d: f64,
    p_w_sum: f64,
    p_r_mean: f64,
    p_v: f64,
    opp_ratings: &[f64],
    outcomes: &[u8],
    alpha: Option<f64>,
    dr_max: Option<f64>,
    s_factor: Option<f64>,
) -> js_sys::Float64Array {
    use crate::yuksel::*;
    let config = YukselConfig {
        alpha: alpha.unwrap_or(2.0),
        dr_max: dr_max.unwrap_or(350.0),
        s_factor: s_factor.unwrap_or(1.0),
    };
    let results: Vec<(YukselRating, crate::Outcomes)> = opp_ratings
        .iter()
        .zip(outcomes)
        .map(|(&r, &o)| {
            let outcome = match o {
                1 => crate::Outcomes::LOSS,
                2 => crate::Outcomes::DRAW,
                _ => crate::Outcomes::WIN,
            };
            (
                YukselRating {
                    rating: r,
                    ..YukselRating::new()
                },
                outcome,
            )
        })
        .collect();
    let new_player = yuksel_rating_period(
        &YukselRating {
            rating: p_rating,
            d: p_d,
            w_sum: p_w_sum,
            r_mean: p_r_mean,
            v: p_v,
        },
        &results,
        &config,
    );
    let arr = js_sys::Float64Array::new_with_length(5);
    arr.set_index(0, new_player.rating);
    arr.set_index(1, new_player.d);
    arr.set_index(2, new_player.w_sum);
    arr.set_index(3, new_player.r_mean);
    arr.set_index(4, new_player.v);
    arr
}

/// Volatility range [rating − 1.96·φ, rating + 1.96·φ].
/// Returns Float64Array of [low, high].
#[wasm_bindgen]
pub fn yuksel_volatility_range(
    rating: f64,
    d: f64,
    w_sum: f64,
    r_mean: f64,
    v: f64,
) -> js_sys::Float64Array {
    use crate::yuksel::*;
    let (low, high) = volatility_range(&YukselRating {
        rating,
        d,
        w_sum,
        r_mean,
        v,
    });
    let arr = js_sys::Float64Array::new_with_length(2);
    arr.set_index(0, low);
    arr.set_index(1, high);
    arr
}

/// Desired win probability for the next game (Eq 24/25).
#[wasm_bindgen]
pub fn yuksel_desired_win_probability(
    wins: u32,
    games_played: u32,
    n: Option<u32>,
    lambda: Option<f64>,
    delta_p: Option<f64>,
) -> f64 {
    use crate::yuksel::*;
    let config = YukselMatchConfig {
        n: n.unwrap_or(5) as usize,
        lambda: lambda.unwrap_or(0.5),
        delta_p: delta_p.unwrap_or(1.0 / 12.0),
    };
    desired_win_probability(wins as usize, games_played as usize, &config)
}

/// Desired opponent rating r★ such that the player achieves win probability p (Eq 26).
#[wasm_bindgen]
pub fn yuksel_desired_opponent_rating(
    player_rating: f64,
    wins: u32,
    games_played: u32,
    n: Option<u32>,
    lambda: Option<f64>,
    delta_p: Option<f64>,
) -> f64 {
    use crate::yuksel::*;
    let config = YukselMatchConfig {
        n: n.unwrap_or(5) as usize,
        lambda: lambda.unwrap_or(0.5),
        delta_p: delta_p.unwrap_or(1.0 / 12.0),
    };
    desired_opponent_rating(player_rating, wins as usize, games_played as usize, &config)
}

/// Acceptable opponent rating window [r_low, r_high].
/// Returns Float64Array of [low, high].
#[wasm_bindgen]
pub fn yuksel_opponent_rating_window(
    player_rating: f64,
    wins: u32,
    games_played: u32,
    n: Option<u32>,
    lambda: Option<f64>,
    delta_p: Option<f64>,
) -> js_sys::Float64Array {
    use crate::yuksel::*;
    let config = YukselMatchConfig {
        n: n.unwrap_or(5) as usize,
        lambda: lambda.unwrap_or(0.5),
        delta_p: delta_p.unwrap_or(1.0 / 12.0),
    };
    let (low, high) =
        opponent_rating_window(player_rating, wins as usize, games_played as usize, &config);
    let arr = js_sys::Float64Array::new_with_length(2);
    arr.set_index(0, low);
    arr.set_index(1, high);
    arr
}

/// Match a group of players by sorting on desired opponent rating and pairing adjacent entries.
/// Returns Uint32Array of paired player indices: [a0, b0, a1, b1, ...].
#[wasm_bindgen]
pub fn yuksel_match_group(player_indices: &[u32], desired_ratings: &[f64]) -> js_sys::Uint32Array {
    use crate::yuksel::*;
    let mut pairs: Vec<(usize, f64)> = player_indices
        .iter()
        .zip(desired_ratings)
        .map(|(&idx, &r)| (idx as usize, r))
        .collect();
    let matches = match_group(&mut pairs);
    let arr = js_sys::Uint32Array::new_with_length((matches.len() * 2) as u32);
    for (i, (a, b)) in matches.iter().enumerate() {
        arr.set_index((i * 2) as u32, *a as u32);
        arr.set_index((i * 2 + 1) as u32, *b as u32);
    }
    arr
}
