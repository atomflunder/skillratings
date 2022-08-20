use std::f64::consts::{FRAC_1_SQRT_2, PI, SQRT_2};

use crate::{config::TrueSkillConfig, outcomes::Outcomes, rating::TrueSkillRating};
#[allow(clippy::doc_markdown)]
#[must_use]
/// Calculates the TrueSkill rating of two players based on their ratings, uncertainties, and the outcome of the game.
///
/// Takes in two players, outcome of the game and a [`TrueSkillConfig`].
///
/// Instead of the traditional way of calculating the TrueSkill for multiple teams with multiple people in them,
/// we are calculating the TrueSkill rating for two players only, like in the Elo Calculation.
///
/// The outcome of the match is in the perspective of `player_one`.
/// This means `Outcomes::WIN` is a win for `player_one` and `Outcomes::LOSS` is a win for `player_two`.
///
/// **Caution regarding usage of TrueSkill**:
/// Microsoft permits only Xbox Live games or non-commercial projects to use TrueSkill(TM).
/// If your project is commercial, you should use another rating system included here.
///
/// # Example
/// ```
/// use skillratings::{rating::TrueSkillRating, trueskill::trueskill, outcomes::Outcomes, config::TrueSkillConfig};
///
/// let player_one = TrueSkillRating::new();
/// let player_two = TrueSkillRating {
///     rating: 50.0,
///     uncertainty: 1.21,
/// };
///
/// let outcome = Outcomes::WIN;
///
/// let config = TrueSkillConfig::new();
///
/// let (player_one, player_two) = trueskill(player_one, player_two, outcome, &config);
///
/// assert!(((player_one.rating * 100.0).round() - 4410.0).abs() < f64::EPSILON);
/// assert!(((player_one.uncertainty * 100.0).round() - 528.0).abs() < f64::EPSILON);
///
/// assert!(((player_two.rating * 100.0).round() - 4960.0).abs() < f64::EPSILON);
/// assert!(((player_two.uncertainty * 100.0).round() - 121.0).abs() < f64::EPSILON);
/// ```
///
/// # More:
/// [Wikipedia Article about TrueSkill](https://en.wikipedia.org/wiki/TrueSkill).  
/// [TrueSkill: A Bayesian Skill Rating System (PDF)](https://proceedings.neurips.cc/paper/2006/file/f44ee263952e65b3610b8ba51229d1f9-Paper.pdf).  
/// [The math behind TrueSkill (PDF)](http://www.moserware.com/assets/computing-your-skill/The%20Math%20Behind%20TrueSkill.pdf).
pub fn trueskill(
    player_one: TrueSkillRating,
    player_two: TrueSkillRating,
    outcome: Outcomes,
    config: &TrueSkillConfig,
) -> (TrueSkillRating, TrueSkillRating) {
    let draw_margin = draw_margin(config.draw_probability, config.beta);

    let c = 2.0f64
        .mul_add(
            config.beta.powi(2),
            player_one
                .uncertainty
                .mul_add(player_one.uncertainty, player_two.uncertainty.powi(2)),
        )
        .sqrt();

    let winning_rating = if outcome == Outcomes::WIN || outcome == Outcomes::DRAW {
        player_one.rating
    } else {
        player_two.rating
    };
    let losing_rating = if outcome == Outcomes::WIN || outcome == Outcomes::DRAW {
        player_two.rating
    } else {
        player_one.rating
    };

    let rating_delta = winning_rating - losing_rating;

    let v = if outcome == Outcomes::DRAW {
        v_draw(rating_delta, draw_margin, c)
    } else {
        v_non_draw(rating_delta, draw_margin, c)
    };

    let w = if outcome == Outcomes::DRAW {
        w_draw(rating_delta, draw_margin, c)
    } else {
        w_non_draw(rating_delta, draw_margin, c)
    };

    let rank_multiplier1 = match outcome {
        Outcomes::WIN => 1.0,
        Outcomes::DRAW => 0.0,
        Outcomes::LOSS => -1.0,
    };

    let rank_multiplier2 = match outcome {
        Outcomes::WIN => -1.0,
        Outcomes::DRAW => 0.0,
        Outcomes::LOSS => 1.0,
    };

    let player_one_new = update_rating(
        player_one,
        v,
        w,
        c,
        config.default_dynamics,
        rank_multiplier1,
    );
    let player_two_new = update_rating(
        player_two,
        v,
        w,
        c,
        config.default_dynamics,
        rank_multiplier2,
    );

    (player_one_new, player_two_new)
}

#[must_use]
/// Calculates a `TrueSkill` Rating in a non-traditional way using a rating period,
/// for compatibility with the other algorithms.
///
/// Takes in a player and their results as a Vec of tuples containing the opponent and the outcome.
///
/// All of the outcomes are from the perspective of `player_one`.
/// This means `Outcomes::WIN` is a win for `player_one` and `Outcomes::LOSS` is a win for `player_two`.
///
/// # Example
/// ```
/// use skillratings::{
///     rating::TrueSkillRating, trueskill::trueskill_rating_period, outcomes::Outcomes, config::TrueSkillConfig
/// };
///
/// let player_one = TrueSkillRating::new();
/// let player_two = TrueSkillRating {
///     rating: 30.0,
///     uncertainty: 1.2,
/// };
/// let player_three = TrueSkillRating {
///     rating: 12.0,
///     uncertainty: 1.9,
/// };
/// let player_four = TrueSkillRating {
///     rating: 49.0,
///     uncertainty: 1.2,
/// };
///
/// let player = trueskill_rating_period(
///     player_one,
///     &vec![
///         (player_two, Outcomes::WIN),
///         (player_three, Outcomes::WIN),
///         (player_four, Outcomes::LOSS),
///     ],
///     &TrueSkillConfig::new(),
/// );
///
/// assert!(((player.rating * 100.0).round() - 3277.0).abs() < f64::EPSILON);
/// assert!(((player.uncertainty * 100.0).round() - 566.0).abs() < f64::EPSILON);
/// ```
pub fn trueskill_rating_period(
    player: TrueSkillRating,
    results: &Vec<(TrueSkillRating, Outcomes)>,
    config: &TrueSkillConfig,
) -> TrueSkillRating {
    let mut player = player;

    let draw_margin = draw_margin(config.draw_probability, config.beta);

    for (opponent, result) in results {
        let c = 2.0f64
            .mul_add(
                config.beta.powi(2),
                player
                    .uncertainty
                    .mul_add(player.uncertainty, opponent.uncertainty.powi(2)),
            )
            .sqrt();

        let winning_rating = if result == &Outcomes::WIN || result == &Outcomes::DRAW {
            player.rating
        } else {
            opponent.rating
        };
        let losing_rating = if result == &Outcomes::WIN || result == &Outcomes::DRAW {
            opponent.rating
        } else {
            player.rating
        };

        let rating_delta = winning_rating - losing_rating;

        let v = if result == &Outcomes::DRAW {
            v_draw(rating_delta, draw_margin, c)
        } else {
            v_non_draw(rating_delta, draw_margin, c)
        };

        let w = if result == &Outcomes::DRAW {
            w_draw(rating_delta, draw_margin, c)
        } else {
            w_non_draw(rating_delta, draw_margin, c)
        };

        let rank_multiplier = match *result {
            Outcomes::WIN => 1.0,
            Outcomes::DRAW => 0.0,
            Outcomes::LOSS => -1.0,
        };

        player = update_rating(player, v, w, c, config.default_dynamics, rank_multiplier);
    }

    player
}

#[must_use]
/// Gets the quality of the match, which is equal to the probability that the match will end in a draw.
/// The higher the Value, the better the quality of the match.
///
/// # Example
/// ```
/// use skillratings::{rating::TrueSkillRating, trueskill::match_quality, config::TrueSkillConfig};
///
/// let player_one = TrueSkillRating::new();
/// let player_two = TrueSkillRating::new();
///
/// let config = TrueSkillConfig::new();
///
/// let quality = match_quality(player_one, player_two, &config);
///
/// // According to TrueSkill, there is a 44.7% chance this match will end in a draw.
/// assert!(((quality * 1000.0).round() - 447.0).abs() < f64::EPSILON);
/// ```
pub fn match_quality(
    player_one: TrueSkillRating,
    player_two: TrueSkillRating,
    config: &TrueSkillConfig,
) -> f64 {
    let delta: f64 = player_one.rating - player_two.rating;

    let a = ((2.0 * config.beta.powi(2))
        / player_two.uncertainty.mul_add(
            player_two.uncertainty,
            2.0f64.mul_add(config.beta.powi(2), player_one.uncertainty.powi(2)),
        ))
    .sqrt();

    let b = ((-1.0 * delta.powi(2))
        / (2.0
            * player_two.uncertainty.mul_add(
                player_two.uncertainty,
                2.0f64.mul_add(config.beta.powi(2), player_one.uncertainty.powi(2)),
            )))
    .exp();

    a * b
}

#[must_use]
/// Calculates the expected outcome of two players based on glicko-2.
///
/// Takes in two players and the config and returns the probability of victory for each player.  
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// To see the actual chances of a draw occurring, please use [`match_quality`].
///
/// # Example
/// ```
/// use skillratings::{rating::TrueSkillRating, trueskill::expected_score, config::TrueSkillConfig};
///
/// let better_player = TrueSkillRating {
///     rating: 44.0,
///     uncertainty: 3.0,
/// };
/// let worse_player = TrueSkillRating {
///     rating: 38.0,
///     uncertainty: 3.0,
/// };
///
/// let config = TrueSkillConfig::new();
///
/// let (exp1, exp2) = expected_score(better_player, worse_player, &config);
///
/// assert!((exp1 * 100.0 - 80.0).round().abs() < f64::EPSILON);
/// assert!((exp2 * 100.0 - 20.0).round().abs() < f64::EPSILON);
///
/// assert!((exp1.mul_add(100.0, exp2 * 100.0).round() - 100.0).abs() < f64::EPSILON);
/// ```
pub fn expected_score(
    player_one: TrueSkillRating,
    player_two: TrueSkillRating,
    config: &TrueSkillConfig,
) -> (f64, f64) {
    let delta1 = player_one.rating - player_two.rating;
    let delta2 = player_two.rating - player_one.rating;

    let denom1 = player_two
        .uncertainty
        .mul_add(
            player_two.uncertainty,
            2.0f64.mul_add(config.beta.powi(2), player_one.uncertainty.powi(2)),
        )
        .sqrt();
    let denom2 = player_one
        .uncertainty
        .mul_add(
            player_one.uncertainty,
            2.0f64.mul_add(config.beta.powi(2), player_two.uncertainty.powi(2)),
        )
        .sqrt();

    (
        cdf(delta1 / denom1, 0.0, 1.0),
        cdf(delta2 / denom2, 0.0, 1.0),
    )
}

#[must_use]
/// Gets the conservatively estimated rank of a player using their rating and deviation.
///
/// This is a conservative estimate of player skill,
/// the system is 99% sure the player's skill is higher than displayed.
///
/// The recommended scale used for Xbox Live is 0 (lowest, starting value) to 50 (highest).
///
/// # Example
/// ```
/// use skillratings::{rating::TrueSkillRating, trueskill::get_rank};
///
/// let new_player = TrueSkillRating::new();
/// let older_player = TrueSkillRating {
///     rating: 43.1,
///     uncertainty: 1.92,
/// };
///
/// let new_rank = get_rank(new_player);
/// let older_rank = get_rank(older_player);
///
/// assert!((new_rank.round() - 0.0).abs() < f64::EPSILON);
/// assert!((older_rank.round() - 37.0).abs() < f64::EPSILON);
/// ```
pub fn get_rank(player: TrueSkillRating) -> f64 {
    player.rating - (player.uncertainty * 3.0)
}

fn draw_margin(draw_probability: f64, beta: f64) -> f64 {
    inverse_cdf(0.5 * (draw_probability + 1.0), 0.0, 1.0) * SQRT_2 * beta
}

fn v_non_draw(difference: f64, draw_margin: f64, c: f64) -> f64 {
    let norm = cdf(difference / c - draw_margin / c, 0.0, 1.0);

    if norm < 2.222_758_749e-162 {
        return (-1.0f64).mul_add(difference / c, draw_margin);
    }

    pdf((difference / c) - (draw_margin / c), 0.0, 1.0) / norm
}

fn w_non_draw(difference: f64, draw_margin: f64, c: f64) -> f64 {
    let norm = cdf(difference / c - draw_margin / c, 0.0, 1.0);

    if norm < 2.222_758_749e-162 {
        if (difference / c) < 0.0 {
            return 1.0;
        }
        return 0.0;
    }

    let v = v_non_draw(difference, draw_margin, c);

    v * (v + (difference / c) - (draw_margin / c))
}

fn v_draw(difference: f64, draw_margin: f64, c: f64) -> f64 {
    let difference_abs = difference.abs();

    let norm = cdf((draw_margin / c) - (difference_abs / c), 0.0, 1.0)
        - cdf((-1.0 * (draw_margin / c)) - (difference_abs / c), 0.0, 1.0);

    if norm < 2.222_758_749e-162 {
        if (difference / c) < 0.0 {
            return -1.0 * difference - draw_margin;
        }

        return (-1.0f64).mul_add(difference, draw_margin);
    }

    let x = pdf((-1.0 * (draw_margin / c)) - (difference_abs / c), 0.0, 1.0)
        - pdf((draw_margin / c) - (difference_abs / c), 0.0, 1.0);

    x / norm
}

fn w_draw(difference: f64, draw_margin: f64, c: f64) -> f64 {
    let difference_abs = difference.abs();

    let norm = cdf((draw_margin / c) - (difference_abs / c), 0.0, 1.0)
        - cdf((-1.0 * (draw_margin / c)) - (difference_abs / c), 0.0, 1.0);

    if norm < 2.222_758_749e-162 {
        return 1.0;
    }

    let v = v_draw(difference, draw_margin, c);

    v.mul_add(
        v,
        ((draw_margin / c - difference_abs / c)
            * pdf((draw_margin / c) - (difference_abs / c), 0.0, 1.0)
            - (-1.0 * (draw_margin / c) - (difference_abs / c))
                * pdf(-1.0 * (draw_margin / c) - (difference_abs / c), 0.0, 1.0))
            / norm,
    )
}

fn update_rating(
    player: TrueSkillRating,
    v: f64,
    w: f64,
    c: f64,
    default_dynamics: f64,
    rank_multiplier: f64,
) -> TrueSkillRating {
    let mean_multiplier = player
        .uncertainty
        .mul_add(player.uncertainty, default_dynamics.powi(2))
        / c;

    let variance = player
        .uncertainty
        .mul_add(player.uncertainty, default_dynamics.powi(2));
    let dev_multiplier = variance / c.powi(2);

    let new_rating = (rank_multiplier * mean_multiplier).mul_add(v, player.rating);
    let new_uncertainty = (variance * (1.0 - w * dev_multiplier)).sqrt();

    TrueSkillRating {
        rating: new_rating,
        uncertainty: new_uncertainty,
    }
}

/// The complementary error function.
fn erfc(x: f64) -> f64 {
    let z = x.abs();
    let t = 1.0 / (1.0 + z / 2.0);

    let r = t * t
        .mul_add(
            t.mul_add(
                t.mul_add(
                    t.mul_add(
                        t.mul_add(
                            t.mul_add(
                                t.mul_add(
                                    t.mul_add(t.mul_add(0.170_872_77, -0.822_152_23), 1.488_515_87),
                                    -1.135_203_98,
                                ),
                                0.278_868_07,
                            ),
                            -0.186_288_06,
                        ),
                        0.096_784_18,
                    ),
                    0.374_091_96,
                ),
                1.000_023_68,
            ),
            -z * z - 1.265_512_23,
        )
        .exp();

    if x < 0.0 {
        return 2.0 - r;
    }

    r
}

#[allow(clippy::excessive_precision)]
/// The inverse of the complementary error function.
fn inverse_erfc(mut y: f64) -> f64 {
    if y >= 2.0 {
        return -100.0;
    } else if y <= 0.0 {
        return 100.0;
    }

    let zero_point = y < 1.0;

    if !zero_point {
        y = 2.0 - y;
    }

    let t = (-2.0 * (y / 2.0).ln()).sqrt();

    let mut x = (-1.0 * FRAC_1_SQRT_2)
        * (t.mul_add(0.27061, 2.30753) / t.mul_add(t.mul_add(0.04481, 0.99229), 1.0) - t);

    for _ in 0..2 {
        let err = erfc(x) - y;
        x += err / (1.128_379_167_095_512_57 * (-1.0 * (x.powi(2))).exp() - x * err);
    }

    if zero_point {
        return x;
    }

    x * -1.0
}

/// The cumulative distribution function.
fn cdf(x: f64, mu: f64, sigma: f64) -> f64 {
    0.5 * erfc(-1.0 * (x - mu) / (sigma * SQRT_2))
}

/// The inverse of the cumulative distribution function.
fn inverse_cdf(x: f64, mu: f64, sigma: f64) -> f64 {
    mu - sigma * SQRT_2 * inverse_erfc(2.0 * x)
}

/// The probability density function.
fn pdf(x: f64, mu: f64, sigma: f64) -> f64 {
    1.0 / (2.0 * PI).sqrt() * sigma.abs() * (-1.0 * (((x - mu) / sigma.abs()).powi(2) / 2.0)).exp()
}

mod tests {
    #[allow(unused_imports)]
    use super::*;
    #[allow(unused_imports)]
    use std::f64::{INFINITY, NAN, NEG_INFINITY};

    #[test]
    fn test_trueskill() {
        // This example is taken from:
        // <https://ubm-twvideo01.s3.amazonaws.com/o1/vault/gdc2017/Presentations/Izquierdo_Mario_Ranking_Systems_Elo.pdf>
        // (Page 20)
        let player_one = TrueSkillRating::new();
        let player_two = TrueSkillRating {
            rating: 30.0,
            uncertainty: 1.2,
        };

        let (p1, p2) = trueskill(
            player_one,
            player_two,
            Outcomes::WIN,
            &TrueSkillConfig::new(),
        );

        assert!(((p1.rating * 100.0).round() - 3300.0).abs() < f64::EPSILON);
        assert!(((p1.uncertainty * 100.0).round() - 597.0).abs() < f64::EPSILON);

        assert!(((p2.rating * 100.0).round() - 2983.0).abs() < f64::EPSILON);
        assert!(((p2.uncertainty * 100.0).round() - 120.0).abs() < f64::EPSILON);

        let (p1, p2) = trueskill(
            player_two,
            player_one,
            Outcomes::LOSS,
            &TrueSkillConfig::new(),
        );

        assert!(((p2.rating * 100.0).round() - 3300.0).abs() < f64::EPSILON);
        assert!(((p2.uncertainty * 100.0).round() - 597.0).abs() < f64::EPSILON);

        assert!(((p1.rating * 100.0).round() - 2983.0).abs() < f64::EPSILON);
        assert!(((p1.uncertainty * 100.0).round() - 120.0).abs() < f64::EPSILON);

        let player_two = TrueSkillRating::new();

        let (p1, p2) = trueskill(
            player_one,
            player_two,
            Outcomes::WIN,
            &TrueSkillConfig::new(),
        );

        assert!((p1.rating.round() - 29.0).abs() < f64::EPSILON);
        assert!(((p1.uncertainty * 100.0).round() - 717.0).abs() < f64::EPSILON);

        assert!((p2.rating.round() - 21.0).abs() < f64::EPSILON);
        assert!(((p2.uncertainty * 100.0).round() - 717.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_trueskill_rating_period() {
        let player_one = TrueSkillRating::new();
        let player_two = TrueSkillRating {
            rating: 30.0,
            uncertainty: 1.2,
        };
        let player_three = TrueSkillRating {
            rating: 12.0,
            uncertainty: 1.9,
        };
        let player_four = TrueSkillRating {
            rating: 49.0,
            uncertainty: 1.2,
        };

        let player = trueskill_rating_period(
            player_one,
            &vec![(player_two, Outcomes::WIN)],
            &TrueSkillConfig::new(),
        );

        assert!(((player.rating * 100.0).round() - 3300.0).abs() < f64::EPSILON);
        assert!(((player.uncertainty * 100.0).round() - 597.0).abs() < f64::EPSILON);

        let player = trueskill_rating_period(
            player_one,
            &vec![
                (player_two, Outcomes::WIN),
                (player_three, Outcomes::DRAW),
                (player_four, Outcomes::LOSS),
            ],
            &TrueSkillConfig::new(),
        );

        assert!(((player.rating * 100.0).round() - 3288.0).abs() < f64::EPSILON);
        assert!(((player.uncertainty * 100.0).round() - 423.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_draw() {
        let player_one = TrueSkillRating::new();
        let player_two = TrueSkillRating::new();

        let (p1, p2) = trueskill(
            player_one,
            player_two,
            Outcomes::DRAW,
            &TrueSkillConfig::new(),
        );

        assert!((p1.rating.round() - 25.0).abs() < f64::EPSILON);
        assert!(((p1.uncertainty * 100.0).round() - 646.0).abs() < f64::EPSILON);

        assert!((p2.rating.round() - 25.0).abs() < f64::EPSILON);
        assert!(((p2.uncertainty * 100.0).round() - 646.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_unlikely_values() {
        let player_one = TrueSkillRating {
            rating: -9.0,
            uncertainty: -5.0,
        };
        let player_two = TrueSkillRating {
            rating: 7000.0,
            uncertainty: 6000.0,
        };

        let (p1, p2) = trueskill(
            player_one,
            player_two,
            Outcomes::WIN,
            &TrueSkillConfig::new(),
        );

        assert!((p1.rating.round() - -9.0).abs() < f64::EPSILON);
        assert!((p1.uncertainty.round() - 5.0).abs() < f64::EPSILON);

        assert!((p2.rating.round() - -2969.0).abs() < f64::EPSILON);
        assert!((p2.uncertainty.round() - 2549.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_quality() {
        let player_one = TrueSkillRating::new();
        let player_two = TrueSkillRating::new();

        let quality = match_quality(player_one, player_two, &TrueSkillConfig::new());

        assert!(((quality * 1000.0).round() - 447.0).abs() < f64::EPSILON);

        let player_one = TrueSkillRating {
            rating: 48.0,
            uncertainty: 1.2,
        };
        #[allow(clippy::default_trait_access)]
        let player_two = TrueSkillRating {
            rating: 12.0,
            ..Default::default()
        };

        let quality = match_quality(player_one, player_two, &TrueSkillConfig::new());

        assert!(((quality * 10000.0).round() - 12.0).abs() < f64::EPSILON);

        let quality2 = match_quality(player_two, player_one, &TrueSkillConfig::new());

        assert!((quality - quality2).abs() < f64::EPSILON);
    }

    #[test]
    fn test_expected_score() {
        let player_one = TrueSkillRating::new();
        let player_two = TrueSkillRating::new();

        let (exp1, exp2) = expected_score(player_one, player_two, &TrueSkillConfig::new());

        assert!((exp1 * 100.0 - 50.0).round().abs() < f64::EPSILON);
        assert!((exp2 * 100.0 - 50.0).round().abs() < f64::EPSILON);

        let better_player = TrueSkillRating {
            rating: 44.0,
            uncertainty: 3.0,
        };
        let worse_player = TrueSkillRating {
            rating: 38.0,
            uncertainty: 3.0,
        };

        let (exp1, exp2) = expected_score(better_player, worse_player, &TrueSkillConfig::default());

        assert!((exp1 * 100.0 - 80.0).round().abs() < f64::EPSILON);
        assert!((exp2 * 100.0 - 20.0).round().abs() < f64::EPSILON);

        assert!((exp1.mul_add(100.0, exp2 * 100.0).round() - 100.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_get_rank() {
        let new_player = TrueSkillRating::new();
        let older_player = TrueSkillRating {
            rating: 43.1,
            uncertainty: 1.92,
        };

        let new_rank = get_rank(new_player);
        let older_rank = get_rank(older_player);

        assert!((new_rank.round() - 0.0).abs() < f64::EPSILON);
        assert!((older_rank.round() - 37.0).abs() < f64::EPSILON);
    }

    // Some tests to validate the math.
    #[test]
    fn test_erfc() {
        let err = erfc(0.5);

        assert!((err - 0.479_500).abs() < 0.000_001);
    }

    #[test]
    fn test_inverse_erfc() {
        let err = inverse_erfc(0.5);

        assert!((err - 0.476_936).abs() < 0.000_001);

        let err = inverse_erfc(3.0);

        assert!((err + 100.0).abs() < f64::EPSILON);

        let err = inverse_erfc(-1.0);

        assert!((err - 100.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_cdf() {
        let dist = cdf(5.5, 12.0, 5.0);

        assert!((dist - 0.096_800).abs() < 0.000_001);

        let dist_inf = cdf(INFINITY, 0.0, 1.0);

        assert!((dist_inf - 1.0).abs() < f64::EPSILON);

        let dist_neg_inf = cdf(NEG_INFINITY, 0.0, 1.0);

        assert!((dist_neg_inf - 0.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_inverse_cdf() {
        let dist = inverse_cdf(0.055, 12.0, 5.0);

        assert!((dist - 4.009_034).abs() < 0.000_001);
    }

    #[test]
    fn test_pdf() {
        let prob = pdf(2.5, 0.0, 1.0);

        assert!((prob - 0.017_528).abs() < 0.000_001);
    }

    #[test]
    fn test_wv_edge_cases() {
        let w = w_non_draw(NEG_INFINITY, 0.0, 1.0);

        assert!((w - 1.0).abs() < f64::EPSILON);

        let v = v_non_draw(NEG_INFINITY, 0.0, 1.0);

        assert!(v == INFINITY);

        let w2 = w_draw(NEG_INFINITY, 0.0, 1.0);

        assert!((w2 - 1.0).abs() < f64::EPSILON);

        let v2 = v_draw(NEG_INFINITY, 0.0, 1.0);

        assert!(v2 == INFINITY);

        let w3 = w_non_draw(1.0, INFINITY, 1.0);

        assert!((w3 - 0.0).abs() < f64::EPSILON);

        let v3 = v_draw(INFINITY, f64::MAX, 1.0);

        assert!(v3 == NEG_INFINITY);
    }
}
