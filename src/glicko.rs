use crate::{outcomes::Outcomes, rating::GlickoRating};
use std::f64::consts::PI;

#[must_use]
/// Calculates the glicko scores of two players based on their ratings, deviations, and the outcome of the game.
///
/// Please see [`crate::glicko2::glicko2`] for calculating with the improved version.
///
/// Takes in two players and the outcome of the game.
///
/// Instead of the traditional way of calculating the Glicko for only one player only using a list of results,
/// we are calculating the Glicko rating for two players at once, like in the Elo calculation,
/// to make it easier to see instant results.
///
/// The outcome of the match is in the perspective of `player_one`.
/// This means `Outcomes::WIN` is a win for `player_one` and `Outcomes::LOSS` is a win for `player_two`.
///
/// # Example
/// ```
/// use skillratings::{glicko::glicko, outcomes::Outcomes, rating::GlickoRating};
///
/// let player_one = GlickoRating {
///     rating: 1500.0,
///     deviation: 350.0,
/// };
/// let player_two = GlickoRating {
///     rating: 1500.0,
///     deviation: 350.0,
/// };
///
/// let outcome = Outcomes::WIN;
///
/// let (player_one_new, player_two_new) = glicko(player_one, player_two, outcome);
///
/// assert!((player_one_new.rating.round() - 1662.0).abs() < f64::EPSILON);
/// assert!((player_one_new.deviation.round() - 290.0).abs() < f64::EPSILON);
///
/// assert!((player_two_new.rating.round() - 1338.0).abs() < f64::EPSILON);
/// assert!((player_two_new.deviation.round() - 290.0).abs() < f64::EPSILON);
/// ```
///
/// # More
/// [Wikipedia Article on the Glicko system](https://en.wikipedia.org/wiki/Glicko_rating_system).  
/// [Example of the Glicko system](http://www.glicko.net/glicko/glicko.pdf).
pub fn glicko(
    player_one: GlickoRating,
    player_two: GlickoRating,
    outcome: Outcomes,
) -> (GlickoRating, GlickoRating) {
    let q = 10_f64.ln() / 400.0;

    let outcome1 = match outcome {
        Outcomes::WIN => 1.0,
        Outcomes::DRAW => 0.5,
        Outcomes::LOSS => 0.0,
    };
    let outcome2 = 1.0 - outcome1;

    let g1 = g_value(q, player_two.deviation);
    let g2 = g_value(q, player_one.deviation);

    let e1 = e_value(g1, player_one.rating, player_two.rating);
    let e2 = e_value(g2, player_two.rating, player_one.rating);

    let d1 = d_value(q, g1, e1);
    let d2 = d_value(q, g2, e2);

    let player_one_new_rating = new_rating(
        player_one.rating,
        player_one.deviation,
        outcome1,
        q,
        g1,
        e1,
        d1,
    );
    let player_two_new_rating = new_rating(
        player_two.rating,
        player_two.deviation,
        outcome2,
        q,
        g2,
        e2,
        d2,
    );

    let player_one_new_deviation = new_deviation(player_one.deviation, d1);
    let player_two_new_deviation = new_deviation(player_two.deviation, d2);

    (
        GlickoRating {
            rating: player_one_new_rating,
            deviation: player_one_new_deviation,
        },
        GlickoRating {
            rating: player_two_new_rating,
            deviation: player_two_new_deviation,
        },
    )
}

#[must_use]
/// Calculates the expected outcome of two players based on glicko.
///
/// Takes in two players and returns the probability of victory for each player.  
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// # Example
/// ```
/// use skillratings::{glicko::expected_score, rating::GlickoRating};
///
/// let player_one = GlickoRating {
///     rating: 2500.0,
///     deviation: 41.0,
/// };
/// let player_two = GlickoRating {
///     rating: 1950.0,
///     deviation: 320.0,
/// };
/// let (exp_one, exp_two) = expected_score(player_one, player_two);
/// assert!(((exp_one * 100.0).round() - 90.0).abs() < f64::EPSILON);
/// assert!(((exp_two * 100.0).round() - 10.0).abs() < f64::EPSILON);
/// ```
pub fn expected_score(player_one: GlickoRating, player_two: GlickoRating) -> (f64, f64) {
    let q = 10_f64.ln() / 400.0;
    let g = g_value(q, player_one.deviation.hypot(player_two.deviation));

    let expected_one =
        (1.0 + 10_f64.powf(-1.0 * g * (player_one.rating - player_two.rating) / 400.0)).recip();

    (expected_one, (1.0 - expected_one))
}

#[must_use]
/// Decays a Rating Deviation Value for a player, if they missed playing in a certain rating period.
///
/// The length of the rating period and thus the number of missed periods per player is something to decide and track yourself.
///
/// Takes in a player and a `c` value, that describes how much the rating should change.
/// In [the paper](http://www.glicko.net/glicko/glicko.pdf) a value of 63.2 seems to be a suggested value.
/// The higher the value, the more the rating deviation will decay.  
/// Returns the player with the Rating Deviation Value adjusted.
///
/// # Example
/// ```
/// use skillratings::{glicko::decay_deviation, rating::GlickoRating};
///
/// let player_one = GlickoRating {
///     rating: 2720.0,
///     deviation: 41.3,
/// };
///
/// let player_one_decay = decay_deviation(player_one, 63.2);
///
/// assert!((player_one_decay.deviation.round() - 75.0).abs() < f64::EPSILON);
/// ```
pub fn decay_deviation(player: GlickoRating, c: f64) -> GlickoRating {
    let new_player_deviation = player.deviation.hypot(c).min(350.0);

    GlickoRating {
        rating: player.rating,
        deviation: new_player_deviation,
    }
}

#[must_use]
/// The 95% confidence interval of the lowest to highest rating.
///
/// The system is 95% sure that the "true skill" of the player is inbetween these values.
///
/// # Example
/// ```rust
/// use skillratings::{rating::GlickoRating, glicko::confidence_interval};
///
/// let player = GlickoRating {
///     rating: 2250.0,
///     deviation: 79.0,
/// };
///
/// let (interval_low, interval_high) = confidence_interval(player);
///
/// assert!(interval_low.round() - 2095.0 < f64::EPSILON);
/// assert!(interval_high.round() - 2405.0 < f64::EPSILON);
/// ```
pub fn confidence_interval(player: GlickoRating) -> (f64, f64) {
    (
        // Seems like there is no mul_sub function.
        player.rating - 1.96 * player.deviation,
        1.96f64.mul_add(player.deviation, player.rating),
    )
}

/// The new rating deviation value of the glicko calculation.
/// For more information, see: <http://www.glicko.net/glicko/glicko.pdf>
fn new_deviation(old_deviation: f64, d: f64) -> f64 {
    (old_deviation.powi(2).recip() + d.recip()).recip().sqrt()
}

/// The new rating value of the glicko calculation.
/// For more information, see: <http://www.glicko.net/glicko/glicko.pdf>
fn new_rating(old_rating: f64, deviation: f64, score: f64, q: f64, g: f64, e: f64, d: f64) -> f64 {
    ((q / (deviation.powi(2).recip() + d.recip())) * g).mul_add(score - e, old_rating)
}

/// The g value of the glicko calculation.
/// For more information, see: <http://www.glicko.net/glicko/glicko.pdf>
fn g_value(q: f64, opponent_deviation: f64) -> f64 {
    (1.0 + ((3.0 * q.powi(2) * opponent_deviation.powi(2)) / (PI.powi(2))))
        .sqrt()
        .recip()
}

/// The e value of the glicko calculation.
/// For more information, see: <http://www.glicko.net/glicko/glicko.pdf>
fn e_value(g: f64, rating: f64, opponent_rating: f64) -> f64 {
    (1.0 + (10_f64.powf(-1.0 * g * (rating - opponent_rating) / 400.0))).recip()
}

/// The d value of the glicko calculation.
/// For more information, see: <http://www.glicko.net/glicko/glicko.pdf>
fn d_value(q: f64, g: f64, e: f64) -> f64 {
    (q.powi(2) * g.powi(2) * e * (1.0 - e)).powi(-1)
}

mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    /// This test is taken directly from the official glicko example.  
    /// <http://www.glicko.net/glicko/glicko.pdf>
    fn test_glicko() {
        let player1 = GlickoRating {
            rating: 1500.0,
            deviation: 200.0,
        };

        let opponent1 = GlickoRating {
            rating: 1400.0,
            deviation: 30.0,
        };

        let opponent2 = GlickoRating {
            rating: 1550.0,
            deviation: 100.0,
        };

        let opponent3 = GlickoRating {
            rating: 1700.0,
            deviation: 300.0,
        };

        let (player1, _) = glicko(player1, opponent1, Outcomes::WIN);

        let (player1, _) = glicko(player1, opponent2, Outcomes::LOSS);

        let (player1, _) = glicko(player1, opponent3, Outcomes::LOSS);

        assert!((player1.rating.round() - 1464.0).abs() < f64::EPSILON);
        assert!((player1.deviation - 151.253_743_431_783_2).abs() < f64::EPSILON);
    }

    #[test]
    /// This test is taken directly from the official glicko example.  
    /// <http://www.glicko.net/glicko/glicko.pdf>
    fn test_expected_score() {
        let player_one = GlickoRating {
            rating: 1400.0,
            deviation: 40.0,
        };

        let player_two = GlickoRating {
            rating: 1500.0,
            deviation: 150.0,
        };

        let (exp_one, exp_two) = expected_score(player_one, player_two);

        assert!((exp_one - 0.373_700_405_951_935).abs() < f64::EPSILON);
        assert!((exp_two - 0.626_299_594_048_065).abs() < f64::EPSILON);
        assert!((exp_one + exp_two - 1.0).abs() < f64::EPSILON);
    }

    #[test]
    /// This test is taken directly from the official glicko example.  
    /// <http://www.glicko.net/glicko/glicko.pdf>
    fn test_confidence_interval() {
        let player = GlickoRating {
            rating: 1500.0,
            deviation: 30.0,
        };

        let ci = confidence_interval(player);

        assert!((ci.0.round() - 1441.0).abs() < f64::EPSILON);
        assert!((ci.1.round() - 1559.0).abs() < f64::EPSILON);
    }

    #[test]
    /// This test is taken directly from the official glicko example.  
    /// <http://www.glicko.net/glicko/glicko.pdf>
    fn test_decay_deviation() {
        let player = GlickoRating {
            rating: 1500.0,
            deviation: 50.0,
        };

        let mut player = decay_deviation(player, 63.2);

        assert!((player.deviation - 80.586_847_562_117_73).abs() < f64::EPSILON);

        for _ in 0..29 {
            player = decay_deviation(player, 63.2);
        }

        assert!(((player.deviation * 1000.0).round() - 349_753.0).abs() < f64::EPSILON);

        player = decay_deviation(player, 63.2);

        assert!((player.deviation - 350.0).abs() < f64::EPSILON);
    }
}
