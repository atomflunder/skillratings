//! The Glicko algorithm, developed by Mark Glickman as an improvement on Elo.  
//! It is still being used in some games in favor Glicko-2, such as Pokémon Showdown and Quake Live.
//!
//! If you are looking for the updated Glicko-2 rating system, please see [`Glicko-2`](crate::glicko2).
//!
//! The main improvement over Elo is the rating deviation introduced,
//! which decreases over time as the player plays more matches and the rating becomes more reliable.
//! This allows players to rise and fall through the ranks quickly at the beginning,
//! and not gain or lose as much rating points after completing more matches.
//!
//! # Quickstart
//!
//! This is the most basic example on how to use the Glicko Module.  
//! Please take a look at the functions below to see more advanced use cases.
//!
//! ```
//! use skillratings::{
//!     glicko::glicko, outcomes::Outcomes, rating::GlickoRating
//! };
//!
//! // Initialise a new player rating.
//! let player_one = GlickoRating::new();
//!
//! // Or you can initialise it with your own values of course.
//! // Imagine these numbers being pulled from a database.
//! let (some_rating, some_deviation) = (1325.0, 230.0);
//! let player_two = GlickoRating{
//!     rating: some_rating,
//!     deviation: some_deviation,
//! };
//!
//! // The outcome of the match is from the perspective of player one.
//! let outcome = Outcomes::WIN;
//!
//! // The glicko function will calculate the new ratings for both players and return them.
//! let (new_player_one, new_player_two) = glicko(&player_one, &player_two, &outcome);
//! ```
//!
//! # More Information
//!
//! - [Wikipedia Article](https://en.wikipedia.org/wiki/Glicko_rating_system)
//! - [Original Paper by Mark Glickman](http://www.glicko.net/glicko/glicko.pdf)
//! - [Glicko Calculator](http://www.bjcox.com/?page_id=2)

use crate::{config::GlickoConfig, outcomes::Outcomes, rating::GlickoRating};
use std::f64::consts::PI;

#[must_use]
/// Calculates the [`GlickoRating`]s of two players based on their old ratings, deviations, and the outcome of the game.
///
/// Please see [`Glicko-2`](crate::glicko2) for calculating with the improved version.
///
/// Takes in two players as [`GlickoRating`]s, and an [`Outcome`](Outcomes).
///
/// Instead of the traditional way of calculating the Glicko for only one player only using a list of results,
/// we are calculating the Glicko rating for two players at once, like in the Elo calculation,
/// to make it easier to see instant results.
///
/// For the traditional way of calculating a Glicko rating please see [`glicko_rating_period`].
///
/// The outcome of the match is in the perspective of `player_one`.
/// This means [`Outcomes::WIN`] is a win for `player_one` and [`Outcomes::LOSS`] is a win for `player_two`.
///
/// # Examples
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
/// let (player_one_new, player_two_new) = glicko(&player_one, &player_two, &outcome);
///
/// assert!((player_one_new.rating.round() - 1662.0).abs() < f64::EPSILON);
/// assert!((player_one_new.deviation.round() - 290.0).abs() < f64::EPSILON);
///
/// assert!((player_two_new.rating.round() - 1338.0).abs() < f64::EPSILON);
/// assert!((player_two_new.deviation.round() - 290.0).abs() < f64::EPSILON);
/// ```
pub fn glicko(
    player_one: &GlickoRating,
    player_two: &GlickoRating,
    outcome: &Outcomes,
) -> (GlickoRating, GlickoRating) {
    let q = 10_f64.ln() / 400.0;

    let outcome1 = outcome.to_chess_points();
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

/// The "traditional" way of calculating a [`GlickoRating`] of a player in a rating period.
///
/// Note that in this case, all of the matches are considered to be played at once.  
/// This means that the player will not get updated in-between matches, as you might expect.  
/// This will result in *slightly* different results than if you were to use the [`glicko`] function in a loop.
///
/// Takes in a player as an [`GlickoRating`] and their results as a Vec of tuples containing the opponent as an [`GlickoRating`],
/// the outcome of the game as an [`Outcome`](Outcomes) and a [`GlickoConfig`].
///
/// The outcome of the match is in the perspective of the player.
/// This means [`Outcomes::WIN`] is a win for the player and [`Outcomes::LOSS`] is a win for the opponent.
///
/// If the player's results are empty, the player's rating deviation will automatically be decayed using [`decay_deviation`].
///
/// # Examples
/// ```
/// use skillratings::{glicko::glicko_rating_period, outcomes::Outcomes, rating::GlickoRating, config::GlickoConfig};
///
/// let player = GlickoRating {
///     rating: 1500.0,
///     deviation: 200.0,
/// };
///
/// let opponent1 = GlickoRating {
///     rating: 1400.0,
///     deviation: 30.0,
/// };
///
/// let opponent2 = GlickoRating {
///     rating: 1550.0,
///     deviation: 100.0,
/// };
///
/// let opponent3 = GlickoRating {
///     rating: 1700.0,
///     deviation: 300.0,
/// };
///
/// let results = vec![
///     (opponent1, Outcomes::WIN),
///     (opponent2, Outcomes::LOSS),
///     (opponent3, Outcomes::LOSS),
/// ];
///
/// let config = GlickoConfig::new();
///
/// let new_player = glicko_rating_period(&player, &results, &config);
///
/// assert!((new_player.rating.round() - 1464.0).abs() < f64::EPSILON);
/// assert!((new_player.deviation.round() - 151.0).abs() < f64::EPSILON);
/// ```
#[must_use]
pub fn glicko_rating_period(
    player: &GlickoRating,
    results: &Vec<(GlickoRating, Outcomes)>,
    config: &GlickoConfig,
) -> GlickoRating {
    let q = 10_f64.ln() / 400.0;

    if results.is_empty() {
        return decay_deviation(player, config);
    }

    let d_sq: f64 = (q.powi(2)
        * results
            .iter()
            .map(|r| {
                let g = g_value(q, r.0.deviation);

                let e = e_value(g, player.rating, r.0.rating);

                g.powi(2) * e * (1.0 - e)
            })
            .sum::<f64>())
    .recip();

    let m = results
        .iter()
        .map(|r| {
            let g = g_value(q, r.0.deviation);

            let e = e_value(g, player.rating, r.0.rating);

            let s = r.1.to_chess_points();

            g * (s - e)
        })
        .sum::<f64>();

    let new_rating =
        (q / (player.deviation.powi(2).recip() + d_sq.recip())).mul_add(m, player.rating);
    let new_deviation = (player.deviation.powi(2).recip() + d_sq.recip())
        .recip()
        .sqrt();

    GlickoRating {
        rating: new_rating,
        deviation: new_deviation,
    }
}

#[must_use]
/// Calculates the expected outcome of two players based on glicko.
///
/// Takes in two players as [`GlickoRating`]s and returns the probability of victory for each player as an [`f64`] between 1.0 and 0.0.  
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// # Examples
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
/// let (exp_one, exp_two) = expected_score(&player_one, &player_two);
/// assert!(((exp_one * 100.0).round() - 90.0).abs() < f64::EPSILON);
/// assert!(((exp_two * 100.0).round() - 10.0).abs() < f64::EPSILON);
/// ```
pub fn expected_score(player_one: &GlickoRating, player_two: &GlickoRating) -> (f64, f64) {
    let q = 10_f64.ln() / 400.0;
    let g = g_value(q, player_one.deviation.hypot(player_two.deviation));

    let exp_one = (1.0 + 10_f64.powf(-g * (player_one.rating - player_two.rating) / 400.0)).recip();
    let exp_two = 1.0 - exp_one;

    (exp_one, exp_two)
}

#[must_use]
/// Decays a Rating Deviation Value for a player, if they missed playing in a certain rating period.
///
/// The length of the rating period and thus the number of missed periods per player is something to decide and track yourself.
///
/// Takes in a player as a [`GlickoRating`] and a [`GlickoConfig`], that describes how much the rating should change, and returns the decayed [`GlickoRating`].
///
/// # Examples
/// ```
/// use skillratings::{glicko::decay_deviation, rating::GlickoRating, config::GlickoConfig};
///
/// let player_one = GlickoRating {
///     rating: 2720.0,
///     deviation: 41.3,
/// };
///
/// let config = GlickoConfig::new();
///
/// let player_one_decay = decay_deviation(&player_one, &config);
///
/// assert!((player_one_decay.deviation.round() - 75.0).abs() < f64::EPSILON);
/// ```
pub fn decay_deviation(player: &GlickoRating, config: &GlickoConfig) -> GlickoRating {
    let new_player_deviation = player.deviation.hypot(config.c).min(350.0);

    GlickoRating {
        rating: player.rating,
        deviation: new_player_deviation,
    }
}

#[must_use]
/// The 95% confidence interval of the lowest to highest rating.
///
/// The system is 95% sure that the "true skill" of the player is in-between these values.
///
/// Takes in a player as a [`GlickoRating`] and returns two [`f64`]s that describe the lowest and highest rating.
///
/// # Examples
/// ```rust
/// use skillratings::{rating::GlickoRating, glicko::confidence_interval};
///
/// let player = GlickoRating {
///     rating: 2250.0,
///     deviation: 79.0,
/// };
///
/// let (interval_low, interval_high) = confidence_interval(&player);
///
/// assert!(interval_low.round() - 2095.0 < f64::EPSILON);
/// assert!(interval_high.round() - 2405.0 < f64::EPSILON);
/// ```
pub fn confidence_interval(player: &GlickoRating) -> (f64, f64) {
    (
        // Seems like there is no mul_sub function.
        player.rating - 1.96 * player.deviation,
        1.96f64.mul_add(player.deviation, player.rating),
    )
}

fn new_deviation(old_deviation: f64, d: f64) -> f64 {
    (old_deviation.powi(2).recip() + d.recip()).recip().sqrt()
}

fn new_rating(old_rating: f64, deviation: f64, score: f64, q: f64, g: f64, e: f64, d: f64) -> f64 {
    ((q / (deviation.powi(2).recip() + d.recip())) * g).mul_add(score - e, old_rating)
}

fn g_value(q: f64, opponent_deviation: f64) -> f64 {
    (1.0 + ((3.0 * q.powi(2) * opponent_deviation.powi(2)) / (PI.powi(2))))
        .sqrt()
        .recip()
}

fn e_value(g: f64, rating: f64, opponent_rating: f64) -> f64 {
    (1.0 + (10_f64.powf(-g * (rating - opponent_rating) / 400.0))).recip()
}

fn d_value(q: f64, g: f64, e: f64) -> f64 {
    (q.powi(2) * g.powi(2) * e * (1.0 - e)).powi(-1)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
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

        let (player1, _) = glicko(&player1, &opponent1, &Outcomes::WIN);

        let (player1, _) = glicko(&player1, &opponent2, &Outcomes::LOSS);

        let (player1, _) = glicko(&player1, &opponent3, &Outcomes::LOSS);

        assert!((player1.rating.round() - 1464.0).abs() < f64::EPSILON);
        assert!((player1.deviation - 151.253_743_431_783_2).abs() < f64::EPSILON);
    }

    #[test]
    /// This test is taken directly from the official glicko example.  
    /// <http://www.glicko.net/glicko/glicko.pdf>
    fn test_glicko_rating_period() {
        let player = GlickoRating {
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

        let results = vec![
            (opponent1, Outcomes::WIN),
            (opponent2, Outcomes::LOSS),
            (opponent3, Outcomes::LOSS),
        ];

        let new_player = glicko_rating_period(&player, &results, &GlickoConfig::new());

        assert!((new_player.rating.round() - 1464.0).abs() < f64::EPSILON);
        assert!((new_player.deviation - 151.398_902_447_969_33).abs() < f64::EPSILON);

        let player = GlickoRating {
            rating: 1500.0,
            deviation: 50.0,
        };

        let results: Vec<(GlickoRating, Outcomes)> = Vec::new();

        let new_player = glicko_rating_period(&player, &results, &GlickoConfig::new());

        assert!((new_player.deviation - 80.586_847_562_117_73).abs() < f64::EPSILON);
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

        let (exp_one, exp_two) = expected_score(&player_one, &player_two);

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

        let ci = confidence_interval(&player);

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

        let mut player = decay_deviation(&player, &GlickoConfig::new());

        assert!((player.deviation - 80.586_847_562_117_73).abs() < f64::EPSILON);

        for _ in 0..29 {
            player = decay_deviation(&player, &GlickoConfig::default());
        }

        assert!(((player.deviation * 1000.0).round() - 349_753.0).abs() < f64::EPSILON);

        player = decay_deviation(&player, &GlickoConfig::new());

        assert!((player.deviation - 350.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_unequal_draws() {
        let mut player = GlickoRating::new();

        let mut opponent = GlickoRating {
            rating: 2230.0,
            deviation: 41.0,
        };

        (player, opponent) = glicko(&player, &opponent, &Outcomes::DRAW);

        assert!((player.rating.round() - 1820.0).abs() < f64::EPSILON);
        assert!((player.deviation.round() - 340.0).abs() < f64::EPSILON);

        assert!((opponent.rating.round() - 2227.0).abs() < f64::EPSILON);
        assert!((opponent.deviation.round() - 41.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_default() {
        let player_new = GlickoRating::new();
        let player_default = GlickoRating::default();

        assert!((player_new.rating - player_default.rating).abs() < f64::EPSILON);
        assert!((player_new.deviation - player_new.deviation).abs() < f64::EPSILON);
    }
}
