//! The Glicko-Boost rating algorithm, an improvement on the Glicko rating system designed specifically for chess.\
//! Allows for player advantages, designed for a chess outcome prediction competition.
//!
//! For the original Glicko algorithm, please see [`Glicko`](crate::glicko), or [`Glicko-2`](crate::glicko2).\
//! For an alternative update to the Glicko system, please see [`Sticko`](crate::sticko).
//!
//! In 2012, the data prediction website [Kaggle](https://kaggle.com) hosted the "FIDE/Deloitte Chess Rating Challenge"
//!  where competitors where asked to create a new, more accurate chess rating system.\
//! This is the improved Glicko rating system that Mark Glickman entered.
//!
//! The main improvement over Glicko are the new configurable parameters found in the [`GlickoBoostConfig`]:
//!
//! - Eta (η) an advantage parameter that accounts for inherent advantages in-game (White in Chess, etc.)
//! - B1 and B2 are boost parameters that increase Rating Deviations for "exceptional performances", controlled by the k-parameter
//! - Alpha (α) 0-4 are five parameters to decay idle player's Rating Deviations more accurately.
//!
//! These make Glicko-Boost more configurable and possibly more accurate than the Glicko algorithm.
//! The Rating Boost allows over-achieving players to climb incredibly quickly.\
//! When all parameters (except Alpha (α)) are set to 0, the Glicko-Boost algorithm will produce the exact same results as Glicko.
//!
//! Please note that in this implementation, it does not make much sense to re-rate the player's as described in the [original paper](http://glicko.net/glicko/glicko-boost.pdf),
//! due to the fact that we only play each player once, and not rate a whole tournament.\
//! This means that compared to Table 3 in the original paper, we "skip" Steps 2 and 4, the ratings that are calculated here are comparable to the ratings described in Step 3.
//!
//! # Quickstart
//!
//! ```
//! use skillratings::{
//!     glicko_boost::{glicko_boost, GlickoBoostConfig, GlickoBoostRating},
//!     Outcomes,
//! };
//!
//! // Initialise a new player rating with a rating of 1500 and a deviation of 350.
//! let player_one = GlickoBoostRating::new();
//!
//! // Or you can initialise it with your own values of course.
//! // Imagine these numbers being pulled from a database.
//! let (some_rating, some_deviation) = (1325.0, 230.0);
//! let player_two = GlickoBoostRating {
//!     rating: some_rating,
//!     deviation: some_deviation,
//! };
//!
//! // The outcome of the match is from the perspective of player one.
//! let outcome = Outcomes::WIN;
//!
//! // The config allows you to specify certain values in the Glicko-Boost calculation.
//! // For more information on how to customise the config,
//! // please check out the GlickoBoostConfig struct.
//! let config = GlickoBoostConfig {
//!     // The eta value describes the advantage of player_one.
//!     // 30.0 is roughly accurate for playing White in Chess.
//!     // If player_two was to play White, change this to -30.0.
//!     // By default it is set to 30.0.
//!     // We set this to 0 here to rate an equal game.
//!     eta: 0.0,
//!     // We leave the other settings at their default values.
//!     ..Default::default()
//! };
//!
//! // The glicko_boost function will calculate the new ratings for both players and return them.
//! let (new_player_one, new_player_two) =
//!     glicko_boost(&player_one, &player_two, &outcome, &config);
//! ```
//!
//! # More Information
//!
//! - [Original Paper (PDF)](http://glicko.net/glicko/glicko-boost.pdf)
//! - [FIDE/Deloitte Chess Rating Challenge](https://www.kaggle.com/c/ChessRatings2)
//! - [Wikipedia Article for Glicko and Glicko-2](https://en.wikipedia.org/wiki/Glicko_rating_system)

use std::f64::consts::PI;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::{
    glicko::GlickoRating, glicko2::Glicko2Rating, sticko::StickoRating, Outcomes, Rating,
    RatingPeriodSystem, RatingSystem,
};

#[derive(Copy, Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// The Glicko-Boost rating of a player.
///
/// Similar to [`GlickoRating`].
///
/// The default rating is 1500.0.
/// The default deviation is 350.0.
pub struct GlickoBoostRating {
    /// The player's Glicko-Boost rating number, by default 1500.0.
    pub rating: f64,
    /// The player's Glicko-Boost deviation number, by default 350.0.
    pub deviation: f64,
}

impl GlickoBoostRating {
    #[must_use]
    /// Initialise a new `GlickoBoostRating` with a rating of 1500.0 and a deviation of 350.0.
    pub const fn new() -> Self {
        Self {
            rating: 1500.0,
            deviation: 350.0,
        }
    }
}

impl Default for GlickoBoostRating {
    fn default() -> Self {
        Self::new()
    }
}

impl Rating for GlickoBoostRating {
    fn rating(&self) -> f64 {
        self.rating
    }
    fn uncertainty(&self) -> Option<f64> {
        Some(self.deviation)
    }
    fn new(rating: Option<f64>, uncertainty: Option<f64>) -> Self {
        Self {
            rating: rating.unwrap_or(1500.0),
            deviation: uncertainty.unwrap_or(350.0),
        }
    }
}

impl From<(f64, f64)> for GlickoBoostRating {
    fn from((r, d): (f64, f64)) -> Self {
        Self {
            rating: r,
            deviation: d,
        }
    }
}

impl From<GlickoRating> for GlickoBoostRating {
    fn from(g: GlickoRating) -> Self {
        Self {
            rating: g.rating,
            deviation: g.deviation,
        }
    }
}

impl From<Glicko2Rating> for GlickoBoostRating {
    fn from(g: Glicko2Rating) -> Self {
        Self {
            rating: g.rating,
            deviation: g.deviation,
        }
    }
}

impl From<StickoRating> for GlickoBoostRating {
    fn from(s: StickoRating) -> Self {
        Self {
            rating: s.rating,
            deviation: s.deviation,
        }
    }
}

#[derive(Clone, Copy, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// Constants used in the Glicko-Boost calculations.
///
/// If the `eta` parameter is set to `0.0`,
/// this will behave exactly like the [`Glicko`](crate::glicko::glicko) calculations.
pub struct GlickoBoostConfig {
    /// The advantage parameter of the first player.\
    /// If your game is biased towards player one set this to a positive number,
    /// or set this to a negative number if the second player has an advantage.\
    /// With this you could represent the advantage of playing white in chess,
    /// or home-team advantage in sports like football and so on.\
    /// In chess, a value of `30.0` seems to be about correct.\
    /// By default set to `0.0`.\
    /// If you want to mimic the [`GlickoConfig`](crate::glicko::GlickoConfig), set this to `0.0`.
    pub eta: f64,
    /// The "exceptional performance" threshold.\
    /// For outstanding performances, the rating deviation of the player will get boosted by the b values.
    /// By default set to `1.96`, which is approximately equal to 2.5% of performances.\
    /// The higher this value, the harder it is to reach the threshold.\
    /// If you want to mimic the [`GlickoConfig`](crate::glicko::GlickoConfig), set this to `0.0`.
    pub k: f64,
    /// The rating deviation boost factors. A tuple of 2 [`f64`]s.
    /// The first value is multiplicative, the second additive.\
    /// By default set to 0.20139 and 17.5.\
    /// If k is set to 0, these will do nothing.\
    /// If you want to mimic the [`GlickoConfig`](crate::glicko::GlickoConfig), set both of these to `0.0`.
    pub b: (f64, f64),
    /// The rating deviation increase factors. A tuple of 5 [`f64`]s.
    /// These values regulate the rating deviation increase of player's who have not played in a rating period.\
    /// By default set to 5.83733, -1.75374e-04, -7.080124e-05, 0.001733792, and 0.00026706.
    pub alpha: (f64, f64, f64, f64, f64),
}

impl GlickoBoostConfig {
    #[must_use]
    /// Initialise a new `GlickoBoostConfig` with a eta value of 30.0, a k value of 1.96,
    /// b values of 0.20139 and 17.5, and alpha values of 5.83733, -1.75374e-04, -7.080124e-05, 0.001733792, 0.00026706.
    pub const fn new() -> Self {
        Self {
            eta: 30.0,
            k: 1.96,
            b: (0.20139, 17.5),
            alpha: (
                5.837_33,
                -1.753_74e-04,
                -7.080_124e-05,
                0.001_733_792,
                0.000_267_06,
            ),
        }
    }
}

impl Default for GlickoBoostConfig {
    fn default() -> Self {
        Self::new()
    }
}

/// Struct to calculate ratings and expected score for [`GlickoBoost`]
pub struct GlickoBoost {
    config: GlickoBoostConfig,
}

impl RatingSystem for GlickoBoost {
    type RATING = GlickoBoostRating;
    type CONFIG = GlickoBoostConfig;

    fn new(config: Self::CONFIG) -> Self {
        Self { config }
    }

    fn rate(
        &self,
        player_one: &GlickoBoostRating,
        player_two: &GlickoBoostRating,
        outcome: &Outcomes,
    ) -> (GlickoBoostRating, GlickoBoostRating) {
        glicko_boost(player_one, player_two, outcome, &self.config)
    }

    fn expected_score(
        &self,
        player_one: &GlickoBoostRating,
        player_two: &GlickoBoostRating,
    ) -> (f64, f64) {
        expected_score(player_one, player_two, &self.config)
    }
}

impl RatingPeriodSystem for GlickoBoost {
    type RATING = GlickoBoostRating;
    type CONFIG = GlickoBoostConfig;

    fn new(config: Self::CONFIG) -> Self {
        Self { config }
    }

    fn rate(
        &self,
        player: &GlickoBoostRating,
        results: &[(GlickoBoostRating, Outcomes)],
    ) -> GlickoBoostRating {
        // Need to add a colour indicator to the results, we use white everytime.
        // The advantage of playing white is set to 0 by default, anyways.
        let new_results: Vec<(GlickoBoostRating, Outcomes, bool)> =
            results.iter().map(|r| (r.0, r.1, true)).collect();

        glicko_boost_rating_period(player, &new_results[..], &self.config)
    }

    fn expected_score(&self, player: &Self::RATING, opponents: &[Self::RATING]) -> Vec<f64> {
        let new_opponents: Vec<(GlickoBoostRating, bool)> =
            opponents.iter().map(|o| (*o, true)).collect();

        expected_score_rating_period(player, &new_opponents, &self.config)
    }
}

#[must_use]
/// Calculates the [`GlickoBoostRating`]s of two players based on their old ratings, deviations, and the outcome of the game.
///
/// Please see [`Glicko`](crate::glicko) for calculating with Glicko.
///
/// Takes in two players as [`GlickoBoostRating`]s, an [`Outcome`](Outcomes) and a [`GlickoBoostConfig`].
///
/// Instead of the traditional way of calculating the Glicko-Boost rating for only one player only using a list of results,
/// we are calculating the Glicko-Boost rating for two players at once, like in the Elo calculation,
/// to make it easier to see instant results.
///
/// For the traditional way of calculating a Glicko-Boost rating please see [`glicko_boost_rating_period`].
///
/// The outcome of the match is in the perspective of `player_one`.
/// This means [`Outcomes::WIN`] is a win for `player_one` and [`Outcomes::LOSS`] is a win for `player_two`.
///
/// If you have set up the [`GlickoBoostConfig`] to account for an advantage,
/// this is also determined to be *in favour* the first player, and *against* the second.
///
/// # Examples
/// ```
/// use skillratings::{
///     glicko_boost::{glicko_boost, GlickoBoostConfig, GlickoBoostRating},
///     Outcomes,
/// };
///
/// let player_one = GlickoBoostRating {
///     rating: 1500.0,
///     deviation: 350.0,
/// };
/// let player_two = GlickoBoostRating {
///     rating: 1500.0,
///     deviation: 350.0,
/// };
///
/// let outcome = Outcomes::WIN;
///
/// let config = GlickoBoostConfig {
///     // Player two plays as white in this example.
///     eta: -30.0,
///     ..Default::default()
/// };
///
/// let (new_one, new_two) = glicko_boost(&player_one, &player_two, &outcome, &config);
///
/// assert!((new_one.rating.round() - 1672.0).abs() < f64::EPSILON);
/// assert!((new_one.deviation.round() - 291.0).abs() < f64::EPSILON);
///
/// assert!((new_two.rating.round() - 1328.0).abs() < f64::EPSILON);
/// assert!((new_two.deviation.round() - 291.0).abs() < f64::EPSILON);
/// ```
pub fn glicko_boost(
    player_one: &GlickoBoostRating,
    player_two: &GlickoBoostRating,
    outcome: &Outcomes,
    config: &GlickoBoostConfig,
) -> (GlickoBoostRating, GlickoBoostRating) {
    let q = 10_f64.ln() / 400.0;

    let outcome1 = outcome.to_chess_points();
    let outcome2 = 1.0 - outcome1;

    let colour1 = 1.0;
    let colour2 = -1.0;

    let g1 = g_value(q, player_two.deviation);
    let g2 = g_value(q, player_one.deviation);

    let e1 = e_value(
        g1,
        player_one.rating,
        player_two.rating,
        config.eta,
        colour1,
    );
    let e2 = e_value(
        g2,
        player_two.rating,
        player_one.rating,
        config.eta,
        colour2,
    );

    let d1 = d_value(q, g1, e1);
    let d2 = d_value(q, g2, e2);

    let z1 = z_value(g1, e1, outcome1);
    let z2 = z_value(g2, e2, outcome2);

    let new_deviation1 = new_deviation(player_one.deviation, d1, z1, config);
    let new_deviation2 = new_deviation(player_two.deviation, d2, z2, config);

    let new_rating1 = new_rating(player_one.rating, new_deviation1, outcome1, q, g1, e1);
    let new_rating2 = new_rating(player_two.rating, new_deviation2, outcome2, q, g2, e2);

    let end_deviation1 = (new_deviation1
        .mul_add(
            new_deviation1,
            config
                .alpha
                .4
                .mul_add(
                    (new_rating1 / 1000.0).powi(2),
                    config.alpha.3.mul_add(
                        new_rating1 / 1000.0,
                        (config.alpha.2 * new_deviation1).mul_add(
                            new_rating1 / 1000.0,
                            config.alpha.1.mul_add(new_deviation1, config.alpha.0),
                        ),
                    ),
                )
                .exp(),
        )
        .sqrt())
    .min(350.0);
    let end_deviation2 = (new_deviation2
        .mul_add(
            new_deviation2,
            config
                .alpha
                .4
                .mul_add(
                    (new_rating2 / 1000.0).powi(2),
                    config.alpha.3.mul_add(
                        new_rating2 / 1000.0,
                        (config.alpha.2 * new_deviation2).mul_add(
                            new_rating2 / 1000.0,
                            config.alpha.1.mul_add(new_deviation2, config.alpha.0),
                        ),
                    ),
                )
                .exp(),
        )
        .sqrt())
    .min(350.0);

    (
        GlickoBoostRating {
            rating: new_rating1,
            deviation: end_deviation1,
        },
        GlickoBoostRating {
            rating: new_rating2,
            deviation: end_deviation2,
        },
    )
}

#[must_use]
/// The "traditional" way of calculating a [`GlickoBoostRating`] of a player in a rating period.
///
/// Takes in a player as an [`GlickoBoostRating`] and their results as a Slice of tuples containing the opponent as a [`GlickoBoostRating`],
/// the outcome of the game as an [`Outcome`](Outcomes) and a [`bool`] specifying if the player was playing as player one, and a [`GlickoBoostConfig`].
///
/// ---
///
/// 📌 _**Important note:**_ We need an added parameter in the results tuple here.  \
/// The boolean specifies if the player was playing as the first player, aka White in Chess.
/// If set to `true` the player was playing as White, if set to `false` the player was playing as Black.\
/// In the [`glicko_boost`] function this is determined by the order of players that are input to the function, but we cannot do this here,
/// and because it likely changes from game-to-game, we need a separate parameter controlling it.
///
/// The colour you play in each game matters if the [`GlickoBoostConfig`] is set up with an advantage for the first player.\
/// It makes sense to do so in Chess, or Sports with an home-team-advantage.
///
/// ---
///
/// The outcome of the match is in the perspective of the player.
/// This means [`Outcomes::WIN`] is a win for the player and [`Outcomes::LOSS`] is a win for the opponent.
///
/// If the player's results are empty, the player's rating deviation will automatically be decayed using [`decay_deviation`].
///
/// # Examples
/// ```
/// use skillratings::{
///     glicko_boost::{glicko_boost_rating_period, GlickoBoostConfig, GlickoBoostRating},
///     Outcomes,
/// };
///
/// let player = GlickoBoostRating {
///     rating: 1500.0,
///     deviation: 200.0,
/// };
///
/// let opponent1 = GlickoBoostRating {
///     rating: 1400.0,
///     deviation: 30.0,
/// };
///
/// let opponent2 = GlickoBoostRating {
///     rating: 1550.0,
///     deviation: 100.0,
/// };
///
/// let opponent3 = GlickoBoostRating {
///     rating: 1700.0,
///     deviation: 300.0,
/// };
///
/// let results = vec![
///     // The player was playing as white.
///     (opponent1, Outcomes::WIN, true),
///     // The player was playing as black.
///     (opponent2, Outcomes::LOSS, false),
///     (opponent3, Outcomes::LOSS, true),
/// ];
///
/// let config = GlickoBoostConfig::new();
///
/// let new_player = glicko_boost_rating_period(&player, &results, &config);
///
/// assert!((new_player.rating.round() - 1461.0).abs() < f64::EPSILON);
/// assert!((new_player.deviation.round() - 153.0).abs() < f64::EPSILON);
/// ```
pub fn glicko_boost_rating_period(
    player: &GlickoBoostRating,
    results: &[(GlickoBoostRating, Outcomes, bool)],
    config: &GlickoBoostConfig,
) -> GlickoBoostRating {
    let q = 10_f64.ln() / 400.0;

    if results.is_empty() {
        return decay_deviation(player, config);
    }

    let d_sum: f64 = results
        .iter()
        .map(|r| {
            let g = g_value(q, r.0.deviation);

            let e = e_value(
                g,
                player.rating,
                r.0.rating,
                config.eta,
                if r.2 { 1.0 } else { -1.0 },
            );

            g.powi(2) * e * (1.0 - e)
        })
        .sum();

    let d_sq = (q.powi(2) * d_sum).recip();

    let m = results
        .iter()
        .map(|r| {
            let g = g_value(q, r.0.deviation);

            let e = e_value(
                g,
                player.rating,
                r.0.rating,
                config.eta,
                if r.2 { 1.0 } else { -1.0 },
            );

            let s = r.1.to_chess_points();

            g * (s - e)
        })
        .sum();

    let z = m / d_sum.sqrt();

    let new_deviation = new_deviation(player.deviation, d_sq, z, config);

    let new_rating = (new_deviation.powi(2) * q).mul_add(m, player.rating);

    let end_deviation = (new_deviation
        .mul_add(
            new_deviation,
            config
                .alpha
                .4
                .mul_add(
                    (new_rating / 1000.0).powi(2),
                    config.alpha.3.mul_add(
                        new_rating / 1000.0,
                        (config.alpha.2 * new_deviation).mul_add(
                            new_rating / 1000.0,
                            config.alpha.1.mul_add(new_deviation, config.alpha.0),
                        ),
                    ),
                )
                .exp(),
        )
        .sqrt())
    .min(350.0);

    GlickoBoostRating {
        rating: new_rating,
        deviation: end_deviation,
    }
}

#[must_use]
/// Calculates the expected outcome of two players based on Glicko-Boost.
///
/// Takes in two players as [`GlickoBoostRating`]s and a [`GlickoBoostConfig`] and returns the probability of victory for each player as an [`f64`] between 1.0 and 0.0.\
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// In the config you can specify an advantage or disadvantage parameter for player one, which will affect the expected score.
///
/// # Examples
/// ```
/// use skillratings::glicko_boost::{expected_score, GlickoBoostConfig, GlickoBoostRating};
///
/// let player_one = GlickoBoostRating {
///     rating: 2500.0,
///     deviation: 41.0,
/// };
/// let player_two = GlickoBoostRating {
///     rating: 1950.0,
///     deviation: 320.0,
/// };
///
/// let config = GlickoBoostConfig {
///     // We give an advantage of 30 rating points to player two.
///     eta: -30.0,
///     ..Default::default()
/// };
///
/// let (exp_one, exp_two) = expected_score(&player_one, &player_two, &config);
///
/// assert!(((exp_one * 100.0).round() - 89.0).abs() < f64::EPSILON);
/// assert!(((exp_two * 100.0).round() - 11.0).abs() < f64::EPSILON);
/// ```
pub fn expected_score(
    player_one: &GlickoBoostRating,
    player_two: &GlickoBoostRating,
    config: &GlickoBoostConfig,
) -> (f64, f64) {
    let q = 10_f64.ln() / 400.0;
    let g = g_value(q, player_one.deviation.hypot(player_two.deviation));

    let exp_one = (1.0
        + 10_f64.powf(-g * (player_one.rating + config.eta - player_two.rating) / 400.0))
    .recip();
    let exp_two = 1.0 - exp_one;

    (exp_one, exp_two)
}

#[must_use]
/// Calculates the expected outcome of a player in a rating period or tournament.
///
/// Takes in a player as an [`GlickoBoostRating`] and their results as a Slice of tuples containing the opponent as a [`GlickoBoostRating`],
/// and a [`bool`] specifying if the player was playing as player one, and a [`GlickoBoostConfig`].
///
/// ---
///
/// 📌 _**Important note:**_ The parameters intentionally work different from other expected_score_rating_period functions here.\
/// An additional config is used, because of the set advantage parameter that describes inherit imbalances, like playing White in Chess
/// or a Football team playing at home.
///
/// Because of those advantages, we also need a boolean which specifies if the player was playing as the first / advantaged player (e.g. White in Chess).
/// If set to `true` the player was playing with the advantage, if set to `false` the player was with the disadvantage.\
///
/// If the config is set to not have any advantages, the boolean will not matter.
///
/// ---
///
/// The outcome of the match is in the perspective of the player.
/// This means [`Outcomes::WIN`] is a win for the player and [`Outcomes::LOSS`] is a win for the opponent.
///
/// If the player's results are empty, the player's rating deviation will automatically be decayed using [`decay_deviation`].
///
/// # Examples
/// ```
/// use skillratings::{
///     glicko_boost::{expected_score_rating_period, GlickoBoostConfig, GlickoBoostRating},
///     Outcomes,
/// };
///
/// let player = GlickoBoostRating {
///     rating: 1500.0,
///     deviation: 200.0,
/// };
///
/// let opponent1 = GlickoBoostRating {
///     rating: 1400.0,
///     deviation: 30.0,
/// };
///
/// let opponent2 = GlickoBoostRating {
///     rating: 1550.0,
///     deviation: 100.0,
/// };
///
/// let opponent3 = GlickoBoostRating {
///     rating: 1700.0,
///     deviation: 300.0,
/// };
///
/// let results = vec![
///     // The player was playing as white.
///     (opponent1, true),
///     // The player was playing as black.
///     (opponent2, false),
///     (opponent3, true),
/// ];
///
/// let config = GlickoBoostConfig::new();
///
/// let exp = expected_score_rating_period(&player, &results, &config);
///
/// assert_eq!((exp[0] * 100.0).round(), 65.0);
/// assert_eq!((exp[1] * 100.0).round(), 48.0);
/// assert_eq!((exp[2] * 100.0).round(), 34.0);
/// ```
pub fn expected_score_rating_period(
    player: &GlickoBoostRating,
    opponents: &[(GlickoBoostRating, bool)],
    config: &GlickoBoostConfig,
) -> Vec<f64> {
    opponents
        .iter()
        .map(|o| {
            let q = 10_f64.ln() / 400.0;
            let g = g_value(q, player.deviation.hypot(o.0.deviation));

            (1.0 + 10_f64.powf(-g * (player.rating + config.eta - o.0.rating) / 400.0)).recip()
        })
        .collect()
}

#[must_use]
/// Decays a Rating Deviation Value for a player, if they missed playing in a certain rating period.
///
/// The length of the rating period and thus the number of missed periods per player is something to decide and track yourself.
///
/// Takes in a player as a [`GlickoBoostRating`] and a [`GlickoBoostConfig`], that describes how much the rating should change, and returns the decayed [`GlickoBoostRating`].
///
/// # Examples
/// ```
/// use skillratings::glicko_boost::{decay_deviation, GlickoBoostConfig, GlickoBoostRating};
///
/// let player_one = GlickoBoostRating {
///     rating: 2720.0,
///     deviation: 41.3,
/// };
///
/// let config = GlickoBoostConfig::new();
///
/// let player_one_decay = decay_deviation(&player_one, &config);
///
/// assert!((player_one_decay.deviation.round() - 45.0).abs() < f64::EPSILON);
/// ```
pub fn decay_deviation(
    player: &GlickoBoostRating,
    config: &GlickoBoostConfig,
) -> GlickoBoostRating {
    let decayed_deviation = (player
        .deviation
        .mul_add(
            player.deviation,
            config
                .alpha
                .4
                .mul_add(
                    (player.rating / 1000.0).powi(2),
                    config.alpha.3.mul_add(
                        player.rating / 1000.0,
                        (config.alpha.2 * player.deviation).mul_add(
                            player.rating / 1000.0,
                            config.alpha.1.mul_add(player.deviation, config.alpha.0),
                        ),
                    ),
                )
                .exp(),
        )
        .sqrt())
    .min(350.0);

    GlickoBoostRating {
        rating: player.rating,
        deviation: decayed_deviation,
    }
}

#[must_use]
/// The 95% confidence interval of the lowest to highest rating.
///
/// The system is 95% sure that the "true skill" of the player is in-between these values.
///
/// Takes in a player as a [`GlickoBoostRating`] and returns two [`f64`]s that describe the lowest and highest rating.
///
/// # Examples
/// ```
/// use skillratings::glicko_boost::{confidence_interval, GlickoBoostRating};
///
/// let player = GlickoBoostRating {
///     rating: 2250.0,
///     deviation: 79.0,
/// };
///
/// let (interval_low, interval_high) = confidence_interval(&player);
///
/// assert!(interval_low.round() - 2095.0 < f64::EPSILON);
/// assert!(interval_high.round() - 2405.0 < f64::EPSILON);
/// ```
pub fn confidence_interval(player: &GlickoBoostRating) -> (f64, f64) {
    (
        1.96f64.mul_add(-player.deviation, player.rating),
        1.96f64.mul_add(player.deviation, player.rating),
    )
}

fn new_deviation(old_deviation: f64, d: f64, z: f64, config: &GlickoBoostConfig) -> f64 {
    if z > config.k && config.k != 0.0 {
        let after_deviation = (old_deviation.powi(2).recip() + d.recip()).recip().sqrt();

        let pre_deviation = boost_rd(z, after_deviation, config);

        ((pre_deviation.powi(2).recip() + d.recip()).recip().sqrt()).min(350.0)
    } else {
        ((old_deviation.powi(2).recip() + d.recip()).recip().sqrt()).min(350.0)
    }
}

fn new_rating(old_rating: f64, deviation: f64, score: f64, q: f64, g: f64, e: f64) -> f64 {
    (deviation.powi(2) * q * g).mul_add(score - e, old_rating)
}

fn z_value(g: f64, e: f64, score: f64) -> f64 {
    (g * (score - e)) / (g.powi(2) * e * (1.0 - e)).sqrt()
}

fn boost_rd(z: f64, deviation: f64, config: &GlickoBoostConfig) -> f64 {
    (z - config.k)
        .mul_add(config.b.0, 1.0)
        .mul_add(deviation, config.b.1)
}

// The functions below are very similar to the normal glicko functions,
// but with the advantage parameters.

fn g_value(q: f64, opponent_deviation: f64) -> f64 {
    (1.0 + ((3.0 * q.powi(2) * opponent_deviation.powi(2)) / (PI.powi(2))))
        .sqrt()
        .recip()
}

fn e_value(g: f64, rating: f64, opponent_rating: f64, advantage: f64, colour: f64) -> f64 {
    (1.0 + (10_f64.powf(-g * (colour.mul_add(advantage, rating) - opponent_rating) / 400.0)))
        .recip()
}

fn d_value(q: f64, g: f64, e: f64) -> f64 {
    (q.powi(2) * g.powi(2) * e * (1.0 - e)).powi(-1)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_glicko_boost() {
        let player_one = GlickoBoostRating {
            rating: 1500.0,
            deviation: 200.0,
        };
        let player_two = GlickoBoostRating {
            rating: 1620.0,
            deviation: 105.0,
        };

        let config = GlickoBoostConfig::new();

        let (new_one, new_two) = glicko_boost(&player_one, &player_two, &Outcomes::WIN, &config);

        assert!((new_one.rating.round() - 1606.0).abs() < f64::EPSILON);
        assert!((new_two.rating.round() - 1589.0).abs() < f64::EPSILON);

        assert!((new_one.deviation - 177.634_630_775_565_48).abs() < f64::EPSILON);
        assert!((new_two.deviation - 103.511_394_589_339_77).abs() < f64::EPSILON);
    }

    #[test]
    fn test_one_rp() {
        let player = GlickoBoostRating {
            rating: 1444.0,
            deviation: 85.0,
        };
        let opponent = GlickoBoostRating {
            rating: 1804.0,
            deviation: 55.0,
        };

        let config = GlickoBoostConfig::new();

        let (np, _) = glicko_boost(&player, &opponent, &Outcomes::WIN, &config);
        let rp = glicko_boost_rating_period(&player, &[(opponent, Outcomes::WIN, true)], &config);

        assert_eq!(np, rp);
    }

    #[test]
    /// This is to compare if the base algorithm is compatible with glicko.
    fn test_glicko_comparison() {
        let config = GlickoBoostConfig {
            eta: 0.0,
            k: 0.0,
            b: (0.0, 0.0),
            alpha: (0.0, 0.0, 0.0, 0.0, 0.0),
        };

        let player = GlickoBoostRating {
            rating: 1500.0,
            deviation: 200.0,
        };

        let opponent1 = GlickoBoostRating {
            rating: 1400.0,
            deviation: 30.0,
        };

        let opponent2 = GlickoBoostRating {
            rating: 1550.0,
            deviation: 100.0,
        };

        let opponent3 = GlickoBoostRating {
            rating: 1700.0,
            deviation: 300.0,
        };

        let results = vec![
            (opponent1, Outcomes::WIN, true),
            (opponent2, Outcomes::LOSS, true),
            (opponent3, Outcomes::LOSS, true),
        ];

        let new_player = glicko_boost_rating_period(&player, &results, &config);

        assert!((new_player.rating.round() - 1464.0).abs() < f64::EPSILON);
        assert!((new_player.deviation - 151.402_204_945_799_04).abs() < f64::EPSILON);
    }

    #[test]
    fn test_boost_rd() {
        let rd = 98.6;
        let z = 3.672_028_777_401_921_6;

        let new_rd = boost_rd(z, rd, &GlickoBoostConfig::new());

        assert!((new_rd - 150.095_847_882_423_93).abs() < f64::EPSILON);
    }

    #[test]
    fn test_max_rd() {
        let player_one = GlickoBoostRating::new();
        let player_two = GlickoBoostRating {
            rating: 3500.0,
            deviation: 31.4,
        };

        let (np1, _) = glicko_boost(
            &player_one,
            &player_two,
            &Outcomes::WIN,
            &GlickoBoostConfig::new(),
        );

        assert!((np1.deviation - 350.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_decay() {
        let player = GlickoBoostRating {
            rating: 1302.2,
            deviation: 62.0,
        };

        let decayed_player = decay_deviation(&player, &GlickoBoostConfig::new());
        let rp_player = glicko_boost_rating_period(&player, &[], &GlickoBoostConfig::new());

        assert_eq!(decayed_player, rp_player);
        assert!((decayed_player.deviation - 64.669_444_203_475_88).abs() < f64::EPSILON);
        assert!((decayed_player.rating - player.rating).abs() < f64::EPSILON);
    }

    #[test]
    fn test_confidence_interval() {
        let player = GlickoBoostRating {
            rating: 1500.0,
            deviation: 30.0,
        };

        let ci = confidence_interval(&player);

        assert!((ci.0.round() - 1441.0).abs() < f64::EPSILON);
        assert!((ci.1.round() - 1559.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_expected_score() {
        let player_one = GlickoBoostRating {
            rating: 1400.0,
            deviation: 40.0,
        };

        let player_two = GlickoBoostRating {
            rating: 1500.0,
            deviation: 150.0,
        };

        let glicko_config = GlickoBoostConfig {
            eta: 0.0,
            ..Default::default()
        };

        let boost_config = GlickoBoostConfig::new();

        let (exp_one, exp_two) = expected_score(&player_one, &player_two, &glicko_config);

        assert!((exp_one - 0.373_700_405_951_935).abs() < f64::EPSILON);
        assert!((exp_two - 0.626_299_594_048_065).abs() < f64::EPSILON);
        assert!((exp_one + exp_two - 1.0).abs() < f64::EPSILON);

        let (exp_one, exp_two) = expected_score(&player_one, &player_two, &boost_config);

        assert!((exp_one - 0.410_605_680_590_947_1).abs() < f64::EPSILON);
        assert!((exp_two - 0.589_394_319_409_052_8).abs() < f64::EPSILON);
        assert!((exp_one + exp_two - 1.0).abs() < f64::EPSILON);
    }

    #[test]
    #[allow(clippy::similar_names)]
    fn test_glicko_conv() {
        let glickob = GlickoBoostRating::new();

        let glicko_conv = GlickoRating::from(glickob);
        let glicko2_conv = Glicko2Rating::from(glickob);

        assert!((glicko_conv.rating - 1500.0).abs() < f64::EPSILON);
        assert!((glicko2_conv.rating - 1500.0).abs() < f64::EPSILON);

        let glicko2 = Glicko2Rating::new();
        let glicko = GlickoRating::new();

        assert_eq!(GlickoBoostRating::new(), GlickoBoostRating::from(glicko2));
        assert_eq!(
            GlickoBoostRating::default(),
            GlickoBoostRating::from(glicko)
        );
    }

    #[test]
    #[allow(clippy::similar_names, clippy::too_many_lines)]
    /// This test is taken from the original paper
    fn test_glicko_boost_rating_period() {
        let player_a = GlickoBoostRating {
            rating: 2300.0,
            deviation: 140.0,
        };
        let player_b = GlickoBoostRating {
            rating: 2295.0,
            deviation: 80.0,
        };
        let player_c = GlickoBoostRating {
            rating: 2280.0,
            deviation: 150.0,
        };
        let player_d = GlickoBoostRating {
            rating: 2265.0,
            deviation: 70.0,
        };
        let player_e = GlickoBoostRating {
            rating: 2260.0,
            deviation: 90.0,
        };
        let player_f = GlickoBoostRating {
            rating: 2255.0,
            deviation: 200.0,
        };
        let player_g = GlickoBoostRating {
            rating: 2250.0,
            deviation: 50.0,
        };
        let player_h = GlickoBoostRating {
            rating: 2075.0,
            deviation: 120.0,
        };
        let config = GlickoBoostConfig::new();

        let player_a_results = vec![
            (player_b, Outcomes::LOSS, true),
            (player_c, Outcomes::LOSS, false),
            (player_e, Outcomes::LOSS, true),
            (player_f, Outcomes::WIN, false),
            (player_g, Outcomes::WIN, true),
            (player_h, Outcomes::LOSS, false),
        ];

        let player_b_results = vec![
            (player_a, Outcomes::WIN, false),
            (player_c, Outcomes::DRAW, true),
            (player_d, Outcomes::WIN, true),
            (player_e, Outcomes::DRAW, false),
            (player_f, Outcomes::WIN, true),
            (player_g, Outcomes::WIN, false),
        ];

        let player_c_results = vec![
            (player_a, Outcomes::WIN, true),
            (player_b, Outcomes::DRAW, false),
            (player_d, Outcomes::WIN, false),
            (player_f, Outcomes::WIN, false),
            (player_g, Outcomes::WIN, true),
            (player_h, Outcomes::DRAW, true),
        ];

        let player_d_results = vec![
            (player_b, Outcomes::LOSS, false),
            (player_c, Outcomes::LOSS, true),
            (player_e, Outcomes::LOSS, true),
            (player_f, Outcomes::DRAW, false),
            (player_g, Outcomes::LOSS, true),
            (player_h, Outcomes::LOSS, false),
        ];

        let player_e_results = vec![
            (player_a, Outcomes::WIN, false),
            (player_b, Outcomes::DRAW, true),
            (player_d, Outcomes::WIN, false),
            (player_f, Outcomes::WIN, true),
            (player_g, Outcomes::DRAW, false),
            (player_h, Outcomes::LOSS, true),
        ];

        let player_f_results = vec![
            (player_a, Outcomes::LOSS, true),
            (player_b, Outcomes::LOSS, false),
            (player_c, Outcomes::LOSS, true),
            (player_d, Outcomes::DRAW, true),
            (player_e, Outcomes::LOSS, false),
            (player_h, Outcomes::LOSS, false),
        ];

        let player_g_results = vec![
            (player_a, Outcomes::LOSS, false),
            (player_b, Outcomes::LOSS, true),
            (player_c, Outcomes::LOSS, false),
            (player_d, Outcomes::WIN, false),
            (player_e, Outcomes::DRAW, true),
            (player_h, Outcomes::LOSS, true),
        ];

        let player_h_results = vec![
            (player_a, Outcomes::WIN, true),
            (player_c, Outcomes::DRAW, false),
            (player_d, Outcomes::WIN, true),
            (player_e, Outcomes::WIN, false),
            (player_f, Outcomes::WIN, true),
            (player_g, Outcomes::WIN, false),
        ];

        let new_a = glicko_boost_rating_period(&player_a, &player_a_results, &config);
        let new_b = glicko_boost_rating_period(&player_b, &player_b_results, &config);
        let new_c = glicko_boost_rating_period(&player_c, &player_c_results, &config);
        let new_d = glicko_boost_rating_period(&player_d, &player_d_results, &config);
        let new_e = glicko_boost_rating_period(&player_e, &player_e_results, &config);
        let new_f = glicko_boost_rating_period(&player_f, &player_f_results, &config);
        let new_g = glicko_boost_rating_period(&player_g, &player_g_results, &config);
        let new_h = glicko_boost_rating_period(&player_h, &player_h_results, &config);

        // Due to skipping step 2 and 4, these ratings are comparable to the ratings of step 3.
        // They are not exactly equal (Difference of <1.0%) due to 1: rounding errors and 2: skipping of step 2.
        assert!((new_a.rating - 2_209.502_401_056_321_6).abs() < f64::EPSILON);
        assert!((new_a.deviation - 105.846_722_882_642_35).abs() < f64::EPSILON);

        assert!((new_b.rating - 2_343.331_676_741_51).abs() < f64::EPSILON);
        assert!((new_b.deviation - 73.239_139_081_695_3).abs() < f64::EPSILON);

        assert!((new_c.rating - 2_386.917_144_473_656_7).abs() < f64::EPSILON);
        assert!((new_c.deviation - 109.595_379_480_830_47).abs() < f64::EPSILON);

        assert!((new_d.rating - 2_204.280_099_158_658_7).abs() < f64::EPSILON);
        assert!((new_d.deviation - 66.367_566_777_947_56).abs() < f64::EPSILON);

        assert!((new_e.rating - 2_287.443_303_658_628_3).abs() < f64::EPSILON);
        assert!((new_e.deviation - 79.993_286_777_344_75).abs() < f64::EPSILON);

        assert!((new_f.rating - 2_051.583_061_993_349_5).abs() < f64::EPSILON);
        assert!((new_f.deviation - 122.816_328_678_587_84).abs() < f64::EPSILON);

        assert!((new_g.rating - 2_231.929_694_167_278).abs() < f64::EPSILON);
        assert!((new_g.deviation - 51.016_495_555_231_394).abs() < f64::EPSILON);

        assert!((new_h.rating - 2_348.033_407_382_116).abs() < f64::EPSILON);
        assert!((new_h.deviation - 115.100_184_487_094_04).abs() < f64::EPSILON);
    }

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn test_misc_stuff() {
        let player_one = GlickoBoostRating::new();
        let config = GlickoBoostConfig::new();

        assert_eq!(player_one, player_one.clone());
        assert!((config.eta - config.clone().eta).abs() < f64::EPSILON);

        assert!(!format!("{player_one:?}").is_empty());
        assert!(!format!("{config:?}").is_empty());

        assert_eq!(player_one, GlickoBoostRating::from((1500.0, 350.0)));
    }

    #[test]
    fn test_traits() {
        let player_one: GlickoBoostRating = Rating::new(Some(240.0), Some(90.0));
        let player_two: GlickoBoostRating = Rating::new(Some(240.0), Some(90.0));

        let rating_system: GlickoBoost = RatingSystem::new(GlickoBoostConfig::new());

        assert!((player_one.rating() - 240.0).abs() < f64::EPSILON);
        assert_eq!(player_one.uncertainty(), Some(90.0));

        let (new_player_one, new_player_two) =
            RatingSystem::rate(&rating_system, &player_one, &player_two, &Outcomes::WIN);

        let (exp1, exp2) = RatingSystem::expected_score(&rating_system, &player_one, &player_two);

        assert!((new_player_one.rating - 259.366_898_204_792_6).abs() < f64::EPSILON);
        assert!((new_player_two.rating - 220.633_101_795_207_38).abs() < f64::EPSILON);

        assert!((exp1 - 0.539_945_539_565_174_9).abs() < f64::EPSILON);
        assert!((exp2 - 0.460_054_460_434_825_1).abs() < f64::EPSILON);

        let rating_period_system: GlickoBoost = RatingPeriodSystem::new(GlickoBoostConfig::new());
        let exp_rp =
            RatingPeriodSystem::expected_score(&rating_period_system, &player_one, &[player_two]);
        assert!((exp1 - exp_rp[0]).abs() < f64::EPSILON);

        let player_one: GlickoBoostRating = Rating::new(Some(240.0), Some(90.0));
        let player_two: GlickoBoostRating = Rating::new(Some(240.0), Some(90.0));

        let rating_period: GlickoBoost = RatingPeriodSystem::new(GlickoBoostConfig::new());

        let new_player_one =
            RatingPeriodSystem::rate(&rating_period, &player_one, &[(player_two, Outcomes::WIN)]);

        assert!((new_player_one.rating - 259.366_898_204_792_6).abs() < f64::EPSILON);
    }
}
