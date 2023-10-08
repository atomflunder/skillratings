//! This is the Stephenson rating algorithm, nicknamed "Sticko" due to it being an improvement on the Glicko rating algorithm.  
//! Allows for player advantages, and the winner of a chess outcome prediction competition.
//!
//! For the original Glicko algorithm, please see [`Glicko`](crate::glicko), or for the improved versions see [`Glicko-Boost`](crate::glicko_boost) or [`Glicko-2`](crate::glicko2).
//!
//! In 2012, the data prediction website [Kaggle](https://kaggle.com) hosted the "FIDE/Deloitte Chess Rating Challenge"
//!  where competitors where asked to create a new, more accurate chess rating system.  
//! The winner of the competition was Alec Stephenson, and this was the system he came up with.
//!
//! The main improvements over Glicko are three new configurable parameters found in [`StickoConfig`]:
//!
//! - Gamma (γ) an advantage parameter that accounts for inherent advantages in-game (White in Chess, etc.)
//! - Beta (β) a drift parameter that increases rating for participating
//! - Lambda (λ) a parameter that accounts for the strength of the opponent, regardless of result.
//!
//! These make Sticko more configurable and possibly more accurate than the Glicko algorithm.
//! When all parameters are set to 0, the Sticko algorithm will produce the exact same results as Glicko.
//!
//! # Quickstart
//!
//! This is the most basic example on how to use the Sticko Module.  
//! Please take a look at the functions below to see more advanced use cases.
//!
//! ```
//! use skillratings::{
//!     sticko::{sticko, StickoConfig, StickoRating},
//!     Outcomes,
//! };
//!
//! // Initialise a new player rating with a rating of 1500 and a deviation of 350.
//! let player_one = StickoRating::new();
//!
//! // Or you can initialise it with your own values of course.
//! // Imagine these numbers being pulled from a database.
//! let (some_rating, some_deviation) = (1325.0, 230.0);
//! let player_two = StickoRating {
//!     rating: some_rating,
//!     deviation: some_deviation,
//! };
//!
//! // The outcome of the match is from the perspective of player one.
//! let outcome = Outcomes::WIN;
//!
//! // The config allows you to specify certain values in the Sticko calculation.
//! // For more information on how to customise the config,
//! // please check out the StickoConfig struct.
//! let config = StickoConfig {
//!     // The gamma value describes the advantage of player_one.
//!     // 30.0 is roughly accurate for playing White in Chess.
//!     // If player_two was to play White, change this to -30.0.
//!     // By default it is set to 0.0.
//!     gamma: 30.0,
//!     // We leave the other settings at their default values.
//!     ..Default::default()
//! };
//!
//! // The sticko function will calculate the new ratings for both players and return them.
//! let (new_player_one, new_player_two) = sticko(&player_one, &player_two, &outcome, &config);
//! ```
//!
//! # More Information
//!
//! - [PlayerRatings R Package](https://cran.r-project.org/web/packages/PlayerRatings/index.html)
//! - [FIDE/Deloitte Chess Rating Challenge](https://www.kaggle.com/c/ChessRatings2)
//! - [Comparing Performance of Chess Ratings](https://www.englishchess.org.uk/wp-content/uploads/2012/04/ratings.pdf)
//! - [Comparison of rating systems for Women's Beach Volleyball](http://glicko.net/research/volleyball-FINAL.pdf)

use std::f64::consts::PI;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::{
    glicko::GlickoRating, glicko2::Glicko2Rating, glicko_boost::GlickoBoostRating, Outcomes,
    Rating, RatingPeriodSystem, RatingSystem,
};

#[derive(Copy, Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// The Sticko rating of a player.
///
/// Similar to [`GlickoRating`].
///
/// The default rating is 1500.0.
/// The default deviation is 350.0.
pub struct StickoRating {
    /// The player's Sticko rating number, by default 1500.0.
    pub rating: f64,
    /// The player's Sticko deviation number, by default 350.0.
    pub deviation: f64,
}

impl StickoRating {
    #[must_use]
    /// Initialise a new `StickoRating` with a rating of 1500.0 and a deviation of 350.0.
    pub const fn new() -> Self {
        Self {
            rating: 1500.0,
            deviation: 350.0,
        }
    }
}

impl Default for StickoRating {
    fn default() -> Self {
        Self::new()
    }
}

impl Rating for StickoRating {
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

impl From<(f64, f64)> for StickoRating {
    fn from((r, d): (f64, f64)) -> Self {
        Self {
            rating: r,
            deviation: d,
        }
    }
}

impl From<GlickoRating> for StickoRating {
    fn from(g: GlickoRating) -> Self {
        Self {
            rating: g.rating,
            deviation: g.deviation,
        }
    }
}

impl From<Glicko2Rating> for StickoRating {
    fn from(g: Glicko2Rating) -> Self {
        Self {
            rating: g.rating,
            deviation: g.deviation,
        }
    }
}

impl From<GlickoBoostRating> for StickoRating {
    fn from(g: GlickoBoostRating) -> Self {
        Self {
            rating: g.rating,
            deviation: g.deviation,
        }
    }
}

#[derive(Clone, Copy, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// Constants used in the Sticko calculations.
///
/// If the `h`, `beta`, `lambda` and `gamma` parameters are set to `0.0`,
/// this will behave exactly like the [`Glicko`](crate::glicko::glicko) calculations.
pub struct StickoConfig {
    /// Controls player deviations across time.  
    /// The higher this number, the higher the deviation is going to be.  
    /// By default set to `10.0`.  
    /// If you want to mimic the [`GlickoConfig`](crate::glicko::GlickoConfig), set this to `0.0`.
    /// Do not set this to a negative value.
    pub h: f64,
    /// A bonus parameter, which gives a rating boost for just participating.  
    /// Note that setting this to a positive number will create rating inflation over time.  
    /// By default set to `0.0`.  
    /// If you want to mimic the [`GlickoConfig`](crate::glicko::GlickoConfig), set this to `0.0`.
    /// Do not set this to a negative value.
    pub beta: f64,
    /// The neighbourhood parameter, which shrinks player ratings towards their opponent.  
    /// By default set to `2.0`.  
    /// If you want to mimic the [`GlickoConfig`](crate::glicko::GlickoConfig), set this to `0.0`.
    /// Do not set this to a negative value.
    pub lambda: f64,
    /// The advantage parameter of the first player.  
    /// If your game is biased towards player one set this to a positive number,
    /// or set this to a negative number if the second player has an advantage.  
    /// With this you could represent the advantage of playing white in chess,
    /// or home-team advantage in sports like football and so on.  
    /// In chess, a value of `30.0` seems to be about correct.  
    /// By default set to `0.0`.  
    /// If you want to mimic the [`GlickoConfig`](crate::glicko::GlickoConfig), set this to `0.0`.
    pub gamma: f64,
    /// The c value describes how much the rating deviation should decay in each step.
    /// The higher the value, the more the rating deviation will decay.  
    /// This is similar to the c value in [`GlickoConfig`](crate::glicko::GlickoConfig).
    /// Keep in mind this needs to be set lower than the c in the [`GlickoConfig`](crate::glicko::GlickoConfig)
    /// if the h value here is not equal to zero.  
    /// By default set to `10.0`.
    /// If you want to mimic the [`GlickoConfig`](crate::glicko::GlickoConfig) set this to `63.2`.
    pub c: f64,
}

impl StickoConfig {
    #[must_use]
    /// Initialise a new `StickoConfig` with a h value of `10.0`, a beta value of `0.0`,
    /// a lambda value of `2.0` and a gamma value of `0.0`.
    pub const fn new() -> Self {
        Self {
            h: 10.0,
            beta: 0.0,
            lambda: 2.0,
            gamma: 0.0,
            c: 10.0,
        }
    }
}

impl Default for StickoConfig {
    fn default() -> Self {
        Self::new()
    }
}

/// Struct to calculate ratings and expected score for [`StickoRating`]
pub struct Sticko {
    config: StickoConfig,
}

impl RatingSystem for Sticko {
    type RATING = StickoRating;
    type CONFIG = StickoConfig;

    fn new(config: Self::CONFIG) -> Self {
        Self { config }
    }

    fn rate(
        &self,
        player_one: &StickoRating,
        player_two: &StickoRating,
        outcome: &Outcomes,
    ) -> (StickoRating, StickoRating) {
        sticko(player_one, player_two, outcome, &self.config)
    }

    fn expected_score(&self, player_one: &StickoRating, player_two: &StickoRating) -> (f64, f64) {
        expected_score(player_one, player_two, &self.config)
    }
}

impl RatingPeriodSystem for Sticko {
    type RATING = StickoRating;
    type CONFIG = StickoConfig;

    fn new(config: Self::CONFIG) -> Self {
        Self { config }
    }

    fn rate(&self, player: &StickoRating, results: &[(StickoRating, Outcomes)]) -> StickoRating {
        // Need to add a colour indicator to the results, we use white everytime.
        // The advantage of playing white is set to 0 by default, anyways.
        let new_results: Vec<(StickoRating, Outcomes, bool)> =
            results.iter().map(|r| (r.0, r.1, true)).collect();

        sticko_rating_period(player, &new_results[..], &self.config)
    }

    fn expected_score(&self, player: &Self::RATING, opponents: &[Self::RATING]) -> Vec<f64> {
        let new_opponents: Vec<(StickoRating, bool)> =
            opponents.iter().map(|o| (*o, true)).collect();

        expected_score_rating_period(player, &new_opponents, &self.config)
    }
}

#[must_use]
/// Calculates the [`StickoRating`]s of two players based on their old ratings, deviations, and the outcome of the game.
///
/// Please see [`Glicko`](crate::glicko) for calculating with Glicko.
///
/// Takes in two players as [`StickoRating`]s, an [`Outcome`](Outcomes) and a [`StickoConfig`].
///
/// Instead of the traditional way of calculating the Sticko rating for only one player only using a list of results,
/// we are calculating the Sticko rating for two players at once, like in the Elo calculation,
/// to make it easier to see instant results.
///
/// For the traditional way of calculating a Sticko rating please see [`sticko_rating_period`].
///
/// The outcome of the match is in the perspective of `player_one`.
/// This means [`Outcomes::WIN`] is a win for `player_one` and [`Outcomes::LOSS`] is a win for `player_two`.
///
/// If you have set up the [`StickoConfig`] to account for an advantage,
/// this is also determined to be *in favour* the first player, and *against* the second.
///
/// # Examples
/// ```
/// use skillratings::{
///     sticko::{sticko, StickoConfig, StickoRating},
///     Outcomes,
/// };
///
/// let player_one = StickoRating {
///     rating: 1500.0,
///     deviation: 350.0,
/// };
/// let player_two = StickoRating {
///     rating: 1500.0,
///     deviation: 350.0,
/// };
///
/// let outcome = Outcomes::WIN;
///
/// let config = StickoConfig::new();
///
/// let (new_one, new_two) = sticko(&player_one, &player_two, &outcome, &config);
///
/// assert!((new_one.rating.round() - 1662.0).abs() < f64::EPSILON);
/// assert!((new_one.deviation.round() - 290.0).abs() < f64::EPSILON);
///
/// assert!((new_two.rating.round() - 1338.0).abs() < f64::EPSILON);
/// assert!((new_two.deviation.round() - 290.0).abs() < f64::EPSILON);
/// ```
pub fn sticko(
    player_one: &StickoRating,
    player_two: &StickoRating,
    outcome: &Outcomes,
    config: &StickoConfig,
) -> (StickoRating, StickoRating) {
    // The calculations here are very similar to the glicko calculations by design.
    // Sticko is more or less the same as Glicko, but with a few additional parameters.
    let q = 10.0_f64.ln() / 400.0;

    let outcome1 = outcome.to_chess_points();
    let outcome2 = 1.0 - outcome1;

    let colour1 = 1.0;
    let colour2 = -1.0;

    let lambda1 = (config.lambda / 100.0) * (player_two.rating - player_one.rating);
    let lambda2 = (config.lambda / 100.0) * (player_one.rating - player_two.rating);

    let g1 = g_value(q, player_two.deviation);
    let g2 = g_value(q, player_one.deviation);

    let e1 = e_value(
        g1,
        player_one.rating,
        player_two.rating,
        config.gamma,
        colour1,
    );
    let e2 = e_value(
        g2,
        player_two.rating,
        player_one.rating,
        config.gamma,
        colour2,
    );

    let d1 = d_value(q, g1, e1);
    let d2 = d_value(q, g2, e2);

    let player_one_pre_deviation = player_one.deviation.hypot(config.c).min(350.0);
    let player_two_pre_deviation = player_two.deviation.hypot(config.c).min(350.0);

    let player_one_new_rating = new_rating(
        player_one.rating,
        player_one_pre_deviation,
        q,
        d1,
        g1,
        outcome1,
        e1,
        config.beta,
        lambda1,
    );
    let player_two_new_rating = new_rating(
        player_two.rating,
        player_two_pre_deviation,
        q,
        d2,
        g2,
        outcome2,
        e2,
        config.beta,
        lambda2,
    );

    let player_one_new_deviation = new_deviation(player_one_pre_deviation, d1, config.h);
    let player_two_new_deviation = new_deviation(player_two_pre_deviation, d2, config.h);

    (
        StickoRating {
            rating: player_one_new_rating,
            deviation: player_one_new_deviation,
        },
        StickoRating {
            rating: player_two_new_rating,
            deviation: player_two_new_deviation,
        },
    )
}

#[must_use]
/// The "traditional" way of calculating a [`StickoRating`] of a player in a rating period.
///
/// Takes in a player as an [`StickoRating`] and their results as a Slice of tuples containing the opponent as an [`StickoRating`],
/// the outcome of the game as an [`Outcome`](Outcomes) and a [`bool`] specifying if the player was playing as player one, and a [`StickoConfig`].
///
/// ---
///
/// 📌 _**Important note:**_ We need an added parameter in the results tuple here.    
/// The boolean specifies if the player was playing as the first player, aka White in Chess.
/// If set to `true` the player was playing as White, if set to `false` the player was playing as Black.  
/// In the [`sticko`] function this is determined by the order of players that are input to the function, but we cannot do this here,
/// and because it likely changes from game-to-game, we need a separate parameter controlling it.
///
/// The colour you play in each game matters if the [`StickoConfig`] is set up with an advantage for the first player.  
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
///     sticko::{sticko_rating_period, StickoConfig, StickoRating},
///     Outcomes,
/// };
///
/// let player = StickoRating {
///     rating: 1500.0,
///     deviation: 200.0,
/// };
///
/// let opponent1 = StickoRating {
///     rating: 1400.0,
///     deviation: 30.0,
/// };
///
/// let opponent2 = StickoRating {
///     rating: 1550.0,
///     deviation: 100.0,
/// };
///
/// let opponent3 = StickoRating {
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
/// let config = StickoConfig::new();
///
/// let new_player = sticko_rating_period(&player, &results, &config);
///
/// assert!((new_player.rating.round() - 1465.0).abs() < f64::EPSILON);
/// assert!((new_player.deviation.round() - 152.0).abs() < f64::EPSILON);
/// ```
pub fn sticko_rating_period(
    player: &StickoRating,
    results: &[(StickoRating, Outcomes, bool)],
    config: &StickoConfig,
) -> StickoRating {
    let q = 10_f64.ln() / 400.0;

    if results.is_empty() {
        return decay_deviation(player, config);
    }

    let matches = results.len() as f64;

    let d_sq: f64 = (q.powi(2)
        * results
            .iter()
            .map(|r| {
                let g = g_value(q, r.0.deviation);

                let e = e_value(
                    g,
                    player.rating,
                    r.0.rating,
                    config.gamma,
                    if r.2 { 1.0 } else { -1.0 },
                );

                g.powi(2) * e * (1.0 - e)
            })
            .sum::<f64>())
    .recip();

    let lambda = (config.lambda / 100.0)
        * ((results.iter().map(|r| r.0.rating).sum::<f64>() / matches) - player.rating);

    let m: f64 = results
        .iter()
        .map(|r| {
            let g = g_value(q, r.0.deviation);

            let e = e_value(
                g,
                player.rating,
                r.0.rating,
                config.gamma,
                if r.2 { 1.0 } else { -1.0 },
            );

            let s = r.1.to_chess_points();

            g * (s - e + config.beta)
        })
        .sum();

    let pre_deviation = player.deviation.hypot(config.c).min(350.0);
    let new_deviation = ((pre_deviation
        .mul_add(pre_deviation, config.h * matches)
        .recip()
        + d_sq.recip())
    .recip()
    .sqrt())
    .min(350.0);

    let new_rating =
        (q / (pre_deviation.powi(2).recip() + d_sq.recip())).mul_add(m, player.rating) + lambda;

    StickoRating {
        rating: new_rating,
        deviation: new_deviation,
    }
}

#[must_use]
/// Calculates the expected outcome of two players based on sticko.
///
/// Takes in two players as [`StickoRating`]s and a [`StickoConfig`], which determines the advantage of player one,
/// and returns the probability of victory for each player as an [`f64`] between 1.0 and 0.0.  
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// # Examples
/// ```
/// use skillratings::sticko::{expected_score, StickoConfig, StickoRating};
///
/// let player_one = StickoRating {
///     rating: 1830.0,
///     deviation: 41.0,
/// };
/// let player_two = StickoRating {
///     rating: 1950.0,
///     deviation: 320.0,
/// };
///
/// let config = StickoConfig {
///     // This is approximately the advantage white has in chess.
///     gamma: 30.0,
///     ..Default::default()
/// };
///
/// let (exp_one, exp_two) = expected_score(&player_one, &player_two, &config);
///
/// assert!(((exp_one * 100.0).round() - 41.0).abs() < f64::EPSILON);
/// assert!(((exp_two * 100.0).round() - 59.0).abs() < f64::EPSILON);
/// ```
pub fn expected_score(
    player_one: &StickoRating,
    player_two: &StickoRating,
    config: &StickoConfig,
) -> (f64, f64) {
    let q = 10_f64.ln() / 400.0;
    let g = g_value(q, player_one.deviation.hypot(player_two.deviation));

    let exp_one = (1.0
        + 10_f64.powf(-g * (player_one.rating + config.gamma - player_two.rating) / 400.0))
    .recip();
    let exp_two = 1.0 - exp_one;

    (exp_one, exp_two)
}

#[must_use]
/// Calculates the expected outcome of a player in a rating period or tournament.
///
/// Takes in a players as [`StickoRating`] and a list of opponents as a slice of [`StickoRating`]
/// and returns the probability of victory for each match as an Vec of [`f64`] between 1.0 and 0.0 from the perspective of the player.  
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// ---
///
/// 📌 _**Important note:**_ The parameters intentionally work different from other expected_score_rating_period functions here.  
/// An additional config is used, because of the set advantage parameter that describes inherit imbalances, like playing White in Chess
/// or a Football team playing at home.
///
/// Because of those advantages, we also need a boolean which specifies if the player was playing as the first / advantaged player (e.g. White in Chess).
/// If set to `true` the player was playing with the advantage, if set to `false` the player was with the disadvantage.  
///
/// If the config is set to not have any advantages, the boolean will not matter.
///
/// ---
///
///
/// # Examples
/// ```
/// use skillratings::sticko::{expected_score_rating_period, StickoConfig, StickoRating};
///
/// let player = StickoRating {
///     rating: 1900.0,
///     deviation: 120.0,
/// };
///
/// let opponent1 = StickoRating {
///     rating: 1930.0,
///     deviation: 120.0,
/// };
///
/// let opponent2 = StickoRating {
///     rating: 1730.0,
///     deviation: 120.0,
/// };
///
/// let results = [
///     // Playing as White in Chess.
///     (opponent1, true),
///     // Playing as Black in Chess.
///     (opponent2, false),
/// ];
///
/// let config = StickoConfig::new();
///
/// let exp = expected_score_rating_period(&player, &results, &config);
///
/// assert_eq!((exp[0] * 100.0).round(), 46.0);
/// assert_eq!((exp[1] * 100.0).round(), 70.0);
/// ```
pub fn expected_score_rating_period(
    player: &StickoRating,
    opponents: &[(StickoRating, bool)],
    config: &StickoConfig,
) -> Vec<f64> {
    opponents
        .iter()
        .map(|o| {
            let q = 10_f64.ln() / 400.0;
            let g = g_value(q, player.deviation.hypot(o.0.deviation));

            let gamma = if o.1 { config.gamma } else { -config.gamma };

            (1.0 + 10_f64.powf(-g * (player.rating + gamma - o.0.rating) / 400.0)).recip()
        })
        .collect()
}

#[must_use]
/// Decays a Rating Deviation Value for a player, if they missed playing in a certain rating period.
///
/// The length of the rating period and thus the number of missed periods per player is something to decide and track yourself.
///
/// Takes in a player as a [`StickoRating`] and a [`StickoConfig`], that describes how much the rating should change, and returns the decayed [`StickoRating`].
///
/// # Examples
/// ```
/// use skillratings::sticko::{decay_deviation, StickoConfig, StickoRating};
///
/// let player_one = StickoRating {
///     rating: 2720.0,
///     deviation: 41.3,
/// };
///
/// let config = StickoConfig::new();
///
/// let player_one_decay = decay_deviation(&player_one, &config);
///
/// assert!((player_one_decay.deviation.round() - 42.0).abs() < f64::EPSILON);
/// ```
pub fn decay_deviation(player: &StickoRating, config: &StickoConfig) -> StickoRating {
    let new_player_deviation = player.deviation.hypot(config.c).min(350.0);

    StickoRating {
        rating: player.rating,
        deviation: new_player_deviation,
    }
}

#[must_use]
/// The 95% confidence interval of the lowest to highest rating.
///
/// The system is 95% sure that the "true skill" of the player is in-between these values.
///
/// Takes in a player as a [`StickoRating`] and returns two [`f64`]s that describe the lowest and highest rating.
///
/// # Examples
/// ```
/// use skillratings::sticko::{confidence_interval, StickoRating};
///
/// let player = StickoRating {
///     rating: 2250.0,
///     deviation: 79.0,
/// };
///
/// let (interval_low, interval_high) = confidence_interval(&player);
///
/// assert!(interval_low.round() - 2095.0 < f64::EPSILON);
/// assert!(interval_high.round() - 2405.0 < f64::EPSILON);
/// ```
pub fn confidence_interval(player: &StickoRating) -> (f64, f64) {
    (
        1.96f64.mul_add(-player.deviation, player.rating),
        1.96f64.mul_add(player.deviation, player.rating),
    )
}

fn new_deviation(old_deviation: f64, d: f64, h: f64) -> f64 {
    ((old_deviation.mul_add(old_deviation, h).recip() + d.recip())
        .recip()
        .sqrt())
    .min(350.0)
}

#[allow(clippy::too_many_arguments)]
fn new_rating(
    old_rating: f64,
    deviation: f64,
    q: f64,
    d: f64,
    g: f64,
    score: f64,
    e: f64,
    beta: f64,
    lambda: f64,
) -> f64 {
    ((q / (deviation.powi(2).recip() + d.recip())) * g).mul_add(score - e + beta, old_rating)
        + lambda
}

// The functions below are very similar to the normal glicko functions,
// but with the advantage parameters.

fn g_value(q: f64, opponent_deviation: f64) -> f64 {
    (1.0 + ((3.0 * q.powi(2) * opponent_deviation.powi(2)) / (PI.powi(2))))
        .sqrt()
        .recip()
}

fn e_value(g: f64, rating: f64, opponent_rating: f64, advantage: f64, colour: f64) -> f64 {
    (1.0 + (10_f64.powf(-g * advantage.mul_add(colour, rating - opponent_rating) / 400.0))).recip()
}

fn d_value(q: f64, g: f64, e: f64) -> f64 {
    (q.powi(2) * g.powi(2) * e * (1.0 - e)).powi(-1)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    /// This is the same test as in glicko.
    /// When using 0.0 for all values in the config,
    /// it should produce the same result.
    fn test_sticko() {
        let config = StickoConfig {
            h: 0.0,
            beta: 0.0,
            lambda: 0.0,
            gamma: 0.0,
            c: 0.0,
        };

        let player1 = StickoRating {
            rating: 1500.0,
            deviation: 200.0,
        };

        let opponent1 = StickoRating {
            rating: 1400.0,
            deviation: 30.0,
        };

        let opponent2 = StickoRating {
            rating: 1550.0,
            deviation: 100.0,
        };

        let opponent3 = StickoRating {
            rating: 1700.0,
            deviation: 300.0,
        };

        let (player1, _) = sticko(&player1, &opponent1, &Outcomes::WIN, &config);

        let (player1, _) = sticko(&player1, &opponent2, &Outcomes::LOSS, &config);

        let (player1, _) = sticko(&player1, &opponent3, &Outcomes::LOSS, &config);

        assert!((player1.rating.round() - 1464.0).abs() < f64::EPSILON);
        assert!((player1.deviation - 151.253_743_431_783_2).abs() < f64::EPSILON);
    }

    #[test]
    /// This is the same test as above.
    /// As explained in the glicko test, the result will be slightly different from above,
    /// because the games in a rating period are considered to be played at the same time.
    fn test_sticko_rating_period() {
        let config = StickoConfig {
            h: 0.0,
            beta: 0.0,
            lambda: 0.0,
            gamma: 0.0,
            c: 0.0,
        };

        let player = StickoRating {
            rating: 1500.0,
            deviation: 200.0,
        };

        let opponent1 = StickoRating {
            rating: 1400.0,
            deviation: 30.0,
        };

        let opponent2 = StickoRating {
            rating: 1550.0,
            deviation: 100.0,
        };

        let opponent3 = StickoRating {
            rating: 1700.0,
            deviation: 300.0,
        };

        let new_player = sticko_rating_period(
            &player,
            &[
                (opponent1, Outcomes::WIN, true),
                (opponent2, Outcomes::LOSS, true),
                (opponent3, Outcomes::LOSS, true),
            ],
            &config,
        );

        assert!((new_player.rating.round() - 1464.0).abs() < f64::EPSILON);
        assert!((new_player.deviation - 151.398_902_447_969_33).abs() < f64::EPSILON);

        let after_player = sticko_rating_period(&player, &[], &config);

        assert_eq!(player, after_player);
    }

    #[test]
    fn test_single_rp() {
        let player = StickoRating {
            rating: 1300.0,
            deviation: 25.0,
        };
        let opponent = StickoRating {
            rating: 1500.0,
            deviation: 34.0,
        };

        let config = StickoConfig {
            h: 10.0,
            beta: 5.0,
            lambda: 5.0,
            gamma: 30.0,
            c: 10.0,
        };

        let (np, _) = sticko(&player, &opponent, &Outcomes::WIN, &config);

        let rp = sticko_rating_period(&player, &[(opponent, Outcomes::WIN, true)], &config);

        assert_eq!(rp, np);
    }

    #[test]
    fn test_sticko_draw() {
        let player_one = StickoRating {
            rating: 2330.0,
            deviation: 200.0,
        };

        let player_two = StickoRating {
            rating: 1800.0,
            deviation: 20.0,
        };

        let config = StickoConfig::new();

        let (p1, p2) = sticko(&player_one, &player_two, &Outcomes::DRAW, &config);

        assert!((p1.rating.round() - 2220.0).abs() < f64::EPSILON);
        assert!((p1.deviation.round() - 195.0).abs() < f64::EPSILON);
        assert!((p2.rating.round() - 1812.0).abs() < f64::EPSILON);
        assert!((p2.deviation.round() - 23.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_expected_score() {
        let player_one = StickoRating {
            rating: 1400.0,
            deviation: 40.0,
        };

        let player_two = StickoRating {
            rating: 1500.0,
            deviation: 150.0,
        };

        let config_no_adv = StickoConfig::new();

        let config_adv = StickoConfig {
            gamma: 30.0,
            ..Default::default()
        };

        let (exp_one, exp_two) = expected_score(&player_one, &player_two, &config_no_adv);

        assert!((exp_one - 0.373_700_405_951_935).abs() < f64::EPSILON);
        assert!((exp_two - 0.626_299_594_048_065).abs() < f64::EPSILON);
        assert!((exp_one + exp_two - 1.0).abs() < f64::EPSILON);

        let (exp_one, exp_two) = expected_score(&player_one, &player_two, &config_adv);

        assert!((exp_one - 0.410_605_680_590_947_1).abs() < f64::EPSILON);
        assert!((exp_two - 0.589_394_319_409_053).abs() < f64::EPSILON);
        assert!((exp_one + exp_two - 1.0).abs() < f64::EPSILON);
    }

    #[test]
    #[allow(clippy::similar_names)]
    fn sticko_glicko_conversions() {
        let sticko = StickoRating::new();

        let glicko_conv = GlickoRating::from(sticko);
        let glicko2_conv = Glicko2Rating::from(sticko);
        let glickob_conv = GlickoBoostRating::from(sticko);

        assert!((glicko_conv.rating - 1500.0).abs() < f64::EPSILON);
        assert!((glicko2_conv.rating - 1500.0).abs() < f64::EPSILON);
        assert!((glickob_conv.rating - 1500.0).abs() < f64::EPSILON);

        let glicko2 = Glicko2Rating::new();
        let glicko = GlickoRating::new();
        let glickob = GlickoBoostRating::new();

        assert_eq!(StickoRating::new(), StickoRating::from(glicko2));
        assert_eq!(StickoRating::default(), StickoRating::from(glicko));
        assert_eq!(StickoRating::default(), StickoRating::from(glickob));
    }

    #[test]
    fn test_confidence_interval() {
        let player = StickoRating {
            rating: 1500.0,
            deviation: 30.0,
        };

        let ci = confidence_interval(&player);

        assert!((ci.0.round() - 1441.0).abs() < f64::EPSILON);
        assert!((ci.1.round() - 1559.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_white_black() {
        let player = StickoRating {
            rating: 1300.0,
            deviation: 120.0,
        };
        let opponent1 = StickoRating {
            rating: 1500.0,
            deviation: 105.0,
        };
        let opponent2 = StickoRating {
            rating: 1200.0,
            deviation: 125.0,
        };
        let opponent3 = StickoRating {
            rating: 1560.0,
            deviation: 140.0,
        };

        let config = StickoConfig {
            gamma: 30.0,
            ..Default::default()
        };

        let tournament1 = vec![
            (opponent1, Outcomes::WIN, true),
            (opponent2, Outcomes::LOSS, true),
            (opponent3, Outcomes::DRAW, true),
        ];

        let tournament2 = vec![
            (opponent1, Outcomes::WIN, false),
            (opponent2, Outcomes::LOSS, false),
            (opponent3, Outcomes::DRAW, false),
        ];

        let comp_player = sticko_rating_period(&player, &tournament1, &StickoConfig::new());

        assert!((comp_player.rating.round() - 1329.0).abs() < f64::EPSILON);
        assert!((comp_player.deviation.round() - 108.0).abs() < f64::EPSILON);

        let white_player = sticko_rating_period(&player, &tournament1, &config);

        assert!((white_player.rating.round() - 1323.0).abs() < f64::EPSILON);
        assert!((white_player.deviation.round() - 108.0).abs() < f64::EPSILON);

        let black_player = sticko_rating_period(&player, &tournament2, &config);

        assert!((black_player.rating.round() - 1335.0).abs() < f64::EPSILON);
        assert!((black_player.deviation.round() - 109.0).abs() < f64::EPSILON);
    }

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn test_misc_stuff() {
        let player_one = StickoRating::new();
        let config = StickoConfig::new();

        assert_eq!(player_one, player_one.clone());
        assert!((config.c - config.clone().c).abs() < f64::EPSILON);

        assert!(!format!("{player_one:?}").is_empty());
        assert!(!format!("{config:?}").is_empty());

        assert_eq!(player_one, StickoRating::from((1500.0, 350.0)));
    }

    #[test]
    fn test_traits() {
        let player_one: StickoRating = Rating::new(Some(240.0), Some(90.0));
        let player_two: StickoRating = Rating::new(Some(240.0), Some(90.0));

        let rating_system: Sticko = RatingSystem::new(StickoConfig::new());

        assert!((player_one.rating() - 240.0).abs() < f64::EPSILON);
        assert_eq!(player_one.uncertainty(), Some(90.0));

        let (new_player_one, new_player_two) =
            RatingSystem::rate(&rating_system, &player_one, &player_two, &Outcomes::WIN);

        let (exp1, exp2) = RatingSystem::expected_score(&rating_system, &player_one, &player_two);

        assert!((new_player_one.rating - 261.352_796_989_360_1).abs() < f64::EPSILON);
        assert!((new_player_two.rating - 218.647_203_010_639_9).abs() < f64::EPSILON);
        assert!((exp1 - 0.5).abs() < f64::EPSILON);
        assert!((exp2 - 0.5).abs() < f64::EPSILON);

        let rating_period_system: Sticko = RatingPeriodSystem::new(StickoConfig::new());
        let exp_rp =
            RatingPeriodSystem::expected_score(&rating_period_system, &player_one, &[player_two]);
        assert!((exp1 - exp_rp[0]).abs() < f64::EPSILON);

        let player_one: StickoRating = Rating::new(Some(240.0), Some(90.0));
        let player_two: StickoRating = Rating::new(Some(240.0), Some(90.0));

        let rating_period: Sticko = RatingPeriodSystem::new(StickoConfig::new());

        let new_player_one =
            RatingPeriodSystem::rate(&rating_period, &player_one, &[(player_two, Outcomes::WIN)]);

        assert!((new_player_one.rating - 261.352_796_989_360_1).abs() < f64::EPSILON);
    }
}
