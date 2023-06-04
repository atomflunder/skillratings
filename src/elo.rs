//! The Elo algorithm, the most widespread rating system and the gold-standard in chess and other games.  
//! Used in the official FIDE chess ratings, many online games, and the basis of even more rating systems.
//!
//! The higher the Elo rating number, the stronger the player.
//! Compared to other rating algorithms, Elo ratings are relatively static, but very transparent and simple to calculate.
//!
//! # Quickstart
//!
//! This is the most basic example on how to use the Elo Module.  
//! Please take a look at the functions below to see more advanced use cases.
//!
//! ```
//! use skillratings::{
//!     elo::{elo, EloConfig, EloRating},
//!     Outcomes,
//! };
//!
//! // Initialise a new player rating.
//! let player_one = EloRating::new();
//!
//! // Or you can initialise it with your own values of course.
//! // Imagine these numbers being pulled from a database.
//! let some_rating = 1325.0;
//! let player_two = EloRating {
//!     rating: some_rating,
//! };
//!
//! // The outcome of the match is from the perspective of player one.
//! let outcome = Outcomes::WIN;
//!
//! // The config allows you to specify certain values in the Elo calculation.
//! // Here we modify the k-value to be 20.0, instead of the usual 32.0.
//! // To simplify massively: This means the ratings will not change as much.
//! let config = EloConfig { k: 20.0 };
//!
//! // The elo function will calculate the new ratings for both players and return them.
//! let (new_player_one, new_player_two) = elo(&player_one, &player_two, &outcome, &config);
//! ```
//!
//! # More Information
//!
//! - [Wikipedia Article](https://en.wikipedia.org/wiki/Elo_rating_system)
//! - [Elo Calculator](https://www.omnicalculator.com/sports/elo)
//! - [FIDE Ratings](https://ratings.fide.com/)

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::{
    dwz::DWZRating, fifa::FifaRating, ingo::IngoRating, uscf::USCFRating, Outcomes, Rating,
    RatingPeriodSystem, RatingSystem,
};

/// The Elo rating of a player.
///
/// The default rating is 1000.0.
#[derive(Copy, Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct EloRating {
    /// The player's Elo rating number, by default 1000.0.
    pub rating: f64,
}

impl EloRating {
    /// Initialise a new `EloRating` with a rating of 1000.0.
    #[must_use]
    pub const fn new() -> Self {
        Self { rating: 1000.0 }
    }
}

impl Default for EloRating {
    fn default() -> Self {
        Self::new()
    }
}

impl Rating for EloRating {
    fn rating(&self) -> f64 {
        self.rating
    }
    fn uncertainty(&self) -> Option<f64> {
        None
    }
    fn new(rating: Option<f64>, _uncertainty: Option<f64>) -> Self {
        Self {
            rating: rating.unwrap_or(1000.0),
        }
    }
}

impl From<f64> for EloRating {
    fn from(r: f64) -> Self {
        Self { rating: r }
    }
}

impl From<IngoRating> for EloRating {
    fn from(i: IngoRating) -> Self {
        Self {
            rating: 8.0f64.mul_add(-i.rating, 2840.0),
        }
    }
}

impl From<DWZRating> for EloRating {
    fn from(d: DWZRating) -> Self {
        Self { rating: d.rating }
    }
}

impl From<USCFRating> for EloRating {
    fn from(u: USCFRating) -> Self {
        if u.rating > 2060.0 {
            Self {
                rating: (u.rating - 180.0) / 0.94,
            }
        } else {
            Self {
                rating: (u.rating - 20.0) / 1.02,
            }
        }
    }
}

impl From<FifaRating> for EloRating {
    fn from(f: FifaRating) -> Self {
        Self { rating: f.rating }
    }
}

#[derive(Clone, Copy, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// Constants used in the Elo calculations.
pub struct EloConfig {
    /// The k-value is the maximum amount of rating change from a single match.
    /// In chess, k-values from 40 to 10 are used, with the most common being 32, 24, 16 or 10.
    /// The higher the number, the more volatile the ranking.  
    /// Here the default is 32.
    pub k: f64,
}

impl EloConfig {
    #[must_use]
    /// Initialise a new `EloConfig` with a k value of `32.0`.
    pub const fn new() -> Self {
        Self { k: 32.0 }
    }
}

impl Default for EloConfig {
    fn default() -> Self {
        Self::new()
    }
}

/// Struct to calculate ratings and expected score for [`EloRating`]
pub struct Elo {
    config: EloConfig,
}

impl RatingSystem for Elo {
    type RATING = EloRating;
    type CONFIG = EloConfig;

    fn new(config: Self::CONFIG) -> Self {
        Self { config }
    }

    fn rate(
        &self,
        player_one: &EloRating,
        player_two: &EloRating,
        outcome: &Outcomes,
    ) -> (EloRating, EloRating) {
        elo(player_one, player_two, outcome, &self.config)
    }

    fn expected_score(&self, player_one: &EloRating, player_two: &EloRating) -> (f64, f64) {
        expected_score(player_one, player_two)
    }
}

impl RatingPeriodSystem for Elo {
    type RATING = EloRating;
    type CONFIG = EloConfig;

    fn new(config: Self::CONFIG) -> Self {
        Self { config }
    }

    fn rate(&self, player: &EloRating, results: &[(EloRating, Outcomes)]) -> EloRating {
        elo_rating_period(player, results, &self.config)
    }
}

/// Calculates the [`EloRating`]s of two players based on their old ratings and the outcome of the game.
///
/// Takes in two players as [`EloRating`]s, an [`Outcome`](Outcomes) and an [`EloConfig`].
///
/// The outcome of the match is in the perspective of `player_one`.
/// This means [`Outcomes::WIN`] is a win for `player_one` and [`Outcomes::LOSS`] is a win for `player_two`.
///
/// # Examples
/// ```
/// use skillratings::{
///     elo::{elo, EloConfig, EloRating},
///     Outcomes,
/// };
///
/// let player_one = EloRating { rating: 600.0 };
/// let player_two = EloRating { rating: 711.0 };
///
/// let outcome = Outcomes::WIN;
///
/// let config = EloConfig::new();
///
/// let (new_one, new_two) = elo(&player_one, &player_two, &outcome, &config);
///
/// assert!((new_one.rating.round() - 621.0).abs() < f64::EPSILON);
/// assert!((new_two.rating.round() - 690.0).abs() < f64::EPSILON);
/// ```
#[must_use]
pub fn elo(
    player_one: &EloRating,
    player_two: &EloRating,
    outcome: &Outcomes,
    config: &EloConfig,
) -> (EloRating, EloRating) {
    let (one_expected, two_expected) = expected_score(player_one, player_two);

    let outcome1 = outcome.to_chess_points();
    let outcome2 = 1.0 - outcome1;

    let one_new_elo = config.k.mul_add(outcome1 - one_expected, player_one.rating);
    let two_new_elo = config.k.mul_add(outcome2 - two_expected, player_two.rating);

    (
        EloRating {
            rating: one_new_elo,
        },
        EloRating {
            rating: two_new_elo,
        },
    )
}

#[must_use]
/// Calculates an [`EloRating`] in a non-traditional way using a rating period,
/// for compatibility with the other algorithms.
///
/// Takes in a player as an [`EloRating`] and their results as a Vec of tuples containing the opponent as an [`EloRating`]
/// and the outcome of the game as an [`Outcome`](Outcomes).
///
/// All of the outcomes are from the perspective of the player.
/// This means [`Outcomes::WIN`] is a win for the player and [`Outcomes::LOSS`] is a win for the opponent.
///
/// # Examples
/// ```
/// use skillratings::{
///     elo::{elo_rating_period, EloConfig, EloRating},
///     Outcomes,
/// };
///
/// let player = EloRating { rating: 1204.0 };
///
/// // Here we assume that we just play against 3 new players, for simplicity.
/// let opponent1 = EloRating::new();
/// let opponent2 = EloRating::new();
/// let opponent3 = EloRating::new();
///
/// let new_player = elo_rating_period(
///     &player,
///     &vec![
///         (opponent1, Outcomes::WIN),
///         (opponent2, Outcomes::DRAW),
///         (opponent3, Outcomes::WIN),
///     ],
///     &EloConfig::new(),
/// );
///
/// assert!((new_player.rating.round() - 1210.0).abs() < f64::EPSILON);
/// ```
pub fn elo_rating_period(
    player: &EloRating,
    results: &[(EloRating, Outcomes)],
    config: &EloConfig,
) -> EloRating {
    let mut player_rating = player.rating;

    for (opponent, result) in results {
        // Normally we would just call expected_points(),
        // but we would have to construct a rating first which seems inefficient.
        // So we are just calculating it ourselves.
        let exp = (1.0 + 10_f64.powf((opponent.rating - player_rating) / 400.0)).recip();

        let outcome = result.to_chess_points();

        player_rating = config.k.mul_add(outcome - exp, player_rating);
    }

    EloRating {
        rating: player_rating,
    }
}

/// Calculates the expected score of two players based on their elo rating.
///
/// Takes in two players as [`EloRating`]s and returns the probability of victory for each player as an [`f64`] between 1.0 and 0.0.  
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// # Examples
/// ```
/// use skillratings::elo::{expected_score, EloRating};
///
/// let player_one = EloRating { rating: 1320.0 };
/// let player_two = EloRating { rating: 1217.0 };
///
/// let (exp1, exp2) = expected_score(&player_one, &player_two);
///
/// assert!(((exp1 * 100.0).round() - 64.0).abs() < f64::EPSILON);
/// assert!(((exp2 * 100.0).round() - 36.0).abs() < f64::EPSILON);
/// ```
#[must_use]
pub fn expected_score(player_one: &EloRating, player_two: &EloRating) -> (f64, f64) {
    let exp_one = (1.0 + 10_f64.powf((player_two.rating - player_one.rating) / 400.0)).recip();
    let exp_two = 1.0 - exp_one;

    (exp_one, exp_two)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_elo() {
        let (winner_new_elo, loser_new_elo) = elo(
            &EloRating { rating: 1000.0 },
            &EloRating { rating: 1000.0 },
            &Outcomes::WIN,
            &EloConfig::new(),
        );
        assert!((winner_new_elo.rating - 1016.0).abs() < f64::EPSILON);
        assert!((loser_new_elo.rating - 984.0).abs() < f64::EPSILON);

        let (winner_new_elo, loser_new_elo) = elo(
            &EloRating { rating: 1000.0 },
            &EloRating { rating: 1000.0 },
            &Outcomes::LOSS,
            &EloConfig::new(),
        );
        assert!((winner_new_elo.rating - 984.0).abs() < f64::EPSILON);
        assert!((loser_new_elo.rating - 1016.0).abs() < f64::EPSILON);

        let (winner_new_elo, loser_new_elo) = elo(
            &EloRating { rating: 1000.0 },
            &EloRating { rating: 1000.0 },
            &Outcomes::DRAW,
            &EloConfig::new(),
        );
        assert!((winner_new_elo.rating - 1000.0).abs() < f64::EPSILON);
        assert!((loser_new_elo.rating - 1000.0).abs() < f64::EPSILON);

        let (winner_new_elo, loser_new_elo) = elo(
            &EloRating { rating: 500.0 },
            &EloRating { rating: 1500.0 },
            &Outcomes::WIN,
            &EloConfig::default(),
        );
        assert!((winner_new_elo.rating.round() - 532.0).abs() < f64::EPSILON);
        assert!((loser_new_elo.rating.round() - 1468.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_elo_rating_period() {
        let player = EloRating::new();

        let opponent1 = EloRating::new();
        let opponent2 = EloRating::new();
        let opponent3 = EloRating::new();

        let new_player = elo_rating_period(
            &player,
            &[
                (opponent1, Outcomes::WIN),
                (opponent2, Outcomes::DRAW),
                (opponent3, Outcomes::LOSS),
            ],
            &EloConfig::new(),
        );

        assert!((new_player.rating.round() - 999.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_expected_score() {
        let player_one = EloRating::new();
        let player_two = EloRating::default();

        let (winner_expected, loser_expected) = expected_score(&player_one, &player_two);

        assert!((winner_expected - 0.5).abs() < f64::EPSILON);
        assert!((loser_expected - 0.5).abs() < f64::EPSILON);

        let player_one = EloRating { rating: 2251.0 };
        let player_two = EloRating { rating: 1934.0 };

        let (winner_expected, loser_expected) = expected_score(&player_one, &player_two);

        assert!(((winner_expected * 100.0).round() - 86.0).abs() < f64::EPSILON);
        assert!(((loser_expected * 100.0).round() - 14.0).abs() < f64::EPSILON);

        assert!((winner_expected + loser_expected - 1.0).abs() < f64::EPSILON);
    }

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn test_misc_stuff() {
        let player_one = EloRating::new();
        let config = EloConfig::new();

        assert_eq!(player_one, player_one.clone());
        assert!((config.k - config.clone().k).abs() < f64::EPSILON);

        assert!(!format!("{player_one:?}").is_empty());
        assert!(!format!("{config:?}").is_empty());

        assert_eq!(player_one, EloRating::from(1000.0));
    }

    #[test]
    fn test_traits() {
        let player_one: EloRating = Rating::new(Some(240.0), Some(90.0));
        let player_two: EloRating = Rating::new(Some(240.0), Some(90.0));

        let rating_system: Elo = RatingSystem::new(EloConfig::new());

        assert!((player_one.rating() - 240.0).abs() < f64::EPSILON);
        assert_eq!(player_one.uncertainty(), None);

        let (new_player_one, new_player_two) =
            RatingSystem::rate(&rating_system, &player_one, &player_two, &Outcomes::WIN);

        let (exp1, exp2) = RatingSystem::expected_score(&rating_system, &player_one, &player_two);

        assert!((new_player_one.rating - 256.0).abs() < f64::EPSILON);
        assert!((new_player_two.rating - 224.0).abs() < f64::EPSILON);
        assert!((exp1 - 0.5).abs() < f64::EPSILON);
        assert!((exp2 - 0.5).abs() < f64::EPSILON);

        let player_one: EloRating = Rating::new(Some(240.0), Some(90.0));
        let player_two: EloRating = Rating::new(Some(240.0), Some(90.0));

        let rating_period: Elo = RatingPeriodSystem::new(EloConfig::new());

        let new_player_one =
            RatingPeriodSystem::rate(&rating_period, &player_one, &[(player_two, Outcomes::WIN)]);

        assert!((new_player_one.rating - 256.0).abs() < f64::EPSILON);
    }
}
