//! The EGF (European Go Federation) rating system is a variation of the Elo rating system, adapted for playing Go.  
//! Used for calculating Go player ratings in Europe since 1998.
//!
//! The ratings are loosely centred around the Go ranks, ranging from 30 *kyu* (lowest) to 9 *dan* (highest).  
//! A rating of 2100 equals a rank of 1 *dan*, and 1 rank up and down equals a gain or loss of around 100 points.  
//! So a 2 *dan* rank would equal around 2200 points, and so on. The lowest rank, 30 *kyu*, is equal to -900 points.  
//! You start at a rating of 0, around 21 *kyu*.
//! The [full table of ranks/rating can be found here](https://senseis.xmp.net/?GoR),
//! or [here is an unofficial comparison table](https://forums.online-go.com/t/go-ranks-vs-chess-ratings/41594/42)
//! that includes USCF chess ratings.
//!
//! # Quickstart
//!
//! This is the most basic example on how to use the EGF Module.  
//! Please take a look at the functions below to see more advanced use cases.
//!
//! ```
//! use skillratings::{
//!     egf::{egf, EGFConfig, EGFRating},
//!     Outcomes,
//! };
//!
//! // Initialise a new player rating with a rating of 0.
//! let player_one = EGFRating::new();
//!
//! // Or you can initialise it with your own values of course.
//! // Imagine these numbers being pulled from a database.
//! let some_rating = 325.0;
//! let player_two = EGFRating {
//!     rating: some_rating,
//! };
//!
//! // The outcome of the match is from the perspective of player one.
//! let outcome = Outcomes::WIN;
//!
//! // The config allows you to specify certain values in the EGF calculation.
//! // We set a handicap of 4.0 here for player one.
//! // This means that player_two starts with 4 handicap stones.
//! // For more details see the "Handicapping in Go" link below.
//! let config = EGFConfig { handicap: 4.0 };
//!
//! // The egf function will calculate the new ratings for both players and return them.
//! let (new_player_one, new_player_two) = egf(&player_one, &player_two, &outcome, &config);
//! ```
//!
//! # More Information
//!
//! - [Wikipedia Article: Go Ratings](https://en.wikipedia.org/wiki/Go_ranks_and_ratings#Elo_ratings_as_used_in_Go)
//! - [Wikipedia Article: EGF](https://en.wikipedia.org/wiki/European_Go_Federation)
//! - [Official EGF Website](https://www.europeangodatabase.eu/EGD/EGF_rating_system.php)
//! - [EGF Rating Calculator](https://www.europeangodatabase.eu/EGD/gor_calculator.php)
//! - [Sensei's library](https://senseis.xmp.net/?GoR)
//! - [Handicapping in Go](https://en.wikipedia.org/wiki/Handicapping_in_Go)

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::Outcomes;

#[derive(Copy, Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// The EGF (European Go Federation) Rating for a player.
///
/// If the player has a Go rank or similar,
/// you can set the rating value manually approximately according to
/// [this unofficial comparison table](https://forums.online-go.com/t/go-ranks-vs-chess-ratings/41594/42).  
/// Keep in mind that here, the lowest possible rating is -900.0.
///
/// The default rating is 0.0.
pub struct EGFRating {
    /// The player's EGF rating number, by default 0.0.
    pub rating: f64,
}

impl EGFRating {
    #[must_use]
    /// Initialise a new `EGFRating` with a rating of 0.0.
    pub const fn new() -> Self {
        Self { rating: 0.0 }
    }
}

impl Default for EGFRating {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Copy, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// Constants used in the EGF Calculations.
pub struct EGFConfig {
    /// The [handicap](https://en.wikipedia.org/wiki/Handicapping_in_Go), of the perspective of player one.  
    /// As a general rule, one handicap point is about equal to a 100 rating point difference.  
    ///
    /// If player one has a handicap in the game,
    /// you can set this number to the amount of handicap stones given to the opponent.  
    /// If player two is the one with the handicap, set this number to the negative amount of stones given.  
    /// If an equal game is played, this value should be 0.0.  
    /// For example, if player two has a handicap of 4 points (player one starts with 4 stones), set this number to -4.0.  
    ///
    /// The maximum number should not exceed 9.0 or -9.0.  
    /// By default set to 0.0.
    pub handicap: f64,
}

impl EGFConfig {
    #[must_use]
    /// Initialises a new `EGFConfig` with a handicap value of `0.0`.
    pub const fn new() -> Self {
        Self { handicap: 0.0 }
    }
}

impl Default for EGFConfig {
    fn default() -> Self {
        Self::new()
    }
}

#[must_use]
/// Calculates the [`EGFRating`]s of two players based on their old ratings and the outcome of the game.
///
/// Takes in two players as [`EGFRating`]s, an [`Outcome`](Outcomes) and an [`EGFConfig`], where you can set up handicaps.
///
/// The outcome of the match is in the perspective of `player_one`.
/// This means [`Outcomes::WIN`] is a win for `player_one` and [`Outcomes::LOSS`] is a win for `player_two`.
///
/// # Examples
///
/// ```
/// use skillratings::{
///     egf::{egf, EGFConfig, EGFRating},
///     Outcomes,
/// };
///
/// let player_one = EGFRating { rating: 950.0 };
/// let player_two = EGFRating { rating: 1200.0 };
///
/// let outcome = Outcomes::WIN;
///
/// let config = EGFConfig::new();
///
/// let (new_one, new_two) = egf(&player_one, &player_two, &outcome, &config);
///
/// assert!((new_one.rating.round() - 989.0).abs() < f64::EPSILON);
/// assert!((new_two.rating.round() - 1173.0).abs() < f64::EPSILON);
/// ```
pub fn egf(
    player_one: &EGFRating,
    player_two: &EGFRating,
    outcome: &Outcomes,
    config: &EGFConfig,
) -> (EGFRating, EGFRating) {
    let con1 = con(player_one.rating);
    let con2 = con(player_two.rating);

    let bonus1 = bonus(player_one.rating);
    let bonus2 = bonus(player_two.rating);

    let (exp1, exp2) = expected_score(player_one, player_two, config);

    let outcome1 = outcome.to_chess_points();
    let outcome2 = 1.0 - outcome1;

    let new_rating1 = new_rating(player_one.rating, con1, outcome1, exp1, bonus1);
    let new_rating2 = new_rating(player_two.rating, con2, outcome2, exp2, bonus2);

    (
        EGFRating {
            rating: new_rating1,
        },
        EGFRating {
            rating: new_rating2,
        },
    )
}

#[must_use]
/// Calculates an [`EGFRating`] in a traditional way using a rating period, commonly used for tournaments.
///
/// Takes in a player as an [`EGFRating`], their results as a Vec of tuples containing the opponent as an [`EGFRating`],
/// the outcome of the game as an [`Outcome`](Outcomes), and an [`EGFConfig`] where you can specify handicaps for the players.
///
/// ---
///
/// 📌 _**Important note:**_ The parameters intentionally work different from other rating_period functions here.  
/// In most cases the config is a separate parameter, because it holds static values that should not change from game-to-game.  
/// Here, the config is in the tuple together with the results, because the handicaps will likely change each game, if used.  
/// Thus it would not make sense to have the config as its own separate parameter, disconnected from the game results.
///
/// ---
///
/// All of the outcomes are from the perspective of the player.
/// This means [`Outcomes::WIN`] is a win for the player and [`Outcomes::LOSS`] is a win for the opponent.
///
/// Keep in mind that [according to EGF](https://www.europeangodatabase.eu/EGD/EGF_rating_system.php#Remarks),
/// the rating will not update in between games of a tournament, in contrast to the other rating_period algorithms.  
/// That means that for example, in game 5 of a tournament,
/// the ratings will still be calculated with the ratings *before* game 1,
/// and not with the ratings after game 4, as you might expect.
///
/// # Examples
/// ```
/// use skillratings::{
///     egf::{egf_rating_period, EGFConfig, EGFRating},
///     Outcomes,
/// };
///
/// let player = EGFRating { rating: 220.0 };
///
/// let opponent1 = EGFRating { rating: 420.0 };
/// let opponent2 = EGFRating { rating: 40.0 };
/// let opponent3 = EGFRating { rating: 1320.0 };
///
/// let config = EGFConfig::new();
///
/// let new_player = egf_rating_period(
///     &player,
///     &vec![
///         // Make sure to include the configs in each game!
///         (opponent1, Outcomes::LOSS, config),
///         (opponent2, Outcomes::WIN, config),
///         (opponent3, Outcomes::DRAW, config),
///     ],
/// );
///
/// assert!((new_player.rating.round() - 273.0).abs() < f64::EPSILON);
/// ```
pub fn egf_rating_period(
    player: &EGFRating,
    results: &Vec<(EGFRating, Outcomes, EGFConfig)>,
) -> EGFRating {
    // According to the EGF, the rating values are not updated in between games.
    // Only the change in rating is kept track of.
    // Reference: https://www.europeangodatabase.eu/EGD/EGF_rating_system.php#Remarks
    let mut rating_change = 0.0;

    let con = con(player.rating);
    let bonus = bonus(player.rating);

    for (opponent, result, config) in results {
        let (exp1, _) = expected_score(player, opponent, config);

        let outcome = result.to_chess_points();

        rating_change += new_rating(player.rating, con, outcome, exp1, bonus) - player.rating;
    }

    EGFRating {
        rating: player.rating + rating_change,
    }
}

#[must_use]
/// Calculates the expected score of two players based on their EGF rating.
///
/// Takes in two players as [`EGFRating`]s and returns the probability of victory for each player as an [`f64`] between 1.0 and 0.0.  
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur, if draws are possible.
///
/// # Examples
/// ```
/// use skillratings::{
///     egf::{expected_score, EGFConfig, EGFRating},
///     Outcomes,
/// };
///
/// let player_one = EGFRating { rating: 1320.0 };
/// let player_two = EGFRating { rating: 1217.0 };
///
/// let config = EGFConfig::new();
///
/// let (exp1, exp2) = expected_score(&player_one, &player_two, &config);
///
/// assert!(((exp1 * 100.0).round() - 59.0).abs() < f64::EPSILON);
/// assert!(((exp2 * 100.0).round() - 41.0).abs() < f64::EPSILON);
/// ```
pub fn expected_score(
    player_one: &EGFRating,
    player_two: &EGFRating,
    config: &EGFConfig,
) -> (f64, f64) {
    let (h1, h2) = if config.handicap.is_sign_negative() {
        (config.handicap.abs(), 0.0)
    } else {
        (0.0, config.handicap.abs())
    };

    let exp_one = (1.0 + (beta(player_two.rating, h2) - beta(player_one.rating, h1)).exp()).recip();

    (exp_one, 1.0 - exp_one)
}

fn new_rating(rating: f64, con: f64, score: f64, exp_score: f64, bonus: f64) -> f64 {
    // The absolute minimum rating is set to be -900.
    (con.mul_add(score - exp_score, rating) + bonus).max(-900.0)
}

/// This factor determines the rating volatility.
/// Think of it like the k-value in Elo, but dynamic to your rating.
fn con(rating: f64) -> f64 {
    ((3300.0 - rating) / 200.0).powf(1.6)
}

/// The bonus parameter counters rating deflation over time.
fn bonus(rating: f64) -> f64 {
    ((2300.0 - rating) / 80.0).exp().ln_1p() / 5.0
}

fn beta(rating: f64, handicap: f64) -> f64 {
    // The explanation of the handicap formula can be found here:
    // https://www.europeangodatabase.eu/EGD/EGF_rating_system_old.php#System
    // The new system doesn't really mention this, but it stayed the same between transitions.
    // You can verify this with the calculator: https://www.europeangodatabase.eu/EGD/gor_calculator.php
    let h = if handicap == 0.0 {
        0.0
    } else {
        100.0 * (handicap - 0.5)
    };
    -7.0 * (3300.0 - h - rating).ln()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_handicap() {
        let config = EGFConfig { handicap: 0.0 };

        let player_one = EGFRating { rating: 1000.0 };
        let player_two = EGFRating { rating: 1300.0 };

        let (new_one, new_two) = expected_score(&player_one, &player_two, &config);

        assert!((new_one - 0.273_222_559_619_518_8).abs() < f64::EPSILON);
        assert!((new_two - 0.726_777_440_380_481_2).abs() < f64::EPSILON);
        assert!((new_one + new_two - 1.0).abs() < f64::EPSILON);

        let handicap_config = EGFConfig { handicap: 1.0 };

        let (new_one, new_two) = expected_score(&player_one, &player_two, &handicap_config);

        assert!((new_one - 0.239_475_310_432_178_62).abs() < f64::EPSILON);
        assert!((new_two - 0.760_524_689_567_821_4).abs() < f64::EPSILON);
        assert!((new_one + new_two - 1.0).abs() < f64::EPSILON);

        let other_handicap_config = EGFConfig { handicap: -1.0 };

        let (new_one, new_two) = expected_score(&player_one, &player_two, &other_handicap_config);

        assert!((new_one - 0.304_813_243_836_844_05).abs() < f64::EPSILON);
        assert!((new_two - 0.695_186_756_163_156).abs() < f64::EPSILON);
        assert!((new_one + new_two - 1.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_egf() {
        let player_one = EGFRating { rating: 1000.0 };
        let player_two = EGFRating { rating: 1300.0 };

        let config = EGFConfig::new();

        let (new_one, new_two) = egf(&player_one, &player_two, &Outcomes::WIN, &config);

        assert!((new_one.rating.round() - 1039.0).abs() < f64::EPSILON);
        assert!((new_two.rating.round() - 1274.0).abs() < f64::EPSILON);

        let player_one = EGFRating { rating: 0.0 };
        let player_two = EGFRating::default();

        let (new_one, new_two) = egf(&player_one, &player_two, &Outcomes::LOSS, &config);

        assert!((new_one.rating.round() - -39.0).abs() < f64::EPSILON);
        assert!((new_two.rating.round() - 50.0).abs() < f64::EPSILON);

        let (new_one, new_two) = egf(&player_one, &player_two, &Outcomes::DRAW, &config);

        assert!((new_one.rating.round() - 6.0).abs() < f64::EPSILON);
        assert!((new_two.rating.round() - 6.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_egf_rating_period() {
        let player = EGFRating::new();

        let config = EGFConfig::default();

        let results = vec![
            (EGFRating { rating: 20.0 }, Outcomes::WIN, config),
            (EGFRating { rating: 40.0 }, Outcomes::DRAW, config),
            (EGFRating { rating: 10.0 }, Outcomes::LOSS, config),
            (EGFRating { rating: -40.0 }, Outcomes::WIN, config),
        ];

        let new_player = egf_rating_period(&player, &results);

        assert!((new_player.rating.round() - 69.0).abs() < f64::EPSILON);
    }

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn test_misc_stuff() {
        let player_one = EGFRating::new();
        let config = EGFConfig::new();

        assert_eq!(player_one, player_one.clone());
        assert!((config.handicap - config.clone().handicap).abs() < f64::EPSILON);

        assert!(!format!("{:?}", player_one).is_empty());
        assert!(!format!("{:?}", config).is_empty());
    }
}
