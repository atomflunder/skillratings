//! The Elo algorithm, the most widespread rating system and the gold-standard in chess and other games.  
//! Used in the official FIDE chess ratings, FIFA World Rankings, and many online video games.
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
//!     elo::elo, outcomes::Outcomes, rating::EloRating, config::EloConfig
//! };
//!
//! // Initialise a new player rating.
//! let player_one = EloRating::new();
//!
//! // Or you can initialise it with your own values of course.
//! // Imagine these numbers being pulled from a database.
//! let some_rating = 1325.0;
//! let player_two = EloRating{
//!     rating: some_rating,
//! };
//!
//! // The outcome of the match is from the perspective of player one.
//! let outcome = Outcomes::WIN;
//!
//! // The config allows you to specify certain values in the Elo calculation.
//! let config = EloConfig::new();
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
//! - [FIFA Ratings](https://www.fifa.com/fifa-world-ranking)

use crate::{config::EloConfig, outcomes::Outcomes, rating::EloRating};

/// Calculates the [`EloRating`]s of two players based on their old ratings and the outcome of the game.
///
/// Takes in two players as [`EloRating`]s, an [`Outcome`](Outcomes) and an [`EloConfig`].
///
/// The outcome of the match is in the perspective of `player_one`.
/// This means [`Outcomes::WIN`] is a win for `player_one` and [`Outcomes::LOSS`] is a win for `player_two`.
///
/// # Examples
/// ```
/// use skillratings::{elo::elo, outcomes::Outcomes, rating::EloRating, config::EloConfig};
///
/// let player_one = EloRating { rating: 1000.0 };
/// let player_two = EloRating { rating: 1000.0 };
///
/// let outcome = Outcomes::WIN;
///
/// let config = EloConfig::new();
///
/// let (player_one_new, player_two_new) = elo(&player_one, &player_two, &outcome, &config);
///
/// assert!((player_one_new.rating - 1016.0).abs() < f64::EPSILON);
/// assert!((player_two_new.rating - 984.0).abs() < f64::EPSILON);
/// ```
#[must_use]
pub fn elo(
    player_one: &EloRating,
    player_two: &EloRating,
    outcome: &Outcomes,
    config: &EloConfig,
) -> (EloRating, EloRating) {
    let (one_expected, two_expected) = expected_score(player_one, player_two);

    let outcome1 = match outcome {
        Outcomes::WIN => 1.0,
        Outcomes::LOSS => 0.0,
        Outcomes::DRAW => 0.5,
    };
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
/// use skillratings::{elo::elo_rating_period, outcomes::Outcomes, rating::EloRating, config::EloConfig};
///
/// let player = EloRating::new();
///
/// let opponent1 = EloRating::new();
/// let opponent2 = EloRating::new();
/// let opponent3 = EloRating::new();
///
/// let new_player = elo_rating_period(
///     &player,
///     &vec![
///         (opponent1, Outcomes::WIN),
///         (opponent2, Outcomes::WIN),
///         (opponent3, Outcomes::WIN),
///     ],
///     &EloConfig::new(),
/// );
///
/// assert!((new_player.rating.round() - 1046.0).abs() < f64::EPSILON);
/// ```
#[must_use]
pub fn elo_rating_period(
    player: &EloRating,
    results: &Vec<(EloRating, Outcomes)>,
    config: &EloConfig,
) -> EloRating {
    let mut player = *player;

    for (opponent, result) in results {
        let (exp, _) = expected_score(&player, opponent);

        let outcome = match result {
            Outcomes::WIN => 1.0,
            Outcomes::LOSS => 0.0,
            Outcomes::DRAW => 0.5,
        };

        player.rating = config.k.mul_add(outcome - exp, player.rating);
    }

    player
}

/// Calculates the expected score of two players based on their elo rating.
/// Meant for usage in the elo function, but you can also use it to predict games yourself.
///
/// Takes in two players as [`EloRating`]s and returns the probability of victory for each player as an [`f64`] between 1.0 and 0.0.  
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// # Examples
/// ```
/// use skillratings::{elo::expected_score, rating::EloRating};
///
/// let player_one = EloRating { rating: 1320.0 };
/// let player_two = EloRating { rating: 1217.0 };
///
/// let (winner_exp, loser_exp) = expected_score(&player_one, &player_two);
///
/// assert!(((winner_exp * 100.0).round() - 64.0).abs() < f64::EPSILON);
/// assert!(((loser_exp * 100.0).round() - 36.0).abs() < f64::EPSILON);
/// ```
#[must_use]
pub fn expected_score(player_one: &EloRating, player_two: &EloRating) -> (f64, f64) {
    (
        1.0 / (1.0 + 10_f64.powf((player_two.rating - player_one.rating) / 400.0)),
        1.0 / (1.0 + 10_f64.powf((player_one.rating - player_two.rating) / 400.0)),
    )
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
            &vec![
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
}
