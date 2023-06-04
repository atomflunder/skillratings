//! The Ingo algorithm, the predecessor of DWZ and one of the first rating algorithms invented in 1947.  
//! Sometimes still used in Xiangqi ("Chinese Chess").
//!
//! Unlike with the other rating systems, with Ingo a lower rating is more desirable,
//! and negative values are possible, though unlikely.  
//! A player with an Ingo rating of 0 has an equivalent Elo rating of 2840, and an Ingo rating of -1 equals 2848 Elo.
//!
//! # Quickstart
//!
//! This is the most basic example on how to use the Ingo Module.  
//! Please take a look at the functions below to see more advanced use cases.
//!
//! ```
//! use skillratings::{
//!     ingo::{ingo, IngoRating},
//!     Outcomes,
//! };
//!
//! // Initialise a new player rating.
//! // We need to set the actual age for the player,
//! // if you are unsure what to set here, choose something that is greater than 25.
//! let player_one = IngoRating::new(19);
//!
//! // Or you can initialise it with your own values of course.
//! // Imagine these numbers being pulled from a database.
//! let (some_rating, some_age) = (150.4, 23);
//! let player_two = IngoRating {
//!     rating: some_rating,
//!     age: some_age,
//! };
//!
//! // The outcome of the match is from the perspective of player one.
//! let outcome = Outcomes::WIN;
//!
//! // The ingo function will calculate the new ratings for both players and return them.
//! let (new_player_one, new_player_two) = ingo(&player_one, &player_two, &outcome);
//! ```
//!
//! # More Information
//!
//! - [Wikipedia Article (German, no english version available)](https://de.wikipedia.org/wiki/Ingo-Zahl)
//! - [Archive of Ingo Ratings (German)](https://www.schachbund.de/ingo-spiegel.html)
//! - [Ingo Rules (German, PDF Download)](https://www.schachbund.de/ingo-spiegel.html?file=files/dsb/historie/ingo-spiegel/Ingo-Regeln.pdf&cid=28120)

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::{elo::EloRating, Outcomes, Rating, RatingPeriodSystem, RatingSystem};

#[derive(Copy, Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// The Ingo rating of a player.
///
/// Note that unlike in the other systems, a lower score is better than a higher score.  
/// Negative values are possible.
///
/// The age is the actual age of the player, if unsure or unavailable set this to `>25`.  
/// Converting from an `EloRating` or using `IngoRating::default()` will set the age to 26.
///
/// The default rating is 230.0.
pub struct IngoRating {
    /// The rating value for a player, by default 230.0.
    /// Note that a lower rating is more desirable.
    pub rating: f64,
    /// The age of the player, if uncertain or unavailable set this to `>25`.
    pub age: usize,
}

impl IngoRating {
    #[must_use]
    /// Initialise a new `IngoRating` with a rating of 230.0 and the given age.  
    /// The age is the actual age of the player, if unsure or unavailable set this to `>25`.
    pub const fn new(age: usize) -> Self {
        Self { rating: 230.0, age }
    }
}

impl Default for IngoRating {
    fn default() -> Self {
        Self::new(26)
    }
}

impl Rating for IngoRating {
    fn rating(&self) -> f64 {
        self.rating
    }
    fn uncertainty(&self) -> Option<f64> {
        None
    }
    fn new(rating: Option<f64>, _uncertainty: Option<f64>) -> Self {
        Self {
            rating: rating.unwrap_or(230.0),
            age: 26,
        }
    }
}

impl From<(f64, usize)> for IngoRating {
    fn from((r, a): (f64, usize)) -> Self {
        Self { rating: r, age: a }
    }
}

// Just in case the age is unknown.
impl From<f64> for IngoRating {
    fn from(r: f64) -> Self {
        Self { rating: r, age: 26 }
    }
}

impl From<EloRating> for IngoRating {
    fn from(e: EloRating) -> Self {
        Self {
            rating: 355.0 - (e.rating / 8.0),
            ..Default::default()
        }
    }
}

/// Struct to calculate ratings and expected score for [`IngoRating`]
pub struct Ingo {}

impl RatingSystem for Ingo {
    type RATING = IngoRating;
    // No need for a config here.
    type CONFIG = ();

    fn new(_config: Self::CONFIG) -> Self {
        Self {}
    }

    fn rate(
        &self,
        player_one: &IngoRating,
        player_two: &IngoRating,
        outcome: &Outcomes,
    ) -> (IngoRating, IngoRating) {
        ingo(player_one, player_two, outcome)
    }

    fn expected_score(&self, player_one: &IngoRating, player_two: &IngoRating) -> (f64, f64) {
        expected_score(player_one, player_two)
    }
}

impl RatingPeriodSystem for Ingo {
    type RATING = IngoRating;
    type CONFIG = ();

    fn new(_config: Self::CONFIG) -> Self {
        Self {}
    }

    fn rate(&self, player: &IngoRating, results: &[(IngoRating, Outcomes)]) -> IngoRating {
        ingo_rating_period(player, results)
    }
}

#[must_use]
/// Calculates the [`IngoRating`]s of two players based on their ratings, and the outcome of the game.
///
/// Takes in two players as [`IngoRating`]s, and an [`Outcome`](Outcomes).
///
/// Instead of the traditional way of calculating the Ingo for only one player only using a list of results,
/// we are calculating the Ingo rating for two players at once, like in the Elo calculation,
/// to make it easier to see instant results.
///
/// The outcome of the match is in the perspective of `player_one`.
/// This means [`Outcomes::WIN`] is a win for `player_one` and [`Outcomes::LOSS`] is a win for `player_two`.
///
/// A lower Ingo rating is more desirable, and while negative values are possible,
/// a player with an Ingo rating of 0 has an equivalent Elo rating of 2840.
///
/// # Examples
/// ```
/// use skillratings::{
///     ingo::{ingo, IngoRating},
///     Outcomes,
/// };
///
/// let player_one = IngoRating {
///     rating: 130.0,
///     age: 40,
/// };
/// let player_two = IngoRating {
///     rating: 160.0,
///     age: 40,
/// };
///
/// let (new_one, new_two) = ingo(&player_one, &player_two, &Outcomes::WIN);
///
/// assert!((new_one.rating.round() - 129.0).abs() < f64::EPSILON);
/// assert!((new_two.rating.round() - 161.0).abs() < f64::EPSILON);
/// ```
pub fn ingo(
    player_one: &IngoRating,
    player_two: &IngoRating,
    outcome: &Outcomes,
) -> (IngoRating, IngoRating) {
    let outcome1 = outcome.to_chess_points();
    let outcome2 = 1.0 - outcome1;

    let perf1 = performance(player_two.rating, outcome1);
    let perf2 = performance(player_one.rating, outcome2);

    let development1 = age_to_devcoefficent(player_one.age);
    let development2 = age_to_devcoefficent(player_two.age);

    let new_rating1 = perf1.mul_add(1.0, player_one.rating * development1) / (1.0 + development1);
    let new_rating2 = perf2.mul_add(1.0, player_two.rating * development2) / (1.0 + development2);

    (
        IngoRating {
            rating: new_rating1,
            age: player_one.age,
        },
        IngoRating {
            rating: new_rating2,
            age: player_two.age,
        },
    )
}

#[must_use]
/// The "traditional" way of calculating a [`IngoRating`] of a player in a rating period.
///
/// Takes in a player as an [`IngoRating`] and their results as a Slice of tuples containing the opponent as an [`IngoRating`],
/// and the outcome of the game as an [`Outcome`](Outcomes)
///
/// The outcome of the match is in the perspective of the player.
/// This means [`Outcomes::WIN`] is a win for the player and [`Outcomes::LOSS`] is a win for the opponent.
///
/// # Examples
/// ```
/// use skillratings::{
///     ingo::{ingo_rating_period, IngoRating},
///     Outcomes,
/// };
///
/// let player = IngoRating {
///     rating: 130.0,
///     age: 40,
/// };
///
/// let opponent1 = IngoRating {
///     rating: 160.0,
///     age: 40,
/// };
///
/// let opponent2 = IngoRating {
///     rating: 160.0,
///     age: 40,
/// };
///
/// let opponent3 = IngoRating {
///     rating: 55.0,
///     age: 40,
/// };
///
/// let opponent4 = IngoRating {
///     rating: 90.0,
///     age: 40,
/// };
///
/// let results = vec![
///     (opponent1, Outcomes::WIN),
///     (opponent2, Outcomes::DRAW),
///     (opponent3, Outcomes::WIN),
///     (opponent4, Outcomes::LOSS),
/// ];
///
/// let new_player = ingo_rating_period(&player, &results);
///
/// assert!((new_player.rating.round() - 126.0).abs() < f64::EPSILON);
/// ```
pub fn ingo_rating_period(player: &IngoRating, results: &[(IngoRating, Outcomes)]) -> IngoRating {
    // Ingo was meant to be used in tournaments, so we do not need to loop over the opponents here.
    let development = age_to_devcoefficent(player.age);

    let average_points =
        results.iter().map(|r| r.1.to_chess_points()).sum::<f64>() / results.len() as f64;

    let average_opponent_rating =
        results.iter().map(|r| r.0.rating).sum::<f64>() / results.len() as f64;

    let performance = performance(average_opponent_rating, average_points);

    let new_rating = performance.mul_add(results.len() as f64, player.rating * development)
        / (results.len() as f64 + development);

    IngoRating {
        rating: new_rating,
        age: player.age,
    }
}

#[must_use]
/// Calculates the expected outcome of two players based on Ingo.
///
/// Takes in two players as [`IngoRating`]s and returns the probability of victory for each player as an [`f64`] between 1.0 and 0.0.  
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// # Examples
/// ```
/// use skillratings::{
///     ingo::{expected_score, IngoRating},
///     Outcomes,
/// };
///
/// let player_one = IngoRating {
///     rating: 130.0,
///     age: 40,
/// };
/// let player_two = IngoRating {
///     rating: 160.0,
///     age: 40,
/// };
///
/// let (exp1, exp2) = expected_score(&player_one, &player_two);
///
/// assert!(((exp1 * 100.0).round() - 80.0).abs() < f64::EPSILON);
/// assert!(((exp2 * 100.0).round() - 20.0).abs() < f64::EPSILON);
///
/// assert!((exp1 + exp2 - 1.0).abs() < f64::EPSILON);
/// ```
pub fn expected_score(player_one: &IngoRating, player_two: &IngoRating) -> (f64, f64) {
    let exp_one = 0.5 + (player_two.rating - player_one.rating) / 100.0;
    let exp_two = 1.0 - exp_one;

    (exp_one, exp_two)
}

fn performance(average_rating: f64, score: f64) -> f64 {
    average_rating - 100.0f64.mul_add(score, -50.0)
}

/// Similar to the DWZ algorithm, we use the age of the player to get the development coefficient.
const fn age_to_devcoefficent(age: usize) -> f64 {
    match age {
        0..=20 => 10.0,
        21..=25 => 15.0,
        _ => 20.0,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ingo() {
        let player_one = IngoRating {
            rating: 130.0,
            age: 40,
        };
        let player_two = IngoRating {
            rating: 160.0,
            age: 40,
        };

        let (p1, p2) = ingo(&player_one, &player_two, &Outcomes::WIN);

        assert!((p1.rating.round() - 129.0).abs() < f64::EPSILON);
        assert!((p2.rating.round() - 161.0).abs() < f64::EPSILON);

        let (p1, p2) = ingo(&player_one, &player_two, &Outcomes::LOSS);

        assert!((p1.rating.round() - 134.0).abs() < f64::EPSILON);
        assert!((p2.rating.round() - 156.0).abs() < f64::EPSILON);

        let young_player = IngoRating {
            rating: 230.0,
            age: 7,
        };

        let old_player = IngoRating {
            rating: 109.0,
            age: 78,
        };

        let (yp, op) = ingo(&young_player, &old_player, &Outcomes::DRAW);

        assert!((yp.rating.round() - 219.0).abs() < f64::EPSILON);
        assert!((op.rating.round() - 115.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_ingo_rating_period() {
        let player_one = IngoRating {
            rating: 130.0,
            age: 22,
        };
        let player_two = IngoRating {
            rating: 160.0,
            age: 40,
        };

        let results = vec![(player_two, Outcomes::WIN)];

        let p1 = ingo_rating_period(&player_one, &results);

        assert!((p1.rating.round() - 129.0).abs() < f64::EPSILON);

        let player_two = IngoRating {
            rating: 160.0,
            age: 40,
        };

        let player_three = IngoRating {
            rating: 160.0,
            age: 40,
        };

        let player_four = IngoRating {
            rating: 55.0,
            age: 22,
        };

        let player_five = IngoRating {
            rating: 90.0,
            age: 40,
        };

        let results = vec![
            (player_two, Outcomes::WIN),
            (player_three, Outcomes::DRAW),
            (player_four, Outcomes::WIN),
            (player_five, Outcomes::LOSS),
        ];

        let p1 = ingo_rating_period(&player_one, &results);

        assert!((p1.rating.round() - 124.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_expected_score() {
        let player_one = IngoRating {
            rating: 130.0,
            age: 40,
        };
        let player_two = IngoRating {
            rating: 160.0,
            age: 40,
        };

        let (exp1, exp2) = expected_score(&player_one, &player_two);

        assert!(((exp1 * 100.0).round() - 80.0).abs() < f64::EPSILON);
        assert!(((exp2 * 100.0).round() - 20.0).abs() < f64::EPSILON);

        assert!((exp1 + exp2 - 1.0).abs() < f64::EPSILON);
    }

    #[test]
    fn ingo_elo_conv() {
        let elo_player = EloRating::new();

        let ingo_player = IngoRating::from(elo_player);

        assert!((ingo_player.rating - 230.0).abs() < f64::EPSILON);
        assert_eq!(ingo_player.age, 26);

        let ingo_player = IngoRating::default();

        let elo_player = EloRating::from(ingo_player);

        assert!((elo_player.rating - 1000.0).abs() < f64::EPSILON);
    }

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn test_misc_stuff() {
        let player_one = IngoRating::new(14);

        assert_eq!(player_one, player_one.clone());

        assert!(!format!("{player_one:?}").is_empty());

        assert_eq!(IngoRating::from((222.0, 26)), IngoRating::from(222.0));
    }

    #[test]
    fn test_traits() {
        let player_one: IngoRating = Rating::new(Some(240.0), Some(90.0));
        let player_two: IngoRating = Rating::new(Some(240.0), Some(90.0));

        let rating_system: Ingo = RatingSystem::new(());

        assert!((player_one.rating() - 240.0).abs() < f64::EPSILON);
        assert_eq!(player_one.uncertainty(), None);

        let (new_player_one, new_player_two) =
            RatingSystem::rate(&rating_system, &player_one, &player_two, &Outcomes::WIN);

        let (exp1, exp2) = RatingSystem::expected_score(&rating_system, &player_one, &player_two);

        assert!((new_player_one.rating - 237.619_047_619_047_62).abs() < f64::EPSILON);
        assert!((new_player_two.rating - 242.380_952_380_952_38).abs() < f64::EPSILON);
        assert!((exp1 - 0.5).abs() < f64::EPSILON);
        assert!((exp2 - 0.5).abs() < f64::EPSILON);

        let player_one: IngoRating = Rating::new(Some(240.0), Some(90.0));
        let player_two: IngoRating = Rating::new(Some(240.0), Some(90.0));

        let rating_period: Ingo = RatingPeriodSystem::new(());

        let new_player_one =
            RatingPeriodSystem::rate(&rating_period, &player_one, &[(player_two, Outcomes::WIN)]);

        assert!((new_player_one.rating - 237.619_047_619_047_62).abs() < f64::EPSILON);
    }
}
