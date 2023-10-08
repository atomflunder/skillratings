//! The DWZ (Deutsche Wertungszahl) algorithm used in the german chess leagues alongside Elo.  
//! DWZ continues to be enhanced over the years, while having similar scores to Elo.
//!
//! DWZ allows young players to rise and fall in the ranks more quickly, while more experienced players ratings are slower to change.  
//! Overachieving players gain more rating while under-performing weak players do not lose rating points as quickly.
//!
//! These factors make DWZ more dynamic than Elo while producing accurate ratings more quickly.
//!
//! # Quickstart
//!
//! This is the most basic example on how to use the DWZ Module.  
//! Please take a look at the functions below to see more advanced use cases.
//!
//! ```
//! use skillratings::{
//!     dwz::{dwz, DWZRating},
//!     Outcomes,
//! };
//!
//! // Initialise a new player rating.
//! // We need to set the actual age for the player,
//! // if you are unsure what to set here, choose something that is greater than 25.
//! let player_one = DWZRating::new(19);
//!
//! // Or you can initialise it with your own values of course.
//! // Imagine these numbers being pulled from a database.
//! // The default rating is 1000, and the index denotes the amount of tournaments played.
//! let (some_rating, some_index, some_age) = (1325.0, 51, 27);
//! let player_two = DWZRating {
//!     rating: some_rating,
//!     index: some_index,
//!     age: some_age,
//! };
//!
//! // The outcome of the match is from the perspective of player one.
//! let outcome = Outcomes::WIN;
//!
//! // The dwz function will calculate the new ratings for both players and return them.
//! let (new_player_one, new_player_two) = dwz(&player_one, &player_two, &outcome);
//! ```
//!
//! # More Information
//!
//! - [Wikipedia Article](https://en.wikipedia.org/wiki/Deutsche_Wertungszahl)
//! - [DWZ Calculator (German)](http://www.wertungszahl.de/)
//! - [DWZ Top 100 Ratings](https://www.schachbund.de/top-100.html)
//! - [Official DWZ scoring system rules (German)](https://www.schachbund.de/wertungsordnung.html)
//! - [Probability Table](https://www.schachbund.de/wertungsordnung-anhang-2-tabellen/articles/wertungsordnung-anhang-21-wahrscheinlichkeitstabelle.html)

use std::{collections::HashMap, error::Error, fmt::Display};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::{elo::EloRating, Outcomes, Rating, RatingPeriodSystem, RatingSystem};

#[derive(Copy, Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// The DWZ (Deutsche Wertungszahl) rating for a player.
///
/// The age is the actual age of the player, if unsure or unavailable set this to `>25`.  
/// Converting from an `EloRating` or using `DWZRating::default()` will set the age to 26.
///
/// The default rating is 1000.0.
pub struct DWZRating {
    /// The player's DWZ rating number, by default 1000.0.
    pub rating: f64,
    /// The player's DWZ index, how many "events" they have completed.
    pub index: usize,
    /// The age of the player, if uncertain or unavailable set this to `>25`.
    pub age: usize,
}

impl DWZRating {
    #[must_use]
    /// Initialise a new `DWZRating` with a rating of 1000.0, an index of 1 and the specified age.  
    /// The age is the actual age of the player, if unsure or unavailable set this to `>25`.
    pub const fn new(age: usize) -> Self {
        Self {
            rating: 1000.0,
            index: 1,
            age,
        }
    }
}

impl Default for DWZRating {
    fn default() -> Self {
        Self::new(26)
    }
}

impl Rating for DWZRating {
    fn rating(&self) -> f64 {
        self.rating
    }
    fn uncertainty(&self) -> Option<f64> {
        None
    }
    fn new(rating: Option<f64>, _uncertainty: Option<f64>) -> Self {
        Self {
            rating: rating.unwrap_or(1000.0),
            index: 1,
            age: 26,
        }
    }
}

impl From<(f64, usize, usize)> for DWZRating {
    fn from((r, i, a): (f64, usize, usize)) -> Self {
        Self {
            rating: r,
            index: i,
            age: a,
        }
    }
}

// Just in case the age is unknown.
impl From<(f64, usize)> for DWZRating {
    fn from((r, i): (f64, usize)) -> Self {
        Self {
            rating: r,
            index: i,
            age: 26,
        }
    }
}

impl From<EloRating> for DWZRating {
    fn from(e: EloRating) -> Self {
        Self {
            rating: e.rating,
            // Recommended according to Wikipedia.
            index: 6,
            ..Default::default()
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// The error types that can occur when calculating a new DWZ Rating.  
/// Only gets raised in the [`get_first_dwz`] function.
pub enum GetFirstDWZError {
    /// The player has played less than 5 games.
    NotEnoughGames,
    /// The player has a winrate of 0% or 100%.
    InvalidWinRate,
}

impl Display for GetFirstDWZError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NotEnoughGames => {
                write!(f, "You need at least 5 games to calculate a DWZ Rating.")
            }
            Self::InvalidWinRate => write!(f, "Your winrate cannot be 0% or 100%."),
        }
    }
}

impl Error for GetFirstDWZError {}

/// Struct to calculate ratings and expected score for [`DWZRating`]
pub struct DWZ {}

impl RatingSystem for DWZ {
    type RATING = DWZRating;
    type CONFIG = ();

    fn new(_config: Self::CONFIG) -> Self {
        Self {}
    }

    fn rate(
        &self,
        player_one: &DWZRating,
        player_two: &DWZRating,
        outcome: &Outcomes,
    ) -> (DWZRating, DWZRating) {
        dwz(player_one, player_two, outcome)
    }

    fn expected_score(&self, player_one: &DWZRating, player_two: &DWZRating) -> (f64, f64) {
        expected_score(player_one, player_two)
    }
}

impl RatingPeriodSystem for DWZ {
    type RATING = DWZRating;
    type CONFIG = ();

    fn new(_config: Self::CONFIG) -> Self {
        Self {}
    }

    fn rate(&self, player: &DWZRating, results: &[(DWZRating, Outcomes)]) -> DWZRating {
        dwz_rating_period(player, results)
    }

    fn expected_score(&self, player: &Self::RATING, opponents: &[Self::RATING]) -> Vec<f64> {
        expected_score_rating_period(player, opponents)
    }
}

#[must_use]
/// Calculates new [`DWZRating`] of two players based on their old rating, index, age and outcome of the game.
///
/// Takes in two players as [`DWZRating`]s and an [`Outcome`](Outcomes).
///
/// Instead of the traditional way of calculating the DWZ for only one player only using a list of results,
/// we are calculating the DWZ rating for two players at once, like in the Elo calculation,
/// to make it easier to see instant results.
///
/// For the traditional way for calculating DWZ in a rating period or tournament, please see [`dwz_rating_period`].  
/// To get a first DWZ rating, please see [`get_first_dwz`].
///
/// The outcome of the match is in the perspective of `player_one`.
/// This means [`Outcomes::WIN`] is a win for `player_one` and [`Outcomes::LOSS`] is a win for `player_two`.
///
/// # Examples
/// ```
/// use skillratings::{
///     dwz::{dwz, DWZRating},
///     Outcomes,
/// };
///
/// let player_one = DWZRating {
///     rating: 1500.0,
///     index: 42,
///     age: 42,
/// };
/// let player_two = DWZRating {
///     rating: 1500.0,
///     index: 12,
///     age: 12,
/// };
///
/// let outcome = Outcomes::WIN;
///
/// let (new_one, new_two) = dwz(&player_one, &player_two, &outcome);
///
/// assert!((new_one.rating.round() - 1519.0).abs() < f64::EPSILON);
/// assert_eq!(new_one.index, 43);
///
/// assert!((new_two.rating.round() - 1464.0).abs() < f64::EPSILON);
/// assert_eq!(new_two.index, 13);
/// ```
pub fn dwz(
    player_one: &DWZRating,
    player_two: &DWZRating,
    outcome: &Outcomes,
) -> (DWZRating, DWZRating) {
    let outcome1 = outcome.to_chess_points();
    let outcome2 = 1.0 - outcome1;

    let (exp1, exp2) = expected_score(player_one, player_two);

    let r1 = new_rating(
        player_one.rating,
        e_value(
            player_one.rating,
            player_one.age,
            outcome1,
            exp1,
            player_one.index,
        ),
        outcome1,
        exp1,
        1.0,
    );
    let r2 = new_rating(
        player_two.rating,
        e_value(
            player_two.rating,
            player_two.age,
            outcome2,
            exp2,
            player_two.index,
        ),
        outcome2,
        exp2,
        1.0,
    );

    (
        DWZRating {
            rating: r1,
            index: player_one.index + 1,
            age: player_one.age,
        },
        {
            DWZRating {
                rating: r2,
                index: player_two.index + 1,
                age: player_two.age,
            }
        },
    )
}

#[must_use]
/// The "traditional" way of calculating a DWZ Rating of a player in a rating period or tournament.
///
/// Takes in a player as an [`DWZRating`] and their results as a Slice of tuples containing the opponent as an [`DWZRating`]
/// and the outcome of the game as an [`Outcome`](Outcomes).
///
/// All of the outcomes are from the perspective of the player.
/// This means [`Outcomes::WIN`] is a win for the player and [`Outcomes::LOSS`] is a win for the opponent.
///
/// # Examples
/// ```
/// use skillratings::{
///     dwz::{dwz_rating_period, DWZRating},
///     Outcomes,
/// };
///
/// let player = DWZRating {
///     rating: 1530.0,
///     index: 17,
///     age: 9,
/// };
///
/// let opponent1 = DWZRating {
///     rating: 1930.0,
///     index: 103,
///     age: 39,
/// };
///
/// let opponent2 = DWZRating {
///     rating: 1930.0,
///     index: 92,
///     age: 14,
/// };
///
/// let results = vec![(opponent1, Outcomes::WIN), (opponent2, Outcomes::DRAW)];
///
/// let new_player = dwz_rating_period(&player, &results);
///
/// assert!((new_player.rating.round() - 1635.0).abs() < f64::EPSILON);
/// assert_eq!(new_player.index, 18);
/// ```
pub fn dwz_rating_period(player: &DWZRating, results: &[(DWZRating, Outcomes)]) -> DWZRating {
    let points = results.iter().map(|r| r.1.to_chess_points()).sum();

    let expected_points = results.iter().map(|r| expected_score(player, &r.0).0).sum();

    let new_rating = (800.0
        / (e_value(
            player.rating,
            player.age,
            points,
            expected_points,
            player.index,
        ) + results.len() as f64))
        .mul_add(points - expected_points, player.rating);

    DWZRating {
        rating: new_rating,
        index: player.index + 1,
        age: player.age,
    }
}

#[must_use]
/// Calculates the expected outcome of two players based on DWZ.
///
/// Takes in two players as [`DWZRating`]s and returns the probability of victory for each player as an [`f64`] between 1.0 and 0.0.  
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// # Examples
/// ```
/// use skillratings::dwz::{expected_score, DWZRating};
///
/// let player_one = DWZRating {
///     rating: 1900.0,
///     index: 42,
///     age: 42,
/// };
/// let player_two = DWZRating {
///     rating: 1500.0,
///     index: 12,
///     age: 12,
/// };
///
/// let (exp_one, exp_two) = expected_score(&player_one, &player_two);
///
/// assert!(((exp_one * 100.0).round() - 91.0).abs() < f64::EPSILON);
/// assert!(((exp_two * 100.0).round() - 9.0).abs() < f64::EPSILON);
/// ```
pub fn expected_score(player_one: &DWZRating, player_two: &DWZRating) -> (f64, f64) {
    let exp_one = (1.0
        + 10.0_f64.powf(-(400.0_f64.recip()) * (player_one.rating - player_two.rating)))
    .recip();
    let exp_two = 1.0 - exp_one;

    (exp_one, exp_two)
}

#[must_use]
/// Calculates the expected outcome of a player in a rating period or tournament.
///
/// Takes in a players as [`DWZRating`] and a list of opponents as a slice of [`DWZRating`]
/// and returns the probability of victory for each match as an Vec of [`f64`] between 1.0 and 0.0 from the perspective of the player.  
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// # Examples
/// ```
/// use skillratings::dwz::{expected_score_rating_period, DWZRating};
///
/// let player = DWZRating {
///     rating: 1900.0,
///     index: 42,
///     age: 42,
/// };
///
/// let opponent1 = DWZRating {
///     rating: 1930.0,
///     index: 103,
///     age: 39,
/// };
///
/// let opponent2 = DWZRating {
///     rating: 1730.0,
///     index: 92,
///     age: 14,
/// };
///
/// let exp = expected_score_rating_period(&player, &[opponent1, opponent2]);
///
/// assert_eq!((exp[0] * 100.0).round(), 46.0);
/// assert_eq!((exp[1] * 100.0).round(), 73.0);
/// ```
pub fn expected_score_rating_period(player: &DWZRating, opponents: &[DWZRating]) -> Vec<f64> {
    opponents
        .iter()
        .map(|o| (1.0 + 10.0_f64.powf(-(400.0_f64.recip()) * (player.rating - o.rating))).recip())
        .collect()
}

/// Gets a proper first [`DWZRating`].
///
/// In the case that you do not have enough opponents to rate a player against,
/// consider using [`DWZRating::from()`](DWZRating) if you have an [`EloRating`](crate::elo::EloRating)
/// or [`DWZRating::new()`](DWZRating) if not.
///
/// Takes in the player's age and their results as a Slice of tuples containing the opponent and the outcome.
/// If the actual player's age is unavailable or unknown, choose something `>25`.
///
///
/// # Errors
///
/// This function returns [`GetFirstDWZError::NotEnoughGames`] if the player has played less than 5 games,
/// or [`GetFirstDWZError::InvalidWinRate`] if the player has a winrate of either 0% or 100%.
///
/// # Examples
/// ```
/// use skillratings::{
///     dwz::{get_first_dwz, DWZRating},
///     Outcomes,
/// };
///
/// let opponent1 = DWZRating {
///     rating: 1300.0,
///     index: 23,
///     age: 17,
/// };
/// let opponent2 = DWZRating {
///     rating: 1540.0,
///     index: 2,
///     age: 29,
/// };
/// let opponent3 = DWZRating {
///     rating: 1200.0,
///     index: 10,
///     age: 7,
/// };
/// let opponent4 = DWZRating {
///     rating: 1290.0,
///     index: 76,
///     age: 55,
/// };
/// let opponent5 = DWZRating {
///     rating: 1400.0,
///     index: 103,
///     age: 11,
/// };
///
/// let player = get_first_dwz(
///     26,
///     &vec![
///         (opponent1, Outcomes::WIN),
///         (opponent2, Outcomes::DRAW),
///         (opponent3, Outcomes::LOSS),
///         (opponent4, Outcomes::WIN),
///         (opponent5, Outcomes::WIN),
///     ],
/// )
/// .unwrap();
///
/// assert!((player.rating - 1491.0).abs() < f64::EPSILON);
/// assert_eq!(player.index, 1);
/// ```
pub fn get_first_dwz(
    player_age: usize,
    results: &[(DWZRating, Outcomes)],
) -> Result<DWZRating, GetFirstDWZError> {
    if results.len() < 5 {
        return Err(GetFirstDWZError::NotEnoughGames);
    }

    let points: f64 = results.iter().map(|r| r.1.to_chess_points()).sum();

    // If you have a 100% or 0% win rate, we return None.
    if (points - results.len() as f64).abs() < f64::EPSILON || points == 0.0 {
        return Err(GetFirstDWZError::InvalidWinRate);
    }

    let average_rating = results.iter().map(|r| r.0.rating).sum::<f64>() / results.len() as f64;

    // We round the f64 before casting to i64, so this lint is unnecessary here.
    #[allow(clippy::cast_possible_truncation)]
    let p = ((points / results.len() as f64) * 100.0).round() as i64;

    // We need to look up the points value in a lookup table:
    // https://www.schachbund.de/wertungsordnung-anhang-2-tabellen/articles/wertungsordnung-anhang-21-wahrscheinlichkeitstabelle.html
    // There seems to be no real way to solve this in a better way, sorry.
    // At least we only need one half of this table.
    let probability_table = HashMap::from([
        (0, -728.),
        (1, -614.),
        (2, -555.),
        (3, -513.),
        (4, -480.),
        (5, -453.),
        (6, -429.),
        (7, -408.),
        (8, -389.),
        (9, -371.),
        (10, -355.),
        (11, -340.),
        (12, -326.),
        (13, -312.),
        (14, -300.),
        (15, -288.),
        (16, -276.),
        (17, -265.),
        (18, -254.),
        (19, -244.),
        (20, -234.),
        (21, -224.),
        (22, -214.),
        (23, -205.),
        (24, -196.),
        (25, -187.),
        (26, -178.),
        (27, -170.),
        (28, -161.),
        (29, -153.),
        (30, -145.),
        (31, -137.),
        (32, -129.),
        (33, -121.),
        (34, -113.),
        (35, -106.),
        (36, -98.),
        (37, -91.),
        (38, -83.),
        (39, -76.),
        (40, -69.),
        (41, -61.),
        (42, -54.),
        (43, -47.),
        (44, -40.),
        (45, -32.),
        (46, -25.),
        (47, -18.),
        (48, -11.),
        (49, -4.),
        (50, -0.),
    ]);

    let mut new_rating = if p > 50 {
        // If the performance is positive, we convert the values above to a positive number.
        // The value for 30 is the same as for 70, but negative.
        let temp = probability_table.get(&(p - 100).abs()).unwrap_or(&0.);

        f64::abs(*temp) + average_rating
    } else {
        // Else we just use the negative number above.
        probability_table.get(&p).unwrap_or(&0.) + average_rating
    };

    // If the rating would be too low we revise it upwards.
    if new_rating <= 800.0 {
        new_rating = 700.0 + (new_rating / 8.0);
    }

    Ok(DWZRating {
        rating: new_rating,
        index: 1,
        age: player_age,
    })
}

fn e_value(rating: f64, age: usize, score: f64, expected_score: f64, index: usize) -> f64 {
    // The variable j is dependent on the age of the player. From wikipedia:
    // "Teenagers up to 20 years: `j = 5.0`, junior adults (21 – 25 years): `j = 10.0`, over-25-year-old's: `j = 15.0`"
    let j = match age {
        0..=20 => 5.0,
        21..=25 => 10.0,
        _ => 15.0,
    };

    // The base value of the development coefficient.
    let e0 = (rating / 1000.0).powi(4) + j;

    // The acceleration factor allows young, over-achieving players to gain rating more quickly.
    let a = if age < 20 && score >= expected_score {
        rating / 2000.0
    } else {
        1.0
    };

    // The breaking value is applied to weak players that under-perform in order to not decrease in rating too rapidly.
    let b = if rating < 1300.0 && score <= expected_score {
        ((1300.0 - rating) / 150.0_f64).exp_m1()
    } else {
        0.0
    };

    // The development coefficient combines the acceleration and breaking values.
    // It also depends on the number of entered tournaments (index).
    let mut e = a.mul_add(e0, b);

    if e <= 5.0 {
        e = 5.0;
    } else if b == 0.0 {
        if e >= 30.0_f64.min(5.0 * index as f64) {
            e = 30.0_f64.min(5.0 * index as f64);
        }
    } else if e >= 150.0 {
        e = 150.0;
    }

    e
}

fn new_rating(
    old_rating: f64,
    e: f64,
    score: f64,
    expected_score: f64,
    matches_played: f64,
) -> f64 {
    (800.0 / (e + matches_played)).mul_add(score - expected_score, old_rating)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    /// The results here have been double-checked with the unofficial DWZ Calculator: <http://www.wertungszahl.de/>.
    fn test_dwz() {
        let mut player_one = DWZRating {
            rating: 1530.0,
            index: 22,
            age: 26,
        };

        let mut player_two = DWZRating {
            rating: 1930.0,
            index: 103,
            age: 39,
        };

        (player_one, player_two) = dwz(&player_one, &player_two, &Outcomes::WIN);

        assert!((player_one.rating.round() - 1564.0).abs() < f64::EPSILON);
        assert_eq!(player_one.index, 23);

        assert!((player_two.rating.round() - 1906.0).abs() < f64::EPSILON);
        assert_eq!(player_two.index, 104);

        (player_one, player_two) = dwz(&player_one, &player_two, &Outcomes::DRAW);

        assert!((player_one.rating.round() - 1578.0).abs() < f64::EPSILON);
        assert_eq!(player_one.index, 24);

        assert!((player_two.rating.round() - 1895.0).abs() < f64::EPSILON);
        assert_eq!(player_two.index, 105);

        player_two.age = 12;

        (player_one, player_two) = dwz(&player_one, &player_two, &Outcomes::LOSS);

        assert!((player_one.rating.round() - 1573.0).abs() < f64::EPSILON);
        assert_eq!(player_one.index, 25);

        assert!((player_two.rating.round() - 1901.0).abs() < f64::EPSILON);
        assert_eq!(player_two.index, 106);
    }

    #[test]
    fn test_dwz_rating_period() {
        let player = DWZRating {
            rating: 1530.0,
            index: 17,
            age: 9,
        };

        let opponent1 = DWZRating {
            rating: 1930.0,
            index: 103,
            age: 39,
        };

        let opponent2 = DWZRating {
            rating: 1930.0,
            index: 92,
            age: 14,
        };

        let results = vec![
            (opponent1, Outcomes::WIN),
            (opponent2, Outcomes::DRAW),
            (opponent1, Outcomes::LOSS),
        ];

        let new_player = dwz_rating_period(&player, &results);

        assert!((new_player.rating.round() - 1619.0).abs() < f64::EPSILON);
        assert_eq!(new_player.index, 18);
    }

    #[test]
    fn test_large_delta() {
        let mut really_good_player = DWZRating {
            rating: 3210.0,
            index: 143,
            age: 25,
        };

        let mut really_bad_player = DWZRating {
            rating: 90.0,
            index: 1,
            age: 12,
        };

        (really_good_player, really_bad_player) =
            dwz(&really_good_player, &really_bad_player, &Outcomes::WIN);

        assert!((really_good_player.rating.round() - 3210.0).abs() < f64::EPSILON);
        assert_eq!(really_good_player.index, 144);

        assert!((really_bad_player.rating.round() - 90.0).abs() < f64::EPSILON);
        assert_eq!(really_bad_player.index, 2);

        really_bad_player.rating = 1.0;
        really_good_player.rating = 32_477_324_874_238.0;

        (really_good_player, really_bad_player) =
            dwz(&really_good_player, &really_bad_player, &Outcomes::WIN);

        assert!((really_good_player.rating.round() - 32_477_324_874_238.0).abs() < f64::EPSILON);

        assert!((really_bad_player.rating.round() - 1.0).abs() < f64::EPSILON);

        really_good_player.rating = 2.0;
        really_good_player.age = 5;

        really_bad_player.rating = 1.0;
        really_bad_player.age = 5;

        (really_good_player, really_bad_player) =
            dwz(&really_good_player, &really_bad_player, &Outcomes::LOSS);

        assert!((really_good_player.rating.round() + 1.0).abs() < f64::EPSILON);
        assert!((really_bad_player.rating.round() - 68.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_expected_score() {
        let player_one = DWZRating {
            rating: 1530.0,
            index: 22,
            age: 26,
        };

        let player_two = DWZRating {
            rating: 1930.0,
            index: 103,
            age: 39,
        };

        let (exp1, exp2) = expected_score(&player_one, &player_two);

        assert!(((exp1 * 100.0).round() - 9.0).abs() < f64::EPSILON);
        assert!(((exp2 * 100.0).round() - 91.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_first_dwz() {
        let o1 = DWZRating {
            rating: 1300.0,
            index: 23,
            age: 17,
        };

        let o2 = DWZRating {
            rating: 1540.0,
            index: 2,
            age: 29,
        };

        let o3 = DWZRating {
            rating: 1200.0,
            index: 10,
            age: 7,
        };

        let o4 = DWZRating {
            rating: 1290.0,
            index: 76,
            age: 55,
        };

        let o5 = DWZRating {
            rating: 1400.0,
            index: 103,
            age: 11,
        };

        #[allow(clippy::unwrap_used)]
        let player = get_first_dwz(
            26,
            &[
                (o1, Outcomes::WIN),
                (o2, Outcomes::DRAW),
                (o3, Outcomes::LOSS),
                (o4, Outcomes::WIN),
                (o5, Outcomes::WIN),
            ],
        )
        .unwrap();

        assert!((player.rating - 1491.0).abs() < f64::EPSILON);
        assert_eq!(player.index, 1);

        let all_win_player = get_first_dwz(
            17,
            &[
                (o1, Outcomes::WIN),
                (o2, Outcomes::WIN),
                (o3, Outcomes::WIN),
                (o4, Outcomes::WIN),
                (o5, Outcomes::WIN),
            ],
        );

        assert_eq!(all_win_player, Err(GetFirstDWZError::InvalidWinRate));

        let all_lose_player = get_first_dwz(
            17,
            &[
                (o1, Outcomes::LOSS),
                (o2, Outcomes::LOSS),
                (o3, Outcomes::LOSS),
                (o4, Outcomes::LOSS),
                (o5, Outcomes::LOSS),
            ],
        );

        assert_eq!(all_lose_player, Err(GetFirstDWZError::InvalidWinRate));

        let less_than_5 = get_first_dwz(
            32,
            &[
                (o1, Outcomes::LOSS),
                (o2, Outcomes::WIN),
                (o3, Outcomes::DRAW),
                (o4, Outcomes::LOSS),
            ],
        );

        assert_eq!(less_than_5, Err(GetFirstDWZError::NotEnoughGames));
    }

    #[test]
    fn test_new_dwz_bad_players() {
        let o1 = DWZRating {
            rating: 1300.0,
            index: 23,
            age: 17,
        };

        let o2 = DWZRating {
            rating: 1540.0,
            index: 2,
            age: 29,
        };

        let o3 = DWZRating {
            rating: 1200.0,
            index: 10,
            age: 7,
        };

        let o4 = DWZRating {
            rating: 1290.0,
            index: 76,
            age: 55,
        };

        let o5 = DWZRating {
            rating: 1400.0,
            index: 103,
            age: 11,
        };

        #[allow(clippy::unwrap_used)]
        let bad_player = get_first_dwz(
            26,
            &[
                (o1, Outcomes::LOSS),
                (o2, Outcomes::DRAW),
                (o3, Outcomes::LOSS),
                (o4, Outcomes::LOSS),
                (o5, Outcomes::LOSS),
            ],
        )
        .unwrap();

        assert!((bad_player.rating.round() - 991.0).abs() < f64::EPSILON);
        assert_eq!(bad_player.index, 1);

        let o4 = DWZRating {
            rating: 430.0,
            index: 76,
            age: 55,
        };

        let o5 = DWZRating {
            rating: 520.0,
            index: 103,
            age: 11,
        };

        #[allow(clippy::unwrap_used)]
        let really_bad_player = get_first_dwz(
            26,
            &vec![
                (o1, Outcomes::LOSS),
                (o2, Outcomes::DRAW),
                (o3, Outcomes::LOSS),
                (o4, Outcomes::LOSS),
                (o5, Outcomes::LOSS),
                (o3, Outcomes::LOSS),
                (o4, Outcomes::LOSS),
                (o5, Outcomes::LOSS),
                (o3, Outcomes::LOSS),
                (o4, Outcomes::LOSS),
                (o5, Outcomes::LOSS),
                (o4, Outcomes::LOSS),
                (o5, Outcomes::LOSS),
                (o4, Outcomes::LOSS),
                (o5, Outcomes::LOSS),
                (o4, Outcomes::LOSS),
                (o5, Outcomes::LOSS),
                (o4, Outcomes::LOSS),
                (o5, Outcomes::LOSS),
            ],
        )
        .unwrap();

        assert!((really_bad_player.rating.round() - 722.0).abs() < f64::EPSILON);
        assert_eq!(really_bad_player.index, 1);
    }

    #[test]
    fn elo_conversion() {
        let player_one = EloRating { rating: 1200.0 };

        let player_one_dwz = DWZRating::from(player_one);

        assert!((player_one_dwz.rating.round() - 1200.0).abs() < f64::EPSILON);
        assert_eq!(player_one_dwz.index, 6);
        assert_eq!(player_one_dwz.age, 26);

        let player_one_back = EloRating::from(player_one_dwz);

        assert!((player_one_back.rating.round() - 1200.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_misc_stuff() {
        let player_one = DWZRating::default();

        let player_two = DWZRating::new(26);

        assert_eq!(player_one, player_two);

        assert_eq!(player_one, player_one.clone());
        assert!(!format!("{player_one:?}").is_empty());

        assert_eq!(
            DWZRating::from((1400.0, 20)),
            DWZRating::from((1400.0, 20, 26))
        );

        assert!(!format!("{:?}", GetFirstDWZError::NotEnoughGames).is_empty());
        assert!(!format!("{:?}", GetFirstDWZError::InvalidWinRate).is_empty());

        assert!(!format!("{}", GetFirstDWZError::NotEnoughGames).is_empty());
        assert!(!format!("{}", GetFirstDWZError::InvalidWinRate).is_empty());

        assert_eq!(
            GetFirstDWZError::NotEnoughGames,
            GetFirstDWZError::NotEnoughGames.clone()
        );
    }

    #[test]
    fn test_traits() {
        let player_one: DWZRating = Rating::new(Some(240.0), Some(90.0));
        let player_two: DWZRating = Rating::new(Some(240.0), Some(90.0));

        let rating_system: DWZ = RatingSystem::new(());

        assert!((player_one.rating() - 240.0).abs() < f64::EPSILON);
        assert_eq!(player_one.uncertainty(), None);

        let (new_player_one, new_player_two) =
            RatingSystem::rate(&rating_system, &player_one, &player_two, &Outcomes::WIN);

        let (exp1, exp2) = RatingSystem::expected_score(&rating_system, &player_one, &player_two);

        assert!((new_player_one.rating - 306.666_666_666_666_7).abs() < f64::EPSILON);
        assert!((new_player_two.rating - 237.350_993_377_483_43).abs() < f64::EPSILON);
        assert!((exp1 - 0.5).abs() < f64::EPSILON);
        assert!((exp2 - 0.5).abs() < f64::EPSILON);

        let rating_period_system: DWZ = RatingPeriodSystem::new(());
        let exp_rp =
            RatingPeriodSystem::expected_score(&rating_period_system, &player_one, &[player_two]);
        assert!((exp1 - exp_rp[0]).abs() < f64::EPSILON);

        let player_one: DWZRating = Rating::new(Some(240.0), Some(90.0));
        let player_two: DWZRating = Rating::new(Some(240.0), Some(90.0));

        let rating_period: DWZ = RatingPeriodSystem::new(());

        let new_player_one =
            RatingPeriodSystem::rate(&rating_period, &player_one, &[(player_two, Outcomes::WIN)]);

        assert!((new_player_one.rating - 306.666_666_666_666_7).abs() < f64::EPSILON);
    }
}
