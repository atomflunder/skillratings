//! The FIFA Men's rating algorithm, officially called *The FIFA/Coca-Cola World Ranking*.  
//! Used to rank men's national football (or soccer) teams in FIFA-recognised competitions since 2018.
//!
//! The algorithm is largely based on the Elo rating system, but with a few modifications specific to football (soccer).  
//! The main differences being an importance factor, that rates the importance of a given match,
//! and a penalty-shootout factor that accounts for the fact that penalty shootouts are often more random than regular play.
//!
//! Please note that the FIFA Women's ranking algorithm works in a very different way, and is not implemented in this crate.
//! This is due to the way the outcome and score of the match is taken into account in the calculations.  
//! For more information please see the [Wikipedia Article on FIFA Women's World Rankings](https://en.wikipedia.org/wiki/FIFA_Women%27s_World_Rankings).
//!
//! # Quickstart
//!
//! ```
//! use skillratings::{
//!     fifa::{fifa, FifaConfig, FifaRating},
//!     Outcomes,
//! };
//!
//! // Initialise a new team rating.
//! let team_one = FifaRating::new();
//!
//! // Or you can initialise it with your own values of course.
//! // Imagine these numbers being pulled from a database.
//! let some_rating = 1325.0;
//! let team_two = FifaRating {
//!     rating: some_rating,
//! };
//!
//! // The outcome of the match is from the perspective of team one.
//! let outcome = Outcomes::WIN;
//!
//! // The config allows you to specify certain values in the Fifa calculation.
//! // Here we set the importance factor to 50.0, and the knockout factor to false.
//! // Which corresponds to a World Cup Group Stage match.
//! let config = FifaConfig {
//!     importance: 50.0,
//!     knockout: false,
//!     ..Default::default()
//! };
//!
//! // The fifa function will calculate the new ratings for both teams and return them.
//! let (new_team_one, new_team_two) = fifa(&team_one, &team_two, &outcome, &config);
//! ```
//!
//! # More Information:
//! - [Original Paper for FIFA Men's Ratings (PDF)](https://digitalhub.fifa.com/m/f99da4f73212220/original/edbm045h0udbwkqew35a-pdf.pdf)
//! - [Wikipedia Article for FIFA Men's Ratings](https://en.wikipedia.org/wiki/FIFA_Men%27s_World_Ranking#Current_calculation_method)
//! - [Official FIFA Men's Ratings](https://www.fifa.com/fifa-world-ranking)
//! - [FIFA ranking calculator](https://football-ranking.com/fifa_world_rankings_calculate)

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::{elo::EloRating, Outcomes, Rating, RatingPeriodSystem, RatingSystem};

/// The Fifa rating of a team.
///
/// The default rating is 1000.0.
#[derive(Copy, Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct FifaRating {
    /// The teams Fifa rating number, by default 1000.0.
    pub rating: f64,
}

impl FifaRating {
    /// Initialise a new `FifaRating` with a rating of 1000.0.
    #[must_use]
    pub const fn new() -> Self {
        Self { rating: 1000.0 }
    }
}

impl Default for FifaRating {
    fn default() -> Self {
        Self::new()
    }
}

impl Rating for FifaRating {
    fn rating(&self) -> f64 {
        self.rating
    }
    fn uncertainty(&self) -> f64 {
        0.0
    }
    fn new(rating: Option<f64>, _uncertainty: Option<f64>) -> Self {
        Self {
            rating: rating.unwrap_or(1000.0),
        }
    }
}

impl From<f64> for FifaRating {
    fn from(r: f64) -> Self {
        Self { rating: r }
    }
}

impl From<EloRating> for FifaRating {
    fn from(e: EloRating) -> Self {
        Self { rating: e.rating }
    }
}

#[derive(Clone, Copy, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// Constants used in the Fifa calculations.
pub struct FifaConfig {
    /// The importance factor of a given match.
    ///
    /// According to Fifa, here are the suggested values:
    ///
    /// - `5.0`: Friendly matches played outside of International Match Calendar windows
    /// - `10.0`: Friendly matches played during International Match Calendar windows
    /// - `15.0`: Group phase matches of Nations League competitions
    /// - `25.0`: Play-off and final matches of Nations League competitions
    /// - `25.0`: Qualification matches for Confederations final competitions and for FIFA World Cup final competitions
    /// - `35.0`: Confederation final competition matches up until the Quarter-Final stage
    /// - `40.0`: Confederation final competition matches from the Quarter-Final stage onwards; all FIFA Confederations Cup matches
    /// - `50.0`: FIFA World Cup final competition matches up until Quarter-Final stage
    /// - `60.0`: FIFA World Cup final competition matches from Quarter-Final stage onwards
    ///
    /// The higher the importance factor, the bigger the rating changes will be.
    ///
    /// Here, the default value is 10.0, which corresponds to a friendly match.
    pub importance: f64,
    /// This value determines if the match played was a knock-out match of a final competition.
    ///
    /// If set to true, teams cannot lose rating points,
    /// in order to protect the point totals of the teams that reached the knock-out stages of a tournament.
    ///
    /// Here, the default value is false.
    pub knockout: bool,
    /// This value determines if the match was decided in a penalty shootout.
    ///
    /// If set to true, the team that won will gain less rating than normal (possibly losing rating),
    /// and the team that lost will receive the same amount as in a drawn match.
    ///
    /// If you set this value to true, but also specify the match ended in a draw with [`Outcomes::DRAW`],
    /// the match will still be treated as a normal draw for both teams, as it is not possible to draw on penalties.
    ///
    /// Please note that in some instances, games end with a winner after regular or extended time,
    /// but still include a penalty shootout to determine the team that eventually will get to the next round.  
    /// These games should be treated as normal wins and defeats.
    ///
    /// Here, the default value is false.
    pub penalties: bool,
}

impl FifaConfig {
    #[must_use]
    /// Initialise a new `FifaConfig` with an importance value of `10.0` (Friendly match),
    /// knockout value set to `false` and penalties value set to `false`.
    pub const fn new() -> Self {
        Self {
            importance: 10.0,
            knockout: false,
            penalties: false,
        }
    }
}

impl Default for FifaConfig {
    fn default() -> Self {
        Self::new()
    }
}

/// Struct to calculate ratings and expected score for [`FifaRating`]
pub struct Fifa {
    config: FifaConfig,
}

impl RatingSystem for Fifa {
    type RATING = FifaRating;
    type CONFIG = FifaConfig;

    fn new(config: Self::CONFIG) -> Self {
        Self { config }
    }

    fn rate(
        &self,
        player_one: &FifaRating,
        player_two: &FifaRating,
        outcome: &Outcomes,
    ) -> (FifaRating, FifaRating) {
        fifa(player_one, player_two, outcome, &self.config)
    }

    fn expected_score(&self, player_one: &FifaRating, player_two: &FifaRating) -> (f64, f64) {
        expected_score(player_one, player_two)
    }
}

impl RatingPeriodSystem for Fifa {
    type RATING = FifaRating;
    type CONFIG = FifaConfig;

    fn new(config: Self::CONFIG) -> Self {
        Self { config }
    }

    fn rate(&self, player: &FifaRating, results: &[(FifaRating, Outcomes)]) -> FifaRating {
        // Need to add a config to the results.
        let new_results: Vec<(FifaRating, Outcomes, FifaConfig)> =
            results.iter().map(|r| (r.0, r.1, self.config)).collect();

        fifa_rating_period(player, &new_results[..])
    }
}

#[must_use]
/// Calculates the [`FifaRating`]s of two teams based on their old ratings and the outcome of the game.
///
/// Takes in two teams as [`FifaRating`]s, an [`Outcome`](Outcomes) and a [`FifaConfig`].
///
/// The outcome of the match is in the perspective of `player_one`.
/// This means [`Outcomes::WIN`] is a win for `player_one` and [`Outcomes::LOSS`] is a win for `player_two`.
///
/// # Examples
/// ```
/// use skillratings::{
///     fifa::{fifa, FifaConfig, FifaRating},
///     Outcomes,
/// };
///
/// let player_one = FifaRating { rating: 600.0 };
/// let player_two = FifaRating { rating: 711.0 };
///
/// let outcome = Outcomes::WIN;
///
/// let config = FifaConfig::new();
///
/// let (new_one, new_two) = fifa(&player_one, &player_two, &outcome, &config);
///
/// assert!((new_one.rating.round() - 606.0).abs() < f64::EPSILON);
/// assert!((new_two.rating.round() - 705.0).abs() < f64::EPSILON);
/// ```
pub fn fifa(
    player_one: &FifaRating,
    player_two: &FifaRating,
    outcome: &Outcomes,
    config: &FifaConfig,
) -> (FifaRating, FifaRating) {
    let (one_expected, two_expected) = expected_score(player_one, player_two);

    // I guess you can set the penalty shootout to true and have the outcome be a draw,
    // in this case we are just handling it like a draw.
    let outcome1 = if config.penalties {
        if outcome == &Outcomes::WIN {
            0.75
        } else {
            0.5
        }
    } else {
        outcome.to_chess_points()
    };
    let outcome2 = if config.penalties {
        if outcome == &Outcomes::WIN {
            0.75
        } else {
            0.5
        }
    } else {
        1.0 - outcome1
    };

    let mut new_rating1 = config
        .importance
        .mul_add(outcome1 - one_expected, player_one.rating);
    let mut new_rating2 = config
        .importance
        .mul_add(outcome2 - two_expected, player_two.rating);

    // If the match is played in a knock-out stage of a final tournament,
    // the teams cannot lose any rating points.
    if config.knockout && player_one.rating > new_rating1 {
        new_rating1 = player_one.rating;
    }

    if config.knockout && player_two.rating > new_rating2 {
        new_rating2 = player_two.rating;
    }

    (
        FifaRating {
            rating: new_rating1,
        },
        FifaRating {
            rating: new_rating2,
        },
    )
}

#[must_use]
/// Calculates a [`FifaRating`] in a non-traditional way using a rating period,
/// for compatibility with the other algorithms, and for ease of use in tournaments.
///
/// Takes in a player a a [`FifaRating`], their results as a Slice of tuples containing the opponent as a [`FifaRating`],
/// the outcome of the game as an [`Outcome`](Outcomes), and a [`FifaConfig`] where you can specify the parameters of a given match.
///
/// ---
///
/// 📌 _**Important note:**_ The parameters intentionally work different from other rating_period functions here.  
/// In most cases the config is a separate parameter, because it holds static values that should not change from game-to-game.  
/// Here, the config is in the tuple together with the results,
/// because the circumstances of a match can, and will probably, change from game-to-game.
/// Thus it would not make sense to have the config as its own separate parameter, disconnected from the game results.
///
/// ---
///
/// All of the outcomes are from the perspective of the player.
/// This means [`Outcomes::WIN`] is a win for the player and [`Outcomes::LOSS`] is a win for the opponent.
///
/// # Examples
/// ```
/// use skillratings::{
///     fifa::{fifa_rating_period, FifaConfig, FifaRating},
///     Outcomes,
/// };
///
/// let player = FifaRating { rating: 1380.0 };
///
/// let opponent1 = FifaRating { rating: 1032.0 };
/// let opponent2 = FifaRating { rating: 1440.0 };
/// let opponent3 = FifaRating { rating: 1320.0 };
///
/// let config1 = FifaConfig::new();
/// let config2 = FifaConfig {
///     importance: 60.0,
///     knockout: true,
///     penalties: false,
/// };
/// let config3 = FifaConfig {
///     importance: 40.0,
///     knockout: true,
///     penalties: true,
/// };
///
/// let new_player = fifa_rating_period(
///     &player,
///     &vec![
///         // Make sure to include the configs in each game!
///         (opponent1, Outcomes::LOSS, config1),
///         (opponent2, Outcomes::WIN, config2),
///         (opponent3, Outcomes::LOSS, config3),
///     ],
/// );
///
/// assert!((new_player.rating.round() - 1406.0).abs() < f64::EPSILON);
/// ```
pub fn fifa_rating_period(
    player: &FifaRating,
    results: &[(FifaRating, Outcomes, FifaConfig)],
) -> FifaRating {
    let mut player_rating = player.rating;

    for (opponent, result, config) in results {
        // Normally we would just call expected_points(),
        // but we would have to construct a rating first which seems inefficient.
        // So we are just calculating it ourselves.
        let exp = (1.0 + 10_f64.powf(-(player_rating - opponent.rating) / 600.0)).recip();

        let outcome = if config.penalties {
            if result == &Outcomes::WIN {
                0.75
            } else {
                0.5
            }
        } else {
            result.to_chess_points()
        };

        let new_rating = config.importance.mul_add(outcome - exp, player_rating);

        if !(config.knockout && player_rating > new_rating) {
            player_rating = new_rating;
        }
    }

    FifaRating {
        rating: player_rating,
    }
}

#[must_use]
/// Calculates the expected score of two players based on their Fifa rating.
///
/// Takes in two players as [`FifaRating`]s and returns the probability of victory for each player as a [`f64`] between 1.0 and 0.0.  
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// # Examples
/// ```
/// use skillratings::fifa::{expected_score, FifaRating};
///
/// let player_one = FifaRating { rating: 1320.0 };
/// let player_two = FifaRating { rating: 1217.0 };
///
/// let (exp1, exp2) = expected_score(&player_one, &player_two);
///
/// assert!(((exp1 * 100.0).round() - 60.0).abs() < f64::EPSILON);
/// assert!(((exp2 * 100.0).round() - 40.0).abs() < f64::EPSILON);
/// ```
pub fn expected_score(player_one: &FifaRating, player_two: &FifaRating) -> (f64, f64) {
    let exp_one = (1.0 + 10_f64.powf(-(player_one.rating - player_two.rating) / 600.0)).recip();
    let exp_two = 1.0 - exp_one;

    (exp_one, exp_two)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    /// This test is taken from the official example given in the paper.
    fn test_fifa() {
        let team_a = FifaRating::from(1300.0);
        let team_b = FifaRating { rating: 1500.0 };

        let config = FifaConfig {
            importance: 25.0,
            knockout: false,
            penalties: false,
        };

        let (new_a, new_b) = fifa(&team_a, &team_b, &Outcomes::WIN, &config);

        assert!((new_a.rating.round() - 1317.0).abs() < f64::EPSILON);
        assert!((new_b.rating.round() - 1483.0).abs() < f64::EPSILON);

        let new = fifa_rating_period(&team_a, &[(team_b, Outcomes::WIN, config)]);

        assert_eq!(new, new_a);

        let new = fifa_rating_period(&team_b, &[(team_a, Outcomes::LOSS, config)]);

        assert_eq!(new, new_b);
    }

    #[test]
    fn test_fifa_rating_period() {
        let mut team_a = FifaRating::new();

        let mut results = Vec::new();

        let team_b = FifaRating::default();
        let team_c = FifaRating::from(1600.0);
        let team_d = FifaRating::from(890.0);
        let team_e = FifaRating::from(1300.0);

        results.push((team_b, Outcomes::WIN, FifaConfig::new()));
        results.push((team_c, Outcomes::DRAW, FifaConfig::default()));
        results.push((
            team_d,
            Outcomes::LOSS,
            FifaConfig {
                importance: 60.0,
                knockout: true,
                penalties: true,
            },
        ));
        results.push((
            team_e,
            Outcomes::WIN,
            FifaConfig {
                importance: 25.0,
                knockout: true,
                penalties: true,
            },
        ));

        let new_team = fifa_rating_period(&team_a, &results);

        assert!((new_team.rating.round() - 1022.0).abs() < f64::EPSILON);

        for r in results {
            (team_a, _) = fifa(&team_a, &r.0, &r.1, &r.2);
        }

        assert_eq!(team_a, new_team);
    }

    #[test]
    #[allow(clippy::similar_names)]
    fn test_draws() {
        let team_a = FifaRating::from(1300.0);
        let team_b = FifaRating::from(1500.0);

        let config1 = FifaConfig {
            importance: 25.0,
            knockout: false,
            penalties: false,
        };

        let config2 = FifaConfig {
            importance: 25.0,
            knockout: false,
            penalties: true,
        };

        let (new_a, new_b) = fifa(&team_a, &team_b, &Outcomes::DRAW, &config1);
        let (new_a2, new_b2) = fifa(&team_a, &team_b, &Outcomes::DRAW, &config2);

        assert_eq!(new_a, new_a2);
        assert_eq!(new_b, new_b2);
    }

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn test_misc_stuff() {
        let team_a = FifaRating::from(1300.0);
        let team_b = EloRating::from(team_a);

        assert_eq!(team_a, FifaRating::from(team_b));

        let player_one = FifaRating::new();
        let config = FifaConfig::new();

        assert_eq!(player_one, player_one.clone());
        assert!((config.importance - config.clone().importance).abs() < f64::EPSILON);

        assert!(!format!("{:?}", player_one).is_empty());
        assert!(!format!("{:?}", config).is_empty());

        assert_eq!(player_one, FifaRating::from(1000.));
    }
}
