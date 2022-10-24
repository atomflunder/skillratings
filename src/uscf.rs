//! The USCF (US Chess Federation) Rating Algorithm, developed by Mark Glickman as an improvement on Elo.  
//! Used to rate US Chess events in favour of Elo, and continues to be enhanced over the years.
//!
//! This rating system was, just like Glicko, developed by Mark Glickman, and is thus heavily influenced by it.
//!
//! This implementation of the USCF Rating System uses the formulas found in the
//! [Approximating Formulas for the US Chess Rating System](http://www.glicko.net/ratings/approx.pdf) document,
//! and thus may yield *slightly* different calculations to the proper formulas in some circumstances,
//! specifically if a player has 8 or less games played, with only wins or only losses.
//!
//! Also, this implementation can not determine rating floors for each player, except the most basic floor.  
//! This is due to the rating floor being determined by the *all-time highest* rating for each player, which we cannot know.  
//! This is something you need to keep track of yourself, unfortunately.  
//! The formula for calculating a rating floor (the lowest rating possible) of a player is the player's highest ever achieved rating,
//! subtracted by 200 points and then using the floor just below that.
//! As of 2022, rating floors exist at every 100 point mark between 1200 and 2100.  
//! For example, a player with an all-time highest rating of 1941 has a rating floor of 1700.
//!
//! # Quickstart
//!
//! This is the most basic example on how to use the USCF Module.  
//! Please take a look at the functions below to see more advanced use cases.
//!
//! ```
//! use skillratings::{
//!     uscf::{uscf, USCFConfig, USCFRating},
//!     Outcomes,
//! };
//!
//! // Initialise a new player rating.
//! // We need to set the actual age for the player,
//! // if you are unsure what to set here, set this to 26.
//! let player_one = USCFRating::new(18);
//!
//! // Or you can initialise it with your own values of course.
//! // Imagine these numbers being pulled from a database.
//! let (some_rating, some_games) = (1325.0, 44);
//! let player_two = USCFRating {
//!     rating: some_rating,
//!     games: some_games,
//! };
//!
//! // The outcome of the match is from the perspective of player one.
//! let outcome = Outcomes::WIN;
//!
//! // The config allows you to specify the t value in the USCF calculation.
//! // It determines how easy or hard it is to gain bonus rating points.
//! // The recommended value changes periodically, as of right now it is 14.0.
//! // Here we set it to 12.0, the recommended value from 2015 to 2017.
//! let config = USCFConfig { t: 12.0 };
//!
//! // The uscf function will calculate the new ratings for both players and return them.
//! let (new_player_one, new_player_two) = uscf(&player_one, &player_two, &outcome, &config);
//! ```
//!
//! # More Information
//!
//! - [US Chess Federation](https://new.uschess.org/)
//! - [The US Chess Rating System (PDF)](https://new.uschess.org/sites/default/files/media/documents/the-us-chess-rating-system-revised-september-2020.pdf)
//! - [Approximating US Chess Rating (PDF)](http://www.glicko.net/ratings/approx.pdf)
//! - [USCF Calculator](https://www.uschess.org/index.php/Players-Ratings/Do-NOT-edit-CLOSE-immediately.html)
//! - [Wikipedia: USCF](https://en.wikipedia.org/wiki/United_States_Chess_Federation)

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::{elo::EloRating, Outcomes};

#[derive(Copy, Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// The USCF (US Chess Federation) rating for a player.
///
/// The age is the actual age of the player,
/// if unsure or unavailable the official guidelines say to set this to `26`,
/// if the player is inferred to be an adult, or to `15` if not.  
///
/// The default rating is dependent on the age of the player.  
/// If the player is 26 or older this will be 1300.0, if the player is 15 the rating will be 750.0.  
/// The minimum rating value is set to be 100.0.
pub struct USCFRating {
    /// The player's USCF rating number.
    pub rating: f64,
    /// The player's completed games.
    pub games: usize,
}

impl USCFRating {
    #[must_use]
    /// Initialise a new `USCFRating` with a new rating dependent on the age of the player.  
    /// The age is the actual age of the player, if unsure or unavailable set this to `26`.  
    /// The rating of a 26 year old will be 1300.0.
    pub fn new(age: usize) -> Self {
        Self {
            rating: if age < 2 {
                100.0
            } else if age > 26 {
                1300.0
            } else {
                age as f64 * 50.0
            },
            games: 0,
        }
    }
}

impl Default for USCFRating {
    fn default() -> Self {
        Self::new(26)
    }
}

impl From<EloRating> for USCFRating {
    fn from(e: EloRating) -> Self {
        if e.rating > 2000.0 {
            Self {
                rating: 0.94f64.mul_add(e.rating, 180.0),
                games: 10,
            }
        } else {
            Self {
                rating: 1.02f64.mul_add(e.rating, 20.0),
                games: 5,
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// Constants used in the USCF Rating calculations.
pub struct USCFConfig {
    /// The t value controls the difficulty of earning bonus rating points.  
    /// The higher the t value, the more difficult it is.
    ///
    /// The USCF changes this value periodically.
    /// As of 2022, the last change was in May 2017 where this was set from 12 to 14.
    /// The lowest value was 6, from 2008 to 2012.  
    /// By default set to 14.0.
    pub t: f64,
}

impl USCFConfig {
    #[must_use]
    /// Initialise a new `USCFConfig` with a t value of 14.0.
    pub const fn new() -> Self {
        Self { t: 14.0 }
    }
}

impl Default for USCFConfig {
    fn default() -> Self {
        Self::new()
    }
}

#[must_use]
/// Calculates the [`USCFRating`]s of two players based on their old ratings, deviations, and the outcome of the game.
///
/// Takes in two players as [`USCFRating`]s, an [`Outcome`](Outcomes), and a [`USCFConfig`].
///
/// Instead of the traditional way of calculating the USCF Rating for only one player only using a list of results,
/// we are calculating the USCF Rating for two players at once, like in the Elo calculation,
/// to make it easier to see instant results.
///
/// For the traditional way of calculating a USCF Rating rating please see [`uscf_rating_period`].
///
/// The outcome of the match is in the perspective of `player_one`.
/// This means [`Outcomes::WIN`] is a win for `player_one` and [`Outcomes::LOSS`] is a win for `player_two`.
///
/// # Examples
/// ```
/// use skillratings::{
///     uscf::{uscf, USCFConfig, USCFRating},
///     Outcomes,
/// };
///
/// let player_one = USCFRating {
///     rating: 1250.0,
///     games: 30,
/// };
/// let player_two = USCFRating {
///     rating: 1400.0,
///     games: 9,
/// };
///
/// let outcome = Outcomes::WIN;
///
/// let config = USCFConfig::new();
///
/// let (new_one, new_two) = uscf(&player_one, &player_two, &outcome, &config);
///
/// assert!((new_one.rating.round() - 1289.0).abs() < f64::EPSILON);
/// assert_eq!(new_one.games, 31);
///
/// assert!((new_two.rating.round() - 1344.0).abs() < f64::EPSILON);
/// assert_eq!(new_two.games, 10);
/// ```
pub fn uscf(
    player_one: &USCFRating,
    player_two: &USCFRating,
    outcome: &Outcomes,
    config: &USCFConfig,
) -> (USCFRating, USCFRating) {
    let outcome1 = outcome.to_chess_points();
    let outcome2 = 1.0 - outcome1;

    let e1 = e_value(player_one.rating, player_two.rating);
    let e2 = e_value(player_two.rating, player_one.rating);

    let new_rating1 = if player_one.games <= 8 {
        new_rating_provisional(
            player_one.rating,
            player_one.games,
            1,
            player_two.rating,
            if outcome == &Outcomes::WIN { 1 } else { 0 },
            if outcome == &Outcomes::LOSS { 1 } else { 0 },
        )
    } else {
        new_rating(
            player_one.rating,
            player_one.games,
            1,
            outcome1,
            e1,
            *config,
        )
    }
    .max(100.0);
    let new_rating2 = if player_two.games <= 8 {
        new_rating_provisional(
            player_two.rating,
            player_two.games,
            1,
            player_one.rating,
            if outcome == &Outcomes::LOSS { 1 } else { 0 },
            if outcome == &Outcomes::WIN { 1 } else { 0 },
        )
    } else {
        new_rating(
            player_two.rating,
            player_two.games,
            1,
            outcome2,
            e2,
            *config,
        )
    }
    .max(100.0);

    (
        USCFRating {
            rating: new_rating1,
            games: player_one.games + 1,
        },
        USCFRating {
            rating: new_rating2,
            games: player_two.games + 1,
        },
    )
}

#[must_use]
/// The "traditional" way of calculating a [`USCFRating`] of a player in a rating period.
///
/// Note that in this case, all of the matches are considered to be played at once.  
/// This means that the player will not get updated in-between matches, as you might expect.  
/// This will result in *slightly* different results than if you were to use the [`uscf`] function in a loop.
///
/// Takes in a player as an [`USCFRating`] and their results as a Vec of tuples containing the opponent as an [`USCFRating`],
/// the outcome of the game as an [`Outcome`](Outcomes) and a [`USCFConfig`].
///
/// The outcome of the match is in the perspective of the player.
/// This means [`Outcomes::WIN`] is a win for the player and [`Outcomes::LOSS`] is a win for the opponent.
///
/// # Examples
/// ```
/// use skillratings::{
///     uscf::{uscf_rating_period, USCFConfig, USCFRating},
///     Outcomes,
/// };
///
/// let player = USCFRating {
///     rating: 1500.0,
///     games: 31,
/// };
///
/// let opponent1 = USCFRating {
///     rating: 1400.0,
///     games: 25,
/// };
///
/// let opponent2 = USCFRating {
///     rating: 1550.0,
///     games: 4,
/// };
///
/// let opponent3 = USCFRating {
///     rating: 1700.0,
///     games: 30,
/// };
///
/// let results = vec![
///     (opponent1, Outcomes::WIN),
///     (opponent2, Outcomes::LOSS),
///     (opponent3, Outcomes::LOSS),
/// ];
///
/// let config = USCFConfig::new();
///
/// let new_player = uscf_rating_period(&player, &results, &config);
///
/// assert!((new_player.rating.round() - 1487.0).abs() < f64::EPSILON);
/// assert_eq!(new_player.games, 34);
/// ```
pub fn uscf_rating_period(
    player: &USCFRating,
    results: &Vec<(USCFRating, Outcomes)>,
    config: &USCFConfig,
) -> USCFRating {
    if results.is_empty() {
        return *player;
    }

    if player.games <= 8 {
        let avg_opponent_rating =
            results.iter().map(|r| r.0.rating).sum::<f64>() / results.len() as f64;

        let wins = results
            .iter()
            .map(|r| if r.1 == Outcomes::WIN { 1 } else { 0 })
            .sum::<i32>();

        let losses = results
            .iter()
            .map(|r| if r.1 == Outcomes::LOSS { 1 } else { 0 })
            .sum::<i32>();

        let new_rating = new_rating_provisional(
            player.rating,
            player.games,
            results.len(),
            avg_opponent_rating,
            wins,
            losses,
        );

        return USCFRating {
            rating: new_rating,
            games: player.games + results.len(),
        };
    }

    let score = results.iter().map(|r| r.1.to_chess_points()).sum::<f64>();

    let exp_sum = results
        .iter()
        .map(|r| e_value(player.rating, r.0.rating))
        .sum::<f64>();

    let new_rating = new_rating(
        player.rating,
        player.games,
        results.len(),
        score,
        exp_sum,
        *config,
    );

    USCFRating {
        rating: new_rating,
        games: player.games + results.len(),
    }
}

#[must_use]
/// Calculates the expected outcome of two players based on the USCF rating algorithm.
///
/// Takes in two players as [`USCFRating`]s and returns the probability of victory for each player as an [`f64`] between 1.0 and 0.0.  
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// # Examples
/// ```
/// use skillratings::uscf::{expected_score, USCFRating};
///
/// let player_one = USCFRating {
///     rating: 1800.0,
///     games: 130,
/// };
/// let player_two = USCFRating {
///     rating: 1950.0,
///     games: 41,
/// };
///
/// let (exp_one, exp_two) = expected_score(&player_one, &player_two);
///
/// assert!(((exp_one * 100.0).round() - 30.0).abs() < f64::EPSILON);
/// assert!(((exp_two * 100.0).round() - 70.0).abs() < f64::EPSILON);
/// ```
pub fn expected_score(player_one: &USCFRating, player_two: &USCFRating) -> (f64, f64) {
    let exp_one = e_value(player_one.rating, player_two.rating);
    let exp_two = 1.0 - exp_one;

    (exp_one, exp_two)
}

/// This formula gets applied if a player has not played at least 8 games.
fn new_rating_provisional(
    rating: f64,
    past_games: usize,
    played_games: usize,
    opponent_rating: f64,
    wins: i32,
    losses: i32,
) -> f64 {
    ((wins - losses) as f64).mul_add(
        400.0,
        (past_games as f64).mul_add(rating, played_games as f64 * opponent_rating),
    ) / (past_games + played_games) as f64
}

/// This formula is used for players with more than 8 games played.
fn new_rating(
    rating: f64,
    past_games: usize,
    played_games: usize,
    score: f64,
    e: f64,
    config: USCFConfig,
) -> f64 {
    let ne = effective_game_number(rating, past_games);
    let k = 800.0 / (ne + played_games as f64);
    let boost = get_boost_value(played_games, k, e, score, config.t);

    k.mul_add(score - e, rating) + boost
}

fn e_value(rating: f64, opponent_rating: f64) -> f64 {
    (10_f64.powf(-(rating - opponent_rating) / 400.0) + 1.0).recip()
}

fn effective_game_number(rating: f64, past_games: usize) -> f64 {
    if rating < 2355.0 {
        50.0 / 0.000_007_39f64
            .mul_add((2569.0 - rating).powi(2), 0.662)
            .sqrt()
    } else {
        50.0
    }
    .min((past_games as f64).min(50.0))
}

fn get_boost_value(played_games: usize, k: f64, e: f64, score: f64, t: f64) -> f64 {
    if played_games < 3 {
        return 0.0;
    }

    let kse = k * (score - e);
    let tm = t * (played_games.min(4) as f64).sqrt();

    if tm >= kse {
        0.0
    } else {
        kse - tm
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_effective_game_number() {
        let player = USCFRating {
            rating: 1700.0,
            games: 30,
        };

        let other_player = USCFRating {
            rating: 2356.0,
            games: 51,
        };

        let n1 = effective_game_number(player.rating, player.games);
        let n2 = effective_game_number(other_player.rating, other_player.games);

        assert!((n1 - 20.011_786_747_374_54).abs() < f64::EPSILON);
        assert!((n2 - 50.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_get_boost_value() {
        let player = USCFRating {
            rating: 1300.0,
            games: 45,
        };

        let played_games = 4;

        let ne = effective_game_number(player.rating, player.games);
        let k = 800.0 / (ne + played_games as f64);
        let e = [1250.0, 1400.0, 1500.0, 1550.0]
            .iter()
            .map(|r| e_value(player.rating, *r))
            .sum::<f64>();

        let boost = get_boost_value(played_games, k, e, 3.5, 12.0);

        assert!((boost - 70.402_444_130_859_96).abs() < f64::EPSILON);

        let zero_boost = get_boost_value(10, k, e, 0.0, 12.0);

        assert!((zero_boost - 0.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_expected_score() {
        let player_one = USCFRating {
            rating: 1300.0,
            games: 30,
        };
        let player_two = USCFRating {
            rating: 1250.0,
            games: 30,
        };

        let (exp_one, exp_two) = expected_score(&player_one, &player_two);

        assert!((exp_one + exp_two - 1.0).abs() < f64::EPSILON);
        assert!((exp_one - 0.571_463_117_408_381_4).abs() < f64::EPSILON);
    }

    #[test]
    fn test_uscf_rating_period_provisional() {
        let player = USCFRating {
            rating: 1500.0,
            games: 6,
        };

        let opponent1 = USCFRating {
            rating: 1400.0,
            games: 10,
        };
        let opponent2 = USCFRating {
            rating: 1550.0,
            games: 10,
        };
        let opponent3 = USCFRating {
            rating: 1650.0,
            games: 10,
        };

        let results = vec![
            (opponent1, Outcomes::WIN),
            (opponent2, Outcomes::LOSS),
            (opponent3, Outcomes::DRAW),
        ];

        let new_player = uscf_rating_period(&player, &results, &USCFConfig::default());

        assert_eq!(new_player.games, 9);
        assert!((new_player.rating - 1_511.111_111_111_111).abs() < f64::EPSILON);
    }

    #[test]
    fn test_uscf_rating_period_proper() {
        let player = USCFRating {
            rating: 1300.0,
            games: 45,
        };

        let opponent1 = USCFRating {
            rating: 1250.0,
            games: 10,
        };
        let opponent2 = USCFRating {
            rating: 1400.0,
            games: 10,
        };
        let opponent3 = USCFRating {
            rating: 1500.0,
            games: 10,
        };
        let opponent4 = USCFRating {
            rating: 1550.0,
            games: 10,
        };

        let results = vec![
            (opponent1, Outcomes::WIN),
            (opponent2, Outcomes::DRAW),
            (opponent3, Outcomes::WIN),
            (opponent4, Outcomes::WIN),
        ];

        // Seems like the example uses an outdated t value.
        let config = USCFConfig { t: 12.0 };

        let new_player = uscf_rating_period(&player, &results, &config);

        assert_eq!(new_player.games, 49);
        assert!((new_player.rating - 1_464.804_888_261_72).abs() < f64::EPSILON);
    }

    #[test]
    fn test_empty_rp() {
        let player = USCFRating {
            rating: 3201.0,
            games: 39,
        };

        let rp = uscf_rating_period(&player, &vec![], &USCFConfig::new());

        assert_eq!(player, rp);
    }

    #[test]
    fn test_one_rp() {
        let player = USCFRating {
            rating: 1500.0,
            games: 40,
        };
        let opponent = USCFRating {
            rating: 1700.0,
            games: 40,
        };

        let config = USCFConfig::new();

        let (np, _) = uscf(&player, &opponent, &Outcomes::WIN, &config);
        let npr = uscf_rating_period(&player, &vec![(opponent, Outcomes::WIN)], &config);

        assert_eq!(np, npr);

        let player = USCFRating {
            rating: 1500.0,
            games: 5,
        };
        let opponent = USCFRating {
            rating: 1700.0,
            games: 5,
        };

        let (np, _) = uscf(&player, &opponent, &Outcomes::WIN, &config);
        let npr = uscf_rating_period(&player, &vec![(opponent, Outcomes::WIN)], &config);

        assert_eq!(np, npr);
    }

    #[test]
    fn test_uscf() {
        let player_one = USCFRating {
            rating: 1300.0,
            games: 32,
        };
        let player_two = USCFRating {
            rating: 1200.0,
            games: 32,
        };

        let config = USCFConfig::new();

        let (new_one, new_two) = uscf(&player_one, &player_two, &Outcomes::WIN, &config);

        assert_eq!(new_one.games, 33);
        assert_eq!(new_two.games, 33);

        assert!((new_one.rating - 1_319.060_726_612_988_7).abs() < f64::EPSILON);
        assert!((new_two.rating - 1_179.614_576_196_810_5).abs() < f64::EPSILON);

        let player_one = USCFRating {
            rating: 1300.0,
            games: 7,
        };
        let player_two = USCFRating {
            rating: 1200.0,
            games: 7,
        };

        let config = USCFConfig::new();

        let (new_one, new_two) = uscf(&player_one, &player_two, &Outcomes::WIN, &config);

        assert!((new_one.rating - 1337.5).abs() < f64::EPSILON);
        assert!((new_two.rating - 1162.5).abs() < f64::EPSILON);
    }

    #[test]
    /// Note: The online conversion calculators all seem to use outdated formulas.
    fn test_rating_conversion() {
        let new = USCFRating::new(26);
        let default = USCFRating::default();

        assert_eq!(new, default);

        let new2 = USCFRating::new(15);

        assert_ne!(new2, default);

        let elo = EloRating { rating: 1779.0 };

        let uscf = USCFRating::from(elo);

        assert!((uscf.rating - 1834.58).abs() < f64::EPSILON);
        assert_eq!(uscf.games, 5);
        assert_eq!(EloRating::from(uscf), elo);

        let elo2 = EloRating { rating: 2235.0 };

        let uscf2 = USCFRating::from(elo2);

        assert!((uscf2.rating - 2280.9).abs() < f64::EPSILON);
        assert_eq!(uscf2.games, 10);
        assert_eq!(EloRating::from(uscf2), elo2);
    }
}
