//! A bayesian approximation method for online ranking. Similar to TrueSkill, but based on a logistical distribution.  
//! Used in games such as Rocket League.
//!
//! Developed by Ruby C. Weng and Chih-Jen Lin.
//! Unlike with the other algorithms, there does not seem to exist a *short* name everyone agrees upon,
//! so we are just calling it Weng-Lin, for short, after the researchers.
//! But the proper name would be `A Bayesian Approximation Method for Online Ranking`.
//!
//! Developed specifically for online games with multiple teams and multiple players,
//! this algorithm aims to be simpler and faster (~2.5 - 6.5x) than TrueSkill while yielding similar accuracy.
//!
//! While TrueSkill is based upon a Gaussian distribution, this algorithm is based upon a logistical distribution, the Bradley-Terry model.
//!
//! # Quickstart
//!
//! This is the most basic example on how to use the Weng-Lin Module.  
//! Please take a look at the functions below to see more advanced use cases.
//!
//! ```
//! use skillratings::{
//!     weng_lin::{weng_lin, WengLinConfig, WengLinRating},
//!     Outcomes,
//! };
//!
//! // Initialise a new player rating.
//! let player_one = WengLinRating::new();
//!
//! // Or you can initialise it with your own values of course.
//! // Imagine these numbers being pulled from a database.
//! let (some_rating, some_uncertainty) = (41.2, 2.12);
//! let player_two = WengLinRating {
//!     rating: some_rating,
//!     uncertainty: some_uncertainty,
//! };
//!
//! // The outcome of the match is from the perspective of player one.
//! let outcome = Outcomes::WIN;
//!
//! // The config allows you to specify certain values in the Weng-Lin calculation.
//! // Here we change the beta value from the default of 25 / 6 ≈ 4.167.
//! // The beta value measures the difference you need in rating points
//! // to achieve an 80% win-rate over another player.
//! // Lower this value if your game is heavily reliant on pure skill,
//! // or increase it if randomness plays a big factor in the outcome of the game.
//! let config = WengLinConfig {
//!     beta: 25.0 / 12.0,
//!     ..Default::default()
//! };
//!
//! // The weng_lin function will calculate the new ratings for both players and return them.
//! let (new_player_one, new_player_two) = weng_lin(&player_one, &player_two, &outcome, &config);
//! ```
//!
//! # More Information
//! - [Original Paper (PDF)](https://jmlr.csail.mit.edu/papers/volume12/weng11a/weng11a.pdf)
//! - [Bradley-Terry model Wikipedia](https://en.wikipedia.org/wiki/Bradley–Terry_model)
//! - [Approximate Bayesian computation Wikipedia](https://en.wikipedia.org/wiki/Approximate_Bayesian_computation)
//! - [Logistic distribution Wikipedia](https://en.wikipedia.org/wiki/Logistic_distribution)

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::{trueskill::TrueSkillRating, Outcomes};

#[derive(Copy, Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// The Weng-Lin rating of a player.
///
/// Similar to [`TrueSkillRating`].
///
/// The default rating is 25.0.  
/// The default uncertainty is 25/3 ≈ 8.33.
pub struct WengLinRating {
    /// The rating value (mu) of the WengLinRating, by default 25.0.
    pub rating: f64,
    /// The uncertainty value (sigma) of the WengLinRating, by default 25/3 ≈ 8.33.
    pub uncertainty: f64,
}

impl WengLinRating {
    #[must_use]
    /// Initialise a new WengLinRating with a rating of 25.0, and an uncertainty of 25/3 ≈ 8.33.
    pub fn new() -> Self {
        Self {
            rating: 25.0,
            uncertainty: 25.0 / 3.0,
        }
    }
}

impl Default for WengLinRating {
    fn default() -> Self {
        Self::new()
    }
}

impl From<TrueSkillRating> for WengLinRating {
    fn from(t: TrueSkillRating) -> Self {
        Self {
            rating: t.rating,
            uncertainty: t.uncertainty,
        }
    }
}

#[derive(Clone, Copy, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// Constants used in the Weng-Lin calculations.
pub struct WengLinConfig {
    /// The skill-class width, aka the number of difference in rating points
    /// needed to have an 80% win probability against another player.  
    /// By default set to 25 / 6 ≈ `4.167`.  
    /// If your game is more reliant on pure skill, decrease this value,
    /// if there are more random factors, increase it.
    pub beta: f64,
    /// The lower ceiling of the sigma value, in the uncertainty calculations.
    /// The lower this value, the lower the possible uncertainty values.  
    /// By default set to 0.000_001.  
    /// Do not set this to a negative value.
    pub uncertainty_tolerance: f64,
}

impl WengLinConfig {
    #[must_use]
    /// Initialise a new `WengLinConfig` with a beta value of 25 / 6 ≈ `4.167`
    /// and an uncertainty tolerance of `0.000_001`.
    pub fn new() -> Self {
        Self {
            beta: 25.0 / 6.0,
            uncertainty_tolerance: 0.000_001,
        }
    }
}

impl Default for WengLinConfig {
    fn default() -> Self {
        Self::new()
    }
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
/// Calculates the [`WengLinRating`]s of two players based on their old ratings, uncertainties, and the outcome of the game.
///
/// Takes in two players as [`WengLinRating`]s, an [`Outcome`](Outcomes), and a [`WengLinConfig`].
///
/// The outcome of the match is in the perspective of `player_one`.
/// This means [`Outcomes::WIN`] is a win for `player_one` and [`Outcomes::LOSS`] is a win for `player_two`.
///
/// Similar to [`weng_lin_rating_period`] and [`weng_lin_teams`].
///
/// # Examples
/// ```
/// use skillratings::{
///     weng_lin::{weng_lin, WengLinConfig, WengLinRating},
///     Outcomes,
/// };
///
/// let player_one = WengLinRating {
///     rating: 42.0,
///     uncertainty: 1.3,
/// };
/// let player_two = WengLinRating::new();
///
/// let (new_one, new_two) = weng_lin(
///     &player_one,
///     &player_two,
///     &Outcomes::WIN,
///     &WengLinConfig::new(),
/// );
///
/// assert!(((new_one.rating * 100.0).round() - 4203.0).abs() < f64::EPSILON);
/// assert!(((new_one.uncertainty * 100.0).round() - 130.0).abs() < f64::EPSILON);
/// assert!(((new_two.rating * 100.0).round() - 2391.0).abs() < f64::EPSILON);
/// assert!(((new_two.uncertainty * 100.0).round() - 803.0).abs() < f64::EPSILON);
/// ```
pub fn weng_lin(
    player_one: &WengLinRating,
    player_two: &WengLinRating,
    outcome: &Outcomes,
    config: &WengLinConfig,
) -> (WengLinRating, WengLinRating) {
    let c = 2.0f64
        .mul_add(
            config.beta.powi(2),
            player_one
                .uncertainty
                .mul_add(player_one.uncertainty, player_two.uncertainty.powi(2)),
        )
        .sqrt();

    let (p1, p2) = expected_score(player_one, player_two, config);

    let outcome1 = outcome.to_chess_points();
    let outcome2 = 1.0 - outcome1;

    let new_rating1 = new_rating(player_one.rating, player_one.uncertainty, c, outcome1, p1);
    let new_rating2 = new_rating(player_two.rating, player_two.uncertainty, c, outcome2, p2);

    let new_uncertainty1 = new_uncertainty(
        player_one.uncertainty,
        c,
        p1,
        p2,
        config.uncertainty_tolerance,
    );
    let new_uncertainty2 = new_uncertainty(
        player_two.uncertainty,
        c,
        p2,
        p1,
        config.uncertainty_tolerance,
    );

    (
        WengLinRating {
            rating: new_rating1,
            uncertainty: new_uncertainty1,
        },
        WengLinRating {
            rating: new_rating2,
            uncertainty: new_uncertainty2,
        },
    )
}

#[must_use]
/// Calculates a [`WengLinRating`] in a non-traditional way using a rating period,
/// for compatibility with the other algorithms.
///
/// Takes in a player as an [`WengLinRating`] and their results as a Vec of tuples containing the opponent as an [`WengLinRating`],
/// the outcome of the game as an [`Outcome`](Outcomes) and a [`WengLinConfig`].
///
/// The outcome of the match is in the perspective of the player.
/// This means [`Outcomes::WIN`] is a win for the player and [`Outcomes::LOSS`] is a win for the opponent.
///
/// Similar to [`weng_lin`] or [`weng_lin_teams`].
///
/// # Examples
/// ```
/// use skillratings::{
///     weng_lin::{weng_lin_rating_period, WengLinConfig, WengLinRating},
///     Outcomes,
/// };
///
/// let player = WengLinRating::new();
///
/// let opponent_one = WengLinRating::new();
/// let opponent_two = WengLinRating {
///     rating: 12.0,
///     uncertainty: 4.2,
/// };
///
/// let new_player = weng_lin_rating_period(
///     &player,
///     &vec![
///         (opponent_one, Outcomes::WIN),
///         (opponent_two, Outcomes::DRAW),
///     ],
///     &WengLinConfig::new(),
/// );
///
/// assert!(((new_player.rating * 100.0).round() - 2578.0).abs() < f64::EPSILON);
/// assert!(((new_player.uncertainty * 100.0).round() - 780.0).abs() < f64::EPSILON);
/// ```
pub fn weng_lin_rating_period(
    player: &WengLinRating,
    results: &Vec<(WengLinRating, Outcomes)>,
    config: &WengLinConfig,
) -> WengLinRating {
    let mut player_rating = player.rating;
    let mut player_uncertainty = player.uncertainty;

    for (opponent, result) in results {
        let c = 2.0f64
            .mul_add(
                config.beta.powi(2),
                player_uncertainty.mul_add(player_uncertainty, opponent.uncertainty.powi(2)),
            )
            .sqrt();

        // Normally we would just call expected_points(),
        // but we would have to construct a rating first which seems inefficient.
        // So we are just calculating it ourselves.
        let e1 = (player_rating / c).exp();
        let e2 = (opponent.rating / c).exp();

        let p1 = e1 / (e1 + e2);
        let p2 = 1.0 - p1;

        let outcome = result.to_chess_points();

        player_rating = new_rating(player_rating, player_uncertainty, c, outcome, p1);
        player_uncertainty =
            new_uncertainty(player_uncertainty, c, p1, p2, config.uncertainty_tolerance);
    }

    WengLinRating {
        rating: player_rating,
        uncertainty: player_uncertainty,
    }
}

#[must_use]
/// Calculates the [`WengLinRating`] of two teams based on their ratings, uncertainties, and the outcome of the game.
///
/// Takes in two teams as a Vec of [`WengLinRating`]s, the outcome of the game as an [`Outcome`](Outcomes) and a [`WengLinConfig`].
///
/// The outcome of the match is in the perspective of `team_one`.
/// This means [`Outcomes::WIN`] is a win for `team_one` and [`Outcomes::LOSS`] is a win for `team_two`.
///
/// Similar to [`weng_lin`].
///
/// # Examples
/// ```
/// use skillratings::{
///     weng_lin::{weng_lin_teams, WengLinConfig, WengLinRating},
///     Outcomes,
/// };
///
/// let team_one = vec![
///     WengLinRating::new(),
///     WengLinRating {
///         rating: 30.0,
///         uncertainty: 1.2,
///     },
///     WengLinRating {
///         rating: 21.0,
///         uncertainty: 6.5,
///     },
/// ];
///
/// let team_two = vec![
///     WengLinRating::default(),
///     WengLinRating {
///         rating: 41.0,
///         uncertainty: 1.4,
///     },
///     WengLinRating {
///         rating: 19.2,
///         uncertainty: 4.3,
///     },
/// ];
///
/// let (new_one, new_two) =
///     weng_lin_teams(&team_one, &team_two, &Outcomes::WIN, &WengLinConfig::new());
///
/// assert!(((new_one[0].rating * 100.0).round() - 2790.0).abs() < f64::EPSILON);
/// assert!(((new_one[1].rating * 100.0).round() - 3006.0).abs() < f64::EPSILON);
/// assert!(((new_one[2].rating * 100.0).round() - 2277.0).abs() < f64::EPSILON);
///
/// assert!(((new_two[0].rating * 100.0).round() - 2210.0).abs() < f64::EPSILON);
/// assert!(((new_two[1].rating * 100.0).round() - 4092.0).abs() < f64::EPSILON);
/// assert!(((new_two[2].rating * 100.0).round() - 1843.0).abs() < f64::EPSILON);
/// ```
pub fn weng_lin_teams(
    team_one: &Vec<WengLinRating>,
    team_two: &Vec<WengLinRating>,
    outcome: &Outcomes,
    config: &WengLinConfig,
) -> (Vec<WengLinRating>, Vec<WengLinRating>) {
    if team_one.is_empty() || team_two.is_empty() {
        return (team_one.clone(), team_two.clone());
    }

    let team_one_uncertainties: f64 = team_one.iter().map(|p| p.uncertainty.powi(2)).sum();
    let team_two_uncertainties: f64 = team_two.iter().map(|p| p.uncertainty.powi(2)).sum();

    let c = 2.0f64
        .mul_add(
            config.beta.powi(2),
            team_one_uncertainties + team_two_uncertainties,
        )
        .sqrt();

    let (p1, p2) = expected_score_teams(team_one, team_two, config);

    let outcome1 = outcome.to_chess_points();
    let outcome2 = 1.0 - outcome1;

    let mut new_team_one = Vec::new();
    let mut new_team_two = Vec::new();

    for player in team_one {
        let new_rating = new_rating_teams(
            player.rating,
            player.uncertainty,
            team_one_uncertainties,
            c,
            outcome1,
            p1,
        );
        let new_uncertainty = new_uncertainty_teams(
            player.uncertainty,
            team_one_uncertainties,
            c,
            p1,
            p2,
            config.uncertainty_tolerance,
        );

        new_team_one.push(WengLinRating {
            rating: new_rating,
            uncertainty: new_uncertainty,
        });
    }

    for player in team_two {
        let new_rating = new_rating_teams(
            player.rating,
            player.uncertainty,
            team_two_uncertainties,
            c,
            outcome2,
            p2,
        );
        let new_uncertainty = new_uncertainty_teams(
            player.uncertainty,
            team_two_uncertainties,
            c,
            p2,
            p1,
            config.uncertainty_tolerance,
        );

        new_team_two.push(WengLinRating {
            rating: new_rating,
            uncertainty: new_uncertainty,
        });
    }

    (new_team_one, new_team_two)
}

#[must_use]
/// Calculates the expected outcome of two players based on the Bradley-Terry model.
///
/// Takes in two players as [`WengLinRating`]s and a [`WengLinConfig`],
/// and returns the probability of victory for each player as an [`f64`] between 1.0 and 0.0.
///
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// Similar to [`expected_score_teams`].
///
/// # Examples
/// ```
/// use skillratings::weng_lin::{expected_score, WengLinConfig, WengLinRating};
///
/// let p1 = WengLinRating {
///     rating: 42.0,
///     uncertainty: 2.1,
/// };
/// let p2 = WengLinRating {
///     rating: 31.0,
///     uncertainty: 1.2,
/// };
///
/// let (exp1, exp2) = expected_score(&p1, &p2, &WengLinConfig::new());
///
/// assert!((exp1 + exp2 - 1.0).abs() < f64::EPSILON);
///
/// assert!(((exp1 * 100.0).round() - 85.0).abs() < f64::EPSILON);
/// ```
pub fn expected_score(
    player_one: &WengLinRating,
    player_two: &WengLinRating,
    config: &WengLinConfig,
) -> (f64, f64) {
    let c = 2.0f64
        .mul_add(
            config.beta.powi(2),
            player_one
                .uncertainty
                .mul_add(player_one.uncertainty, player_two.uncertainty.powi(2)),
        )
        .sqrt();

    let e1 = (player_one.rating / c).exp();
    let e2 = (player_two.rating / c).exp();

    let exp_one = e1 / (e1 + e2);
    let exp_two = 1.0 - exp_one;

    (exp_one, exp_two)
}

#[must_use]
#[allow(clippy::ptr_arg)]
/// Calculates the expected outcome of two teams based on the Bradley-Terry model.
///
/// Takes in two teams as Vec of [`WengLinRating`]s and a [`WengLinConfig`],
/// and returns the probability of victory for each player as an [`f64`] between 1.0 and 0.0.  
///
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// Similar to [`expected_score`].
///
/// # Examples
/// ```
/// use skillratings::weng_lin::{expected_score_teams, WengLinConfig, WengLinRating};
///
/// let team_one = vec![
///     WengLinRating {
///         rating: 42.0,
///         uncertainty: 2.1,
///     },
///     WengLinRating::new(),
///     WengLinRating {
///         rating: 12.0,
///         uncertainty: 3.2,
///     },
/// ];
/// let team_two = vec![
///     WengLinRating {
///         rating: 31.0,
///         uncertainty: 1.2,
///     },
///     WengLinRating::new(),
///     WengLinRating {
///         rating: 41.0,
///         uncertainty: 1.2,
///     },
/// ];
///
/// let (exp1, exp2) = expected_score_teams(&team_one, &team_two, &WengLinConfig::new());
///
/// assert!((exp1 + exp2 - 1.0).abs() < f64::EPSILON);
///
/// assert!(((exp1 * 100.0).round() - 21.0).abs() < f64::EPSILON);
/// ```
pub fn expected_score_teams(
    team_one: &Vec<WengLinRating>,
    team_two: &Vec<WengLinRating>,
    config: &WengLinConfig,
) -> (f64, f64) {
    let team_one_ratings: f64 = team_one.iter().map(|p| p.rating).sum();
    let team_two_ratings: f64 = team_two.iter().map(|p| p.rating).sum();

    let team_one_uncertainties: f64 = team_one.iter().map(|p| p.uncertainty.powi(2)).sum();
    let team_two_uncertainties: f64 = team_two.iter().map(|p| p.uncertainty.powi(2)).sum();

    let c = 2.0f64
        .mul_add(
            config.beta.powi(2),
            team_one_uncertainties + team_two_uncertainties,
        )
        .sqrt();

    let e1 = (team_one_ratings / c).exp();
    let e2 = (team_two_ratings / c).exp();

    let exp_one = e1 / (e1 + e2);
    let exp_two = 1.0 - exp_one;

    (exp_one, exp_two)
}

// We separate the 1v1 and teams functions, because we can use a few shortcuts on the 1v1 functions to increase performance.
fn new_rating(rating: f64, uncertainty: f64, c: f64, score: f64, p: f64) -> f64 {
    (uncertainty.powi(2) / c).mul_add(score - p, rating)
}

fn new_uncertainty(
    uncertainty: f64,
    c: f64,
    p: f64,
    opponent_p: f64,
    uncertainty_tolerance: f64,
) -> f64 {
    let eta = (uncertainty / c).powi(3) * p * opponent_p;

    (uncertainty.powi(2) * (1.0 - eta).max(uncertainty_tolerance)).sqrt()
}

fn new_rating_teams(
    rating: f64,
    uncertainty: f64,
    team_uncertainties: f64,
    c: f64,
    score: f64,
    p: f64,
) -> f64 {
    let delta = (team_uncertainties / c) * (score - p);

    (uncertainty.powi(2) / team_uncertainties).mul_add(delta, rating)
}

fn new_uncertainty_teams(
    uncertainty: f64,
    team_uncertainties: f64,
    c: f64,
    p: f64,
    opponent_p: f64,
    uncertainty_tolerance: f64,
) -> f64 {
    // You could also set gamma to 1/k, with k being the amount of teams in a match.
    // But you need to change the 1v1 uncertainty function above accordingly.
    let gamma = team_uncertainties.sqrt() / c;
    let eta = gamma * (team_uncertainties.sqrt() / c).powi(2) * p * opponent_p;
    let sigma = (1.0 - (uncertainty.powi(2) / team_uncertainties) * eta).max(uncertainty_tolerance);

    (uncertainty.powi(2) * sigma).sqrt()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_weng() {
        let p1 = WengLinRating::new();
        let p2 = WengLinRating::new();

        let (t1, t2) = weng_lin(&p1, &p2, &Outcomes::WIN, &WengLinConfig::new());

        assert!((t1.rating - 27.635_231_383_473_65).abs() < f64::EPSILON);
        assert!((t1.uncertainty - 8.065_506_316_323_548).abs() < f64::EPSILON);
        assert!((t2.rating - 22.364_768_616_526_35).abs() < f64::EPSILON);
        assert!((t2.uncertainty - 8.065_506_316_323_548).abs() < f64::EPSILON);

        let p1 = WengLinRating {
            rating: 42.0,
            uncertainty: 1.3,
        };
        let p2 = WengLinRating::new();

        let (t1, t2) = weng_lin(&p1, &p2, &Outcomes::WIN, &WengLinConfig::new());

        assert!((t1.rating - 42.026_412_401_802_894).abs() < f64::EPSILON);
        assert!((t1.uncertainty - 1.299_823_053_277_078_3).abs() < f64::EPSILON);
        assert!((t2.rating - 23.914_677_769_440_46).abs() < f64::EPSILON);
        assert!((t2.uncertainty - 8.029_022_445_649_298).abs() < f64::EPSILON);

        let (t1, t2) = weng_lin(&p1, &p2, &Outcomes::LOSS, &WengLinConfig::new());

        assert!((t1.rating - 41.862_153_998_286_94).abs() < f64::EPSILON);
        assert!((t1.uncertainty - 1.299_823_053_277_078_3).abs() < f64::EPSILON);
        assert!((t2.rating - 30.664_283_436_598_35).abs() < f64::EPSILON);
        assert!((t2.uncertainty - 8.029_022_445_649_298).abs() < f64::EPSILON);

        let (t1, t2) = weng_lin(&p1, &p2, &Outcomes::DRAW, &WengLinConfig::new());

        assert!((t1.rating - 41.944_283_200_044_92).abs() < f64::EPSILON);
        assert!((t1.uncertainty - 1.299_823_053_277_078_3).abs() < f64::EPSILON);
        assert!((t2.rating - 27.289_480_603_019_403).abs() < f64::EPSILON);
        assert!((t2.uncertainty - 8.029_022_445_649_298).abs() < f64::EPSILON);
    }

    #[test]
    #[allow(clippy::cognitive_complexity)]
    fn test_weng_teams() {
        let t1 = vec![
            WengLinRating::new(),
            WengLinRating {
                rating: 30.0,
                uncertainty: 1.2,
            },
            WengLinRating {
                rating: 21.0,
                uncertainty: 6.5,
            },
        ];

        let t2 = vec![
            WengLinRating::default(),
            WengLinRating {
                rating: 41.0,
                uncertainty: 1.4,
            },
            WengLinRating {
                rating: 19.2,
                uncertainty: 4.3,
            },
        ];

        let (nt1, nt2) = weng_lin_teams(&t1, &t2, &Outcomes::WIN, &WengLinConfig::new());

        assert!((nt1[0].rating - 27.904_443_970_057_24).abs() < f64::EPSILON);
        assert!((nt1[1].rating - 30.060_226_550_163_108).abs() < f64::EPSILON);
        assert!((nt1[2].rating - 22.767_063_711_382_825).abs() < f64::EPSILON);

        assert!((nt2[0].rating - 22.095_556_029_942_76).abs() < f64::EPSILON);
        assert!((nt2[1].rating - 40.918_024_973_389_1).abs() < f64::EPSILON);
        assert!((nt2[2].rating - 18.426_674_366_308_44).abs() < f64::EPSILON);

        assert!((nt1[0].uncertainty - 8.138_803_466_450_47).abs() < f64::EPSILON);
        assert!((nt1[1].uncertainty - 1.199_425_779_255_630_7).abs() < f64::EPSILON);
        assert!((nt1[2].uncertainty - 6.408_113_466_768_933).abs() < f64::EPSILON);

        assert!((nt2[0].uncertainty - 8.160_155_338_979_159).abs() < f64::EPSILON);
        assert!((nt2[1].uncertainty - 1.399_187_149_975_365_4).abs() < f64::EPSILON);
        assert!((nt2[2].uncertainty - 4.276_389_807_576_043).abs() < f64::EPSILON);

        let (nt1, nt2) = weng_lin_teams(&t1, &t2, &Outcomes::DRAW, &WengLinConfig::new());

        assert!((nt1[0].rating - 25.652_558_832_338_293).abs() < f64::EPSILON);
        assert!((nt1[1].rating - 30.013_531_459_947_366).abs() < f64::EPSILON);
        assert!((nt1[2].rating - 21.397_016_793_594_62).abs() < f64::EPSILON);

        assert!((nt2[0].rating - 24.347_441_167_661_707).abs() < f64::EPSILON);
        assert!((nt2[1].rating - 40.981_582_179_516_08).abs() < f64::EPSILON);
        assert!((nt2[2].rating - 19.026_252_295_536_935).abs() < f64::EPSILON);

        // The uncertainties do not change.
        assert!((nt1[0].uncertainty - 8.138_803_466_450_47).abs() < f64::EPSILON);

        let (nt1, nt2) = weng_lin_teams(&t1, &t2, &Outcomes::LOSS, &WengLinConfig::default());

        assert!((nt1[0].rating - 23.400_673_694_619_35).abs() < f64::EPSILON);
        assert!((nt1[1].rating - 29.966_836_369_731_627).abs() < f64::EPSILON);
        assert!((nt1[2].rating - 20.026_969_875_806_41).abs() < f64::EPSILON);

        assert!((nt2[0].rating - 26.599_326_305_380_65).abs() < f64::EPSILON);
        assert!((nt2[1].rating - 41.045_139_385_643_06).abs() < f64::EPSILON);
        assert!((nt2[2].rating - 19.625_830_224_765_43).abs() < f64::EPSILON);
    }

    #[test]
    fn test_empty_team() {
        let t1 = vec![WengLinRating::new()];
        let t2 = Vec::new();

        let (nt1, nt2) = weng_lin_teams(&t1, &t2, &Outcomes::DRAW, &WengLinConfig::new());

        assert_eq!(t1, nt1);
        assert_eq!(t2, nt2);
    }

    #[test]
    #[allow(clippy::similar_names)]
    fn test_1v1_team() {
        let player_one = WengLinRating {
            rating: 20.0,
            uncertainty: 3.2,
        };
        let player_two = WengLinRating {
            rating: 22.2,
            uncertainty: 4.1,
        };

        let config = WengLinConfig::new();

        let outcome = Outcomes::WIN;

        let (op1, op2) = weng_lin(&player_one, &player_two, &outcome, &config);
        let (tp1, tp2) = weng_lin_teams(&vec![player_one], &vec![player_two], &outcome, &config);

        assert_eq!(op1, tp1[0]);
        assert_eq!(op2, tp2[0]);
    }

    #[test]
    fn test_expected() {
        let p1 = WengLinRating::new();
        let p2 = WengLinRating::new();

        let (exp1, exp2) = expected_score(&p1, &p2, &WengLinConfig::new());

        assert!((exp1 - exp2).abs() < f64::EPSILON);

        let p1 = WengLinRating {
            rating: 42.0,
            uncertainty: 2.1,
        };
        let p2 = WengLinRating {
            rating: 31.0,
            uncertainty: 1.2,
        };

        let (exp1, exp2) = expected_score(&p1, &p2, &WengLinConfig::new());

        assert!((exp1 + exp2 - 1.0).abs() < f64::EPSILON);

        assert!((exp1 - 0.849_021_123_412_260_5).abs() < f64::EPSILON);
        assert!((exp2 - 0.150_978_876_587_739_42).abs() < f64::EPSILON);
    }

    #[test]
    fn test_expected_teams() {
        let p1 = vec![WengLinRating::new()];
        let p2 = vec![WengLinRating::new()];

        let (exp1, exp2) = expected_score_teams(&p1, &p2, &WengLinConfig::new());

        assert!((exp1 - exp2).abs() < f64::EPSILON);

        let mut p1 = vec![WengLinRating {
            rating: 42.0,
            uncertainty: 2.1,
        }];
        let mut p2 = vec![WengLinRating {
            rating: 31.0,
            uncertainty: 1.2,
        }];

        let (exp1, exp2) = expected_score_teams(&p1, &p2, &WengLinConfig::new());

        assert!((exp1 + exp2 - 1.0).abs() < f64::EPSILON);

        assert!((exp1 - 0.849_021_123_412_260_5).abs() < f64::EPSILON);
        assert!((exp2 - 0.150_978_876_587_739_42).abs() < f64::EPSILON);

        p1.push(WengLinRating::new());
        p1.push(WengLinRating {
            rating: 12.0,
            uncertainty: 3.2,
        });

        p2.push(WengLinRating {
            rating: 41.0,
            uncertainty: 1.2,
        });

        let (exp1, exp2) = expected_score_teams(&p1, &p2, &WengLinConfig::new());

        assert!((exp1 + exp2 - 1.0).abs() < f64::EPSILON);

        // Even if they are better, team two is a player down.
        assert!((exp1 - 0.653_518_078_332_893_4).abs() < f64::EPSILON);

        p2.push(WengLinRating::new());

        let (exp1, _) = expected_score_teams(&p1, &p2, &WengLinConfig::new());

        assert!((exp1 - 0.213_836_440_502_453_18).abs() < f64::EPSILON);
    }

    #[test]
    fn test_rating_period() {
        let player = WengLinRating::new();

        let opponent_one = WengLinRating::new();
        let opponent_two = WengLinRating {
            rating: 12.0,
            uncertainty: 4.2,
        };

        let (normal_player, _) = weng_lin(
            &player,
            &opponent_one,
            &Outcomes::WIN,
            &WengLinConfig::new(),
        );
        let (normal_player, _) = weng_lin(
            &normal_player,
            &opponent_two,
            &Outcomes::DRAW,
            &WengLinConfig::new(),
        );
        let (normal_player, _) = weng_lin(
            &normal_player,
            &opponent_two,
            &Outcomes::LOSS,
            &WengLinConfig::new(),
        );

        let rating_player = weng_lin_rating_period(
            &player,
            &vec![
                (opponent_one, Outcomes::WIN),
                (opponent_two, Outcomes::DRAW),
                (opponent_two, Outcomes::LOSS),
            ],
            &WengLinConfig::new(),
        );

        assert!((normal_player.rating - rating_player.rating).abs() < f64::EPSILON);
        assert!((normal_player.uncertainty - rating_player.uncertainty).abs() < f64::EPSILON);
    }

    #[test]
    fn trueskill_conversion() {
        let weng_lin_player = WengLinRating::new();

        let trueskill_player = TrueSkillRating::from(weng_lin_player);

        assert_eq!(trueskill_player, TrueSkillRating::new());

        let other_weng_lin_player = WengLinRating::from(TrueSkillRating {
            rating: 35.0,
            uncertainty: 4.0,
        });

        assert!((other_weng_lin_player.rating - 35.0).abs() < f64::EPSILON);
    }
}
