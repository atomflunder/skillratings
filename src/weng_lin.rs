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
//! // to achieve a ~67% win-rate over another player.
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

use crate::{
    trueskill::TrueSkillRating, MultiTeamOutcome, MultiTeamRatingSystem, Outcomes, Rating,
    RatingPeriodSystem, RatingSystem, TeamRatingSystem,
};
use std::cmp::Ordering;

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

impl Rating for WengLinRating {
    fn rating(&self) -> f64 {
        self.rating
    }
    fn uncertainty(&self) -> Option<f64> {
        Some(self.uncertainty)
    }
    fn new(rating: Option<f64>, uncertainty: Option<f64>) -> Self {
        Self {
            rating: rating.unwrap_or(25.0),
            uncertainty: uncertainty.unwrap_or(25.0 / 3.0),
        }
    }
}

impl From<(f64, f64)> for WengLinRating {
    fn from((r, u): (f64, f64)) -> Self {
        Self {
            rating: r,
            uncertainty: u,
        }
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
    /// needed to have a ~67% win probability against another player.  
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

/// Struct to calculate ratings and expected score for [`WengLinRating`]
pub struct WengLin {
    config: WengLinConfig,
}

impl RatingSystem for WengLin {
    type RATING = WengLinRating;
    type CONFIG = WengLinConfig;

    fn new(config: Self::CONFIG) -> Self {
        Self { config }
    }

    fn rate(
        &self,
        player_one: &WengLinRating,
        player_two: &WengLinRating,
        outcome: &Outcomes,
    ) -> (WengLinRating, WengLinRating) {
        weng_lin(player_one, player_two, outcome, &self.config)
    }

    fn expected_score(&self, player_one: &WengLinRating, player_two: &WengLinRating) -> (f64, f64) {
        expected_score(player_one, player_two, &self.config)
    }
}

impl RatingPeriodSystem for WengLin {
    type RATING = WengLinRating;
    type CONFIG = WengLinConfig;

    fn new(config: Self::CONFIG) -> Self {
        Self { config }
    }

    fn rate(&self, player: &WengLinRating, results: &[(WengLinRating, Outcomes)]) -> WengLinRating {
        weng_lin_rating_period(player, results, &self.config)
    }
}

impl TeamRatingSystem for WengLin {
    type RATING = WengLinRating;
    type CONFIG = WengLinConfig;

    fn new(config: Self::CONFIG) -> Self {
        Self { config }
    }

    fn rate(
        &self,
        team_one: &[WengLinRating],
        team_two: &[WengLinRating],
        outcome: &Outcomes,
    ) -> (Vec<WengLinRating>, Vec<WengLinRating>) {
        weng_lin_two_teams(team_one, team_two, outcome, &self.config)
    }

    fn expected_score(&self, team_one: &[Self::RATING], team_two: &[Self::RATING]) -> (f64, f64) {
        expected_score_two_teams(team_one, team_two, &self.config)
    }
}

impl MultiTeamRatingSystem for WengLin {
    type RATING = WengLinRating;
    type CONFIG = WengLinConfig;

    fn new(config: Self::CONFIG) -> Self {
        Self { config }
    }

    fn rate(
        &self,
        teams_and_ranks: &[(&[Self::RATING], MultiTeamOutcome)],
    ) -> Vec<Vec<WengLinRating>> {
        weng_lin_multi_team(teams_and_ranks, &self.config)
    }

    fn expected_score(&self, teams: &[&[Self::RATING]]) -> Vec<f64> {
        expected_score_multi_team(teams, &self.config)
    }
}

#[must_use]
/// Calculates the [`WengLinRating`]s of two players based on their old ratings, uncertainties, and the outcome of the game.
///
/// Takes in two players as [`WengLinRating`]s, an [`Outcome`](Outcomes), and a [`WengLinConfig`].
///
/// The outcome of the match is in the perspective of `player_one`.
/// This means [`Outcomes::WIN`] is a win for `player_one` and [`Outcomes::LOSS`] is a win for `player_two`.
///
/// Similar to [`weng_lin_rating_period`] and [`weng_lin_two_teams`].
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

    let (p1, p2) = p_value(player_one.rating, player_two.rating, c);

    let outcome1 = outcome.to_chess_points();
    let outcome2 = 1.0 - outcome1;

    let new_rating1 = new_rating(player_one.rating, player_one.uncertainty, c, p1, outcome1);
    let new_rating2 = new_rating(player_two.rating, player_two.uncertainty, c, p2, outcome2);

    let new_uncertainty1 =
        new_uncertainty(player_one.uncertainty, c, p1, config.uncertainty_tolerance);
    let new_uncertainty2 =
        new_uncertainty(player_two.uncertainty, c, p2, config.uncertainty_tolerance);

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
/// Takes in a player as an [`WengLinRating`] and their results as a Slice of tuples containing the opponent as an [`WengLinRating`],
/// the outcome of the game as an [`Outcome`](Outcomes) and a [`WengLinConfig`].
///
/// The outcome of the match is in the perspective of the player.
/// This means [`Outcomes::WIN`] is a win for the player and [`Outcomes::LOSS`] is a win for the opponent.
///
/// Similar to [`weng_lin`] or [`weng_lin_two_teams`].
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
    results: &[(WengLinRating, Outcomes)],
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

        let (p, _) = p_value(player_rating, opponent.rating, c);
        let outcome = result.to_chess_points();

        player_rating = new_rating(player_rating, player_uncertainty, c, p, outcome);
        player_uncertainty =
            new_uncertainty(player_uncertainty, c, p, config.uncertainty_tolerance);
    }

    WengLinRating {
        rating: player_rating,
        uncertainty: player_uncertainty,
    }
}

#[must_use]
/// Calculates the [`WengLinRating`] of two teams based on their ratings, uncertainties, and the outcome of the game.
///
/// Takes in two teams as a Slice of [`WengLinRating`]s, the outcome of the game as an [`Outcome`](Outcomes) and a [`WengLinConfig`].
///
/// The outcome of the match is in the perspective of `team_one`.
/// This means [`Outcomes::WIN`] is a win for `team_one` and [`Outcomes::LOSS`] is a win for `team_two`.
///
/// Similar to [`weng_lin`].
///
/// # Examples
/// ```
/// use skillratings::{
///     weng_lin::{weng_lin_two_teams, WengLinConfig, WengLinRating},
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
///     weng_lin_two_teams(&team_one, &team_two, &Outcomes::WIN, &WengLinConfig::new());
///
/// assert!(((new_one[0].rating * 100.0).round() - 2790.0).abs() < f64::EPSILON);
/// assert!(((new_one[1].rating * 100.0).round() - 3006.0).abs() < f64::EPSILON);
/// assert!(((new_one[2].rating * 100.0).round() - 2277.0).abs() < f64::EPSILON);
///
/// assert!(((new_two[0].rating * 100.0).round() - 2210.0).abs() < f64::EPSILON);
/// assert!(((new_two[1].rating * 100.0).round() - 4092.0).abs() < f64::EPSILON);
/// assert!(((new_two[2].rating * 100.0).round() - 1843.0).abs() < f64::EPSILON);
/// ```
pub fn weng_lin_two_teams(
    team_one: &[WengLinRating],
    team_two: &[WengLinRating],
    outcome: &Outcomes,
    config: &WengLinConfig,
) -> (Vec<WengLinRating>, Vec<WengLinRating>) {
    if team_one.is_empty() || team_two.is_empty() {
        return (team_one.to_vec(), team_two.to_vec());
    }

    let team_one_rating: f64 = team_one.iter().map(|p| p.rating).sum();
    let team_two_rating: f64 = team_two.iter().map(|p| p.rating).sum();

    let team_one_uncertainty_sq: f64 = team_one.iter().map(|p| p.uncertainty.powi(2)).sum();
    let team_two_uncertainty_sq: f64 = team_two.iter().map(|p| p.uncertainty.powi(2)).sum();

    let c = 2.0f64
        .mul_add(
            config.beta.powi(2),
            team_one_uncertainty_sq + team_two_uncertainty_sq,
        )
        .sqrt();

    let (p1, p2) = p_value(team_one_rating, team_two_rating, c);

    let outcome1 = outcome.to_chess_points();
    let outcome2 = 1.0 - outcome1;

    // Small delta is equivalent to omega as there are only two teams.
    let team_one_small_delta = small_delta(team_one_uncertainty_sq, c, p1, outcome1);
    let team_two_small_delta = small_delta(team_two_uncertainty_sq, c, p2, outcome2);

    // Eta is equivalent to large delta as there are only two teams.
    let team_one_eta = eta(
        team_one_uncertainty_sq,
        c,
        p1,
        gamma(team_one_uncertainty_sq, c),
    );
    let team_two_eta = eta(
        team_two_uncertainty_sq,
        c,
        p2,
        gamma(team_two_uncertainty_sq, c),
    );

    let mut new_team_one = Vec::new();
    let mut new_team_two = Vec::new();

    for player in team_one {
        let player_uncertainty_sq = player.uncertainty.powi(2);
        let new_rating = new_rating_teams(
            player.rating,
            player_uncertainty_sq,
            team_one_uncertainty_sq,
            team_one_small_delta,
        );
        let new_uncertainty = new_uncertainty_teams(
            player_uncertainty_sq,
            team_one_uncertainty_sq,
            config.uncertainty_tolerance,
            team_one_eta,
        );

        new_team_one.push(WengLinRating {
            rating: new_rating,
            uncertainty: new_uncertainty,
        });
    }

    for player in team_two {
        let player_uncertainty_sq = player.uncertainty.powi(2);
        let new_rating = new_rating_teams(
            player.rating,
            player_uncertainty_sq,
            team_two_uncertainty_sq,
            team_two_small_delta,
        );
        let new_uncertainty = new_uncertainty_teams(
            player_uncertainty_sq,
            team_two_uncertainty_sq,
            config.uncertainty_tolerance,
            team_two_eta,
        );

        new_team_two.push(WengLinRating {
            rating: new_rating,
            uncertainty: new_uncertainty,
        });
    }

    (new_team_one, new_team_two)
}

#[must_use]
/// Calculates the [`WengLinRating`] of several teams based on their ratings, uncertainties, and ranks of the teams.
///
/// Takes in a slice, which contains tuples of teams, which are just slices of [`WengLinRating`]s,
/// as well the rank of the team as an [`MultiTeamOutcome`] and a [`WengLinConfig`].
///
/// Ties are represented by several teams having the same rank.
///
/// Returns new ratings and uncertainties of players in the teams in the same order.
///
/// Similar to [`weng_lin_two_teams`].
///
/// # Examples
/// ```
/// use skillratings::{
///     weng_lin::{weng_lin_multi_team, WengLinConfig, WengLinRating},
///     MultiTeamOutcome,
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
/// let team_three = vec![
///     WengLinRating::default(),
///     WengLinRating {
///         rating: 29.4,
///         uncertainty: 1.6,
///     },
///     WengLinRating {
///         rating: 17.2,
///         uncertainty: 2.1,
///     },
/// ];
///
/// let teams_and_ranks = vec![
///     (&team_one[..], MultiTeamOutcome::new(2)), // Team 1 takes the second place.
///     (&team_two[..], MultiTeamOutcome::new(1)), // Team 2 takes the first place.
///     (&team_three[..], MultiTeamOutcome::new(3)), // Team 3 takes the third place.
/// ];
///
/// let new_teams = weng_lin_multi_team(&teams_and_ranks, &WengLinConfig::new());
///
/// assert_eq!(new_teams.len(), 3);
///
/// let new_one = &new_teams[0];
/// let new_two = &new_teams[1];
/// let new_three = &new_teams[2];
///
/// assert!(((new_one[0].rating * 100.0).round() - 2538.0).abs() < f64::EPSILON);
/// assert!(((new_one[1].rating * 100.0).round() - 3001.0).abs() < f64::EPSILON);
/// assert!(((new_one[2].rating * 100.0).round() - 2123.0).abs() < f64::EPSILON);
///
/// assert!(((new_two[0].rating * 100.0).round() - 2796.0).abs() < f64::EPSILON);
/// assert!(((new_two[1].rating * 100.0).round() - 4108.0).abs() < f64::EPSILON);
/// assert!(((new_two[2].rating * 100.0).round() - 1999.0).abs() < f64::EPSILON);
///
/// assert!(((new_three[0].rating * 100.0).round() - 2166.0).abs() < f64::EPSILON);
/// assert!(((new_three[1].rating * 100.0).round() - 2928.0).abs() < f64::EPSILON);
/// assert!(((new_three[2].rating * 100.0).round() - 1699.0).abs() < f64::EPSILON);
/// ```
pub fn weng_lin_multi_team(
    teams_and_ranks: &[(&[WengLinRating], MultiTeamOutcome)],
    config: &WengLinConfig,
) -> Vec<Vec<WengLinRating>> {
    if teams_and_ranks.is_empty() {
        return Vec::new();
    }

    // Just returning the original teams if a team is empty.
    for (team, _) in teams_and_ranks {
        if team.is_empty() {
            return teams_and_ranks
                .iter()
                .map(|(team, _)| team.to_vec())
                .collect();
        }
    }

    let mut teams_ratings = Vec::with_capacity(teams_and_ranks.len());
    let mut teams_uncertainties_sq = Vec::with_capacity(teams_and_ranks.len());

    for (team, _) in teams_and_ranks {
        let team_rating: f64 = team.iter().map(|p| p.rating).sum();
        let team_uncertainty_sq: f64 = team.iter().map(|p| p.uncertainty.powi(2)).sum();

        teams_ratings.push(team_rating);
        teams_uncertainties_sq.push(team_uncertainty_sq);
    }

    let mut new_teams = Vec::with_capacity(teams_and_ranks.len());
    for (i, (team_one, rank_one)) in teams_and_ranks.iter().enumerate() {
        let mut omega = 0.0;
        let mut large_delta = 0.0;

        for (q, (_, rank_two)) in teams_and_ranks.iter().enumerate() {
            if i == q {
                continue;
            }

            let c = 2.0f64
                .mul_add(
                    config.beta.powi(2),
                    teams_uncertainties_sq[i] + teams_uncertainties_sq[q],
                )
                .sqrt();

            let (p, _) = p_value(teams_ratings[i], teams_ratings[q], c);
            let score = match rank_two.cmp(rank_one) {
                Ordering::Greater => 1.0,
                Ordering::Equal => 0.5,
                Ordering::Less => 0.0,
            };

            let small_delta = small_delta(teams_uncertainties_sq[i], c, p, score);
            let eta = eta(
                teams_uncertainties_sq[i],
                c,
                p,
                gamma(teams_uncertainties_sq[i], c),
            );

            omega += small_delta;
            large_delta += eta;
        }

        let mut new_team = Vec::with_capacity(team_one.len());
        for player in *team_one {
            let player_uncertainty_sq = player.uncertainty.powi(2);
            let new_rating = new_rating_teams(
                player.rating,
                player_uncertainty_sq,
                teams_uncertainties_sq[i],
                omega,
            );
            let new_uncertainty = new_uncertainty_teams(
                player_uncertainty_sq,
                teams_uncertainties_sq[i],
                config.uncertainty_tolerance,
                large_delta,
            );

            new_team.push(WengLinRating {
                rating: new_rating,
                uncertainty: new_uncertainty,
            });
        }
        new_teams.push(new_team);
    }

    new_teams
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
/// Similar to [`expected_score_two_teams`] and [`expected_score_multi_team`].
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

    p_value(player_one.rating, player_two.rating, c)
}

#[must_use]
/// Calculates the expected outcome of two teams based on the Bradley-Terry model.
///
/// Takes in two teams as a Slice of [`WengLinRating`]s and a [`WengLinConfig`],
/// and returns the probability of victory for each player as an [`f64`] between 1.0 and 0.0.  
///
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// Similar to [`expected_score`] and [`expected_score_multi_team`].
///
/// # Examples
/// ```
/// use skillratings::weng_lin::{expected_score_two_teams, WengLinConfig, WengLinRating};
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
/// let (exp1, exp2) = expected_score_two_teams(&team_one, &team_two, &WengLinConfig::new());
///
/// assert!((exp1 + exp2 - 1.0).abs() < f64::EPSILON);
///
/// assert!(((exp1 * 100.0).round() - 21.0).abs() < f64::EPSILON);
/// ```
pub fn expected_score_two_teams(
    team_one: &[WengLinRating],
    team_two: &[WengLinRating],
    config: &WengLinConfig,
) -> (f64, f64) {
    let team_one_rating: f64 = team_one.iter().map(|p| p.rating).sum();
    let team_two_rating: f64 = team_two.iter().map(|p| p.rating).sum();

    let team_one_uncertainty_sq: f64 = team_one.iter().map(|p| p.uncertainty.powi(2)).sum();
    let team_two_uncertainty_sq: f64 = team_two.iter().map(|p| p.uncertainty.powi(2)).sum();

    let c = 2.0f64
        .mul_add(
            config.beta.powi(2),
            team_one_uncertainty_sq + team_two_uncertainty_sq,
        )
        .sqrt();

    p_value(team_one_rating, team_two_rating, c)
}

#[must_use]
/// Calculates the expected outcome of mulitple teams based on the Bradley-Terry model.
///
/// Takes in a slice of teams as a slice of [`WengLinRating`]s and a [`WengLinConfig`],
/// and returns the probability of victory for each team as an [`f64`] between 1.0 and 0.0.
///
/// 1.0 means a certain victory for the team, 0.0 means certain loss.
/// Values near `1 / Number of Teams` mean a draw is likely to occur.
///
/// Similar to [`expected_score`] and [`expected_score_two_teams`].
///
/// # Examples
/// ```
/// use skillratings::weng_lin::{expected_score_multi_team, WengLinConfig, WengLinRating};
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
/// let team_three = vec![
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
/// let exp =
///     expected_score_multi_team(&[&team_one, &team_two, &team_three], &WengLinConfig::new());
///
/// assert!((exp[0] + exp[1] + exp[2] - 1.0).abs() < f64::EPSILON);
/// assert_eq!((exp[0] * 100.0).round(), 14.0);
/// assert_eq!((exp[1] * 100.0).round(), 43.0);
/// assert_eq!((exp[2] * 100.0).round(), 43.0);
/// ```
pub fn expected_score_multi_team(teams: &[&[WengLinRating]], config: &WengLinConfig) -> Vec<f64> {
    let mut ratings = Vec::with_capacity(teams.len());

    for team in teams {
        let team_rating: f64 = team.iter().map(|p| p.rating).sum();
        ratings.push(team_rating);
    }

    let mut uncertainties_sq = Vec::with_capacity(teams.len());

    for team in teams {
        let team_uncertainty_sq: f64 = team.iter().map(|p| p.uncertainty.powi(2)).sum();
        uncertainties_sq.push(team_uncertainty_sq);
    }

    let c = 2.0f64
        .mul_add(config.beta.powi(2), uncertainties_sq.iter().sum::<f64>())
        .sqrt();

    let mut exps = Vec::with_capacity(ratings.len());

    let mut sum = 0.0;

    for rating in ratings {
        let e = (rating / c).exp();
        exps.push(e);
        sum += e;
    }

    for exp in &mut exps {
        *exp /= sum;
    }

    exps
}

fn p_value(rating_one: f64, rating_two: f64, c_value: f64) -> (f64, f64) {
    let e1 = (rating_one / c_value).exp();
    let e2 = (rating_two / c_value).exp();

    let exp_one = e1 / (e1 + e2);
    let exp_two = 1.0 - exp_one;

    (exp_one, exp_two)
}

fn small_delta(team_uncertainty_sq: f64, c_value: f64, p_value: f64, score: f64) -> f64 {
    (team_uncertainty_sq / c_value) * (score - p_value)
}

// You could also set gamma to 1/k, with k being the amount of teams in a match.
// But you need to change the 1v1 uncertainty function below accordingly.
fn gamma(team_uncertainty_sq: f64, c_value: f64) -> f64 {
    team_uncertainty_sq.sqrt() / c_value
}

fn eta(team_uncertainty_sq: f64, c_value: f64, p_value: f64, gamma: f64) -> f64 {
    gamma * team_uncertainty_sq / c_value.powi(2) * p_value * (1.0 - p_value)
}

// We separate the 1v1 and teams functions, because we can use a few shortcuts on the 1v1 functions to increase performance.
fn new_rating(
    player_rating: f64,
    player_uncertainty: f64,
    c_value: f64,
    p_value: f64,
    score: f64,
) -> f64 {
    (player_uncertainty.powi(2) / c_value).mul_add(score - p_value, player_rating)
}

fn new_uncertainty(
    player_uncertainty: f64,
    c_value: f64,
    p_value: f64,
    uncertainty_tolerance: f64,
) -> f64 {
    let eta = (player_uncertainty / c_value).powi(3) * p_value * (1.0 - p_value);
    (player_uncertainty.powi(2) * (1.0 - eta).max(uncertainty_tolerance)).sqrt()
}

fn new_rating_teams(
    player_rating: f64,
    player_uncertainty_sq: f64,
    team_uncertainty_sq: f64,
    omega: f64,
) -> f64 {
    (player_uncertainty_sq / team_uncertainty_sq).mul_add(omega, player_rating)
}

fn new_uncertainty_teams(
    player_uncertainty_sq: f64,
    team_uncertainty_sq: f64,
    uncertainty_tolerance: f64,
    large_delta: f64,
) -> f64 {
    let new_player_uncertainty_sq = (1.0
        - ((player_uncertainty_sq / team_uncertainty_sq) * large_delta))
        .max(uncertainty_tolerance);
    (player_uncertainty_sq * new_player_uncertainty_sq).sqrt()
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
    fn test_weng_two_teams() {
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

        let (nt1, nt2) = weng_lin_two_teams(&t1, &t2, &Outcomes::WIN, &WengLinConfig::new());

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

        let (nt1, nt2) = weng_lin_two_teams(&t1, &t2, &Outcomes::DRAW, &WengLinConfig::new());

        assert!((nt1[0].rating - 25.652_558_832_338_293).abs() < f64::EPSILON);
        assert!((nt1[1].rating - 30.013_531_459_947_366).abs() < f64::EPSILON);
        assert!((nt1[2].rating - 21.397_016_793_594_62).abs() < f64::EPSILON);

        assert!((nt2[0].rating - 24.347_441_167_661_707).abs() < f64::EPSILON);
        assert!((nt2[1].rating - 40.981_582_179_516_08).abs() < f64::EPSILON);
        assert!((nt2[2].rating - 19.026_252_295_536_935).abs() < f64::EPSILON);

        // The uncertainties do not change.
        assert!((nt1[0].uncertainty - 8.138_803_466_450_47).abs() < f64::EPSILON);

        let (nt1, nt2) = weng_lin_two_teams(&t1, &t2, &Outcomes::LOSS, &WengLinConfig::default());

        assert!((nt1[0].rating - 23.400_673_694_619_35).abs() < f64::EPSILON);
        assert!((nt1[1].rating - 29.966_836_369_731_627).abs() < f64::EPSILON);
        assert!((nt1[2].rating - 20.026_969_875_806_41).abs() < f64::EPSILON);

        assert!((nt2[0].rating - 26.599_326_305_380_65).abs() < f64::EPSILON);
        assert!((nt2[1].rating - 41.045_139_385_643_06).abs() < f64::EPSILON);
        assert!((nt2[2].rating - 19.625_830_224_765_43).abs() < f64::EPSILON);
    }

    #[test]
    #[allow(clippy::cognitive_complexity)]
    fn test_weng_multi_team_two() {
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

        let game = vec![
            (&t1[..], MultiTeamOutcome::new(1)),
            (&t2[..], MultiTeamOutcome::new(2)),
        ];

        let results = weng_lin_multi_team(&game, &WengLinConfig::new());

        assert_eq!(results.len(), 2);

        let nt1 = &results[0];
        let nt2 = &results[1];

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

        let game = vec![
            (&t1[..], MultiTeamOutcome::new(1)),
            (&t2[..], MultiTeamOutcome::new(1)),
        ];

        let results = weng_lin_multi_team(&game, &WengLinConfig::new());

        assert_eq!(results.len(), 2);

        let nt1 = &results[0];
        let nt2 = &results[1];

        assert!((nt1[0].rating - 25.652_558_832_338_293).abs() < f64::EPSILON);
        assert!((nt1[1].rating - 30.013_531_459_947_366).abs() < f64::EPSILON);
        assert!((nt1[2].rating - 21.397_016_793_594_62).abs() < f64::EPSILON);

        assert!((nt2[0].rating - 24.347_441_167_661_707).abs() < f64::EPSILON);
        assert!((nt2[1].rating - 40.981_582_179_516_08).abs() < f64::EPSILON);
        assert!((nt2[2].rating - 19.026_252_295_536_935).abs() < f64::EPSILON);

        // The uncertainties do not change.
        assert!((nt1[0].uncertainty - 8.138_803_466_450_47).abs() < f64::EPSILON);

        let game = vec![
            (&t1[..], MultiTeamOutcome::new(2)),
            (&t2[..], MultiTeamOutcome::new(1)),
        ];

        let results = weng_lin_multi_team(&game, &WengLinConfig::new());

        assert_eq!(results.len(), 2);

        let nt1 = &results[0];
        let nt2 = &results[1];

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

        let (nt1, nt2) = weng_lin_two_teams(&t1, &t2, &Outcomes::DRAW, &WengLinConfig::new());

        assert_eq!(t1, nt1);
        assert_eq!(t2, nt2);

        let game = vec![
            (&t1[..], MultiTeamOutcome::new(1)),
            (&t2[..], MultiTeamOutcome::new(1)),
        ];

        let results = weng_lin_multi_team(&game, &WengLinConfig::new());

        assert_eq!(results.len(), 2);
        assert_eq!(results[0], t1);
        assert_eq!(results[1], t2);

        let result = weng_lin_multi_team(&[], &WengLinConfig::new());
        assert_eq!(result.len(), 0);
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
        let (tp1, tp2) = weng_lin_two_teams(&[player_one], &[player_two], &outcome, &config);

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

        let (exp1, exp2) = expected_score_two_teams(&p1, &p2, &WengLinConfig::new());

        assert!((exp1 - exp2).abs() < f64::EPSILON);

        let mut p1 = vec![WengLinRating {
            rating: 42.0,
            uncertainty: 2.1,
        }];
        let mut p2 = vec![WengLinRating {
            rating: 31.0,
            uncertainty: 1.2,
        }];

        let (exp1, exp2) = expected_score_two_teams(&p1, &p2, &WengLinConfig::new());

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

        let (exp1, exp2) = expected_score_two_teams(&p1, &p2, &WengLinConfig::new());

        assert!((exp1 + exp2 - 1.0).abs() < f64::EPSILON);

        // Even if they are better, team two is a player down.
        assert!((exp1 - 0.653_518_078_332_893_4).abs() < f64::EPSILON);

        p2.push(WengLinRating::new());

        let (exp1, _) = expected_score_two_teams(&p1, &p2, &WengLinConfig::new());

        assert!((exp1 - 0.213_836_440_502_453_18).abs() < f64::EPSILON);
    }

    #[test]
    fn test_expected_score_multi_teams() {
        let team_one = vec![WengLinRating::new()];
        let team_two = vec![WengLinRating::new()];
        let team_three = vec![WengLinRating::new()];
        let team_four = vec![WengLinRating::new()];

        let exp = expected_score_multi_team(
            &[&team_one, &team_two, &team_three, &team_four],
            &WengLinConfig::new(),
        );

        assert_eq!(exp.len(), 4);
        assert!((exp.iter().sum::<f64>() - 1.0).abs() < f64::EPSILON);
        assert!((exp[0] - 0.25).abs() < f64::EPSILON);
        assert!((exp[1] - 0.25).abs() < f64::EPSILON);
        assert!((exp[2] - 0.25).abs() < f64::EPSILON);
        assert!((exp[3] - 0.25).abs() < f64::EPSILON);

        let team_one = vec![WengLinRating {
            rating: 42.0,
            uncertainty: 2.1,
        }];
        let team_two = vec![WengLinRating {
            rating: 31.0,
            uncertainty: 1.2,
        }];

        let exp = expected_score_multi_team(&[&team_one, &team_two], &WengLinConfig::new());

        assert!((exp[0] + exp[1] - 1.0).abs() < f64::EPSILON);

        assert!((exp[0] - 0.849_021_123_412_260_5).abs() < f64::EPSILON);
        assert!((exp[1] - 0.150_978_876_587_739_42).abs() < f64::EPSILON);
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
            &[
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

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn test_misc_stuff() {
        let player_one = WengLinRating::new();
        let config = WengLinConfig::new();

        assert_eq!(player_one, player_one.clone());
        assert!((config.beta - config.clone().beta).abs() < f64::EPSILON);

        assert!(!format!("{:?}", player_one).is_empty());
        assert!(!format!("{:?}", config).is_empty());

        assert_eq!(player_one, WengLinRating::from((25.0, 25.0 / 3.0)));
    }
}
