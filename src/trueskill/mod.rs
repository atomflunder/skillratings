//! The TrueSkill rating algorithm, developed by Microsoft for Halo 3.  
//! Used in the Halo games, the Forza Games, Tom Clancy's: Rainbow Six Siege, and most Xbox Live games.
//!
//! Developed specifically for online games with multiple teams and multiple players.
//!
//! **Caution:** TrueSkill is patented. If you have a commercial project, it is recommended to use another algorithm included here,
//! for example the [`Weng-Lin algorithm`](crate::weng_lin).
//!
//! TrueSkill uses a normal distribution with a skill rating (μ) and an uncertainty value (σ) to represent a players skill level,
//! similar to [`Glicko`](crate::glicko) and [`Glicko-2`](crate::glicko2).
//! TrueSkill is good at assessing a player's skill level quickly.
//! The amount of matches needed depends on the game mode, as follows (The actual numbers may vary):
//!
//! | Match type | Matches needed |
//! | ---------- | -------------- |
//! | 8 Players Free-For-All | 3 |
//! | 4 Players Free-For-All | 5 |
//! | 4 Teams / 2 Players each | 10 |
//! | 1 Player vs 1 Player | 12 |
//! | 4 Teams / 4 Players each | 20 |
//! | 4 Players vs 4 Players | 46 |
//! | 8 Players vs 8 Players | 91 |
//!
//! Whereas other algorithms might need more matches in the same circumstances.
//!
//! The drawback is that the calculations are complex, and thus players may find it unintuitive in certain scenarios.  
//! For example, players might gain rank(s) when losing a match due to the uncertainty value decreasing.
//!
//! # Quickstart
//!
//! This is the most basic example on how to use the TrueSkill Module.  
//! Please take a look at the functions below to see more advanced use cases.
//!
//! ```
//! use skillratings::{
//!     trueskill::{trueskill, TrueSkillConfig, TrueSkillRating},
//!     Outcomes,
//! };
//!
//! // Initialise a new player rating with a rating of 25, and an uncertainty of 25/3 ≈ 8.33.
//! let player_one = TrueSkillRating::new();
//!
//! // Or you can initialise it with your own values of course.
//! // Imagine these numbers being pulled from a database.
//! let (some_rating, some_uncertainty) = (34.2, 2.3);
//! let player_two = TrueSkillRating {
//!     rating: some_rating,
//!     uncertainty: some_uncertainty,
//! };
//!
//! // The outcome of the match is from the perspective of player one.
//! let outcome = Outcomes::WIN;
//!
//! // The config allows you to specify certain values in the TrueSkill calculation.
//! // We set the draw probability to 0.05 (5%) instead of the default 0.1 (10%).
//! // This means that in our game, draws will be very rare to occur.
//! // Change this value to reflect the outcomes of your game.
//! // For example in chess, it might be a good idea to increase this value.
//! // For more information on how to customise the config,
//! // please check out the TrueSkillConfig struct.
//! let config = TrueSkillConfig {
//!     draw_probability: 0.05,
//!     ..Default::default()
//! };
//!
//! // The trueskill function will calculate the new ratings for both players and return them.
//! let (new_player_one, new_player_two) = trueskill(&player_one, &player_two, &outcome, &config);
//! ```
//!
//! # More Information
//! - [Wikipedia Article](https://en.wikipedia.org/wiki/TrueSkill)
//! - [Original Paper (PDF)](https://proceedings.neurips.cc/paper/2006/file/f44ee263952e65b3610b8ba51229d1f9-Paper.pdf)
//! - [The math behind TrueSkill (PDF)](http://www.moserware.com/assets/computing-your-skill/The%20Math%20Behind%20TrueSkill.pdf)
//! - [Moserware: Computing Your Skill](http://www.moserware.com/2010/03/computing-your-skill.html)
//! - [TrueSkill Calculator](https://trueskill-calculator.vercel.app/)

mod factor_graph;
mod gaussian;
mod matrix;
mod weights;

use std::cell::RefCell;
use std::f64::consts::{FRAC_1_SQRT_2, FRAC_2_SQRT_PI, PI, SQRT_2};
use std::rc::Rc;

use factor_graph::{LikelihoodFactor, PriorFactor, SumFactor, TruncateFactor, Variable};
use gaussian::Gaussian;
use matrix::Matrix;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::trueskill::weights::{get_weights, WeightError};
use crate::{weng_lin::WengLinRating, Outcomes};
use crate::{
    MultiTeamOutcome, MultiTeamRatingSystem, Rating, RatingPeriodSystem, RatingSystem,
    TeamRatingSystem,
};

const MIN_DELTA: f64 = 0.0001;

#[derive(Copy, Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// The TrueSkill rating of a player.
///
/// The default rating is 25.0.  
/// The default uncertainty is 25/3 ≈ 8.33.
pub struct TrueSkillRating {
    /// The rating value (mu) of the TrueSkilLRating, by default 25.0.
    pub rating: f64,
    /// The uncertainty value (sigma) of the TrueSkillRating, by default 25/3 ≈ 8.33.
    pub uncertainty: f64,
}

impl TrueSkillRating {
    #[must_use]
    /// Initialise a new TrueSkillRating with a rating of 25.0, and an uncertainty of 25/3 ≈ 8.33.
    pub fn new() -> Self {
        Self {
            rating: 25.0,
            uncertainty: 25.0 / 3.0,
        }
    }
}

impl Default for TrueSkillRating {
    fn default() -> Self {
        Self::new()
    }
}

impl Rating for TrueSkillRating {
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

impl From<(f64, f64)> for TrueSkillRating {
    fn from((r, u): (f64, f64)) -> Self {
        Self {
            rating: r,
            uncertainty: u,
        }
    }
}

impl From<WengLinRating> for TrueSkillRating {
    fn from(w: WengLinRating) -> Self {
        Self {
            rating: w.rating,
            uncertainty: w.uncertainty,
        }
    }
}

#[derive(Clone, Copy, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// Constants used in the TrueSkill calculations.
pub struct TrueSkillConfig {
    /// The probability of draws occurring in match.
    /// The higher the probability, the bigger the updates to the ratings in a non-drawn outcome.  
    /// By default set to `0.1`, meaning 10% chance of a draw.  
    /// Increase or decrease the value to match the values occurring in your game.
    pub draw_probability: f64,
    /// The skill-class width, aka the number of difference in rating points
    /// needed to have an 80% win probability against another player.  
    /// By default set to (25 / 3) * 0.5 ≈ `4.167`.  
    /// If your game is more reliant on pure skill, decrease this value,
    /// if there are more random factors, increase it.
    pub beta: f64,
    /// The additive dynamics factor.
    /// It determines how easy it will be for a player to move up and down a leaderboard.
    /// A larger value will tend to cause more volatility of player positions.
    /// By default set to 25 / 300 ≈ `0.0833`.
    pub dynamics_factor: f64,
}

impl TrueSkillConfig {
    #[must_use]
    /// Initialise a new `TrueSkillConfig` with a draw probability of `0.1`,
    /// a beta value of `(25 / 3) * 0.5 ≈ 4.167` and a default dynamics value of 25 / 300 ≈ `0.0833`.
    pub fn new() -> Self {
        Self {
            draw_probability: 0.1,
            beta: (25.0 / 3.0) * 0.5,
            dynamics_factor: 25.0 / 300.0,
        }
    }
}

impl Default for TrueSkillConfig {
    fn default() -> Self {
        Self::new()
    }
}

/// Struct to calculate ratings and expected score for [`TrueSkillRating`]
pub struct TrueSkill {
    config: TrueSkillConfig,
}

impl RatingSystem for TrueSkill {
    type RATING = TrueSkillRating;
    type CONFIG = TrueSkillConfig;

    fn new(config: Self::CONFIG) -> Self {
        Self { config }
    }

    fn rate(
        &self,
        player_one: &TrueSkillRating,
        player_two: &TrueSkillRating,
        outcome: &Outcomes,
    ) -> (TrueSkillRating, TrueSkillRating) {
        trueskill(player_one, player_two, outcome, &self.config)
    }

    fn expected_score(
        &self,
        player_one: &TrueSkillRating,
        player_two: &TrueSkillRating,
    ) -> (f64, f64) {
        expected_score(player_one, player_two, &self.config)
    }
}

impl RatingPeriodSystem for TrueSkill {
    type RATING = TrueSkillRating;
    type CONFIG = TrueSkillConfig;

    fn new(config: Self::CONFIG) -> Self {
        Self { config }
    }

    fn rate(
        &self,
        player: &TrueSkillRating,
        results: &[(TrueSkillRating, Outcomes)],
    ) -> TrueSkillRating {
        trueskill_rating_period(player, results, &self.config)
    }

    fn expected_score(&self, player: &Self::RATING, opponents: &[Self::RATING]) -> Vec<f64> {
        expected_score_rating_period(player, opponents, &self.config)
    }
}

impl TeamRatingSystem for TrueSkill {
    type RATING = TrueSkillRating;
    type CONFIG = TrueSkillConfig;

    fn new(config: Self::CONFIG) -> Self {
        Self { config }
    }

    fn rate(
        &self,
        team_one: &[TrueSkillRating],
        team_two: &[TrueSkillRating],
        outcome: &Outcomes,
    ) -> (Vec<TrueSkillRating>, Vec<TrueSkillRating>) {
        trueskill_two_teams(team_one, team_two, outcome, &self.config)
    }

    fn expected_score(&self, team_one: &[Self::RATING], team_two: &[Self::RATING]) -> (f64, f64) {
        expected_score_two_teams(team_one, team_two, &self.config)
    }
}

impl MultiTeamRatingSystem for TrueSkill {
    type RATING = TrueSkillRating;
    type CONFIG = TrueSkillConfig;

    fn new(config: Self::CONFIG) -> Self {
        Self { config }
    }

    fn rate(
        &self,
        teams_and_ranks: &[(&[Self::RATING], MultiTeamOutcome)],
    ) -> Vec<Vec<TrueSkillRating>> {
        trueskill_multi_team(teams_and_ranks, &self.config, None)
            .unwrap_or_else(|_| teams_and_ranks.iter().map(|(t, _)| t.to_vec()).collect())
    }

    fn expected_score(&self, teams: &[&[Self::RATING]]) -> Vec<f64> {
        expected_score_multi_team(teams, &self.config)
    }
}

#[must_use]
/// Calculates the [`TrueSkillRating`]s of two players based on their old ratings, uncertainties, and the outcome of the game.
///
/// Takes in two players as [`TrueSkillRating`]s, an [`Outcome`](Outcomes), and a [`TrueSkillConfig`].
///
/// The outcome of the match is in the perspective of `player_one`.
/// This means [`Outcomes::WIN`] is a win for `player_one` and [`Outcomes::LOSS`] is a win for `player_two`.
///
/// Similar to [`trueskill_rating_period`], [`trueskill_two_teams`] and [`trueskill_multi_team`].
///
/// This algorithm uses some shortcuts to speed-up and simplify 1-vs-1 ratings. This is fine for 99.9% of use-cases,
/// but if you need maximum precision or need custom weightings, consider using [`trueskill_multi_team`].
///
/// **Caution regarding usage of TrueSkill**:  
/// Microsoft permits only Xbox Live games or non-commercial projects to use TrueSkill.  
/// If your project is commercial, you should use another rating system included here.
///
/// # Examples
/// ```
/// use skillratings::{
///     trueskill::{trueskill, TrueSkillConfig, TrueSkillRating},
///     Outcomes,
/// };
///
/// let player_one = TrueSkillRating::new();
/// let player_two = TrueSkillRating {
///     rating: 50.0,
///     uncertainty: 1.21,
/// };
///
/// let outcome = Outcomes::WIN;
///
/// let config = TrueSkillConfig::new();
///
/// let (new_one, new_two) = trueskill(&player_one, &player_two, &outcome, &config);
///
/// assert!(((new_one.rating * 100.0).round() - 4410.0).abs() < f64::EPSILON);
/// assert!(((new_one.uncertainty * 100.0).round() - 528.0).abs() < f64::EPSILON);
///
/// assert!(((new_two.rating * 100.0).round() - 4960.0).abs() < f64::EPSILON);
/// assert!(((new_two.uncertainty * 100.0).round() - 121.0).abs() < f64::EPSILON);
/// ```
pub fn trueskill(
    player_one: &TrueSkillRating,
    player_two: &TrueSkillRating,
    outcome: &Outcomes,
    config: &TrueSkillConfig,
) -> (TrueSkillRating, TrueSkillRating) {
    let draw_margin = draw_margin(config.draw_probability, config.beta, 2.0);

    let c = 2.0f64
        .mul_add(
            config.beta.powi(2),
            player_one
                .uncertainty
                .mul_add(player_one.uncertainty, player_two.uncertainty.powi(2)),
        )
        .sqrt();

    // We need the rating of the winner minus the rating of the loser
    let rating_delta = match outcome {
        Outcomes::WIN | Outcomes::DRAW => player_one.rating - player_two.rating,
        Outcomes::LOSS => player_two.rating - player_one.rating,
    };

    let (v, w) = if outcome == &Outcomes::DRAW {
        (
            v_draw(rating_delta, draw_margin, c),
            w_draw(rating_delta, draw_margin, c),
        )
    } else {
        (
            v_non_draw(rating_delta, draw_margin, c),
            w_non_draw(rating_delta, draw_margin, c),
        )
    };

    // We could add these to the rating_delta match statement above,
    // but that would yield no noticeable performance gains while sacrificing readability.
    let (rank_multiplier1, rank_multiplier2) = match outcome {
        Outcomes::WIN | Outcomes::DRAW => (1.0, -1.0),
        Outcomes::LOSS => (-1.0, 1.0),
    };

    let new_rating1 = new_rating(
        player_one.rating,
        player_one.uncertainty,
        v,
        c,
        config.dynamics_factor,
        rank_multiplier1,
    );
    let new_rating2 = new_rating(
        player_two.rating,
        player_two.uncertainty,
        v,
        c,
        config.dynamics_factor,
        rank_multiplier2,
    );

    let new_uncertainty1 = new_uncertainty(player_one.uncertainty, c, w, config.dynamics_factor);
    let new_uncertainty2 = new_uncertainty(player_two.uncertainty, c, w, config.dynamics_factor);

    (
        TrueSkillRating {
            rating: new_rating1,
            uncertainty: new_uncertainty1,
        },
        TrueSkillRating {
            rating: new_rating2,
            uncertainty: new_uncertainty2,
        },
    )
}

#[must_use]
/// Calculates a [`TrueSkillRating`] in a non-traditional way using a rating period,
/// for compatibility with the other algorithms.
///
/// Takes in a player as an [`TrueSkillRating`] and their results as a Slice of tuples containing the opponent as an [`TrueSkillRating`],
/// the outcome of the game as an [`Outcome`](Outcomes) and a [`TrueSkillConfig`].
///
/// The outcome of the match is in the perspective of the player.
/// This means [`Outcomes::WIN`] is a win for the player and [`Outcomes::LOSS`] is a win for the opponent.
///
/// Similar to [`trueskill`].
///
/// **Caution regarding usage of TrueSkill**:  
/// Microsoft permits only Xbox Live games or non-commercial projects to use TrueSkill.  
/// If your project is commercial, you should use another rating system included here.
///
/// # Examples
/// ```
/// use skillratings::{
///     trueskill::{trueskill_rating_period, TrueSkillConfig, TrueSkillRating},
///     Outcomes,
/// };
///
/// let player_one = TrueSkillRating::new();
/// let player_two = TrueSkillRating {
///     rating: 30.0,
///     uncertainty: 1.2,
/// };
/// let player_three = TrueSkillRating {
///     rating: 12.0,
///     uncertainty: 1.9,
/// };
/// let player_four = TrueSkillRating {
///     rating: 49.0,
///     uncertainty: 1.2,
/// };
///
/// let new_player = trueskill_rating_period(
///     &player_one,
///     &vec![
///         (player_two, Outcomes::WIN),
///         (player_three, Outcomes::WIN),
///         (player_four, Outcomes::LOSS),
///     ],
///     &TrueSkillConfig::new(),
/// );
///
/// assert!(((new_player.rating * 100.0).round() - 3277.0).abs() < f64::EPSILON);
/// assert!(((new_player.uncertainty * 100.0).round() - 566.0).abs() < f64::EPSILON);
/// ```
pub fn trueskill_rating_period(
    player: &TrueSkillRating,
    results: &[(TrueSkillRating, Outcomes)],
    config: &TrueSkillConfig,
) -> TrueSkillRating {
    let mut player_rating = player.rating;
    let mut player_uncertainty = player.uncertainty;

    let draw_margin = draw_margin(config.draw_probability, config.beta, 2.0);

    for (opponent, result) in results {
        let c = 2.0f64
            .mul_add(
                config.beta.powi(2),
                player_uncertainty.mul_add(player_uncertainty, opponent.uncertainty.powi(2)),
            )
            .sqrt();

        let rating_delta = match result {
            Outcomes::WIN | Outcomes::DRAW => player_rating - opponent.rating,
            Outcomes::LOSS => opponent.rating - player_rating,
        };

        let (v, w) = if result == &Outcomes::DRAW {
            (
                v_draw(rating_delta, draw_margin, c),
                w_draw(rating_delta, draw_margin, c),
            )
        } else {
            (
                v_non_draw(rating_delta, draw_margin, c),
                w_non_draw(rating_delta, draw_margin, c),
            )
        };

        let rank_multiplier = match result {
            Outcomes::WIN | Outcomes::DRAW => 1.0,
            Outcomes::LOSS => -1.0,
        };

        player_rating = new_rating(
            player_rating,
            player_uncertainty,
            v,
            c,
            config.dynamics_factor,
            rank_multiplier,
        );
        player_uncertainty = new_uncertainty(player_uncertainty, c, w, config.dynamics_factor);
    }

    TrueSkillRating {
        rating: player_rating,
        uncertainty: player_uncertainty,
    }
}

#[must_use]
/// Calculates the [`TrueSkillRating`] of two teams based on their ratings, uncertainties, and the outcome of the game.
///
/// Takes in two teams as a Slice of [`TrueSkillRating`]s, the outcome of the game as an [`Outcome`](Outcomes) and a [`TrueSkillConfig`].
///
/// The outcome of the match is in the perspective of `team_one`.
/// This means [`Outcomes::WIN`] is a win for `team_one` and [`Outcomes::LOSS`] is a win for `team_two`.
///
/// Similar to [`trueskill`] and [`trueskill_multi_team`].
///
/// This algorithm uses some shortcuts to speed-up and simplify Team-vs-Team ratings. This is fine for 99.9% of use-cases,
/// but if you need maximum precision or need custom weightings, consider using [`trueskill_multi_team`].
///
/// **Caution regarding usage of TrueSkill**:
/// Microsoft permits only Xbox Live games or non-commercial projects to use TrueSkill(TM).
/// If your project is commercial, you should use another rating system included here.
///
/// # Examples
/// ```
/// use skillratings::{
///     trueskill::{trueskill_two_teams, TrueSkillConfig, TrueSkillRating},
///     Outcomes,
/// };
///
/// let player_one = TrueSkillRating {
///     rating: 20.0,
///     uncertainty: 8.0,
/// };
/// let player_two = TrueSkillRating {
///     rating: 25.0,
///     uncertainty: 6.0,
/// };
///
/// let player_three = TrueSkillRating {
///     rating: 35.0,
///     uncertainty: 7.0,
/// };
/// let player_four = TrueSkillRating {
///     rating: 40.0,
///     uncertainty: 5.0,
/// };
///
/// let (team_one, team_two) = trueskill_two_teams(
///     &vec![player_one, player_two],
///     &vec![player_three, player_four],
///     &Outcomes::WIN,
///     &TrueSkillConfig::new(),
/// );
///
/// assert!((team_one[0].rating - 29.698_800_676_796_665).abs() < f64::EPSILON);
/// assert!((team_one[1].rating - 30.456_035_750_156_31).abs() < f64::EPSILON);
/// assert!((team_two[0].rating - 27.574_109_105_332_1).abs() < f64::EPSILON);
/// assert!((team_two[1].rating - 36.210_764_756_738_115).abs() < f64::EPSILON);
/// ```
pub fn trueskill_two_teams(
    team_one: &[TrueSkillRating],
    team_two: &[TrueSkillRating],
    outcome: &Outcomes,
    config: &TrueSkillConfig,
) -> (Vec<TrueSkillRating>, Vec<TrueSkillRating>) {
    if team_one.is_empty() || team_two.is_empty() {
        return (team_one.to_vec(), team_two.to_vec());
    }

    let total_players = (team_one.len() + team_two.len()) as f64;

    let draw_margin = draw_margin(config.draw_probability, config.beta, total_players);

    let rating_one_sum: f64 = team_one.iter().map(|p| p.rating).sum();
    let rating_two_sum: f64 = team_two.iter().map(|p| p.rating).sum();

    let uncertainty_one_sum: f64 = team_one.iter().map(|p| p.uncertainty.powi(2)).sum();
    let uncertainty_two_sum: f64 = team_two.iter().map(|p| p.uncertainty.powi(2)).sum();

    let c = total_players
        .mul_add(
            config.beta.powi(2),
            uncertainty_one_sum + uncertainty_two_sum,
        )
        .sqrt();

    let rating_delta = match outcome {
        Outcomes::WIN | Outcomes::DRAW => rating_one_sum - rating_two_sum,
        Outcomes::LOSS => rating_two_sum - rating_one_sum,
    };

    let (v, w) = if outcome == &Outcomes::DRAW {
        (
            v_draw(rating_delta, draw_margin, c),
            w_draw(rating_delta, draw_margin, c),
        )
    } else {
        (
            v_non_draw(rating_delta, draw_margin, c),
            w_non_draw(rating_delta, draw_margin, c),
        )
    };

    let (rank_multiplier1, rank_multiplier2) = match outcome {
        Outcomes::WIN | Outcomes::DRAW => (1.0, -1.0),
        Outcomes::LOSS => (-1.0, 1.0),
    };

    let mut new_team_one = Vec::new();
    let mut new_team_two = Vec::new();

    for player in team_one {
        let new_rating = new_rating(
            player.rating,
            player.uncertainty,
            v,
            c,
            config.dynamics_factor,
            rank_multiplier1,
        );
        let new_uncertainty = new_uncertainty(player.uncertainty, c, w, config.dynamics_factor);

        new_team_one.push(TrueSkillRating {
            rating: new_rating,
            uncertainty: new_uncertainty,
        });
    }

    for player in team_two {
        let new_rating = new_rating(
            player.rating,
            player.uncertainty,
            v,
            c,
            config.dynamics_factor,
            rank_multiplier2,
        );
        let new_uncertainty = new_uncertainty(player.uncertainty, c, w, config.dynamics_factor);

        new_team_two.push(TrueSkillRating {
            rating: new_rating,
            uncertainty: new_uncertainty,
        });
    }

    (new_team_one, new_team_two)
}

#[allow(clippy::too_many_lines)]
/// Calculates the [`TrueSkillRating`] of multiple teams based on their ratings, uncertainties, and the outcome of the game.
///
/// Takes in a slice, which contains tuples of teams, which are just slices of [`TrueSkillRating`]s,
/// as well the rank of the team as an [`MultiTeamOutcome`] and a [`TrueSkillConfig`] and Weights.
///
/// Weights are values between `0.0` and `1.0` that describe the contribution of a player to the team.
/// A Weight of `1.0` means the Player has played the whole match, and values below `1.0` mean the Player has left the game early.
/// A Weight of `0.0` means the Player has not played at all.  
/// **If you do not wish to use weights, pass in `None`.**
///
/// Ties are represented by several teams having the same rank.
///
/// Similar to [`trueskill`] and [`trueskill_two_teams`].
///
/// **Caution regarding usage of TrueSkill**:
/// Microsoft permits only Xbox Live games or non-commercial projects to use TrueSkill(TM).
/// If your project is commercial, you should use another rating system included here.
///
/// # Examples
/// ```
/// use skillratings::{
///     trueskill::{trueskill_multi_team, TrueSkillConfig, TrueSkillRating},
///     MultiTeamOutcome,
/// };
///
/// let team_one = vec![
///     TrueSkillRating::new(),
///     TrueSkillRating {
///         rating: 30.0,
///         uncertainty: 1.2,
///     },
///     TrueSkillRating {
///         rating: 21.0,
///         uncertainty: 6.5,
///     },
/// ];
///
/// let team_two = vec![
///     TrueSkillRating::default(),
///     TrueSkillRating {
///         rating: 41.0,
///         uncertainty: 1.4,
///     },
///     TrueSkillRating {
///         rating: 19.2,
///         uncertainty: 4.3,
///     },
/// ];
///
/// let team_three = vec![
///     TrueSkillRating::default(),
///     TrueSkillRating {
///         rating: 29.4,
///         uncertainty: 1.6,
///     },
///     TrueSkillRating {
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
/// let weights: &[&[f64]] = &[
///     &[1.0, 1.0, 0.5], // Player 3 left halfway through the match
///     &[1.0, 0.9, 1.0], // Player 2 left close to the end
///     &[1.0, 1.0, 1.0],
/// ];
///
/// let new_teams = trueskill_multi_team(&teams_and_ranks, &TrueSkillConfig::new(), Some(&weights)).unwrap();
///
/// assert_eq!(new_teams.len(), 3);
///
/// let new_one = &new_teams[0];
/// let new_two = &new_teams[1];
/// let new_three = &new_teams[2];
///
/// assert!((new_one[0].rating - 28.180_576_928_436_55).abs() < f64::EPSILON);
/// assert!((new_one[1].rating - 30.066_263_874_493_455).abs() < f64::EPSILON);
/// assert!((new_one[2].rating - 21.967_593_771_099_708).abs() < f64::EPSILON);
///
/// assert!((new_two[0].rating - 27.657_276_377_119_9).abs() < f64::EPSILON);
/// assert!((new_two[1].rating - 41.067_731_453_349_65).abs() < f64::EPSILON);
/// assert!((new_two[2].rating - 19.907_710_735_630_584).abs() < f64::EPSILON);
///
/// assert!((new_three[0].rating - 19.162_146_694_443_58).abs() < f64::EPSILON);
/// assert!((new_three[1].rating - 29.184_231_167_296_68).abs() < f64::EPSILON);
/// assert!((new_three[2].rating - 16.828_726_305_722_817).abs() < f64::EPSILON);
/// ```
///
/// # Errors
///
/// This function may return a [`WeightError`] if it receives invalid weights. Weights are invalid if:
/// - The number of teams does not match the `teams_and_ranks` argument
/// - The number of players inside a team does not match
/// - One of the weights is negative
pub fn trueskill_multi_team(
    teams_and_ranks: &[(&[TrueSkillRating], MultiTeamOutcome)],
    config: &TrueSkillConfig,
    weights: Option<&[&[f64]]>,
) -> Result<Vec<Vec<TrueSkillRating>>, WeightError> {
    if teams_and_ranks.is_empty() {
        return Ok(Vec::new());
    }

    // Just returning the original teams if a team is empty.
    for (team, _) in teams_and_ranks {
        if team.is_empty() {
            return Ok(teams_and_ranks
                .iter()
                .map(|(team, _)| team.to_vec())
                .collect());
        }
    }

    let weights = get_weights(
        &teams_and_ranks
            .iter()
            .map(|(t, _)| *t)
            .collect::<Vec<&[TrueSkillRating]>>(),
        weights,
    )?;

    let mut sorted_teams_and_ranks_with_pos = Vec::new();
    for (pos, ((team, outcome), weights)) in teams_and_ranks.iter().zip(&weights).enumerate() {
        sorted_teams_and_ranks_with_pos.push((pos, (*team, *outcome, &weights[..])));
    }
    sorted_teams_and_ranks_with_pos.sort_by_key(|v| v.1 .1);

    let teams_and_ranks: Vec<(&[TrueSkillRating], MultiTeamOutcome)> =
        sorted_teams_and_ranks_with_pos
            .iter()
            .map(|v| (v.1 .0, v.1 .1))
            .collect();

    let weights: Vec<&[f64]> = sorted_teams_and_ranks_with_pos
        .iter()
        .map(|v| v.1 .2)
        .collect();

    let mut flattened_ratings = Vec::new();
    for &(team, _) in &teams_and_ranks {
        for &player in team {
            flattened_ratings.push(player);
        }
    }

    let mut flattened_weights = Vec::new();
    for group in weights {
        for &weight in group {
            flattened_weights.push(weight);
        }
    }

    let rating_vars = {
        let mut v = Vec::with_capacity(flattened_ratings.len());
        for _ in 0..flattened_ratings.len() {
            v.push(Rc::new(RefCell::new(Variable::new())));
        }

        v
    };
    let perf_vars = {
        let mut v = Vec::with_capacity(flattened_ratings.len());
        for _ in 0..flattened_ratings.len() {
            v.push(Rc::new(RefCell::new(Variable::new())));
        }

        v
    };
    let team_perf_vars = {
        let mut v = Vec::with_capacity(teams_and_ranks.len());
        for _ in 0..teams_and_ranks.len() {
            v.push(Rc::new(RefCell::new(Variable::new())));
        }

        v
    };
    let team_diff_vars = {
        let mut v = Vec::with_capacity(teams_and_ranks.len() - 1);
        for _ in 0..(teams_and_ranks.len() - 1) {
            v.push(Rc::new(RefCell::new(Variable::new())));
        }

        v
    };
    let team_sizes = team_sizes(&teams_and_ranks);

    let rating_layer = run_schedule(
        &rating_vars,
        &perf_vars,
        &team_perf_vars,
        &team_diff_vars,
        &team_sizes,
        &teams_and_ranks,
        &flattened_ratings,
        &flattened_weights,
        config.dynamics_factor,
        config.beta,
        config.draw_probability,
        MIN_DELTA,
    );

    let mut transformed_groups = Vec::new();
    let mut iter_team_sizes = vec![0];
    iter_team_sizes.extend_from_slice(&team_sizes[..(team_sizes.len() - 1)]);

    for (start, end) in iter_team_sizes.into_iter().zip(&team_sizes) {
        let mut group = Vec::new();
        for f in &rating_layer[start..*end] {
            let gaussian = f.variable.borrow().gaussian;
            let mu = gaussian.mu();
            let sigma = gaussian.sigma();

            group.push(TrueSkillRating {
                rating: mu,
                uncertainty: sigma,
            });
        }

        transformed_groups.push(group);
    }

    let mut unsorted_with_pos = sorted_teams_and_ranks_with_pos
        .iter()
        .map(|v| v.0)
        .zip(transformed_groups)
        .collect::<Vec<_>>();
    unsorted_with_pos.sort_by_key(|v| v.0);

    Ok(unsorted_with_pos.into_iter().map(|v| v.1).collect())
}

#[must_use]
/// Gets the quality of the match, which is equal to the probability that the match will end in a draw.
/// The higher the Value, the better the quality of the match.
///
/// Takes in two players as [`TrueSkillRating`]s and returns the probability of a draw occurring as an [`f64`] between 1.0 and 0.0.
///
/// Similar to [`match_quality_rating_period`], [`match_quality_two_teams`] and [`match_quality_multi_team`].
///
/// # Examples
/// ```
/// use skillratings::trueskill::{match_quality, TrueSkillConfig, TrueSkillRating};
///
/// let player_one = TrueSkillRating::new();
/// let player_two = TrueSkillRating::new();
///
/// let config = TrueSkillConfig::new();
///
/// let quality = match_quality(&player_one, &player_two, &config);
///
/// // According to TrueSkill, there is a 44.7% chance this match will end in a draw.
/// assert!(((quality * 1000.0).round() - 447.0).abs() < f64::EPSILON);
/// ```
pub fn match_quality(
    player_one: &TrueSkillRating,
    player_two: &TrueSkillRating,
    config: &TrueSkillConfig,
) -> f64 {
    let delta: f64 = player_one.rating - player_two.rating;

    let a = ((2.0 * config.beta.powi(2))
        / player_two.uncertainty.mul_add(
            player_two.uncertainty,
            2.0f64.mul_add(config.beta.powi(2), player_one.uncertainty.powi(2)),
        ))
    .sqrt();

    let b = ((-delta.powi(2))
        / (2.0
            * player_two.uncertainty.mul_add(
                player_two.uncertainty,
                2.0f64.mul_add(config.beta.powi(2), player_one.uncertainty.powi(2)),
            )))
    .exp();

    a * b
}

#[must_use]
/// Gets the quality of multiple matches, which is equal to the probability that the match will end in a draw.
/// The higher the Value, the better the quality of the match.
///
/// Takes in a player as a [`TrueSkillRating`], and a slice of opponents as [`TrueSkillRating`]s and returns the probabilities of a draw occurring as a Vec of [`f64`]s between 1.0 and 0.0.
///
/// Similar to [`match_quality`].
///
/// # Examples
/// ```
/// use skillratings::trueskill::{match_quality_rating_period, TrueSkillConfig, TrueSkillRating};
///
/// let player_one = TrueSkillRating::new();
/// let player_two = TrueSkillRating::new();
/// let player_three = TrueSkillRating {
///     rating: 13.0,
///     uncertainty: 6.1,
/// };
///
/// let config = TrueSkillConfig::new();
///
/// let qualities = match_quality_rating_period(&player_one, &[player_two, player_three], &config);
///
/// // According to TrueSkill, there is a 44.7% chance this first match will end in a draw.
/// assert!(((qualities[0] * 1000.0).round() - 447.0).abs() < f64::EPSILON);
/// // Similarly, the second match has a 29.8% chance of ending in a draw.
/// assert!(((qualities[1] * 1000.0).round() - 298.0).abs() < f64::EPSILON);
/// ```
pub fn match_quality_rating_period(
    player: &TrueSkillRating,
    results: &[TrueSkillRating],
    config: &TrueSkillConfig,
) -> Vec<f64> {
    // No shortcut, it's just a simple loop.
    // But this is implemented here for consistency with the other functions.
    results
        .iter()
        .map(|r| match_quality(player, r, config))
        .collect()
}

#[must_use]
/// Gets the quality of the match, which is equal to the probability that the match will end in a draw.
/// The higher the Value, the better the quality of the match.
///
/// Takes in two teams as a Slice of [`TrueSkillRating`]s and returns the probability of a draw occurring as an [`f64`] between 1.0 and 0.0.
///
/// Similar to [`match_quality`] and [`match_quality_multi_team`].
///
/// # Examples
/// ```
/// use skillratings::trueskill::{match_quality_two_teams, TrueSkillConfig, TrueSkillRating};
///
/// let player_one = TrueSkillRating {
///     rating: 20.0,
///     uncertainty: 8.0,
/// };
/// let player_two = TrueSkillRating {
///     rating: 25.0,
///     uncertainty: 6.0,
/// };
///
/// let player_three = TrueSkillRating {
///     rating: 35.0,
///     uncertainty: 7.0,
/// };
/// let player_four = TrueSkillRating {
///     rating: 40.0,
///     uncertainty: 5.0,
/// };
///
/// let quality = match_quality_two_teams(
///     &vec![player_one, player_two],
///     &vec![player_three, player_four],
///     &TrueSkillConfig::new(),
/// );
///
/// // There is a 8.4% chance of a draw occurring.
/// assert!((quality - 0.084_108_145_418_343_24).abs() < f64::EPSILON);
/// ```
pub fn match_quality_two_teams(
    team_one: &[TrueSkillRating],
    team_two: &[TrueSkillRating],
    config: &TrueSkillConfig,
) -> f64 {
    let total_players = (team_one.len() + team_two.len()) as f64;

    let rating_one_sum: f64 = team_one.iter().map(|p| p.rating).sum();
    let rating_two_sum: f64 = team_two.iter().map(|p| p.rating).sum();

    let uncertainty_one_sum: f64 = team_one.iter().map(|p| p.uncertainty.powi(2)).sum();
    let uncertainty_two_sum: f64 = team_two.iter().map(|p| p.uncertainty.powi(2)).sum();

    let a = ((total_players * config.beta.powi(2))
        / (total_players.mul_add(config.beta.powi(2), uncertainty_one_sum) + uncertainty_two_sum))
        .sqrt();

    let b = ((-(rating_one_sum - rating_two_sum).powi(2))
        / (2.0
            * (total_players.mul_add(config.beta.powi(2), uncertainty_one_sum)
                + uncertainty_two_sum)))
        .exp();

    a * b
}

/// Gets the quality of the match, which is equal to the probability that the match will end in a draw.
/// The higher the Value, the better the quality of the match.
///
/// Takes in multiple teams as a Slices of [`TrueSkillRating`]s, a [`TrueSkillConfig`]
/// and returns the probability of a draw occurring as an [`f64`] between 1.0 and 0.0.
///
/// Similar to [`match_quality`] and [`match_quality_two_teams`].
///
/// # Examples
/// ```
/// use skillratings::trueskill::{match_quality_multi_team, TrueSkillConfig, TrueSkillRating};
///
/// let team_one = vec![
///     TrueSkillRating {
///         rating: 20.0,
///         uncertainty: 2.0,
///     },
///     TrueSkillRating {
///         rating: 25.0,
///         uncertainty: 2.0,
///     },
/// ];
/// let team_two = vec![
///     TrueSkillRating {
///         rating: 35.0,
///         uncertainty: 2.0,
///     },
///     TrueSkillRating {
///         rating: 20.0,
///         uncertainty: 3.0,
///     },
/// ];
/// let team_three = vec![
///     TrueSkillRating {
///         rating: 20.0,
///         uncertainty: 2.0,
///     },
///     TrueSkillRating {
///         rating: 22.0,
///         uncertainty: 1.0,
///     },
/// ];
///
/// let quality = match_quality_multi_team(
///     &[&team_one, &team_two, &team_three],
///     &TrueSkillConfig::new(),
///     None,
/// );
///
/// // There is a ~28.6% chance of a draw occurring.
/// assert_eq!(quality.unwrap(), 0.285_578_468_347_742_1);
/// ```
///
/// # Errors
///
/// This function may return a [`WeightError`] if it receives invalid weights. Weights are invalid if:
/// - The number of teams does not match the `teams_and_ranks` argument
/// - The number of players inside a team does not match
/// - One of the weights is negative
pub fn match_quality_multi_team(
    teams: &[&[TrueSkillRating]],
    config: &TrueSkillConfig,
    weights: Option<&[&[f64]]>,
) -> Result<f64, WeightError> {
    if teams.is_empty() {
        return Ok(0.0);
    }

    for team in teams {
        if team.is_empty() {
            return Ok(0.0);
        }
    }

    let total_players = teams.iter().map(|t| t.len()).sum::<usize>();

    let team_uncertainties_sq_flatten = teams
        .iter()
        .flat_map(|team| {
            team.iter()
                .map(|p| p.uncertainty.powi(2))
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let team_ratings_flatten = teams
        .iter()
        .flat_map(|team| team.iter().map(|p| p.rating).collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let mean_matrix = Matrix::new_from_data(&team_ratings_flatten, total_players, 1);
    let variance_matrix = Matrix::new_diagonal(&team_uncertainties_sq_flatten);

    let rotated_a_matrix = Matrix::create_rotated_a_matrix(teams, weights)?;
    let a_matrix = rotated_a_matrix.transpose();

    let a_ta = rotated_a_matrix.clone() * a_matrix.clone() * config.beta.powi(2);
    let atsa = rotated_a_matrix.clone() * variance_matrix * a_matrix.clone();
    let start = a_matrix * mean_matrix.transpose();
    let middle = a_ta.clone() + atsa;

    let end = rotated_a_matrix * mean_matrix;

    let e_arg = (start * middle.inverse() * end * -0.5).determinant();
    let s_arg = a_ta.determinant() / middle.determinant();

    Ok(e_arg.exp() * s_arg.sqrt())
}

#[must_use]
/// Calculates the expected outcome of two players based on TrueSkill.
///
/// Takes in two players as [`TrueSkillRating`]s and a [`TrueSkillConfig`]
/// and returns the probability of victory for each player as an [`f64`] between 1.0 and 0.0.  
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// Similar to [`expected_score_two_teams`] and [`expected_score_multi_team`].
///
/// To see the actual chances of a draw occurring, please use [`match_quality`].
///
/// # Examples
/// ```
/// use skillratings::trueskill::{expected_score, TrueSkillConfig, TrueSkillRating};
///
/// let better_player = TrueSkillRating {
///     rating: 44.0,
///     uncertainty: 3.0,
/// };
/// let worse_player = TrueSkillRating {
///     rating: 38.0,
///     uncertainty: 3.0,
/// };
///
/// let config = TrueSkillConfig::new();
///
/// let (exp1, exp2) = expected_score(&better_player, &worse_player, &config);
///
/// // Player one has an 80% chance to win and player two a 20% chance.
/// assert!((exp1 * 100.0 - 80.0).round().abs() < f64::EPSILON);
/// assert!((exp2 * 100.0 - 20.0).round().abs() < f64::EPSILON);
///
/// assert!((exp1.mul_add(100.0, exp2 * 100.0).round() - 100.0).abs() < f64::EPSILON);
/// ```
pub fn expected_score(
    player_one: &TrueSkillRating,
    player_two: &TrueSkillRating,
    config: &TrueSkillConfig,
) -> (f64, f64) {
    let delta = player_one.rating - player_two.rating;

    let denom = player_two
        .uncertainty
        .mul_add(
            player_two.uncertainty,
            2.0f64.mul_add(config.beta.powi(2), player_one.uncertainty.powi(2)),
        )
        .sqrt();

    let exp_one = cdf(delta / denom, 0.0, 1.0);
    let exp_two = 1.0 - exp_one;

    (exp_one, exp_two)
}

#[must_use]
/// Calculates the expected outcome of two teams based on TrueSkill.
///
/// Takes in two teams as a Slice of [`TrueSkillRating`]s and a [`TrueSkillConfig`]
/// and returns the probability of victory for each player as an [`f64`] between 1.0 and 0.0.  
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// Similar to [`expected_score`] and [`expected_score_multi_team`].
///
/// To see the actual chances of a draw occurring, please use [`match_quality_two_teams`].
///
/// # Examples
/// ```
/// use skillratings::trueskill::{expected_score_two_teams, TrueSkillConfig, TrueSkillRating};
///
/// let player_one = TrueSkillRating {
///     rating: 38.0,
///     uncertainty: 3.0,
/// };
/// let player_two = TrueSkillRating {
///     rating: 38.0,
///     uncertainty: 3.0,
/// };
///
/// let player_three = TrueSkillRating {
///     rating: 44.0,
///     uncertainty: 3.0,
/// };
/// let player_four = TrueSkillRating {
///     rating: 44.0,
///     uncertainty: 3.0,
/// };
///
/// let (exp1, exp2) = expected_score_two_teams(
///     &vec![player_one, player_two],
///     &vec![player_three, player_four],
///     &TrueSkillConfig::new(),
/// );
///
/// assert!((exp1 + exp2 - 1.0).abs() < f64::EPSILON);
///
/// // There is a 12% chance for team_one to win and an 88% for team two.
/// assert!(((exp1 * 100.0).round() - 12.0).abs() < f64::EPSILON);
/// assert!(((exp2 * 100.0).round() - 88.0).abs() < f64::EPSILON);
/// ```
pub fn expected_score_two_teams(
    team_one: &[TrueSkillRating],
    team_two: &[TrueSkillRating],
    config: &TrueSkillConfig,
) -> (f64, f64) {
    let player_count = (team_one.len() + team_two.len()) as f64;

    let rating_one_sum: f64 = team_one.iter().map(|p| p.rating).sum();
    let rating_two_sum: f64 = team_two.iter().map(|p| p.rating).sum();

    let uncertainty_one_sum: f64 = team_one.iter().map(|p| p.uncertainty.powi(2)).sum();
    let uncertainty_two_sum: f64 = team_two.iter().map(|p| p.uncertainty.powi(2)).sum();

    let delta = rating_one_sum - rating_two_sum;

    let denom = (uncertainty_two_sum
        + player_count.mul_add(config.beta.powi(2), uncertainty_one_sum))
    .sqrt();

    let exp_one = cdf(delta / denom, 0.0, 1.0);
    let exp_two = 1.0 - exp_one;

    (exp_one, exp_two)
}

#[must_use]
/// Calculates the expected outcome of multiple teams based on TrueSkill.
///
/// Takes in multiple teams as Slices of [`TrueSkillRating`]s, a [`TrueSkillConfig`]
/// and returns the probability of victory for each team as an [`f64`] between 1.0 and 0.0.  
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near `1 / Number of Teams` mean a draw is likely to occur.
///
/// Similar to [`expected_score`] and [`expected_score_multi_team`].
///
/// To see the actual chances of a draw occurring, please use [`match_quality_multi_team`].
///
/// # Examples
/// ```
/// use skillratings::trueskill::{expected_score_multi_team, TrueSkillConfig, TrueSkillRating};
///
/// let team_one = vec![
///     TrueSkillRating {
///         rating: 38.0,
///         uncertainty: 3.0,
///     },
///     TrueSkillRating {
///         rating: 38.0,
///         uncertainty: 3.0,
///     },
/// ];
///
/// let team_two = vec![
///     TrueSkillRating {
///         rating: 44.0,
///         uncertainty: 3.0,
///     },
///     TrueSkillRating {
///         rating: 44.0,
///         uncertainty: 3.0,
///     },
/// ];
///
/// let team_three = vec![
///     TrueSkillRating {
///         rating: 50.0,
///         uncertainty: 3.0,
///     },
///     TrueSkillRating {
///         rating: 50.0,
///         uncertainty: 3.0,
///     },
/// ];
///
/// let exp = expected_score_multi_team(
///     &[&team_one, &team_two, &team_three],
///     &TrueSkillConfig::new(),
/// );
///
/// assert!((exp.iter().sum::<f64>() - 1.0).abs() < f64::EPSILON);
///
/// // Team one has a 6% chance of winning, Team two a 33% and Team three a 61% chance.
/// assert!((exp[0] * 100.0 - 6.0).round().abs() < f64::EPSILON);
/// assert!((exp[1] * 100.0 - 33.0).round().abs() < f64::EPSILON);
/// assert!((exp[2] * 100.0 - 61.0).round().abs() < f64::EPSILON);
/// ```
pub fn expected_score_multi_team(
    teams: &[&[TrueSkillRating]],
    config: &TrueSkillConfig,
) -> Vec<f64> {
    let player_count = teams.iter().map(|t| t.len()).sum::<usize>() as f64;

    let mut win_probabilities = Vec::with_capacity(teams.len());
    let mut total_probability = 0.0;

    for (i, team_one) in teams.iter().enumerate() {
        // We are calculating the probability of team_one winning against all other teams.
        // We do this for every team, sum up the probabilities
        // and then divide by the total probability to get the probability of winning for each team.
        let mut current_team_probabilities = Vec::with_capacity(teams.len() - 1);
        let team_one_ratings = team_one.iter().map(|p| p.rating).sum::<f64>();
        let team_one_uncertainties = team_one.iter().map(|p| p.uncertainty.powi(2)).sum::<f64>();

        for (j, team_two) in teams.iter().enumerate() {
            if i == j {
                continue;
            }

            let team_two_ratings = team_two.iter().map(|p| p.rating).sum::<f64>();
            let team_two_uncertainties =
                team_two.iter().map(|p| p.uncertainty.powi(2)).sum::<f64>();

            let delta = team_one_ratings - team_two_ratings;
            let denom = (team_two_uncertainties
                + player_count.mul_add(config.beta.powi(2), team_one_uncertainties))
            .sqrt();

            let result = cdf(delta / denom, 0.0, 1.0);

            current_team_probabilities.push(result);
            total_probability += result;
        }

        win_probabilities.push(current_team_probabilities);
    }

    let mut expected_scores = Vec::new();

    for probability in win_probabilities {
        expected_scores.push(probability.iter().sum::<f64>() / total_probability);
    }

    expected_scores
}

#[must_use]
/// Calculates the expected outcome of a player in a rating period or tournament.
///
/// Takes in a players as [`TrueSkillRating`], a list of opponents as a slice of [`TrueSkillRating`] and a [`TrueSkillConfig`]
/// and returns the probability of victory for each match as an Vec of [`f64`] between 1.0 and 0.0 from the perspective of the player.  
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// # Examples
/// ```
/// use skillratings::trueskill::{expected_score_rating_period, TrueSkillConfig, TrueSkillRating};
///
/// let player = TrueSkillRating {
///     rating: 19.0,
///     uncertainty: 4.0,
/// };
///
/// let opponent1 = TrueSkillRating {
///     rating: 19.3,
///     uncertainty: 4.0,
/// };
///
/// let opponent2 = TrueSkillRating {
///     rating: 17.3,
///     uncertainty: 4.0,
/// };
///
/// let config = TrueSkillConfig::new();
///
/// let exp = expected_score_rating_period(&player, &[opponent1, opponent2], &config);
///
/// assert_eq!((exp[0] * 100.0).round(), 49.0);
/// assert_eq!((exp[1] * 100.0).round(), 58.0);
/// ```
pub fn expected_score_rating_period(
    player: &TrueSkillRating,
    opponents: &[TrueSkillRating],
    config: &TrueSkillConfig,
) -> Vec<f64> {
    opponents
        .iter()
        .map(|o| {
            let delta = player.rating - o.rating;

            let denom = o
                .uncertainty
                .mul_add(
                    o.uncertainty,
                    2.0f64.mul_add(config.beta.powi(2), player.uncertainty.powi(2)),
                )
                .sqrt();

            cdf(delta / denom, 0.0, 1.0)
        })
        .collect()
}

#[must_use]
/// Gets the conservatively estimated rank of a player using their rating and deviation.
///
/// This is a conservative estimate of player skill,
/// the system is 99% sure the player's skill is higher than displayed.
///
/// Takes in a player as a [`TrueSkillRating`] and returns the rank as an [`f64`].
///
/// The recommended scale used for Xbox Live is 0 (lowest, starting value) to 50 (highest).
///
/// # Example
/// ```
/// use skillratings::trueskill::{get_rank, TrueSkillRating};
///
/// let new_player = TrueSkillRating::new();
/// let older_player = TrueSkillRating {
///     rating: 43.1,
///     uncertainty: 1.92,
/// };
///
/// let new_rank = get_rank(&new_player);
/// let older_rank = get_rank(&older_player);
///
/// assert!((new_rank.round() - 0.0).abs() < f64::EPSILON);
/// assert!((older_rank.round() - 37.0).abs() < f64::EPSILON);
/// ```
pub fn get_rank(player: &TrueSkillRating) -> f64 {
    player.uncertainty.mul_add(-3.0, player.rating)
}

fn draw_margin(draw_probability: f64, beta: f64, total_players: f64) -> f64 {
    inverse_cdf((draw_probability + 1.0) / 2.0, 0.0, 1.0) * total_players.sqrt() * beta
}

fn v_non_draw(difference: f64, draw_margin: f64, c: f64) -> f64 {
    let diff_c = difference / c;
    let draw_c = draw_margin / c;

    let norm = cdf(diff_c - draw_c, 0.0, 1.0);

    if norm < 2.222_758_749e-162 {
        -diff_c + draw_c
    } else {
        pdf(diff_c - draw_c, 0.0, 1.0) / norm
    }
}

fn w_non_draw(difference: f64, draw_margin: f64, c: f64) -> f64 {
    let diff_c = difference / c;
    let draw_c = draw_margin / c;

    let norm = cdf(diff_c - draw_c, 0.0, 1.0);

    if norm < 2.222_758_749e-162 {
        if diff_c < 0.0 {
            return 1.0;
        }
        return 0.0;
    }

    let v = v_non_draw(difference, draw_margin, c);

    v * (v + (diff_c) - (draw_c))
}

fn v_draw(difference: f64, draw_margin: f64, c: f64) -> f64 {
    let diff_c = difference / c;
    let draw_c = draw_margin / c;
    let diff_c_abs = diff_c.abs();

    let norm = cdf(draw_c - diff_c_abs, 0.0, 1.0) - cdf(-draw_c - diff_c_abs, 0.0, 1.0);

    if norm < 2.222_758_749e-162 {
        if diff_c < 0.0 {
            return -diff_c - draw_c;
        }
        return -diff_c + draw_c;
    }

    let x = pdf(-draw_c - diff_c_abs, 0.0, 1.0) - pdf(draw_c - diff_c_abs, 0.0, 1.0);

    if diff_c < 0.0 {
        -x / norm
    } else {
        x / norm
    }
}

fn w_draw(difference: f64, draw_margin: f64, c: f64) -> f64 {
    let diff_c = difference / c;
    let draw_c = draw_margin / c;
    let diff_c_abs = diff_c.abs();

    let norm = cdf(draw_c - diff_c_abs, 0.0, 1.0) - cdf(-draw_c - diff_c_abs, 0.0, 1.0);

    if norm < 2.222_758_749e-162 {
        return 1.0;
    }

    let v = v_draw(difference, draw_margin, c);

    let p1 = pdf(draw_c - diff_c_abs, 0.0, 1.0);
    let p2 = pdf(-draw_c - diff_c_abs, 0.0, 1.0);

    v.mul_add(
        v,
        (draw_c - diff_c_abs).mul_add(p1, -(-draw_c - diff_c_abs) * p2) / norm,
    )
}

fn new_rating(
    rating: f64,
    uncertainty: f64,
    v: f64,
    c: f64,
    dynamics_factor: f64,
    rank_multiplier: f64,
) -> f64 {
    let mean_multiplier = uncertainty.mul_add(uncertainty, dynamics_factor.powi(2)) / c;

    (rank_multiplier * mean_multiplier).mul_add(v, rating)
}

fn new_uncertainty(uncertainty: f64, c: f64, w: f64, dynamics_factor: f64) -> f64 {
    let variance = uncertainty.mul_add(uncertainty, dynamics_factor.powi(2));
    let dev_multiplier = variance / c.powi(2);

    (variance * w.mul_add(-dev_multiplier, 1.0)).sqrt()
}

// The following functions could have been imported from some math crate,
// but in order to keep this crate dependency-free, we implement them ourselves.
// For more information:
// - https://en.wikipedia.org/wiki/Error_function#Complementary_error_function
// - https://en.wikipedia.org/wiki/Error_function#Cumulative_distribution_function
// - https://en.wikipedia.org/wiki/Probability_density_function

/// The complementary error function.
fn erfc(x: f64) -> f64 {
    let z = x.abs();
    let t = (1.0 + z / 2.0).recip();

    // I know this looks dumb but clippy insists that mul_add increases performance.
    let r = t * t
        .mul_add(
            t.mul_add(
                t.mul_add(
                    t.mul_add(
                        t.mul_add(
                            t.mul_add(
                                t.mul_add(
                                    t.mul_add(t.mul_add(0.170_872_77, -0.822_152_23), 1.488_515_87),
                                    -1.135_203_98,
                                ),
                                0.278_868_07,
                            ),
                            -0.186_288_06,
                        ),
                        0.096_784_18,
                    ),
                    0.374_091_96,
                ),
                1.000_023_68,
            ),
            (-z).mul_add(z, -1.265_512_23),
        )
        .exp();

    if x < 0.0 {
        2.0 - r
    } else {
        r
    }
}

#[allow(clippy::excessive_precision)]
/// The inverse of the complementary error function.
fn inverse_erfc(y: f64) -> f64 {
    if y >= 2.0 {
        return -100.0;
    } else if y <= 0.0 {
        return 100.0;
    }

    // If y = 0.xyz
    let zero_point = y < 1.0;

    // Overwriting y if it is equal to 1.xyz
    let y = if zero_point { y } else { 2.0 - y };

    let t = (-2.0 * (y / 2.0).ln()).sqrt();

    let mut x = -FRAC_1_SQRT_2
        * (t.mul_add(0.27061, 2.30753) / t.mul_add(t.mul_add(0.04481, 0.99229), 1.0) - t);

    for _ in 0..2 {
        let err = erfc(x) - y;
        x += err / FRAC_2_SQRT_PI.mul_add((-(x.powi(2))).exp(), -x * err);
    }

    if zero_point {
        x
    } else {
        -x
    }
}

/// The cumulative distribution function.
fn cdf(x: f64, mu: f64, sigma: f64) -> f64 {
    // In our case, mu and sigma will always be 0.0 and 1.0.
    0.5 * erfc(-(x - mu) / (sigma * SQRT_2))
}

/// The inverse of the cumulative distribution function.
fn inverse_cdf(x: f64, mu: f64, sigma: f64) -> f64 {
    (sigma * SQRT_2).mul_add(-inverse_erfc(2.0 * x), mu)
}

/// The probability density function.
fn pdf(x: f64, mu: f64, sigma: f64) -> f64 {
    ((2.0 * PI).sqrt() * sigma.abs()).recip() * (-(((x - mu) / sigma.abs()).powi(2) / 2.0)).exp()
}

#[allow(clippy::too_many_arguments)]
fn run_schedule(
    rating_vars: &[Rc<RefCell<Variable>>],
    perf_vars: &[Rc<RefCell<Variable>>],
    team_perf_vars: &[Rc<RefCell<Variable>>],
    team_diff_vars: &[Rc<RefCell<Variable>>],
    team_sizes: &[usize],
    sorted_teams_and_ranks: &[(&[TrueSkillRating], MultiTeamOutcome)],
    flattened_ratings: &[TrueSkillRating],
    flattened_weights: &[f64],
    tau: f64,
    beta: f64,
    draw_probability: f64,
    min_delta: f64,
) -> Vec<PriorFactor> {
    assert!((min_delta > 0.0), "min_delta must be greater than 0");

    let mut id = 0;

    let mut rating_layer = build_rating_layer(rating_vars, flattened_ratings, tau, id);
    id += rating_layer.len();
    let mut perf_layer = build_perf_layer(rating_vars, perf_vars, beta, id);
    id += perf_layer.len();
    let mut team_perf_layer =
        build_team_perf_layer(team_perf_vars, perf_vars, flattened_weights, team_sizes, id);
    id += team_perf_layer.len();

    for factor in &mut rating_layer {
        factor.down();
    }
    for factor in &mut perf_layer {
        factor.down();
    }
    for factor in &mut team_perf_layer {
        factor.down();
    }

    let team_diff_layer = build_team_diff_layer(team_diff_vars, team_perf_vars, id);
    let team_diff_len = team_diff_layer.len();
    id += team_diff_len;

    let trunc_layer = build_trunc_layer(
        team_diff_vars,
        sorted_teams_and_ranks,
        draw_probability,
        beta,
        id,
    );

    let mut delta: f64;
    for _ in 0..10 {
        if team_diff_len == 1 {
            team_diff_layer[0].down();
            delta = trunc_layer[0].up();
        } else {
            delta = 0.0;
            for x in 0..(team_diff_len - 1) {
                team_diff_layer[x].down();
                delta = delta.max(trunc_layer[x].up());
                team_diff_layer[x].up(1);
            }
            for x in (1..team_diff_len).rev() {
                team_diff_layer[x].down();
                delta = delta.max(trunc_layer[x].up());
                team_diff_layer[x].up(0);
            }
        }
        if delta <= min_delta {
            break;
        }
    }

    team_diff_layer[0].up(0);
    team_diff_layer[team_diff_len - 1].up(1);
    for f in &mut team_perf_layer {
        for x in 0..f.terms_len() {
            f.up(x);
        }
    }
    for f in &mut perf_layer {
        f.up();
    }

    rating_layer
}

fn build_rating_layer(
    rating_vars: &[Rc<RefCell<Variable>>],
    flattened_ratings: &[TrueSkillRating],
    tau: f64,
    starting_id: usize,
) -> Vec<PriorFactor> {
    let mut v = Vec::with_capacity(rating_vars.len());
    let mut i = starting_id;
    for (var, rating) in rating_vars.iter().zip(flattened_ratings) {
        v.push(PriorFactor::new(
            i,
            Rc::clone(var),
            Gaussian::with_mu_sigma(rating.rating, rating.uncertainty),
            tau,
        ));
        i += 1;
    }

    v
}

fn build_perf_layer(
    rating_vars: &[Rc<RefCell<Variable>>],
    perf_vars: &[Rc<RefCell<Variable>>],
    beta: f64,
    starting_id: usize,
) -> Vec<LikelihoodFactor> {
    let beta_sq = beta.powi(2);
    let mut v = Vec::with_capacity(rating_vars.len());
    let mut i = starting_id;
    for (rating_var, perf_var) in rating_vars.iter().zip(perf_vars) {
        v.push(LikelihoodFactor::new(
            i,
            Rc::clone(rating_var),
            Rc::clone(perf_var),
            beta_sq,
        ));
        i += 1;
    }

    v
}

fn build_team_perf_layer(
    team_perf_vars: &[Rc<RefCell<Variable>>],
    perf_vars: &[Rc<RefCell<Variable>>],
    flattened_weights: &[f64],
    team_sizes: &[usize],
    starting_id: usize,
) -> Vec<SumFactor> {
    let mut v = Vec::with_capacity(team_perf_vars.len());
    let mut i = starting_id;
    for (team, team_perf_var) in team_perf_vars.iter().enumerate() {
        let start = if team > 0 { team_sizes[team - 1] } else { 0 };

        let end = team_sizes[team];
        let child_perf_vars = perf_vars[start..end].to_vec();
        let coeffs = flattened_weights[start..end].to_vec();

        v.push(SumFactor::new(
            i,
            Rc::clone(team_perf_var),
            child_perf_vars,
            coeffs,
        ));
        i += 1;
    }

    v
}

fn build_team_diff_layer(
    team_diff_vars: &[Rc<RefCell<Variable>>],
    team_perf_vars: &[Rc<RefCell<Variable>>],
    starting_id: usize,
) -> Vec<SumFactor> {
    let mut v = Vec::with_capacity(team_diff_vars.len());
    let mut i = starting_id;
    for (team, team_diff_var) in team_diff_vars.iter().enumerate() {
        v.push(SumFactor::new(
            i,
            Rc::clone(team_diff_var),
            team_perf_vars[team..(team + 2)].to_vec(),
            vec![1.0, -1.0],
        ));
        i += 1;
    }

    v
}

fn build_trunc_layer(
    team_diff_vars: &[Rc<RefCell<Variable>>],
    sorted_teams_and_ranks: &[(&[TrueSkillRating], MultiTeamOutcome)],
    draw_probability: f64,
    beta: f64,
    starting_id: usize,
) -> Vec<TruncateFactor> {
    let mut v = Vec::with_capacity(team_diff_vars.len());
    let mut i = starting_id;
    for (x, team_diff_var) in team_diff_vars.iter().enumerate() {
        let size = sorted_teams_and_ranks[x..(x + 2)]
            .iter()
            .map(|v| v.0.len() as f64)
            .sum();
        let draw_margin = draw_margin(draw_probability, beta, size);
        let v_func: Box<dyn Fn(f64, f64, f64) -> f64>;
        let w_func: Box<dyn Fn(f64, f64, f64) -> f64>;
        if sorted_teams_and_ranks[x].1 == sorted_teams_and_ranks[x + 1].1 {
            v_func = Box::new(v_draw);
            w_func = Box::new(w_draw);
        } else {
            v_func = Box::new(v_non_draw);
            w_func = Box::new(w_non_draw);
        };

        v.push(TruncateFactor::new(
            i,
            Rc::clone(team_diff_var),
            v_func,
            w_func,
            draw_margin,
        ));
        i += 1;
    }

    v
}

fn team_sizes(teams_and_ranks: &[(&[TrueSkillRating], MultiTeamOutcome)]) -> Vec<usize> {
    let mut team_sizes = Vec::new();
    for (team, _) in teams_and_ranks {
        if team_sizes.is_empty() {
            team_sizes.push(team.len());
        } else {
            team_sizes.push(team.len() + team_sizes[team_sizes.len() - 1]);
        }
    }

    team_sizes
}

#[cfg(test)]
mod tests {
    #![allow(clippy::unwrap_used)]

    use crate::MultiTeamOutcome;

    use super::*;

    #[test]
    /// This example is taken from this presentation (Page 20):
    /// https://ubm-twvideo01.s3.amazonaws.com/o1/vault/gdc2017/Presentations/Izquierdo_Mario_Ranking_Systems_Elo.pdf
    fn test_trueskill() {
        let player_one = TrueSkillRating::new();
        let player_two = TrueSkillRating {
            rating: 30.0,
            uncertainty: 1.2,
        };

        let (p1, p2) = trueskill(
            &player_one,
            &player_two,
            &Outcomes::WIN,
            &TrueSkillConfig::new(),
        );

        assert!(((p1.rating * 100.0).round() - 3300.0).abs() < f64::EPSILON);
        assert!(((p1.uncertainty * 100.0).round() - 597.0).abs() < f64::EPSILON);

        assert!(((p2.rating * 100.0).round() - 2983.0).abs() < f64::EPSILON);
        assert!(((p2.uncertainty * 100.0).round() - 120.0).abs() < f64::EPSILON);

        let (p1, p2) = trueskill(
            &player_two,
            &player_one,
            &Outcomes::LOSS,
            &TrueSkillConfig::new(),
        );

        assert!(((p2.rating * 100.0).round() - 3300.0).abs() < f64::EPSILON);
        assert!(((p2.uncertainty * 100.0).round() - 597.0).abs() < f64::EPSILON);

        assert!(((p1.rating * 100.0).round() - 2983.0).abs() < f64::EPSILON);
        assert!(((p1.uncertainty * 100.0).round() - 120.0).abs() < f64::EPSILON);

        let player_two = TrueSkillRating::new();

        let (p1, p2) = trueskill(
            &player_one,
            &player_two,
            &Outcomes::WIN,
            &TrueSkillConfig::new(),
        );

        assert!((p1.rating.round() - 29.0).abs() < f64::EPSILON);
        assert!(((p1.uncertainty * 100.0).round() - 717.0).abs() < f64::EPSILON);

        assert!((p2.rating.round() - 21.0).abs() < f64::EPSILON);
        assert!(((p2.uncertainty * 100.0).round() - 717.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_trueskill_rating_period() {
        let player_one = TrueSkillRating::new();
        let player_two = TrueSkillRating {
            rating: 30.0,
            uncertainty: 1.2,
        };
        let player_three = TrueSkillRating {
            rating: 12.0,
            uncertainty: 1.9,
        };
        let player_four = TrueSkillRating {
            rating: 49.0,
            uncertainty: 1.2,
        };

        let player = trueskill_rating_period(
            &player_one,
            &[(player_two, Outcomes::WIN)],
            &TrueSkillConfig::new(),
        );

        assert!(((player.rating * 100.0).round() - 3300.0).abs() < f64::EPSILON);
        assert!(((player.uncertainty * 100.0).round() - 597.0).abs() < f64::EPSILON);

        let player = trueskill_rating_period(
            &player_one,
            &[
                (player_two, Outcomes::WIN),
                (player_three, Outcomes::DRAW),
                (player_four, Outcomes::LOSS),
            ],
            &TrueSkillConfig::new(),
        );

        assert!(((player.rating * 100.0).round() - 2291.0).abs() < f64::EPSILON);
        assert!(((player.uncertainty * 100.0).round() - 430.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_draw() {
        let player_one = TrueSkillRating::new();
        let player_two = TrueSkillRating {
            rating: 30.0,
            uncertainty: 1.2,
        };

        let (p1, p2) = trueskill(
            &player_one,
            &player_two,
            &Outcomes::DRAW,
            &TrueSkillConfig::new(),
        );

        assert!((p1.rating - 28.282_523_394_245_658).abs() < f64::EPSILON);
        assert!(((p1.uncertainty * 100.0).round() - 488.0).abs() < f64::EPSILON);

        assert!((p2.rating - 29.931_612_181_339_364).abs() < f64::EPSILON);
        assert!(((p2.uncertainty * 100.0).round() - 119.0).abs() < f64::EPSILON);

        let (p2, p1) = trueskill(
            &player_two,
            &player_one,
            &Outcomes::DRAW,
            &TrueSkillConfig::new(),
        );

        assert!((p1.rating - 28.282_523_394_245_658).abs() < f64::EPSILON);
        assert!(((p1.uncertainty * 100.0).round() - 488.0).abs() < f64::EPSILON);

        assert!((p2.rating - 29.931_612_181_339_364).abs() < f64::EPSILON);
        assert!(((p2.uncertainty * 100.0).round() - 119.0).abs() < f64::EPSILON);

        let p1 = trueskill_rating_period(
            &player_one,
            &[(player_two, Outcomes::DRAW)],
            &TrueSkillConfig::new(),
        );

        assert!((p1.rating - 28.282_523_394_245_658).abs() < f64::EPSILON);
        assert!(((p1.uncertainty * 100.0).round() - 488.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_unlikely_values() {
        let player_one = TrueSkillRating {
            rating: -9.0,
            uncertainty: -5.0,
        };
        let player_two = TrueSkillRating {
            rating: 7000.0,
            uncertainty: 6000.0,
        };

        let (p1, p2) = trueskill(
            &player_one,
            &player_two,
            &Outcomes::WIN,
            &TrueSkillConfig::new(),
        );

        assert!((p1.rating.round() - -9.0).abs() < f64::EPSILON);
        assert!((p1.uncertainty.round() - 5.0).abs() < f64::EPSILON);

        assert!((p2.rating.round() - -2969.0).abs() < f64::EPSILON);
        assert!((p2.uncertainty.round() - 2549.0).abs() < f64::EPSILON);
    }

    #[test]
    #[allow(clippy::cognitive_complexity)]
    /// This test is taken from:
    /// <https://github.com/moserware/Skills/blob/master/UnitTests/TrueSkill/TrueSkillCalculatorTests.cs>
    fn test_teams() {
        let player_one = TrueSkillRating {
            rating: 20.0,
            uncertainty: 8.0,
        };
        let player_two = TrueSkillRating {
            rating: 25.0,
            uncertainty: 6.0,
        };

        let player_three = TrueSkillRating {
            rating: 35.0,
            uncertainty: 7.0,
        };
        let player_four = TrueSkillRating {
            rating: 40.0,
            uncertainty: 5.0,
        };

        let (team_one, team_two) = trueskill_two_teams(
            &[player_one, player_two],
            &[player_three, player_four],
            &Outcomes::WIN,
            &TrueSkillConfig::new(),
        );

        assert!((team_one[0].rating - 29.698_800_676_796_665).abs() < f64::EPSILON);
        assert!((team_one[1].rating - 30.456_035_750_156_31).abs() < f64::EPSILON);
        assert!((team_two[0].rating - 27.574_109_105_332_1).abs() < f64::EPSILON);
        assert!((team_two[1].rating - 36.210_764_756_738_115).abs() < f64::EPSILON);

        assert!((team_one[0].uncertainty - 7.007_955_406_085_773).abs() < f64::EPSILON);
        assert!((team_one[1].uncertainty - 5.594_025_202_259_947).abs() < f64::EPSILON);
        assert!((team_two[0].uncertainty - 6.346_250_279_230_62).abs() < f64::EPSILON);
        assert!((team_two[1].uncertainty - 4.767_945_180_134_836).abs() < f64::EPSILON);

        let (team_two, team_one) = trueskill_two_teams(
            &[player_three, player_four],
            &[player_one, player_two],
            &Outcomes::LOSS,
            &TrueSkillConfig::new(),
        );

        assert!((team_one[0].rating - 29.698_800_676_796_665).abs() < f64::EPSILON);
        assert!((team_one[1].rating - 30.456_035_750_156_31).abs() < f64::EPSILON);
        assert!((team_two[0].rating - 27.574_109_105_332_1).abs() < f64::EPSILON);
        assert!((team_two[1].rating - 36.210_764_756_738_115).abs() < f64::EPSILON);

        assert!((team_one[0].uncertainty - 7.007_955_406_085_773).abs() < f64::EPSILON);
        assert!((team_one[1].uncertainty - 5.594_025_202_259_947).abs() < f64::EPSILON);
        assert!((team_two[0].uncertainty - 6.346_250_279_230_62).abs() < f64::EPSILON);
        assert!((team_two[1].uncertainty - 4.767_945_180_134_836).abs() < f64::EPSILON);

        let player_one = TrueSkillRating {
            rating: 15.0,
            uncertainty: 8.0,
        };
        let player_two = TrueSkillRating {
            rating: 20.0,
            uncertainty: 6.0,
        };

        let player_three = TrueSkillRating {
            rating: 25.0,
            uncertainty: 4.0,
        };
        let player_four = TrueSkillRating {
            rating: 30.0,
            uncertainty: 3.0,
        };

        let (team_one, team_two) = trueskill_two_teams(
            &[player_one, player_two],
            &[player_three, player_four],
            &Outcomes::DRAW,
            &TrueSkillConfig::new(),
        );

        assert!((team_one[0].rating - 21.571_213_060_731_655).abs() < f64::EPSILON);
        assert!((team_one[1].rating - 23.696_619_260_051_385).abs() < f64::EPSILON);
        assert!((team_two[0].rating - 23.356_662_026_148_804).abs() < f64::EPSILON);
        assert!((team_two[1].rating - 29.075_310_476_318_872).abs() < f64::EPSILON);

        assert!((team_one[0].uncertainty - 6.555_663_733_192_403).abs() < f64::EPSILON);
        assert!((team_one[1].uncertainty - 5.417_723_612_401_869).abs() < f64::EPSILON);
        assert!((team_two[0].uncertainty - 3.832_975_356_683_128).abs() < f64::EPSILON);
        assert!((team_two[1].uncertainty - 2.930_957_525_591_959_5).abs() < f64::EPSILON);

        let (team_one, _) =
            trueskill_two_teams(&[player_one], &[], &Outcomes::WIN, &TrueSkillConfig::new());

        assert_eq!(team_one[0], player_one);
    }

    #[test]
    fn test_solo_team() {
        let player_one = TrueSkillRating::new();
        let player_two = TrueSkillRating {
            rating: 12.0,
            uncertainty: 3.2,
        };

        let (p1, p2) = trueskill(
            &player_one,
            &player_two,
            &Outcomes::WIN,
            &TrueSkillConfig::new(),
        );
        let (tp1, tp2) = trueskill_two_teams(
            &[player_one],
            &[player_two],
            &Outcomes::WIN,
            &TrueSkillConfig::new(),
        );
        let mp = trueskill_multi_team(
            &[
                (&[player_one], MultiTeamOutcome::new(1)),
                (&[player_two], MultiTeamOutcome::new(2)),
            ],
            &TrueSkillConfig::new(),
            None,
        )
        .unwrap();

        assert_eq!(p1, tp1[0]);
        assert_eq!(p2, tp2[0]);

        // There is a small difference to be found, since the trueskill_multi_team uses the full algorithm,
        // while the 1vs1 and Team vs Team implementations use some shortcuts.
        // But they still should yield roughly the same result.
        assert!((p1.rating - mp[0][0].rating).abs() < 0.001);
        assert!((p2.rating - mp[1][0].rating).abs() < 0.001);

        assert!((p1.uncertainty - mp[0][0].uncertainty).abs() < 0.001);
        assert!((p2.uncertainty - mp[1][0].uncertainty).abs() < 0.001);
    }

    #[test]
    fn test_match_quality_two_teams() {
        let player_one = TrueSkillRating {
            rating: 20.0,
            uncertainty: 8.0,
        };
        let player_two = TrueSkillRating {
            rating: 25.0,
            uncertainty: 6.0,
        };

        let player_three = TrueSkillRating {
            rating: 35.0,
            uncertainty: 7.0,
        };
        let player_four = TrueSkillRating {
            rating: 40.0,
            uncertainty: 5.0,
        };

        let quality = match_quality_two_teams(
            &[player_one, player_two],
            &[player_three, player_four],
            &TrueSkillConfig::new(),
        );

        let quality2 = match_quality_two_teams(
            &[player_three, player_four],
            &[player_one, player_two],
            &TrueSkillConfig::new(),
        );

        assert!((quality - 0.084_108_145_418_343_24).abs() < f64::EPSILON);

        assert!((quality - quality2).abs() < f64::EPSILON);
    }

    #[test]
    fn test_expected_score_two_teams() {
        let player_one = TrueSkillRating {
            rating: 38.0,
            uncertainty: 3.0,
        };
        let player_two = TrueSkillRating {
            rating: 38.0,
            uncertainty: 3.0,
        };

        let player_three = TrueSkillRating {
            rating: 44.0,
            uncertainty: 3.0,
        };
        let player_four = TrueSkillRating {
            rating: 44.0,
            uncertainty: 3.0,
        };

        let (exp1, exp2) = expected_score_two_teams(
            &[player_one, player_two],
            &[player_three, player_four],
            &TrueSkillConfig::new(),
        );

        assert!((exp1 + exp2 - 1.0).abs() < f64::EPSILON);

        assert!((exp1 - 0.121_280_517_547_482_7).abs() < f64::EPSILON);
    }

    #[test]
    fn test_quality() {
        let player_one = TrueSkillRating::new();
        let player_two = TrueSkillRating::new();

        let quality = match_quality(&player_one, &player_two, &TrueSkillConfig::new());

        assert!(((quality * 1000.0).round() - 447.0).abs() < f64::EPSILON);

        let player_one = TrueSkillRating {
            rating: 48.0,
            uncertainty: 1.2,
        };

        let player_two = TrueSkillRating {
            rating: 12.0,
            ..Default::default()
        };

        let quality = match_quality(&player_one, &player_two, &TrueSkillConfig::new());

        assert!(((quality * 10000.0).round() - 12.0).abs() < f64::EPSILON);

        let quality2 = match_quality(&player_two, &player_one, &TrueSkillConfig::new());

        assert!((quality - quality2).abs() < f64::EPSILON);
    }

    #[test]
    fn test_expected_score() {
        let player_one = TrueSkillRating::new();
        let player_two = TrueSkillRating::new();

        let (exp1, exp2) = expected_score(&player_one, &player_two, &TrueSkillConfig::new());

        assert!(exp1.mul_add(100.0, -50.0).round().abs() < f64::EPSILON);
        assert!(exp2.mul_add(100.0, -50.0).round().abs() < f64::EPSILON);

        let better_player = TrueSkillRating {
            rating: 44.0,
            uncertainty: 3.0,
        };
        let worse_player = TrueSkillRating {
            rating: 38.0,
            uncertainty: 3.0,
        };

        let (exp1, exp2) =
            expected_score(&better_player, &worse_player, &TrueSkillConfig::default());

        assert!(exp1.mul_add(100.0, -80.0).round().abs() < f64::EPSILON);
        assert!(exp2.mul_add(100.0, -20.0).round().abs() < f64::EPSILON);

        assert!((exp1.mul_add(100.0, exp2 * 100.0).round() - 100.0).abs() < f64::EPSILON);

        // Testing if the other functions give the same result.
        let team_one = [TrueSkillRating::from((44.0, 3.0))];
        let team_two = [TrueSkillRating::from((38.0, 3.0))];

        let (e0, e1) = expected_score_two_teams(&team_one, &team_two, &TrueSkillConfig::new());
        let e = expected_score_multi_team(&[&team_one, &team_two], &TrueSkillConfig::new());

        assert!((e0 - e[0]).abs() < f64::EPSILON);
        assert!((e1 - e[1]).abs() < f64::EPSILON);
        assert!((exp1 - e[0]).abs() < f64::EPSILON);
        assert!((exp2 - e[1]).abs() < f64::EPSILON);
    }

    #[test]
    fn test_match_quality_multi_team() {
        let team_one = vec![TrueSkillRating::new(); 2];
        let team_two = vec![TrueSkillRating::from((30.0, 3.0)); 2];
        let team_three = vec![TrueSkillRating::from((40.0, 2.0)); 2];

        let exp = match_quality_multi_team(
            &[&team_one, &team_two, &team_three],
            &TrueSkillConfig::new(),
            None,
        )
        .unwrap();

        // Double checked this with the most popular python implementation.
        assert!((exp - 0.017_538_349_223_941_27).abs() < f64::EPSILON);

        let exp = match_quality_multi_team(&[], &TrueSkillConfig::default(), None).unwrap();

        assert!(exp < f64::EPSILON);

        let exp =
            match_quality_multi_team(&[&team_one, &[]], &TrueSkillConfig::default(), None).unwrap();

        assert!(exp < f64::EPSILON);
    }

    #[test]
    fn test_match_quality_weighted() {
        let team_one = vec![TrueSkillRating::new(); 2];
        let team_two = vec![TrueSkillRating::from((30.0, 3.0)); 3];
        let team_three = vec![TrueSkillRating::from((40.0, 2.0)); 2];

        let exp = match_quality_multi_team(
            &[&team_one, &team_two, &team_three],
            &TrueSkillConfig::new(),
            Some(&[&[1.0, 0.8], &[1.0, 0.1, 0.222], &[0.12, 0.99]]),
        )
        .unwrap();

        assert!((exp - 0.325_899_305_412_196_14).abs() < f64::EPSILON);
    }

    #[test]
    fn test_multi_team_expected() {
        let team_one = vec![
            TrueSkillRating {
                rating: 38.0,
                uncertainty: 3.0,
            },
            TrueSkillRating {
                rating: 38.0,
                uncertainty: 3.0,
            },
        ];

        let team_two = vec![
            TrueSkillRating {
                rating: 44.0,
                uncertainty: 3.0,
            },
            TrueSkillRating {
                rating: 44.0,
                uncertainty: 3.0,
            },
        ];

        let team_three = vec![
            TrueSkillRating {
                rating: 50.0,
                uncertainty: 3.0,
            },
            TrueSkillRating {
                rating: 50.0,
                uncertainty: 3.0,
            },
        ];

        let exp = expected_score_multi_team(
            &[&team_one, &team_two, &team_three],
            &TrueSkillConfig::new(),
        );

        assert!((exp.iter().sum::<f64>() - 1.0).abs() < f64::EPSILON);

        assert_eq!(
            exp,
            vec![
                0.058_904_655_169_257_615,
                0.333_333_333_333_333_3,
                0.607_762_011_497_409
            ]
        );

        let team_one = vec![TrueSkillRating::new(); 10];
        let team_two = vec![TrueSkillRating::new(); 10];
        let team_three = vec![TrueSkillRating::new(); 10];
        let team_four = vec![TrueSkillRating::new(); 10];

        let exp = expected_score_multi_team(
            &[&team_one, &team_two, &team_three, &team_four],
            &TrueSkillConfig::new(),
        );

        assert!((exp.iter().sum::<f64>() - 1.0).abs() < f64::EPSILON);
        assert_eq!(
            exp,
            vec![
                0.249_999_999_999_999_97,
                0.249_999_999_999_999_97,
                0.249_999_999_999_999_97,
                0.249_999_999_999_999_97
            ]
        );
    }

    #[test]
    fn test_get_rank() {
        let new_player = TrueSkillRating::new();
        let older_player = TrueSkillRating {
            rating: 43.1,
            uncertainty: 1.92,
        };

        let new_rank = get_rank(&new_player);
        let older_rank = get_rank(&older_player);

        assert!((new_rank.round() - 0.0).abs() < f64::EPSILON);
        assert!((older_rank.round() - 37.0).abs() < f64::EPSILON);
    }

    // Some tests to validate the math.
    #[test]
    fn test_erfc() {
        let err = erfc(0.5);

        assert!((err - 0.479_500).abs() < 0.000_001);
    }

    #[test]
    fn test_inverse_erfc() {
        let err = inverse_erfc(0.5);

        assert!((err - 0.476_936).abs() < 0.000_001);

        let err = inverse_erfc(3.0);

        assert!((err + 100.0).abs() < f64::EPSILON);

        let err = inverse_erfc(-1.0);

        assert!((err - 100.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_cdf() {
        let dist = cdf(5.5, 12.0, 5.0);

        assert!((dist - 0.096_800).abs() < 0.000_001);

        let dist_inf = cdf(f64::INFINITY, 0.0, 1.0);

        assert!((dist_inf - 1.0).abs() < f64::EPSILON);

        let dist_neg_inf = cdf(f64::NEG_INFINITY, 0.0, 1.0);

        assert!((dist_neg_inf - 0.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_inverse_cdf() {
        let dist = inverse_cdf(0.055, 12.0, 5.0);

        assert!((dist - 4.009_034).abs() < 0.000_001);
    }

    #[test]
    fn test_pdf() {
        let p = pdf(2.5, 0.0, 1.0);

        assert!((p - 0.017_528).abs() < 0.000_001);
    }

    #[test]
    fn test_wv_edge_cases() {
        let w = w_non_draw(f64::NEG_INFINITY, 0.0, 1.0);

        assert!((w - 1.0).abs() < f64::EPSILON);

        let v = v_non_draw(f64::NEG_INFINITY, 0.0, 1.0);

        assert!(v == f64::INFINITY);

        let w2 = w_draw(f64::NEG_INFINITY, 0.0, 1.0);

        assert!((w2 - 1.0).abs() < f64::EPSILON);

        let v2 = v_draw(f64::NEG_INFINITY, 0.0, 1.0);

        assert!(v2 == f64::INFINITY);

        let w3 = w_non_draw(1.0, f64::INFINITY, 1.0);

        assert!((w3 - 0.0).abs() < f64::EPSILON);

        let v3 = v_draw(f64::INFINITY, f64::MAX, 1.0);

        assert!(v3 == f64::NEG_INFINITY);
    }

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn test_misc_stuff() {
        let player_one = TrueSkillRating::new();
        let config = TrueSkillConfig::new();

        assert_eq!(player_one, player_one.clone());
        assert!((config.beta - config.clone().beta).abs() < f64::EPSILON);

        assert!(!format!("{player_one:?}").is_empty());
        assert!(!format!("{config:?}").is_empty());

        assert_eq!(player_one, TrueSkillRating::from((25.0, 25.0 / 3.0)));
    }

    #[test]
    fn test_traits() {
        let player_one: TrueSkillRating = Rating::new(Some(24.0), Some(2.0));
        let player_two: TrueSkillRating = Rating::new(Some(24.0), Some(2.0));

        let rating_system: TrueSkill = RatingSystem::new(TrueSkillConfig::new());

        assert!((player_one.rating() - 24.0).abs() < f64::EPSILON);
        assert_eq!(player_one.uncertainty(), Some(2.0));

        let (new_player_one, new_player_two) =
            RatingSystem::rate(&rating_system, &player_one, &player_two, &Outcomes::WIN);

        let (exp1, exp2) = RatingSystem::expected_score(&rating_system, &player_one, &player_two);

        assert!((new_player_one.rating - 24.534_185_520_312_818).abs() < f64::EPSILON);
        assert!((new_player_two.rating - 23.465_814_479_687_182).abs() < f64::EPSILON);
        assert!((exp1 + exp2 - 1.0).abs() < f64::EPSILON);

        let rating_period_system: TrueSkill = RatingPeriodSystem::new(TrueSkillConfig::new());
        let exp_rp =
            RatingPeriodSystem::expected_score(&rating_period_system, &player_one, &[player_two]);
        assert!((exp1 - exp_rp[0]).abs() < f64::EPSILON);

        let player_one: TrueSkillRating = Rating::new(Some(24.0), Some(2.0));
        let player_two: TrueSkillRating = Rating::new(Some(24.0), Some(2.0));

        let rating_period: TrueSkill = RatingPeriodSystem::new(TrueSkillConfig::new());

        let new_player_one =
            RatingPeriodSystem::rate(&rating_period, &player_one, &[(player_two, Outcomes::WIN)]);

        assert!((new_player_one.rating - 24.534_185_520_312_818).abs() < f64::EPSILON);

        let player_one: TrueSkillRating = Rating::new(Some(24.0), Some(2.0));
        let player_two: TrueSkillRating = Rating::new(Some(24.0), Some(2.0));

        let team_rating: TrueSkill = TeamRatingSystem::new(TrueSkillConfig::new());

        let (new_team_one, new_team_two) =
            TeamRatingSystem::rate(&team_rating, &[player_one], &[player_two], &Outcomes::WIN);

        assert!((new_team_one[0].rating - 24.534_185_520_312_818).abs() < f64::EPSILON);
        assert!((new_team_two[0].rating - 23.465_814_479_687_182).abs() < f64::EPSILON);

        let (exp1, exp2) =
            TeamRatingSystem::expected_score(&rating_system, &[player_one], &[player_two]);

        assert!((exp1 + exp2 - 1.0).abs() < f64::EPSILON);

        let multi_team_rating: TrueSkill = MultiTeamRatingSystem::new(TrueSkillConfig::new());
        let mtr = MultiTeamRatingSystem::rate(
            &multi_team_rating,
            &[
                (&[player_one], MultiTeamOutcome::new(1)),
                (&[player_two], MultiTeamOutcome::new(2)),
            ],
        );

        assert!((mtr[0][0].rating - 24.534_091_256_161_39).abs() < f64::EPSILON);
        assert!((mtr[1][0].rating - 23.465_908_743_838_607).abs() < f64::EPSILON);

        let exp = MultiTeamRatingSystem::expected_score(
            &multi_team_rating,
            &[&[player_one], &[player_two]],
        );

        assert!(((exp.iter().sum::<f64>()) - 1.0).abs() < f64::EPSILON);
    }

    #[test]
    /// This example is taken from the Python TrueSkill package:
    /// https://github.com/sublee/trueskill/blob/3ff78b26c8374b30cc0d6bae32cb45c9bee46a1d/trueskilltest.py#L311
    fn test_trueskill_multi_team() {
        let t1p1 = TrueSkillRating {
            rating: 40.0,
            uncertainty: 4.0,
        };
        let t1p2 = TrueSkillRating {
            rating: 45.0,
            uncertainty: 3.0,
        };

        let t2p1 = TrueSkillRating {
            rating: 20.0,
            uncertainty: 7.0,
        };
        let t2p2 = TrueSkillRating {
            rating: 19.0,
            uncertainty: 6.0,
        };
        let t2p3 = TrueSkillRating {
            rating: 30.0,
            uncertainty: 9.0,
        };
        let t2p4 = TrueSkillRating {
            rating: 10.0,
            uncertainty: 4.0,
        };

        let t3p1 = TrueSkillRating {
            rating: 50.0,
            uncertainty: 5.0,
        };
        let t3p2 = TrueSkillRating {
            rating: 30.0,
            uncertainty: 2.0,
        };

        let t1 = [t1p1, t1p2];
        let t2 = [t2p1, t2p2, t2p3, t2p4];
        let t3 = [t3p1, t3p2];

        let teams_and_ranks = [
            (&t1[..], MultiTeamOutcome::new(0)),
            (&t2[..], MultiTeamOutcome::new(1)),
            (&t3[..], MultiTeamOutcome::new(1)),
        ];
        let results =
            trueskill_multi_team(&teams_and_ranks, &TrueSkillConfig::new(), None).unwrap();

        assert!((results[0][0].rating - 40.876_849_177_315_655).abs() < f64::EPSILON);
        assert!((results[0][1].rating - 45.493_394_092_398_45).abs() < f64::EPSILON);

        assert!((results[1][0].rating - 19.608_650_920_845_23).abs() < f64::EPSILON);
        assert!((results[1][1].rating - 18.712_463_514_890_54).abs() < f64::EPSILON);
        assert!((results[1][2].rating - 29.353_112_227_810_637).abs() < f64::EPSILON);
        assert!((results[1][3].rating - 9.872_175_198_037_164).abs() < f64::EPSILON);

        assert!((results[2][0].rating - 48.829_832_201_455_31).abs() < f64::EPSILON);
        assert!((results[2][1].rating - 29.812_500_188_902_998).abs() < f64::EPSILON);

        assert!((results[0][0].uncertainty - 3.839_527_589_355_369_8).abs() < f64::EPSILON);
        assert!((results[0][1].uncertainty - 2.933_671_613_522_051).abs() < f64::EPSILON);

        assert!((results[1][0].uncertainty - 6.396_044_310_523_896).abs() < f64::EPSILON);
        assert!((results[1][1].uncertainty - 5.624_556_429_622_889).abs() < f64::EPSILON);
        assert!((results[1][2].uncertainty - 7.673_456_361_986_593).abs() < f64::EPSILON);
        assert!((results[1][3].uncertainty - 3.891_408_425_994_520_3).abs() < f64::EPSILON);

        assert!((results[2][0].uncertainty - 4.590_018_525_151_379).abs() < f64::EPSILON);
        assert!((results[2][1].uncertainty - 1.976_314_792_712_798).abs() < f64::EPSILON);
    }

    #[test]
    fn test_trueskill_multi_team_weighted() {
        let t1p1 = TrueSkillRating {
            rating: 40.0,
            uncertainty: 4.0,
        };
        let t1p2 = TrueSkillRating {
            rating: 45.0,
            uncertainty: 3.0,
        };

        let t2p1 = TrueSkillRating {
            rating: 20.0,
            uncertainty: 7.0,
        };
        let t2p2 = TrueSkillRating {
            rating: 19.0,
            uncertainty: 6.0,
        };
        let t2p3 = TrueSkillRating {
            rating: 30.0,
            uncertainty: 9.0,
        };
        let t2p4 = TrueSkillRating {
            rating: 10.0,
            uncertainty: 4.0,
        };

        let t3p1 = TrueSkillRating {
            rating: 50.0,
            uncertainty: 5.0,
        };
        let t3p2 = TrueSkillRating {
            rating: 30.0,
            uncertainty: 2.0,
        };

        let t1 = [t1p1, t1p2];
        let t2 = [t2p1, t2p2, t2p3, t2p4];
        let t3 = [t3p1, t3p2];

        let teams_and_ranks = [
            (&t1[..], MultiTeamOutcome::new(0)),
            (&t2[..], MultiTeamOutcome::new(1)),
            (&t3[..], MultiTeamOutcome::new(1)),
        ];

        let results = trueskill_multi_team(
            &teams_and_ranks,
            &TrueSkillConfig::new(),
            Some(&[&[0.7, 1.0], &[0.33, 0.33, 0.9, 1.0], &[1.0, 0.0333]]),
        )
        .unwrap();

        assert!((results[0][0].rating - 40.027_231_346_252_364).abs() < f64::EPSILON);
        assert!((results[0][1].rating - 45.021_889_715_580_61).abs() < f64::EPSILON);

        assert!((results[1][0].rating - 20.098_036_453_507_61).abs() < f64::EPSILON);
        assert!((results[1][1].rating - 19.072_030_467_824_426).abs() < f64::EPSILON);
        assert!((results[1][2].rating - 30.441_957_784_082_72).abs() < f64::EPSILON);
        assert!((results[1][3].rating - 10.097_034_118_427_457).abs() < f64::EPSILON);

        assert!((results[2][0].rating - 49.787_633_108_041_61).abs() < f64::EPSILON);
        assert!((results[2][1].rating - 29.998_866_859_567_126).abs() < f64::EPSILON);

        assert!((results[0][0].uncertainty - 3.990_045_287_325_199).abs() < f64::EPSILON);
        assert!((results[0][1].uncertainty - 2.991_832_600_124_326_7).abs() < f64::EPSILON);

        assert!((results[1][0].uncertainty - 6.888_248_560_920_697).abs() < f64::EPSILON);
        assert!((results[1][1].uncertainty - 5.930_038_936_969_937).abs() < f64::EPSILON);
        assert!((results[1][2].uncertainty - 7.023_102_950_896_653).abs() < f64::EPSILON);
        assert!((results[1][3].uncertainty - 3.805_217_937_147_945).abs() < f64::EPSILON);

        assert!((results[2][0].uncertainty - 4.589_940_271_249_083).abs() < f64::EPSILON);
        assert!((results[2][1].uncertainty - 2.001_707_343_315_781_7).abs() < f64::EPSILON);
    }

    #[test]
    fn test_ffa() {
        let p1 = TrueSkillRating {
            rating: 41.023,
            uncertainty: 2.1333,
        };
        let p2 = TrueSkillRating {
            rating: 21.0,
            uncertainty: 1.87,
        };

        let p3 = TrueSkillRating {
            rating: 42.0,
            uncertainty: 1.223,
        };

        let teams_and_ranks: &[(&[TrueSkillRating], MultiTeamOutcome)] = &[
            (&[p1], MultiTeamOutcome::new(1)),
            (&[p2], MultiTeamOutcome::new(3)),
            (&[p3], MultiTeamOutcome::new(2)),
        ];

        let results = trueskill_multi_team(teams_and_ranks, &TrueSkillConfig::new(), None).unwrap();

        assert!((results[0][0].rating - 41.720_925_460_665_01).abs() < f64::EPSILON);
        assert!((results[1][0].rating - 20.997_268_045_415_94).abs() < f64::EPSILON);
        assert!((results[2][0].rating - 41.771_076_420_914_83).abs() < f64::EPSILON);

        assert!((results[0][0].uncertainty - 2.050_533_079_246_658_7).abs() < f64::EPSILON);
        assert!((results[1][0].uncertainty - 1.870_534_805_422_220_2).abs() < f64::EPSILON);
        assert!((results[2][0].uncertainty - 1.209_939_281_670_434_9).abs() < f64::EPSILON);
    }

    #[test]
    fn test_unlikely_ffa() {
        let p1 = TrueSkillRating {
            rating: 0.4,
            uncertainty: 8.1333,
        };
        let p2 = TrueSkillRating {
            rating: -21.0,
            uncertainty: 1.87,
        };

        let p3 = TrueSkillRating {
            rating: 122.0,
            uncertainty: 0.01,
        };

        let p4 = TrueSkillRating {
            rating: -1.0,
            uncertainty: -1.223,
        };

        let teams_and_ranks: &[(&[TrueSkillRating], MultiTeamOutcome)] = &[
            (&[p1], MultiTeamOutcome::new(1)),
            (&[p2], MultiTeamOutcome::new(3)),
            (&[p3], MultiTeamOutcome::new(2)),
            (&[p4], MultiTeamOutcome::new(2)),
        ];

        let results = trueskill_multi_team(teams_and_ranks, &TrueSkillConfig::new(), None).unwrap();

        assert!((results[0][0].rating - 46.844_398_641_974_97).abs() < f64::EPSILON);
        assert!((results[1][0].rating - -21.0).abs() < f64::EPSILON);
        assert!((results[2][0].rating - 121.973_594_228_967_43).abs() < f64::EPSILON);
        assert!((results[3][0].rating - 3.577_783_039_440_43).abs() < f64::EPSILON);

        assert!((results[0][0].uncertainty - 4.453_979_220_477_661).abs() < f64::EPSILON);
        assert!((results[1][0].uncertainty - 1.871_855_882_391_709_3).abs() < f64::EPSILON);
        assert!((results[2][0].uncertainty - 0.083_922_196_135_183_55).abs() < f64::EPSILON);
        assert!((results[3][0].uncertainty - 1.197_926_990_096_302_3).abs() < f64::EPSILON);
    }

    #[test]
    fn test_multi_teams_empty() {
        let res = trueskill_multi_team(&[], &TrueSkillConfig::new(), None);

        assert!(res.unwrap().is_empty());

        let res = trueskill_multi_team(
            &[
                (&[TrueSkillRating::new()], MultiTeamOutcome::new(1)),
                (&[], MultiTeamOutcome::new(2)),
            ],
            &TrueSkillConfig::new(),
            None,
        )
        .unwrap();

        assert!(res[1].is_empty());
        assert!((res[0][0].rating - 25.0).abs() < f64::EPSILON);
        assert!((res[0][0].uncertainty - 25.0 / 3.0).abs() < f64::EPSILON);
    }
}
