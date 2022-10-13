//! The TrueSkill rating algorithm, developed by Microsoft for Halo 3.  
//! Used in the Halo games, the Forza Games, Tom Clancy's: Rainbow Six Siege, and most Xbox Live games.
//!
//! Developed specifically for online games with multiple teams and multiple players.
//!
//! **Caution:** TrueSkill is patented. If you have a commercial project, it is recommended to use another algorithm included here.
//!
//! TrueSkill uses a normal distribution with a skill rating (μ) and an uncertainty value (σ) to represent a players skill level,
//! similar to [`Glicko`](crate::glicko) and [`Glicko-2`](crate::glicko2).
//! TrueSkill is good at assessing a player's skill level quickly.
//! The amount of matches needed depends on the game mode, as follows (The actual numbers may vary):
//!
//! | Match type | Matches needed |
//! | ---------- | -------------- |
//! | 1 Player vs 1 Player | 12 |
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
//!     trueskill::trueskill, outcomes::Outcomes, rating::TrueSkillRating, config::TrueSkillConfig
//! };
//!
//! // Initialise a new player rating.
//! let player_one = TrueSkillRating::new();
//!
//! // Or you can initialise it with your own values of course.
//! // Imagine these numbers being pulled from a database.
//! let (some_rating, some_uncertainty) = (34.2, 2.3);
//! let player_two = TrueSkillRating{
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
//! - [TrueSkill Ranking System](https://www.microsoft.com/en-us/research/project/trueskill-ranking-system/)
//! - [Original Paper (PDF)](https://proceedings.neurips.cc/paper/2006/file/f44ee263952e65b3610b8ba51229d1f9-Paper.pdf)
//! - [The math behind TrueSkill (PDF)](http://www.moserware.com/assets/computing-your-skill/The%20Math%20Behind%20TrueSkill.pdf)
//! - [Moserware: Computing Your Skill](http://www.moserware.com/2010/03/computing-your-skill.html)

use std::f64::consts::{FRAC_1_SQRT_2, PI, SQRT_2};

use crate::{config::TrueSkillConfig, outcomes::Outcomes, rating::TrueSkillRating};
#[must_use]
/// Calculates the [`TrueSkillRating`]s of two players based on their old ratings, uncertainties, and the outcome of the game.
///
/// Takes in two players as [`TrueSkillRating`]s, an [`Outcome`](Outcomes), and a [`TrueSkillConfig`].
///
/// The outcome of the match is in the perspective of `player_one`.
/// This means [`Outcomes::WIN`] is a win for `player_one` and [`Outcomes::LOSS`] is a win for `player_two`.
///
/// Similar to [`trueskill_rating_period`] and [`trueskill_teams`].
///
/// **Caution regarding usage of TrueSkill**:  
/// Microsoft permits only Xbox Live games or non-commercial projects to use TrueSkill.  
/// If your project is commercial, you should use another rating system included here.
///
/// # Examples
/// ```
/// use skillratings::{rating::TrueSkillRating, trueskill::trueskill, outcomes::Outcomes, config::TrueSkillConfig};
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
/// let (player_one, player_two) = trueskill(&player_one, &player_two, &outcome, &config);
///
/// assert!(((player_one.rating * 100.0).round() - 4410.0).abs() < f64::EPSILON);
/// assert!(((player_one.uncertainty * 100.0).round() - 528.0).abs() < f64::EPSILON);
///
/// assert!(((player_two.rating * 100.0).round() - 4960.0).abs() < f64::EPSILON);
/// assert!(((player_two.uncertainty * 100.0).round() - 121.0).abs() < f64::EPSILON);
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

    let player_one_new = update_rating(
        *player_one,
        v,
        w,
        c,
        config.default_dynamics,
        rank_multiplier1,
    );
    let player_two_new = update_rating(
        *player_two,
        v,
        w,
        c,
        config.default_dynamics,
        rank_multiplier2,
    );

    (player_one_new, player_two_new)
}

#[must_use]
/// Calculates a [`TrueSkillRating`] in a non-traditional way using a rating period,
/// for compatibility with the other algorithms.
///
/// Takes in a player as an [`TrueSkillRating`] and their results as a Vec of tuples containing the opponent as an [`TrueSkillRating`],
/// the outcome of the game as an [`Outcome`](Outcomes) and a [`TrueSkillConfig`].
///
/// The outcome of the match is in the perspective of the player.
/// This means [`Outcomes::WIN`] is a win for the player and [`Outcomes::LOSS`] is a win for the opponent.
///
/// Similar to [`trueskill`] or [`trueskill_teams`].
///
/// **Caution regarding usage of TrueSkill**:  
/// Microsoft permits only Xbox Live games or non-commercial projects to use TrueSkill.  
/// If your project is commercial, you should use another rating system included here.
///
/// # Examples
/// ```
/// use skillratings::{
///     rating::TrueSkillRating, trueskill::trueskill_rating_period, outcomes::Outcomes, config::TrueSkillConfig
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
/// let player = trueskill_rating_period(
///     &player_one,
///     &vec![
///         (player_two, Outcomes::WIN),
///         (player_three, Outcomes::WIN),
///         (player_four, Outcomes::LOSS),
///     ],
///     &TrueSkillConfig::new(),
/// );
///
/// assert!(((player.rating * 100.0).round() - 3277.0).abs() < f64::EPSILON);
/// assert!(((player.uncertainty * 100.0).round() - 566.0).abs() < f64::EPSILON);
/// ```
pub fn trueskill_rating_period(
    player: &TrueSkillRating,
    results: &Vec<(TrueSkillRating, Outcomes)>,
    config: &TrueSkillConfig,
) -> TrueSkillRating {
    let mut player = *player;

    let draw_margin = draw_margin(config.draw_probability, config.beta, 2.0);

    for (opponent, result) in results {
        let c = 2.0f64
            .mul_add(
                config.beta.powi(2),
                player
                    .uncertainty
                    .mul_add(player.uncertainty, opponent.uncertainty.powi(2)),
            )
            .sqrt();

        let rating_delta = match result {
            Outcomes::WIN | Outcomes::DRAW => player.rating - opponent.rating,
            Outcomes::LOSS => opponent.rating - player.rating,
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

        player = update_rating(player, v, w, c, config.default_dynamics, rank_multiplier);
    }

    player
}

#[must_use]
#[allow(clippy::as_conversions, clippy::cast_precision_loss)]
/// Calculates the [`TrueSkillRating`] of two teams based on their ratings, uncertainties, and the outcome of the game.
///
/// Takes in two teams as a Vec of [`TrueSkillRating`]s, the outcome of the game as an [`Outcome`](Outcomes) and a [`TrueSkillConfig`].
///
/// The outcome of the match is in the perspective of `team_one`.
/// This means [`Outcomes::WIN`] is a win for `team_one` and [`Outcomes::LOSS`] is a win for `team_two`.
///
/// Similar to [`trueskill`].
///
/// **Caution regarding usage of TrueSkill**:
/// Microsoft permits only Xbox Live games or non-commercial projects to use TrueSkill(TM).
/// If your project is commercial, you should use another rating system included here.
///
/// # Examples
/// ```
/// use skillratings::{
///     trueskill::trueskill_teams, rating::TrueSkillRating, outcomes::Outcomes, config::TrueSkillConfig
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
/// let (team_one, team_two) = trueskill_teams(
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
pub fn trueskill_teams(
    team_one: &Vec<TrueSkillRating>,
    team_two: &Vec<TrueSkillRating>,
    outcome: &Outcomes,
    config: &TrueSkillConfig,
) -> (Vec<TrueSkillRating>, Vec<TrueSkillRating>) {
    if team_one.is_empty() || team_two.is_empty() {
        return (team_one.clone(), team_two.clone());
    }

    let total_players = (team_one.len() + team_two.len()) as f64;

    let draw_margin = draw_margin(config.draw_probability, config.beta, total_players);

    let rating_one_sum = team_one.iter().map(|p| p.rating).sum::<f64>();
    let rating_two_sum = team_two.iter().map(|p| p.rating).sum::<f64>();

    let uncertainty_one_sum = team_one.iter().map(|p| p.uncertainty.powi(2)).sum::<f64>();
    let uncertainty_two_sum = team_two.iter().map(|p| p.uncertainty.powi(2)).sum::<f64>();

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

    let new_team_one = update_rating_teams(
        team_one.clone(),
        v,
        w,
        c,
        config.default_dynamics,
        rank_multiplier1,
    );
    let new_team_two = update_rating_teams(
        team_two.clone(),
        v,
        w,
        c,
        config.default_dynamics,
        rank_multiplier2,
    );

    (new_team_one, new_team_two)
}

#[must_use]
/// Gets the quality of the match, which is equal to the probability that the match will end in a draw.
/// The higher the Value, the better the quality of the match.
///
/// Takes in two players as [`TrueSkillRating`]s and returns the probability of a draw occurring as an [`f64`] between 1.0 and 0.0.
///
/// Similar to [`match_quality_teams`].
///
/// # Examples
/// ```
/// use skillratings::{rating::TrueSkillRating, trueskill::match_quality, config::TrueSkillConfig};
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

    let b = ((-1.0 * delta.powi(2))
        / (2.0
            * player_two.uncertainty.mul_add(
                player_two.uncertainty,
                2.0f64.mul_add(config.beta.powi(2), player_one.uncertainty.powi(2)),
            )))
    .exp();

    a * b
}

#[must_use]
#[allow(
    clippy::as_conversions,
    clippy::cast_precision_loss,
    clippy::needless_pass_by_value
)]
/// Gets the quality of the match, which is equal to the probability that the match will end in a draw.
/// The higher the Value, the better the quality of the match.
///
/// Takes in two teams as Vec of [`TrueSkillRating`]s and returns the probability of a draw occurring as an [`f64`] between 1.0 and 0.0.
///
/// Similar to [`match_quality`].
///
/// # Examples
/// ```
/// use skillratings::{rating::TrueSkillRating, trueskill::match_quality_teams, config::TrueSkillConfig};
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
/// let qual = match_quality_teams(
///     &vec![player_one, player_two],
///     &vec![player_three, player_four],
///     &TrueSkillConfig::new(),
/// );
///
/// // There is a 8.4% chance of a draw occurring.
/// assert!((qual - 0.084_108_145_418_343_24).abs() < f64::EPSILON);
/// ```
pub fn match_quality_teams(
    team_one: &Vec<TrueSkillRating>,
    team_two: &Vec<TrueSkillRating>,
    config: &TrueSkillConfig,
) -> f64 {
    let total_players = (team_one.len() + team_two.len()) as f64;

    let rating_one_sum = team_one.iter().map(|p| p.rating).sum::<f64>();
    let rating_two_sum = team_two.iter().map(|p| p.rating).sum::<f64>();

    let uncertainty_one_sum = team_one.iter().map(|p| p.uncertainty.powi(2)).sum::<f64>();
    let uncertainty_two_sum = team_two.iter().map(|p| p.uncertainty.powi(2)).sum::<f64>();

    let a = ((total_players * config.beta.powi(2))
        / (total_players.mul_add(config.beta.powi(2), uncertainty_one_sum) + uncertainty_two_sum))
        .sqrt();

    let b = ((-1.0 * (rating_one_sum - rating_two_sum).powi(2))
        / (2.0
            * (total_players.mul_add(config.beta.powi(2), uncertainty_one_sum)
                + uncertainty_two_sum)))
        .exp();

    a * b
}

#[must_use]
/// Calculates the expected outcome of two players based on TrueSkill.
///
/// Takes in two players as [`TrueSkillRating`]s and returns the probability of victory for each player as an [`f64`] between 1.0 and 0.0.  
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// Similar to [`expected_score_teams`].
///
/// To see the actual chances of a draw occurring, please use [`match_quality`].
///
/// # Examples
/// ```
/// use skillratings::{rating::TrueSkillRating, trueskill::expected_score, config::TrueSkillConfig};
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
    let delta1 = player_one.rating - player_two.rating;
    let delta2 = player_two.rating - player_one.rating;

    let denom1 = player_two
        .uncertainty
        .mul_add(
            player_two.uncertainty,
            2.0f64.mul_add(config.beta.powi(2), player_one.uncertainty.powi(2)),
        )
        .sqrt();
    let denom2 = player_one
        .uncertainty
        .mul_add(
            player_one.uncertainty,
            2.0f64.mul_add(config.beta.powi(2), player_two.uncertainty.powi(2)),
        )
        .sqrt();

    (
        cdf(delta1 / denom1, 0.0, 1.0),
        cdf(delta2 / denom2, 0.0, 1.0),
    )
}

#[must_use]
#[allow(
    clippy::needless_pass_by_value,
    clippy::as_conversions,
    clippy::cast_precision_loss
)]
/// Calculates the expected outcome of two teams based on TrueSkill.
///
/// Takes in two teams as Vec of [`TrueSkillRating`]s and returns the probability of victory for each player as an [`f64`] between 1.0 and 0.0.  
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// Similar to [`expected_score`].
///
/// To see the actual chances of a draw occurring, please use [`match_quality_teams`].
///
/// # Examples
/// ```
/// use skillratings::{rating::TrueSkillRating, trueskill::expected_score_teams, config::TrueSkillConfig};
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
/// let (exp1, exp2) = expected_score_teams(
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
pub fn expected_score_teams(
    team_one: &Vec<TrueSkillRating>,
    team_two: &Vec<TrueSkillRating>,
    config: &TrueSkillConfig,
) -> (f64, f64) {
    let player_count = (team_one.len() + team_two.len()) as f64;

    let rating_one_sum = team_one.iter().map(|p| p.rating).sum::<f64>();
    let rating_two_sum = team_two.iter().map(|p| p.rating).sum::<f64>();

    let uncertainty_one_sum = team_one.iter().map(|p| p.uncertainty.powi(2)).sum::<f64>();
    let uncertainty_two_sum = team_two.iter().map(|p| p.uncertainty.powi(2)).sum::<f64>();

    let delta1 = rating_one_sum - rating_two_sum;
    let delta2 = rating_two_sum - rating_one_sum;

    let denom1 = (uncertainty_two_sum
        + player_count.mul_add(config.beta.powi(2), uncertainty_one_sum))
    .sqrt();
    let denom2 = (uncertainty_one_sum
        + player_count.mul_add(config.beta.powi(2), uncertainty_two_sum))
    .sqrt();

    (
        cdf(delta1 / denom1, 0.0, 1.0),
        cdf(delta2 / denom2, 0.0, 1.0),
    )
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
/// use skillratings::{rating::TrueSkillRating, trueskill::get_rank};
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
    player.rating - (player.uncertainty * 3.0)
}

fn draw_margin(draw_probability: f64, beta: f64, total_players: f64) -> f64 {
    inverse_cdf(0.5 * (draw_probability + 1.0), 0.0, 1.0) * total_players.sqrt() * beta
}

fn v_non_draw(difference: f64, draw_margin: f64, c: f64) -> f64 {
    let norm = cdf(difference / c - draw_margin / c, 0.0, 1.0);

    if norm < 2.222_758_749e-162 {
        return (-1.0f64).mul_add(difference / c, draw_margin);
    }

    pdf((difference / c) - (draw_margin / c), 0.0, 1.0) / norm
}

fn w_non_draw(difference: f64, draw_margin: f64, c: f64) -> f64 {
    let norm = cdf(difference / c - draw_margin / c, 0.0, 1.0);

    if norm < 2.222_758_749e-162 {
        if (difference / c) < 0.0 {
            return 1.0;
        }
        return 0.0;
    }

    let v = v_non_draw(difference, draw_margin, c);

    v * (v + (difference / c) - (draw_margin / c))
}

fn v_draw(difference: f64, draw_margin: f64, c: f64) -> f64 {
    let difference_abs = difference.abs();

    let norm = cdf((draw_margin / c) - (difference_abs / c), 0.0, 1.0)
        - cdf((-1.0 * (draw_margin / c)) - (difference_abs / c), 0.0, 1.0);

    if norm < 2.222_758_749e-162 {
        if (difference / c) < 0.0 {
            return -1.0 * difference - draw_margin;
        }

        return (-1.0f64).mul_add(difference, draw_margin);
    }

    let x = pdf((-1.0 * (draw_margin / c)) - (difference_abs / c), 0.0, 1.0)
        - pdf((draw_margin / c) - (difference_abs / c), 0.0, 1.0);

    if (difference / c) < 0.0 {
        return (-1.0 * x) / norm;
    }

    x / norm
}

fn w_draw(difference: f64, draw_margin: f64, c: f64) -> f64 {
    let difference_abs = difference.abs();

    let norm = cdf((draw_margin / c) - (difference_abs / c), 0.0, 1.0)
        - cdf((-1.0 * (draw_margin / c)) - (difference_abs / c), 0.0, 1.0);

    if norm < 2.222_758_749e-162 {
        return 1.0;
    }

    let v = v_draw(difference, draw_margin, c);

    let p1 = pdf((draw_margin / c) - (difference_abs / c), 0.0, 1.0);
    let p2 = pdf((-1.0 * (draw_margin / c)) - (difference_abs / c), 0.0, 1.0);

    v.mul_add(
        v,
        (((draw_margin / c) - (difference_abs / c)) * p1
            - ((-1.0 * (draw_margin / c)) - (difference_abs / c)) * p2)
            / norm,
    )
}

fn update_rating(
    player: TrueSkillRating,
    v: f64,
    w: f64,
    c: f64,
    default_dynamics: f64,
    rank_multiplier: f64,
) -> TrueSkillRating {
    let mean_multiplier = player
        .uncertainty
        .mul_add(player.uncertainty, default_dynamics.powi(2))
        / c;

    let variance = player
        .uncertainty
        .mul_add(player.uncertainty, default_dynamics.powi(2));
    let dev_multiplier = variance / c.powi(2);

    let new_rating = (rank_multiplier * mean_multiplier).mul_add(v, player.rating);
    let new_uncertainty = (variance * (1.0 - w * dev_multiplier)).sqrt();

    TrueSkillRating {
        rating: new_rating,
        uncertainty: new_uncertainty,
    }
}

fn update_rating_teams(
    team: Vec<TrueSkillRating>,
    v: f64,
    w: f64,
    c: f64,
    default_dynamics: f64,
    rank_multiplier: f64,
) -> Vec<TrueSkillRating> {
    let mut new_team = Vec::new();

    for player in team {
        let mean_multiplier =
            (player.uncertainty).mul_add(player.uncertainty, default_dynamics.powi(2)) / c;

        let dev_multiplier = player
            .uncertainty
            .mul_add(player.uncertainty, default_dynamics.powi(2))
            / c.powi(2);

        let rating_delta = rank_multiplier * mean_multiplier * v;
        let new_rating = player.rating + rating_delta;

        let new_uncertainty = (player
            .uncertainty
            .mul_add(player.uncertainty, default_dynamics.powi(2))
            * (1.0 - w * dev_multiplier))
            .sqrt();

        new_team.push(TrueSkillRating {
            rating: new_rating,
            uncertainty: new_uncertainty,
        });
    }

    new_team
}

/// The complementary error function.
fn erfc(x: f64) -> f64 {
    let z = x.abs();
    let t = 1.0 / (1.0 + z / 2.0);

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
            -z * z - 1.265_512_23,
        )
        .exp();

    if x < 0.0 {
        return 2.0 - r;
    }

    r
}

#[allow(clippy::excessive_precision)]
/// The inverse of the complementary error function.
fn inverse_erfc(mut y: f64) -> f64 {
    if y >= 2.0 {
        return -100.0;
    } else if y <= 0.0 {
        return 100.0;
    }

    let zero_point = y < 1.0;

    if !zero_point {
        y = 2.0 - y;
    }

    let t = (-2.0 * (y / 2.0).ln()).sqrt();

    let mut x = (-1.0 * FRAC_1_SQRT_2)
        * (t.mul_add(0.27061, 2.30753) / t.mul_add(t.mul_add(0.04481, 0.99229), 1.0) - t);

    for _ in 0..2 {
        let err = erfc(x) - y;
        x += err / (1.128_379_167_095_512_57 * (-1.0 * (x.powi(2))).exp() - x * err);
    }

    if zero_point {
        return x;
    }

    x * -1.0
}

/// The cumulative distribution function.
fn cdf(x: f64, mu: f64, sigma: f64) -> f64 {
    0.5 * erfc(-1.0 * (x - mu) / (sigma * SQRT_2))
}

/// The inverse of the cumulative distribution function.
fn inverse_cdf(x: f64, mu: f64, sigma: f64) -> f64 {
    mu - sigma * SQRT_2 * inverse_erfc(2.0 * x)
}

/// The probability density function.
fn pdf(x: f64, mu: f64, sigma: f64) -> f64 {
    1.0 / (2.0 * PI).sqrt() * sigma.abs() * (-1.0 * (((x - mu) / sigma.abs()).powi(2) / 2.0)).exp()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::f64::{INFINITY, NEG_INFINITY};

    #[test]
    fn test_trueskill() {
        // This example is taken from:
        // <https://ubm-twvideo01.s3.amazonaws.com/o1/vault/gdc2017/Presentations/Izquierdo_Mario_Ranking_Systems_Elo.pdf>
        // (Page 20)
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
            &vec![(player_two, Outcomes::WIN)],
            &TrueSkillConfig::new(),
        );

        assert!(((player.rating * 100.0).round() - 3300.0).abs() < f64::EPSILON);
        assert!(((player.uncertainty * 100.0).round() - 597.0).abs() < f64::EPSILON);

        let player = trueskill_rating_period(
            &player_one,
            &vec![
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
            &vec![(player_two, Outcomes::DRAW)],
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

        let (team_one, team_two) = trueskill_teams(
            &vec![player_one, player_two],
            &vec![player_three, player_four],
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

        let (team_two, team_one) = trueskill_teams(
            &vec![player_three, player_four],
            &vec![player_one, player_two],
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

        let (team_one, team_two) = trueskill_teams(
            &vec![player_one, player_two],
            &vec![player_three, player_four],
            &Outcomes::DRAW,
            &TrueSkillConfig::new(),
        );

        assert!((team_one[0].rating - 21.571_213_060_731_655).abs() < f64::EPSILON);
        assert!((team_one[1].rating - 23.696_619_260_051_385).abs() < f64::EPSILON);
        assert!((team_two[0].rating - 23.356_662_026_148_804).abs() < f64::EPSILON);
        assert!((team_two[1].rating - 29.075_310_476_318_872).abs() < f64::EPSILON);

        assert!((team_one[0].uncertainty - 6.555_663_733_192_404).abs() < f64::EPSILON);
        assert!((team_one[1].uncertainty - 5.417_723_612_401_869).abs() < f64::EPSILON);
        assert!((team_two[0].uncertainty - 3.832_975_356_683_128).abs() < f64::EPSILON);
        assert!((team_two[1].uncertainty - 2.930_957_525_591_959_5).abs() < f64::EPSILON);

        let (team_one, _) = trueskill_teams(
            &vec![player_one],
            &vec![],
            &Outcomes::WIN,
            &TrueSkillConfig::new(),
        );

        assert_eq!(team_one[0], player_one);
    }

    #[test]
    fn test_match_quality_teams() {
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

        let qual = match_quality_teams(
            &vec![player_one, player_two],
            &vec![player_three, player_four],
            &TrueSkillConfig::new(),
        );

        let qual2 = match_quality_teams(
            &vec![player_three, player_four],
            &vec![player_one, player_two],
            &TrueSkillConfig::new(),
        );

        assert!((qual - 0.084_108_145_418_343_24).abs() < f64::EPSILON);

        assert!((qual - qual2).abs() < f64::EPSILON);
    }

    #[test]
    fn test_expected_score_teams() {
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

        let (exp1, exp2) = expected_score_teams(
            &vec![player_one, player_two],
            &vec![player_three, player_four],
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
        #[allow(clippy::default_trait_access)]
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

        assert!((exp1 * 100.0 - 50.0).round().abs() < f64::EPSILON);
        assert!((exp2 * 100.0 - 50.0).round().abs() < f64::EPSILON);

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

        assert!((exp1 * 100.0 - 80.0).round().abs() < f64::EPSILON);
        assert!((exp2 * 100.0 - 20.0).round().abs() < f64::EPSILON);

        assert!((exp1.mul_add(100.0, exp2 * 100.0).round() - 100.0).abs() < f64::EPSILON);
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

        let dist_inf = cdf(INFINITY, 0.0, 1.0);

        assert!((dist_inf - 1.0).abs() < f64::EPSILON);

        let dist_neg_inf = cdf(NEG_INFINITY, 0.0, 1.0);

        assert!((dist_neg_inf - 0.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_inverse_cdf() {
        let dist = inverse_cdf(0.055, 12.0, 5.0);

        assert!((dist - 4.009_034).abs() < 0.000_001);
    }

    #[test]
    fn test_pdf() {
        let prob = pdf(2.5, 0.0, 1.0);

        assert!((prob - 0.017_528).abs() < 0.000_001);
    }

    #[test]
    fn test_wv_edge_cases() {
        let w = w_non_draw(NEG_INFINITY, 0.0, 1.0);

        assert!((w - 1.0).abs() < f64::EPSILON);

        let v = v_non_draw(NEG_INFINITY, 0.0, 1.0);

        assert!(v == INFINITY);

        let w2 = w_draw(NEG_INFINITY, 0.0, 1.0);

        assert!((w2 - 1.0).abs() < f64::EPSILON);

        let v2 = v_draw(NEG_INFINITY, 0.0, 1.0);

        assert!(v2 == INFINITY);

        let w3 = w_non_draw(1.0, INFINITY, 1.0);

        assert!((w3 - 0.0).abs() < f64::EPSILON);

        let v3 = v_draw(INFINITY, f64::MAX, 1.0);

        assert!(v3 == NEG_INFINITY);
    }
}
