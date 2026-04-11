//! Implements the rating estimation and matchmaking system from Cem Yuksel's 2024 paper
//! "Skill-Based Matchmaking for Competitive Two-Player Games"
//!
//! The system has two components:
//! 1. A rating estimation engine that tracks 5 persistent values per player and
//!    adapts automatically to rating changes without parameter tuning.
//! 2. A matchmaking module that promotes a target win rate by selecting opponents
//!    based on recent win/loss history.
//!
//! The recommended entry point is [`yuksel`] for zero-sum 1v1 updates (Algorithm 2),
//! or [`yuksel_rating_period`] for batch updates over a rating period.
//!
//! # Quickstart
//!
//! ```
//! use skillratings::{
//!     yuksel::{yuksel, YukselConfig, YukselRating},
//!     Outcomes,
//! };
//!
//! // Initialise a new player rating with a rating of 1500, a derivative sum value of 0,
//! // a weighted sum of 1e-10, a weighted mean of 0 and a weighed variance of 0.
//! let player_one = YukselRating::new();
//! // Or you can initialise it with your own values of course.
//! // Imagine these numbers being pulled from a database.
//! let player_two = YukselRating {
//!     rating: 1700.0,
//!     ..YukselRating::new()
//! };
//!
//! // The config allows you to specify certain values in the Yuksel calculation
//! let config = YukselConfig::new();
//!
//! // The yuksel function will calculate the new ratings for both players and return them.
//! let (new_one, new_two) = yuksel(&player_one, &player_two, &Outcomes::WIN, &config);
//! ```
//!
//! # More Information
//!
//! - [Paper (DOI)](https://doi.org/10.1145/3651303)

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use std::f64::consts::PI;

use crate::{Outcomes, Rating, RatingPeriodSystem, RatingSystem};

/// `q = ln(10) / 400` (Eq 4).
const Q: f64 = std::f64::consts::LN_10 / 400.0;

/// Elo win probability (Eq 1). Satisfies `f_e(a,b) + f_e(b,a) = 1`.
fn f_e(r_a: f64, r_b: f64) -> f64 {
    1.0 / (1.0 + 10.0_f64.powf(-(r_a - r_b) / 400.0))
}

/// Glicko deviation scaling factor (Eq 4). `g(0) = 1`, `g(φ) → 0` as `φ → ∞`.
fn g(phi: f64) -> f64 {
    1.0 / (1.0 + 3.0 * Q * Q * phi * phi / (PI * PI)).sqrt()
}

/// Derivative of `f_e` w.r.t. `r_a` (Appendix A): `q · f_E(a,b) · f_E(b,a)`.
fn df_e(r_a: f64, r_b: f64) -> f64 {
    let fe = f_e(r_a, r_b);
    Q * fe * (1.0 - fe)
}

/// Inverse of `f_e` (Eq 26): opponent rating `r★` such that `f_E(a, ★) = p`.
fn inverse_f_e(p: f64, r_a: f64) -> f64 {
    400.0_f64.mul_add((1.0 / p - 1.0).log10(), r_a)
}

/// Stores the 5 persistent values from the paper: estimated rating `r`,
/// running derivative sum `D`, running weight sum `W`, running weighted
/// mean `R`, and running weighted variance `V`.
#[derive(Copy, Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct YukselRating {
    /// The player's estimated rating `r` (default 1500.0).
    pub rating: f64,
    /// Running derivative sum `D` (Eq 15). Initialized to 0.0.
    pub d: f64,
    /// Running weight sum `W` (Eq 9). Initialized to 1e-10.
    pub w_sum: f64,
    /// Running weighted mean `R` (Eq 10). Initialized to 0.0.
    pub r_mean: f64,
    /// Running weighted variance `V` (Eq 11). Initialized to 0.0.
    pub v: f64,
}

impl YukselRating {
    #[must_use]
    /// Initialise a new `YukselRating` with default values (r=1500, D=0, W=1e-10, R=0, V=0).
    pub const fn new() -> Self {
        Self {
            rating: 1500.0,
            d: 0.0,
            w_sum: 1e-10,
            r_mean: 0.0,
            v: 0.0,
        }
    }

    #[must_use]
    /// Rating deviation φ = √(V/W) (Eq 12).
    pub fn deviation(&self) -> f64 {
        (self.v / self.w_sum).max(0.0).sqrt()
    }
}

impl Default for YukselRating {
    fn default() -> Self {
        Self::new()
    }
}

impl Rating for YukselRating {
    fn rating(&self) -> f64 {
        self.rating
    }

    fn uncertainty(&self) -> Option<f64> {
        Some(self.deviation())
    }

    fn new(rating: Option<f64>, _uncertainty: Option<f64>) -> Self {
        Self {
            rating: rating.unwrap_or(1500.0),
            d: 0.0,
            w_sum: 1e-10,
            r_mean: 0.0,
            v: 0.0,
        }
    }
}

impl From<f64> for YukselRating {
    fn from(r: f64) -> Self {
        Self {
            rating: r,
            ..Self::new()
        }
    }
}

/// Configuration for the Yuksel rating system (Section 3).
#[derive(Clone, Copy, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct YukselConfig {
    /// Weight decay parameter `α` that controls responsiveness to rating changes (Eq 8).
    /// Higher values make the system more responsive. Default: 2.0.
    pub alpha: f64,
    /// Maximum allowed rating change per game `Δr_max` (Algorithm 1, line 5).
    /// Prevents divergence in Newton iterations. Default: 350.0.
    pub dr_max: f64,
    /// Optional scaling factor `s ≤ 1` applied to rating changes (Algorithm 1, line 9).
    /// Use 1.0 normally, or 0.95 for stability without matchmaking. Default: 1.0.
    pub s_factor: f64,
}

impl YukselConfig {
    #[must_use]
    /// Creates a new `YukselConfig` with the paper's default values (α=2, Δr_max=350, s=1).
    pub const fn new() -> Self {
        Self {
            alpha: 2.0,
            dr_max: 350.0,
            s_factor: 1.0,
        }
    }
}

impl Default for YukselConfig {
    fn default() -> Self {
        Self::new()
    }
}

/// Update rating and running statistics (Algorithm 1, lines 8–14).
/// Uses the OLD deviation (from `rating.v` / `rating.w_sum`) for the weight ω,
/// because `rating` is an immutable reference to the state before this update.
fn update_stats(rating: &YukselRating, new_d: f64, dr: f64, config: &YukselConfig) -> YukselRating {
    let new_r = config.s_factor.mul_add(dr, rating.rating); // line 9
    let phi = rating.deviation(); // φ from OLD V/W
    let omega = g(config.alpha * phi); // line 10
    let new_w = omega.mul_add(rating.w_sum, 1.0); // line 11 (Eq 9)
    let delta_r = new_r - rating.r_mean; // line 12
    let new_r_mean = rating.r_mean + delta_r / new_w; // line 13 (Eq 10)
    let new_v = omega.mul_add(rating.v, delta_r * (new_r - new_r_mean)); // line 14 (Eq 11)

    YukselRating {
        rating: new_r,
        d: new_d,
        w_sum: new_w,
        r_mean: new_r_mean,
        v: new_v,
    }
}

/// Incremental non-zero-sum rating update — Algorithm 1.
fn yuksel_rating_update(
    player_a: &YukselRating,
    player_b: &YukselRating,
    outcome: Outcomes,
    config: &YukselConfig,
) -> YukselRating {
    let s = outcome.to_chess_points();
    let phi_a = player_a.deviation();
    let phi_b = player_b.deviation();
    let fe = f_e(player_a.rating, player_b.rating);
    let dfe = df_e(player_a.rating, player_b.rating);

    // Eq 17: F_a (not stored)
    let f_val = g(phi_b) * (s - fe);

    // Eq 15: update D_a
    let mut new_d = g(phi_b).mul_add(dfe, g(config.alpha * phi_a) * player_a.d);

    // Rating change Δr = F_a / D_a (Algorithm 1, line 4)
    let raw_dr = f_val / new_d;
    let dr = raw_dr.clamp(-config.dr_max, config.dr_max);

    // Correct D_a when clamped (Algorithm 1, line 6)
    if raw_dr.abs() > config.dr_max {
        new_d = f_val / dr;
    }

    update_stats(player_a, new_d, dr, config)
}

/// Zero-sum rating update for two players after a game — Algorithm 2.
///
/// The rating change is symmetric: player `a` gains exactly what player `b` loses
/// (scaled by `s_factor`). Recommended for production use.
///
/// # Examples
///
/// ```
/// use skillratings::{
///     yuksel::{yuksel, YukselConfig, YukselRating},
///     Outcomes,
/// };
///
/// let player_one = YukselRating::new();
/// let player_two = YukselRating {
///     rating: 1700.0,
///     ..YukselRating::new()
/// };
/// let config = YukselConfig::new();
///
/// let (new_one, new_two) = yuksel(&player_one, &player_two, &Outcomes::WIN, &config);
/// // Zero-sum: total rating is preserved.
/// let old_sum = player_one.rating + player_two.rating;
/// let new_sum = new_one.rating + new_two.rating;
/// assert!((old_sum - new_sum).abs() < 1e-9);
/// ```
#[must_use]
pub fn yuksel(
    player_a: &YukselRating,
    player_b: &YukselRating,
    outcome: &Outcomes,
    config: &YukselConfig,
) -> (YukselRating, YukselRating) {
    let s = outcome.to_chess_points();
    let phi_a = player_a.deviation();
    let phi_b = player_b.deviation();
    let fe = f_e(player_a.rating, player_b.rating);
    let dfe = df_e(player_a.rating, player_b.rating);

    // Eq 17: F values (not stored)
    let f_a = g(phi_b) * (s - fe);
    let f_b = g(phi_a) * (fe - s);

    // Eq 15: update D values
    let mut d_a = g(phi_b).mul_add(dfe, g(config.alpha * phi_a) * player_a.d);
    let mut d_b = g(phi_a).mul_add(dfe, g(config.alpha * phi_b) * player_b.d);

    // Eq 22: least-squares zero-sum solution
    let raw_dr = d_a.mul_add(f_a, -(d_b * f_b)) / d_a.mul_add(d_a, d_b * d_b);
    let dr = raw_dr.clamp(-config.dr_max, config.dr_max);

    // Correct D values to satisfy Eq 16 (Algorithm 2, lines 8–11)
    if dr.abs() > f64::EPSILON {
        d_a = f_a / dr;
        d_b = -f_b / dr;
    }

    let new_a = update_stats(player_a, d_a, dr, config);
    let new_b = update_stats(player_b, d_b, -dr, config);

    (new_a, new_b)
}

/// Calculates the expected win probabilities for both players using Elo's formula (Eq 1).
///
/// # Examples
///
/// ```
/// use skillratings::yuksel::{expected_score, YukselRating};
///
/// let player_one = YukselRating {
///     rating: 1800.0,
///     ..YukselRating::new()
/// };
/// let player_two = YukselRating {
///     rating: 1500.0,
///     ..YukselRating::new()
/// };
///
/// let (exp_one, exp_two) = expected_score(&player_one, &player_two);
/// assert!((exp_one + exp_two - 1.0).abs() < 1e-10);
/// assert!(exp_one > 0.5);
/// ```
#[must_use]
pub fn expected_score(player_one: &YukselRating, player_two: &YukselRating) -> (f64, f64) {
    let exp_one = f_e(player_one.rating, player_two.rating);
    (exp_one, 1.0 - exp_one)
}

/// Calculates a player's updated rating after a rating period of multiple games
/// using incremental non-zero-sum updates (Algorithm 1 applied sequentially).
///
/// # Examples
///
/// ```
/// use skillratings::{
///     yuksel::{yuksel_rating_period, YukselConfig, YukselRating},
///     Outcomes,
/// };
///
/// let player = YukselRating::new();
/// let results = vec![
///     (
///         YukselRating {
///             rating: 1400.0,
///             ..YukselRating::new()
///         },
///         Outcomes::WIN,
///     ),
///     (
///         YukselRating {
///             rating: 1600.0,
///             ..YukselRating::new()
///         },
///         Outcomes::LOSS,
///     ),
/// ];
/// let config = YukselConfig::new();
///
/// let new_player = yuksel_rating_period(&player, &results, &config);
/// ```
#[must_use]
pub fn yuksel_rating_period(
    player: &YukselRating,
    results: &[(YukselRating, Outcomes)],
    config: &YukselConfig,
) -> YukselRating {
    results
        .iter()
        .fold(*player, |current, (opponent, outcome)| {
            yuksel_rating_update(&current, opponent, *outcome, config)
        })
}

/// Returns a volatility range `[rating − 1.96·φ, rating + 1.96·φ]` indicating
/// how much the player's estimated rating has been fluctuating.
///
/// # Examples
///
/// ```
/// use skillratings::yuksel::{volatility_range, YukselRating};
///
/// let player = YukselRating::new();
/// let (low, high) = volatility_range(&player);
/// // Fresh player — no history, so the range collapses.
/// assert!((low - high).abs() < 1.0);
/// ```
#[must_use]
pub fn volatility_range(player: &YukselRating) -> (f64, f64) {
    let margin = 1.96 * player.deviation();
    (player.rating - margin, player.rating + margin)
}

/// Configuration for the Yuksel matchmaking system (Section 4).
#[derive(Clone, Copy, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct YukselMatchConfig {
    /// Number of past games to track for the win-rate window `n` (Eq 24).
    /// The total window considered is `2n+1` games. Default: 5.
    pub n: usize,
    /// Target win rate `λ`. Default: 0.5 (50%).
    pub lambda: f64,
    /// Probability tolerance `Δp` for the opponent rating window (Section 4.1).
    /// Must satisfy `Δp < 1/(2n+2)`. Default: 1/12.
    pub delta_p: f64,
}

impl YukselMatchConfig {
    #[must_use]
    /// Creates a new `YukselMatchConfig` with the paper's default values (n=5, λ=0.5, Δp=1/12).
    pub fn new() -> Self {
        Self {
            n: 5,
            lambda: 0.5,
            delta_p: 1.0 / 12.0,
        }
    }
}

impl Default for YukselMatchConfig {
    fn default() -> Self {
        Self::new()
    }
}

/// Desired win probability `p` for the next game (Eq 24/25).
///
/// When the player has played `m ≥ n` games, uses the full window (Eq 24).
/// When `m < n`, uses the partial window (Eq 25).
/// `wins` is the count of wins in the last `min(m, n)` games.
/// Returns `λ` when `games_played = 0`.
#[must_use]
pub fn desired_win_probability(
    wins: usize,
    games_played: usize,
    config: &YukselMatchConfig,
) -> f64 {
    let n = config.n;
    let w = wins as f64;

    if games_played >= n {
        // Eq 24
        config.lambda.mul_add((2 * n + 1) as f64, -w) / (n + 1) as f64
    } else {
        // Eq 25
        config.lambda.mul_add((n + games_played + 1) as f64, -w) / (n + 1) as f64
    }
}

/// Desired opponent rating `r★` that gives win probability `p` for a player
/// with rating `player_rating` (Eq 26).
/// Combines [`desired_win_probability`] with the inverse Elo function.
#[must_use]
pub fn desired_opponent_rating(
    player_rating: f64,
    wins: usize,
    games_played: usize,
    config: &YukselMatchConfig,
) -> f64 {
    let p = desired_win_probability(wins, games_played, config).clamp(1e-6, 1.0 - 1e-6);
    inverse_f_e(p, player_rating)
}

/// Acceptable opponent rating window `[r_low, r_high]` around the desired
/// opponent rating (Section 4.1).
/// The window spans `f⁻¹(p ± Δp)`, where `Δp < 1/(2n+2)`.
#[must_use]
pub fn opponent_rating_window(
    player_rating: f64,
    wins: usize,
    games_played: usize,
    config: &YukselMatchConfig,
) -> (f64, f64) {
    let p = desired_win_probability(wins, games_played, config);
    let p_low = (p - config.delta_p).clamp(1e-6, 1.0 - 1e-6);
    let p_high = (p + config.delta_p).clamp(1e-6, 1.0 - 1e-6);

    // Higher desired probability → weaker opponent (lower rating)
    let r_low = inverse_f_e(p_high, player_rating);
    let r_high = inverse_f_e(p_low, player_rating);
    (r_low, r_high)
}

/// Match a group of players by sorting on desired opponent rating and pairing
/// adjacent entries (Section 4.2).
///
/// Input is a mutable slice of `(player_index, desired_opponent_rating)`.
/// The slice is sorted in place by desired opponent rating.
/// Returns paired player indices. If the count is odd, the last player is unmatched.
///
/// For the small-window shuffle described in the paper (e.g. 5% of population),
/// shuffle the slice within a window before calling this function, or use
/// the simulation example for a complete demonstration.
pub fn match_group(desired_ratings: &mut [(usize, f64)]) -> Vec<(usize, usize)> {
    desired_ratings.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal));
    desired_ratings
        .chunks_exact(2)
        .map(|pair| (pair[0].0, pair[1].0))
        .collect()
}

/// Adapter for using the Yuksel system via the [`RatingSystem`] and [`RatingPeriodSystem`] traits.
pub struct Yuksel {
    config: YukselConfig,
}

impl RatingSystem for Yuksel {
    type RATING = YukselRating;
    type CONFIG = YukselConfig;

    fn new(config: Self::CONFIG) -> Self {
        Self { config }
    }

    fn rate(
        &self,
        player_one: &YukselRating,
        player_two: &YukselRating,
        outcome: &Outcomes,
    ) -> (YukselRating, YukselRating) {
        yuksel(player_one, player_two, outcome, &self.config)
    }

    fn expected_score(&self, player_one: &YukselRating, player_two: &YukselRating) -> (f64, f64) {
        expected_score(player_one, player_two)
    }
}

impl RatingPeriodSystem for Yuksel {
    type RATING = YukselRating;
    type CONFIG = YukselConfig;

    fn new(config: Self::CONFIG) -> Self {
        Self { config }
    }

    fn rate(&self, player: &YukselRating, results: &[(YukselRating, Outcomes)]) -> YukselRating {
        yuksel_rating_period(player, results, &self.config)
    }

    fn expected_score(&self, player: &YukselRating, opponents: &[YukselRating]) -> Vec<f64> {
        opponents
            .iter()
            .map(|opp| f_e(player.rating, opp.rating))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // -- Math tests --

    #[test]
    fn test_f_e_sums_to_one() {
        let pairs = [
            (1500.0, 1500.0),
            (1200.0, 1800.0),
            (0.0, 3000.0),
            (2000.0, 1000.0),
            (1500.0, 1850.0),
        ];
        for (r_a, r_b) in pairs {
            let sum = f_e(r_a, r_b) + f_e(r_b, r_a);
            assert!(
                (sum - 1.0).abs() < 1e-10,
                "f_E({r_a},{r_b}) + f_E({r_b},{r_a}) = {sum}"
            );
        }
    }

    #[test]
    fn test_f_e_equal_ratings() {
        let p = f_e(1500.0, 1500.0);
        assert!((p - 0.5).abs() < 1e-10);
    }

    #[test]
    fn test_g_zero() {
        assert!((g(0.0) - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_g_decreases() {
        assert!(g(100.0) < g(50.0));
        assert!(g(50.0) < g(0.0));
    }

    #[test]
    fn test_df_e_symmetric() {
        let d1 = df_e(1500.0, 1800.0);
        let d2 = df_e(1800.0, 1500.0);
        assert!((d1 - d2).abs() < 1e-10);
    }

    #[test]
    fn test_inverse_f_e_roundtrip() {
        let r_a = 1500.0;
        let r_b = 1800.0;
        let p = f_e(r_a, r_b);
        let r_star = inverse_f_e(p, r_a);
        assert!((r_star - r_b).abs() < 1e-6);
    }

    #[test]
    fn test_inverse_f_e_equal_at_half() {
        let r_a = 1500.0;
        let r_star = inverse_f_e(0.5, r_a);
        assert!((r_star - r_a).abs() < 1e-6);
    }

    // -- Rating update tests --

    #[test]
    fn test_default_rating() {
        let r = YukselRating::new();
        assert!((r.rating - 1500.0).abs() < f64::EPSILON);
        assert!((r.d - 0.0).abs() < f64::EPSILON);
        assert!((r.deviation() - 0.0).abs() < 1e-3);
    }

    #[test]
    fn test_zero_sum_updates() {
        let a = YukselRating::new();
        let b = YukselRating {
            rating: 1700.0,
            ..YukselRating::new()
        };
        let config = YukselConfig::new();

        let (new_a, new_b) = yuksel(&a, &b, &Outcomes::WIN, &config);
        let change_a = new_a.rating - a.rating;
        let change_b = new_b.rating - b.rating;

        assert!((change_a + change_b).abs() < 1e-10);
    }

    #[test]
    fn test_zero_sum_multiple_games() {
        let mut a = YukselRating::new();
        let mut b = YukselRating {
            rating: 1800.0,
            ..YukselRating::new()
        };
        let config = YukselConfig::new();

        let original_sum = a.rating + b.rating;
        let outcomes = [
            Outcomes::WIN,
            Outcomes::LOSS,
            Outcomes::WIN,
            Outcomes::WIN,
            Outcomes::LOSS,
        ];

        for outcome in &outcomes {
            let (new_a, new_b) = yuksel(&a, &b, outcome, &config);
            a = new_a;
            b = new_b;
        }

        let new_sum = a.rating + b.rating;
        assert!(
            (original_sum - new_sum).abs() < 1e-6,
            "Total rating not preserved: {original_sum} vs {new_sum}"
        );
    }

    #[test]
    fn test_win_increases_rating() {
        let a = YukselRating::new();
        let b = YukselRating::new();
        let config = YukselConfig::new();

        let new_a = yuksel_rating_update(&a, &b, Outcomes::WIN, &config);
        assert!(new_a.rating > a.rating);
    }

    #[test]
    fn test_loss_decreases_rating() {
        let a = YukselRating::new();
        let b = YukselRating::new();
        let config = YukselConfig::new();

        let new_a = yuksel_rating_update(&a, &b, Outcomes::LOSS, &config);
        assert!(new_a.rating < a.rating);
    }

    #[test]
    fn test_convergence() {
        let actual_rating = 2000.0;
        let mut player = YukselRating::new();
        let config = YukselConfig::new();

        let mut seed: u64 = 42;
        let mut next_rand = || -> f64 {
            seed = seed.wrapping_mul(6_364_136_223_846_793_005).wrapping_add(1);
            (seed >> 33) as f64 / (1u64 << 31) as f64
        };

        for _ in 0..200 {
            let opp_rating = 1300.0 + next_rand() * 700.0;
            let opponent = YukselRating {
                rating: opp_rating,
                ..YukselRating::new()
            };

            let win_prob = f_e(actual_rating, opp_rating);
            let outcome = if next_rand() < win_prob {
                Outcomes::WIN
            } else {
                Outcomes::LOSS
            };

            player = yuksel_rating_update(&player, &opponent, outcome, &config);
        }

        let error = (player.rating - actual_rating).abs();
        assert!(
            error < 300.0,
            "Rating {:.1} did not converge toward actual {actual_rating:.1} (error: {error:.1})",
            player.rating
        );
    }

    #[test]
    fn test_rating_period() {
        let player = YukselRating::new();
        let config = YukselConfig::new();
        let opp = YukselRating {
            rating: 1500.0,
            ..YukselRating::new()
        };

        let results = vec![
            (opp, Outcomes::WIN),
            (opp, Outcomes::WIN),
            (opp, Outcomes::WIN),
        ];

        let new_player = yuksel_rating_period(&player, &results, &config);
        assert!(
            new_player.rating > player.rating,
            "Three wins should increase rating: {} vs {}",
            new_player.rating,
            player.rating
        );
    }

    #[test]
    fn test_s_factor_dampens() {
        let a = YukselRating::new();
        let b = YukselRating {
            rating: 1700.0,
            ..YukselRating::new()
        };

        let config_full = YukselConfig::new();
        let config_damped = YukselConfig {
            s_factor: 0.95,
            ..YukselConfig::new()
        };

        let new_full = yuksel_rating_update(&a, &b, Outcomes::WIN, &config_full);
        let new_damped = yuksel_rating_update(&a, &b, Outcomes::WIN, &config_damped);

        assert!(
            (new_full.rating - a.rating).abs() > (new_damped.rating - a.rating).abs(),
            "Damped update should produce smaller rating change"
        );
    }

    #[test]
    fn test_expected_score_equal() {
        let a = YukselRating::new();
        let b = YukselRating::new();
        let (ea, eb) = expected_score(&a, &b);
        assert!((ea - 0.5).abs() < 1e-10);
        assert!((eb - 0.5).abs() < 1e-10);
    }

    #[test]
    fn test_expected_score_stronger_favored() {
        let a = YukselRating {
            rating: 1800.0,
            ..YukselRating::new()
        };
        let b = YukselRating::new();
        let (ea, eb) = expected_score(&a, &b);
        assert!(ea > 0.5);
        assert!(eb < 0.5);
    }

    #[test]
    fn test_rating_system_trait() {
        let system: Yuksel = RatingSystem::new(YukselConfig::new());
        let a = YukselRating::new();
        let b = YukselRating {
            rating: 1700.0,
            ..YukselRating::new()
        };

        let (new_a, new_b) = RatingSystem::rate(&system, &a, &b, &Outcomes::WIN);
        assert!(new_a.rating > a.rating);
        assert!(new_b.rating < b.rating);
    }

    #[test]
    fn test_rating_trait() {
        let r = YukselRating::new();
        assert!((r.rating() - 1500.0).abs() < f64::EPSILON);
        assert!(r.uncertainty().is_some());

        let r2: YukselRating = Rating::new(Some(2000.0), None);
        assert!((r2.rating() - 2000.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_from_f64() {
        let r: YukselRating = 1800.0.into();
        assert!((r.rating - 1800.0).abs() < f64::EPSILON);
        assert!((r.d - 0.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_derives() {
        let r = YukselRating::new();
        assert_eq!(r, r.clone());
        assert!(!format!("{r:?}").is_empty());

        let c = YukselConfig::new();
        assert!(!format!("{c:?}").is_empty());
    }

    #[test]
    fn test_volatility_range() {
        let fresh = YukselRating::new();
        let (low, high) = volatility_range(&fresh);
        // φ ≈ 0 for a new player, so range collapses
        assert!((high - low).abs() < 1.0);
        assert!((low - 1500.0).abs() < 1.0);

        // Simulate a player with accumulated history
        let seasoned = YukselRating {
            rating: 1800.0,
            v: 2500.0,
            w_sum: 10.0,
            ..YukselRating::new()
        };
        let (low2, high2) = volatility_range(&seasoned);
        // φ = sqrt(2500/10) ≈ 15.8, margin ≈ 31
        assert!(low2 < 1800.0);
        assert!(high2 > 1800.0);
        let width = high2 - low2;
        assert!(width > 50.0 && width < 70.0, "Expected ~62, got {width}");
    }

    // -- Matchmaking tests --

    #[test]
    fn test_desired_prob_initial() {
        let config = YukselMatchConfig::new();
        let p = desired_win_probability(0, 0, &config);
        assert!(
            (p - config.lambda).abs() < 1e-10,
            "Initial desired probability should be λ={}, got {p}",
            config.lambda
        );
    }

    #[test]
    fn test_desired_prob_all_wins() {
        let config = YukselMatchConfig::new();
        let p = desired_win_probability(5, 5, &config);
        assert!(
            p < 0.5,
            "After all wins, desired prob should be < 0.5, got {p}"
        );
        assert!(p > 0.0, "Desired prob should remain positive, got {p}");
    }

    #[test]
    fn test_desired_prob_all_losses() {
        let config = YukselMatchConfig::new();
        let p = desired_win_probability(0, 5, &config);
        assert!(
            p > 0.5,
            "After all losses, desired prob should be > 0.5, got {p}"
        );
        assert!(p < 1.0, "Desired prob should remain < 1, got {p}");
    }

    #[test]
    fn test_desired_prob_partial_window() {
        let config = YukselMatchConfig::new();
        let p = desired_win_probability(1, 3, &config);
        let expected = 3.5 / 6.0;
        assert!((p - expected).abs() < 1e-10, "Expected {expected}, got {p}");
    }

    #[test]
    fn test_desired_opponent_equal_at_half() {
        let config = YukselMatchConfig::new();
        let r_star = desired_opponent_rating(1500.0, 0, 0, &config);
        assert!(
            (r_star - 1500.0).abs() < 1.0,
            "With p=0.5, opponent should match player rating, got {r_star}"
        );
    }

    #[test]
    fn test_desired_opponent_after_wins() {
        let config = YukselMatchConfig::new();
        let r_star = desired_opponent_rating(1500.0, 5, 5, &config);
        assert!(
            r_star > 1500.0,
            "After all wins, desired opponent should be stronger, got {r_star}"
        );
    }

    #[test]
    fn test_desired_opponent_after_losses() {
        let config = YukselMatchConfig::new();
        let r_star = desired_opponent_rating(1500.0, 0, 5, &config);
        assert!(
            r_star < 1500.0,
            "After all losses, desired opponent should be weaker, got {r_star}"
        );
    }

    #[test]
    fn test_opponent_window_contains_desired() {
        let config = YukselMatchConfig::new();
        let r_star = desired_opponent_rating(1500.0, 2, 5, &config);
        let (r_low, r_high) = opponent_rating_window(1500.0, 2, 5, &config);
        assert!(r_low <= r_star && r_star <= r_high);
    }

    #[test]
    fn test_match_group_pairs_adjacent() {
        let mut players = vec![(0, 1500.0), (1, 1600.0), (2, 1200.0), (3, 1800.0)];
        let pairs = match_group(&mut players);
        assert_eq!(pairs.len(), 2);
        assert_eq!(pairs[0], (2, 0));
        assert_eq!(pairs[1], (1, 3));
    }

    #[test]
    fn test_match_group_odd_count() {
        let mut players = vec![(0, 1500.0), (1, 1600.0), (2, 1200.0)];
        let pairs = match_group(&mut players);
        assert_eq!(pairs.len(), 1);
    }

    #[test]
    fn test_new_inits() {
        let r = YukselRating::default();

        assert!((r.rating - 1500.0).abs() < f64::EPSILON);
        assert!(r.d < f64::EPSILON);
        assert!((r.w_sum - 1e-10).abs() < f64::EPSILON);
        assert!(r.r_mean < f64::EPSILON);
        assert!(r.v < f64::EPSILON);

        let c = YukselConfig::default();

        assert!((c.alpha - 2.0).abs() < f64::EPSILON);
        assert!((c.dr_max - 350.0).abs() < f64::EPSILON);
        assert!((c.s_factor - 1.0).abs() < f64::EPSILON);

        let mc = YukselMatchConfig::default();
        assert!((mc.delta_p - 1.0 / 12.0).abs() < f64::EPSILON);
        assert!((mc.lambda - 0.5).abs() < f64::EPSILON);
        assert_eq!(mc.n, 5);
    }

    #[test]
    fn test_traits() {
        let player_one: YukselRating = Rating::new(Some(1532.0), None);
        let player_two: YukselRating = Rating::new(Some(1532.0), None);

        let rating_system: Yuksel = RatingSystem::new(YukselConfig::new());

        assert!((player_one.rating() - 1532.0).abs() < f64::EPSILON);
        assert_eq!(player_one.uncertainty(), Some(0.0));

        let (new_player_one, new_player_two) =
            RatingSystem::rate(&rating_system, &player_one, &player_two, &Outcomes::WIN);

        let (exp1, exp2) = RatingSystem::expected_score(&rating_system, &player_one, &player_two);

        assert!((new_player_one.rating - 1_879.435_585_522_601_3).abs() < f64::EPSILON);
        assert!((new_player_two.rating - 1_184.564_414_477_398_7).abs() < f64::EPSILON);

        assert!((exp1 + exp2 - 1.0).abs() < f64::EPSILON);

        let rating_period_system: Yuksel = RatingPeriodSystem::new(YukselConfig::new());
        let exp_rp =
            RatingPeriodSystem::expected_score(&rating_period_system, &player_one, &[player_two]);
        assert!((exp1 - exp_rp[0]).abs() < f64::EPSILON);

        let player_one: YukselRating = Rating::new(Some(1532.0), None);
        let player_two: YukselRating = Rating::new(Some(1532.0), None);

        let rating_period: Yuksel = RatingPeriodSystem::new(YukselConfig::new());

        let new_player_one =
            RatingPeriodSystem::rate(&rating_period, &player_one, &[(player_two, Outcomes::WIN)]);

        assert!((new_player_one.rating - 1_879.435_585_522_601_5).abs() < f64::EPSILON);
    }
}
