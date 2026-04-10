//! Population simulation replicating Figure 5 from Yuksel (2024).
//!
//! Simulates 1000 players with Gaussian-distributed actual ratings over 1000+
//! rounds, printing RMSE after each round. Uses zero-sum rating updates and
//! skill-based matchmaking with a 5% shuffle window.
//!
//! Run with: `cargo run --example yuksel_simulation`

use std::collections::VecDeque;

use rand::prelude::*;

use skillratings::{
    yuksel::{
        desired_opponent_rating, f_e, match_group, yuksel, YukselConfig, YukselMatchConfig,
        YukselRating,
    },
    Outcomes,
};

const NUM_PLAYERS: usize = 1000;
const NUM_ROUNDS: usize = 1000;
const ACTUAL_MEAN: f64 = 1500.0;
const ACTUAL_STD: f64 = 600.0;
const ACTUAL_MIN: f64 = 0.0;
const ACTUAL_MAX: f64 = 3000.0;
const SHUFFLE_WINDOW: usize = 50; // 5% of 1000
const WIN_HISTORY_N: usize = 5;

struct SimPlayer {
    actual_rating: f64,
    estimated: YukselRating,
    win_history: VecDeque<bool>,
}

/// Box-Muller transform: generates a standard normal sample from two uniform samples.
fn box_muller(rng: &mut impl Rng) -> f64 {
    let u1: f64 = rng.gen::<f64>().max(f64::MIN_POSITIVE);
    let u2: f64 = rng.gen();
    (-2.0 * u1.ln()).sqrt() * (2.0 * std::f64::consts::PI * u2).cos()
}

fn rmse(players: &[SimPlayer]) -> f64 {
    let sum_sq: f64 = players
        .iter()
        .map(|p| {
            let diff = p.estimated.rating - p.actual_rating;
            diff * diff
        })
        .sum();
    (sum_sq / players.len() as f64).sqrt()
}

fn main() {
    let mut rng = StdRng::seed_from_u64(2024);
    let rating_config = YukselConfig::new();
    let match_config = YukselMatchConfig::new();

    // Initialize players with Gaussian actual ratings clamped to [0, 3000]
    let mut players: Vec<SimPlayer> = (0..NUM_PLAYERS)
        .map(|_| {
            let actual = (ACTUAL_MEAN + ACTUAL_STD * box_muller(&mut rng))
                .clamp(ACTUAL_MIN, ACTUAL_MAX);
            SimPlayer {
                actual_rating: actual,
                estimated: YukselRating::new(),
                win_history: VecDeque::with_capacity(WIN_HISTORY_N),
            }
        })
        .collect();

    println!("Round 0: RMSE = {:.1}", rmse(&players));

    for round in 1..=NUM_ROUNDS {
        // Compute desired opponent ratings
        let mut desired: Vec<(usize, f64)> = players
            .iter()
            .enumerate()
            .map(|(i, p)| {
                let wins = p.win_history.iter().filter(|&&w| w).count();
                let games = p.win_history.len();
                let r_star =
                    desired_opponent_rating(p.estimated.rating, wins, games, &match_config);
                (i, r_star)
            })
            .collect();

        // Shuffle within window (swap each player with a random neighbor)
        for i in 0..desired.len() {
            let lo = i.saturating_sub(SHUFFLE_WINDOW / 2);
            let hi = (i + SHUFFLE_WINDOW / 2).min(desired.len() - 1);
            if lo < hi {
                let j = rng.gen_range(lo..=hi);
                desired.swap(i, j);
            }
        }

        // Match players
        let pairs = match_group(&mut desired);

        // Simulate games and update ratings
        for (idx_a, idx_b) in &pairs {
            let actual_a = players[*idx_a].actual_rating;
            let actual_b = players[*idx_b].actual_rating;

            let win_prob = f_e(actual_a, actual_b);
            let outcome = if rng.gen::<f64>() < win_prob {
                Outcomes::WIN
            } else {
                Outcomes::LOSS
            };

            let est_a = &players[*idx_a].estimated;
            let est_b = &players[*idx_b].estimated;
            let (new_a, new_b) = yuksel(est_a, est_b, &outcome, &rating_config);

            players[*idx_a].estimated = new_a;
            players[*idx_b].estimated = new_b;

            let a_won = outcome == Outcomes::WIN;
            push_history(&mut players[*idx_a].win_history, a_won, WIN_HISTORY_N);
            push_history(&mut players[*idx_b].win_history, !a_won, WIN_HISTORY_N);
        }

        let current_rmse = rmse(&players);
        if round <= 10 || round % 10 == 0 || round == NUM_ROUNDS {
            println!("Round {round}: RMSE = {current_rmse:.1}");
        }
    }

    let total_wins: usize = players
        .iter()
        .map(|p| p.win_history.iter().filter(|&&w| w).count())
        .sum();
    let total_games: usize = players.iter().map(|p| p.win_history.len()).sum();
    let win_rate = total_wins as f64 / total_games as f64;
    println!("\nFinal RMSE: {:.1}", rmse(&players));
    println!(
        "Population win rate (last {WIN_HISTORY_N} games): {:.1}%",
        win_rate * 100.0
    );
}

fn push_history(history: &mut VecDeque<bool>, won: bool, max_len: usize) {
    if history.len() >= max_len {
        history.pop_front();
    }
    history.push_back(won);
}
