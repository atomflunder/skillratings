//! This is the Stephenson rating algorithm, nicknamed "Sticko" due to it being an improvement on the Glicko rating algorithm.  
//! The winner of a chess outcome prediction competition and used in statistical analysis.
//!
//! For the original Glicko algorithm, please see [`Glicko`](crate::glicko).
//!
//! In 2012, the data prediction website [Kaggle](https://kaggle.com) hosted the "FIDE/Deloitte Chess Rating Challenge"
//!  where competitors where asked to create a new, more accurate chess rating system.  
//! The winner of the competition was Alec Stephenson, and this was the system he came up with.
//!
//! The main improvements over Glicko are three new configurable parameters found in [`StickoConfig`]:
//!
//! - Gamma (γ) an advantage parameter that accounts for inherent advantages in-game (White in Chess, etc.)
//! - Beta (β) a drift parameter that increases rating for participating
//! - Lambda (λ) a parameter that accounts for the strength of the opponent, regardless of result.
//!
//! These make Sticko more configurable and possibly more accurate than the Glicko algorithm.
//! When all parameters are set to 0, the Sticko algorithm will produce the exact same results as Glicko.
//!
//! # Quickstart
//!
//! This is the most basic example on how to use the Sticko Module.  
//! Please take a look at the functions below to see more advanced use cases.
//!
//! ```
//! use skillratings::{
//!     sticko::sticko, outcomes::Outcomes, rating::StickoRating, config::StickoConfig,
//! };
//!
//! // Initialize a new player rating.
//! let player_one = StickoRating::new();
//!
//! // Or you can initialize it with your own values of course.
//! // Imagine these numbers being pulled from a database.
//! let (some_rating, some_deviation) = (1325.0, 230.0);
//! let player_two = StickoRating{
//!     rating: some_rating,
//!     deviation: some_deviation,
//! };
//!
//! // The outcome of the match is from the perspective of player one.
//! let outcome = Outcomes::WIN;
//!
//! // The config allows you to specify certain values in the Sticko calculation.
//! let config = StickoConfig {
//!     // The gamma value describes the advantage of player_one.
//!     // 30.0 is roughly accurate for playing White in Chess.
//!     // If player_two was to play White, change this to -30.0.
//!     // By default it is set to 0.0.
//!     gamma: 30.0,
//!     // We leave the other settings at their default values.
//!     ..Default::default()
//! };
//!
//! // The sticko function will calculate the new ratings for both players and return them.
//! let (new_player_one, new_player_two) = sticko(&player_one, &player_two, &outcome, &config);
//! ```
//!
//! # More Information
//!
//! - [PlayerRatings R Package](https://cran.r-project.org/web/packages/PlayerRatings/index.html)
//! - [FIDE/Deloitte Chess Rating Challenge](https://www.kaggle.com/c/ChessRatings2)
//! - [Comparing Performance of Chess Ratings](https://www.englishchess.org.uk/wp-content/uploads/2012/04/ratings.pdf)
//! - [Comparison of rating systems for Women's Beach Volleyball](http://glicko.net/research/volleyball-FINAL.pdf)

use std::f64::consts::PI;

use crate::{config::StickoConfig, outcomes::Outcomes, rating::StickoRating};

#[must_use]
/// Calculates the [`StickoRating`]s of two players based on their old ratings, deviations, and the outcome of the game.
///
/// Please see [`Glicko`](crate::glicko) for calculating with Glicko.
///
/// Takes in two players as [`StickoRating`]s,  an [`Outcome`](Outcomes) and a [`StickoConfig`].
///
/// Instead of the traditional way of calculating the Sticko rating for only one player only using a list of results,
/// we are calculating the Sticko rating for two players at once, like in the Elo calculation,
/// to make it easier to see instant results.
///
/// For the traditional way of calculating a Sticko rating please see [`sticko_rating_period`].
///
/// The outcome of the match is in the perspective of `player_one`.
/// This means [`Outcomes::WIN`] is a win for `player_one` and [`Outcomes::LOSS`] is a win for `player_two`.
///
/// # Examples
/// ```
/// use skillratings::{
///     sticko::sticko, outcomes::Outcomes, rating::StickoRating, config::StickoConfig
/// };
///
/// let player_one = StickoRating {
///     rating: 1500.0,
///     deviation: 350.0,
/// };
/// let player_two = StickoRating {
///     rating: 1500.0,
///     deviation: 350.0,
/// };
///
/// let outcome = Outcomes::WIN;
///
/// let config = StickoConfig::new();
///
/// let (player_one_new, player_two_new) = sticko(&player_one, &player_two, &outcome, &config);
///
/// assert!((player_one_new.rating.round() - 1662.0).abs() < f64::EPSILON);
/// assert!((player_one_new.deviation.round() - 290.0).abs() < f64::EPSILON);
///
/// assert!((player_two_new.rating.round() - 1338.0).abs() < f64::EPSILON);
/// assert!((player_two_new.deviation.round() - 290.0).abs() < f64::EPSILON);
/// ```
pub fn sticko(
    player_one: &StickoRating,
    player_two: &StickoRating,
    outcome: &Outcomes,
    config: &StickoConfig,
) -> (StickoRating, StickoRating) {
    let q = 10.0_f64.ln() / 400.0;

    let outcome1 = match outcome {
        Outcomes::WIN => 1.0,
        Outcomes::DRAW => 0.5,
        Outcomes::LOSS => 0.0,
    };
    let outcome2 = 1.0 - outcome1;

    let colour1 = 1.0;
    let colour2 = -1.0;

    let lambda1 = (config.lambda / 100.0) * (player_two.rating - player_one.rating);
    let lambda2 = (config.lambda / 100.0) * (player_one.rating - player_two.rating);

    let g1 = g_value(q, player_two.deviation);
    let g2 = g_value(q, player_one.deviation);

    let e1 = e_value(
        g1,
        player_one.rating,
        player_two.rating,
        config.gamma,
        colour1,
    );
    let e2 = e_value(
        g2,
        player_two.rating,
        player_one.rating,
        config.gamma,
        colour2,
    );

    let d1 = d_value(q, g1, e1);
    let d2 = d_value(q, g2, e2);

    let player_one_new_rating = new_rating(
        player_one.rating,
        player_one.deviation,
        q,
        d1,
        g1,
        outcome1,
        e1,
        config.beta,
        lambda1,
    );
    let player_two_new_rating = new_rating(
        player_two.rating,
        player_two.deviation,
        q,
        d2,
        g2,
        outcome2,
        e2,
        config.beta,
        lambda2,
    );

    let player_one_new_deviation = new_deviation(player_one.deviation, d1, config.h);
    let player_two_new_deviation = new_deviation(player_two.deviation, d2, config.h);

    (
        StickoRating {
            rating: player_one_new_rating,
            deviation: player_one_new_deviation,
        },
        StickoRating {
            rating: player_two_new_rating,
            deviation: player_two_new_deviation,
        },
    )
}

#[must_use]
/// The "traditional" way of calculating a [`StickoRating`] of a player in a rating period.
///
/// Takes in a player as an [`StickoRating`] and their results as a Vec of tuples containing the opponent as an [`StickoRating`],
/// the outcome of the game as an [`Outcome`](Outcomes) and a [`StickoConfig`].
///
/// The outcome of the match is in the perspective of the player.
/// This means [`Outcomes::WIN`] is a win for the player and [`Outcomes::LOSS`] is a win for the opponent.
///
/// If the player's results are empty, the player's rating deviation will automatically be decayed using [`decay_deviation`].
///
/// # Examples
/// ```
/// use skillratings::{
///     sticko::sticko_rating_period, outcomes::Outcomes, rating::StickoRating, config::StickoConfig
/// };
///
/// let player = StickoRating {
///     rating: 1500.0,
///     deviation: 200.0,
/// };
///
/// let opponent1 = StickoRating {
///     rating: 1400.0,
///     deviation: 30.0,
/// };
///
/// let opponent2 = StickoRating {
///     rating: 1550.0,
///     deviation: 100.0,
/// };
///
/// let opponent3 = StickoRating {
///     rating: 1700.0,
///     deviation: 300.0,
/// };
///
/// let results = vec![
///     (opponent1, Outcomes::WIN),
///     (opponent2, Outcomes::LOSS),
///     (opponent3, Outcomes::LOSS),
/// ];
///
/// let config = StickoConfig::new();
///
/// let new_player = sticko_rating_period(&player, &results, &config);
///
/// assert!((new_player.rating.round() - 1467.0).abs() < f64::EPSILON);
/// assert!((new_player.deviation - 151.325_708_084_173_56).abs() < f64::EPSILON);
/// ```
pub fn sticko_rating_period(
    player: &StickoRating,
    results: &Vec<(StickoRating, Outcomes)>,
    config: &StickoConfig,
) -> StickoRating {
    let q = 10_f64.ln() / 400.0;

    if results.is_empty() {
        return decay_deviation(player, config);
    }

    let mut player_rating = player.rating;
    let mut player_deviation = player.deviation;

    for (opponent, outcome) in results {
        let outcome = match outcome {
            Outcomes::WIN => 1.0,
            Outcomes::DRAW => 0.5,
            Outcomes::LOSS => 0.0,
        };

        let g = g_value(q, opponent.deviation);

        let e = e_value(g, player_rating, opponent.rating, config.gamma, 1.0);

        let d = d_value(q, g, e);

        let lambda = (config.lambda / 100.0) * (opponent.rating - player_rating);

        let new_rating = new_rating(
            player_rating,
            player_deviation,
            q,
            d,
            g,
            outcome,
            e,
            config.beta,
            lambda,
        );

        let new_deviation = new_deviation(player_deviation, d, config.h);

        player_rating = new_rating;
        player_deviation = new_deviation;
    }

    StickoRating {
        rating: player_rating,
        deviation: player_deviation,
    }
}

#[must_use]
/// Calculates the expected outcome of two players based on sticko.
///
/// Takes in two players as [`StickoRating`]s and a [`StickoConfig`], which determines the advantage of player one,
/// and returns the probability of victory for each player as an [`f64`] between 1.0 and 0.0.  
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// # Examples
/// ```
/// use skillratings::{sticko::expected_score, rating::StickoRating, config::StickoConfig};
///
/// let player_one = StickoRating {
///     rating: 1830.0,
///     deviation: 41.0,
/// };
/// let player_two = StickoRating {
///     rating: 1950.0,
///     deviation: 320.0,
/// };
///
/// let config = StickoConfig {
///     // This is approximately the advantage white has in chess.
///     gamma: 30.0,
///     ..Default::default()
/// };
///
/// let (exp_one, exp_two) = expected_score(&player_one, &player_two, &config);
///
/// assert!(((exp_one * 100.0).round() - 41.0).abs() < f64::EPSILON);
/// assert!(((exp_two * 100.0).round() - 59.0).abs() < f64::EPSILON);
/// ```
pub fn expected_score(
    player_one: &StickoRating,
    player_two: &StickoRating,
    config: &StickoConfig,
) -> (f64, f64) {
    let q = 10_f64.ln() / 400.0;
    let g = g_value(q, player_one.deviation.hypot(player_two.deviation));

    let expected_one = (1.0
        + 10_f64.powf(-1.0 * g * (player_one.rating + config.gamma - player_two.rating) / 400.0))
    .recip();

    (expected_one, (1.0 - expected_one))
}

#[must_use]
/// Decays a Rating Deviation Value for a player, if they missed playing in a certain rating period.
///
/// The length of the rating period and thus the number of missed periods per player is something to decide and track yourself.
///
/// Takes in a player as a [`StickoRating`] and a [`StickoConfig`], that describes how much the rating should change, and returns the decayed [`StickoRating`].
///
/// # Examples
/// ```
/// use skillratings::{sticko::decay_deviation, rating::StickoRating, config::StickoConfig};
///
/// let player_one = StickoRating {
///     rating: 2720.0,
///     deviation: 41.3,
/// };
///
/// let config = StickoConfig::new();
///
/// let player_one_decay = decay_deviation(&player_one, &config);
///
/// assert!((player_one_decay.deviation.round() - 42.0).abs() < f64::EPSILON);
/// ```
pub fn decay_deviation(player: &StickoRating, config: &StickoConfig) -> StickoRating {
    let new_player_deviation = player.deviation.hypot(config.c).min(350.0);

    StickoRating {
        rating: player.rating,
        deviation: new_player_deviation,
    }
}

#[must_use]
/// The 95% confidence interval of the lowest to highest rating.
///
/// The system is 95% sure that the "true skill" of the player is in-between these values.
///
/// Takes in a player as a [`StickoRating`] and returns two [`f64`]s that describe the lowest and highest rating.
///
/// # Examples
/// ```rust
/// use skillratings::{rating::StickoRating, sticko::confidence_interval};
///
/// let player = StickoRating {
///     rating: 2250.0,
///     deviation: 79.0,
/// };
///
/// let (interval_low, interval_high) = confidence_interval(&player);
///
/// assert!(interval_low.round() - 2095.0 < f64::EPSILON);
/// assert!(interval_high.round() - 2405.0 < f64::EPSILON);
/// ```
pub fn confidence_interval(player: &StickoRating) -> (f64, f64) {
    (
        player.rating - 1.96 * player.deviation,
        1.96f64.mul_add(player.deviation, player.rating),
    )
}

fn new_deviation(old_deviation: f64, d: f64, h: f64) -> f64 {
    ((old_deviation.mul_add(old_deviation, h).recip() + d.recip())
        .recip()
        .sqrt())
    .min(350.0)
}

#[allow(clippy::too_many_arguments)]
fn new_rating(
    old_rating: f64,
    deviation: f64,
    q: f64,
    d: f64,
    g: f64,
    score: f64,
    e: f64,
    beta: f64,
    lambda: f64,
) -> f64 {
    ((q / (deviation.powi(2).recip() + d.recip())) * g).mul_add(score - e + beta, old_rating)
        + lambda
}

fn g_value(q: f64, opponent_deviation: f64) -> f64 {
    (1.0 + ((3.0 * q.powi(2) * opponent_deviation.powi(2)) / (PI.powi(2))))
        .sqrt()
        .recip()
}

fn e_value(g: f64, rating: f64, opponent_rating: f64, advantage: f64, colour: f64) -> f64 {
    (1.0 + (10_f64.powf(-1.0 * g * advantage.mul_add(colour, rating - opponent_rating) / 400.0)))
        .recip()
}

fn d_value(q: f64, g: f64, e: f64) -> f64 {
    (q.powi(2) * g.powi(2) * e * (1.0 - e)).powi(-1)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    /// This is the same test as in glicko.
    /// When using 0.0 for all values in the config,
    /// it should produce the same result.
    fn test_sticko() {
        let config = StickoConfig {
            h: 0.0,
            beta: 0.0,
            lambda: 0.0,
            gamma: 0.0,
            c: 0.0,
        };

        let player1 = StickoRating {
            rating: 1500.0,
            deviation: 200.0,
        };

        let opponent1 = StickoRating {
            rating: 1400.0,
            deviation: 30.0,
        };

        let opponent2 = StickoRating {
            rating: 1550.0,
            deviation: 100.0,
        };

        let opponent3 = StickoRating {
            rating: 1700.0,
            deviation: 300.0,
        };

        let (player1, _) = sticko(&player1, &opponent1, &Outcomes::WIN, &config);

        let (player1, _) = sticko(&player1, &opponent2, &Outcomes::LOSS, &config);

        let (player1, _) = sticko(&player1, &opponent3, &Outcomes::LOSS, &config);

        assert!((player1.rating.round() - 1464.0).abs() < f64::EPSILON);
        assert!((player1.deviation - 151.253_743_431_783_2).abs() < f64::EPSILON);
    }

    #[test]
    fn test_sticko_draw() {
        let player_one = StickoRating {
            rating: 2330.0,
            deviation: 200.0,
        };

        let player_two = StickoRating {
            rating: 1800.0,
            deviation: 20.0,
        };

        let config = StickoConfig::new();

        let (p1, p2) = sticko(&player_one, &player_two, &Outcomes::DRAW, &config);

        assert!((p1.rating.round() - 2221.0).abs() < f64::EPSILON);
        assert!((p1.deviation.round() - 195.0).abs() < f64::EPSILON);
        assert!((p2.rating.round() - 1811.0).abs() < f64::EPSILON);
        assert!((p2.deviation.round() - 20.0).abs() < f64::EPSILON);

        let rp1 = sticko_rating_period(&player_one, &vec![(player_two, Outcomes::DRAW)], &config);

        assert_eq!(rp1, p1);
    }

    #[test]
    /// This is the same test as above.
    fn test_sticko_rating_period() {
        let config = StickoConfig {
            h: 0.0,
            beta: 0.0,
            lambda: 0.0,
            gamma: 0.0,
            c: 0.0,
        };

        let player = StickoRating {
            rating: 1500.0,
            deviation: 200.0,
        };

        let opponent1 = StickoRating {
            rating: 1400.0,
            deviation: 30.0,
        };

        let opponent2 = StickoRating {
            rating: 1550.0,
            deviation: 100.0,
        };

        let opponent3 = StickoRating {
            rating: 1700.0,
            deviation: 300.0,
        };

        let new_player = sticko_rating_period(
            &player,
            &vec![
                (opponent1, Outcomes::WIN),
                (opponent2, Outcomes::LOSS),
                (opponent3, Outcomes::LOSS),
            ],
            &config,
        );

        assert!((new_player.rating.round() - 1464.0).abs() < f64::EPSILON);
        assert!((new_player.deviation - 151.253_743_431_783_2).abs() < f64::EPSILON);

        let after_player = sticko_rating_period(&player, &vec![], &config);

        assert_eq!(player, after_player);
    }

    #[test]
    fn test_expected_score() {
        let player_one = StickoRating {
            rating: 1400.0,
            deviation: 40.0,
        };

        let player_two = StickoRating {
            rating: 1500.0,
            deviation: 150.0,
        };

        let config_no_adv = StickoConfig::new();

        let config_adv = StickoConfig {
            gamma: 30.0,
            ..Default::default()
        };

        let (exp_one, exp_two) = expected_score(&player_one, &player_two, &config_no_adv);

        assert!((exp_one - 0.373_700_405_951_935).abs() < f64::EPSILON);
        assert!((exp_two - 0.626_299_594_048_065).abs() < f64::EPSILON);
        assert!((exp_one + exp_two - 1.0).abs() < f64::EPSILON);

        let (exp_one, exp_two) = expected_score(&player_one, &player_two, &config_adv);

        assert!((exp_one - 0.410_605_680_590_947_1).abs() < f64::EPSILON);
        assert!((exp_two - 0.589_394_319_409_053).abs() < f64::EPSILON);
        assert!((exp_one + exp_two - 1.0).abs() < f64::EPSILON);
    }

    #[test]
    #[allow(clippy::similar_names)]
    fn sticko_glicko_conversions() {
        use crate::rating::{Glicko2Rating, GlickoRating};

        let sticko = StickoRating::new();

        let glicko_conv = GlickoRating::from(sticko);
        let glicko2_conv = Glicko2Rating::from(sticko);

        assert!((glicko_conv.rating - 1500.0).abs() < f64::EPSILON);
        assert!((glicko2_conv.rating - 1500.0).abs() < f64::EPSILON);

        let glicko2 = Glicko2Rating::new();
        let glicko = GlickoRating::new();

        assert_eq!(StickoRating::new(), StickoRating::from(glicko2));
        assert_eq!(StickoRating::default(), StickoRating::from(glicko));
    }

    #[test]
    fn test_confidence_interval() {
        let player = StickoRating {
            rating: 1500.0,
            deviation: 30.0,
        };

        let ci = confidence_interval(&player);

        assert!((ci.0.round() - 1441.0).abs() < f64::EPSILON);
        assert!((ci.1.round() - 1559.0).abs() < f64::EPSILON);
    }
}
