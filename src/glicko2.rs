//! The Glicko-2 algorithm, an improvement on Glicko and widely used in online games,
//! like Counter Strike: Global Offensive, Team Fortress 2, Splatoon 2 and most online chess platforms.
//!
//! If you are looking for the regular Glicko rating system, please see [`Glicko`](crate::glicko).
//!
//! The main improvement over Glicko is the rating volatility which is the expected fluctuation of a players rating,
//! based on how consistent a player is performing. The lower the volatility, the more consistent a player performs.
//!
//! # Quickstart
//!
//! This is the most basic example on how to use the Glicko-2 Module.  
//! Please take a look at the functions below to see more advanced use cases.
//!
//! ```
//! use skillratings::{
//!     glicko2::{glicko2, Glicko2Config, Glicko2Rating},
//!     Outcomes,
//! };
//!
//! // Initialise a new player rating.
//! let player_one = Glicko2Rating::new();
//!
//! // Or you can initialise it with your own values of course.
//! // Imagine these numbers being pulled from a database.
//! let (some_rating, some_deviation, some_volatility) = (1325.0, 230.0, 0.05932);
//! let player_two = Glicko2Rating {
//!     rating: some_rating,
//!     deviation: some_deviation,
//!     volatility: some_volatility,
//! };
//!
//! // The outcome of the match is from the perspective of player one.
//! let outcome = Outcomes::WIN;
//!
//! // The config allows you to specify certain values in the Glicko-2 calculation.
//! // Here we set the Tau value to 0.9, instead of the default 0.5.
//! // This will increase the change in volatility over time.
//! // According to Mark Glickman, values between 0.3 and 1.2 are reasonable.
//! let config = Glicko2Config {
//!     tau: 0.9,
//!     ..Default::default()
//! };
//!
//! // The glicko2 function will calculate the new ratings for both players and return them.
//! let (new_player_one, new_player_two) = glicko2(&player_one, &player_two, &outcome, &config);
//! ```
//!
//! # More Information
//!
//! - [Wikipedia Article](https://en.wikipedia.org/wiki/Glicko_rating_system)
//! - [Original Paper by Mark Glickman](http://www.glicko.net/glicko/glicko2.pdf)
//! - [Glicko-2 Calculator](https://fsmosca-glicko2calculator-glicko2calculator-vik8k0.streamlitapp.com/)

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::{
    glicko::GlickoRating, glicko_boost::GlickoBoostRating, sticko::StickoRating, Outcomes,
};
use std::f64::consts::PI;

/// The Glicko-2 rating of a player.
///
/// For the Glicko rating, please see [`GlickoRating`].
///
/// The default rating is 1500.0.  
/// The default deviation is 350.0.  
/// The default volatility is 0.06.
#[derive(Copy, Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Glicko2Rating {
    /// The player's Glicko-2 rating number, by default 1500.0.
    pub rating: f64,
    /// The player's Glicko-2 deviation number, by default 350.0.
    pub deviation: f64,
    /// The player's Glicko-2 volatility number, by default 0.06.
    pub volatility: f64,
}

impl Glicko2Rating {
    /// Initialise a new `Glicko2Rating` with a rating of 1500.0, a deviation of 350.0 and a volatility of 0.06.
    #[must_use]
    pub const fn new() -> Self {
        Self {
            rating: 1500.0,
            deviation: 350.0,
            volatility: 0.06,
        }
    }
}

impl Default for Glicko2Rating {
    fn default() -> Self {
        Self::new()
    }
}

impl From<GlickoRating> for Glicko2Rating {
    fn from(g: GlickoRating) -> Self {
        Self {
            rating: g.rating,
            deviation: g.deviation,
            ..Default::default()
        }
    }
}

impl From<GlickoBoostRating> for Glicko2Rating {
    fn from(g: GlickoBoostRating) -> Self {
        Self {
            rating: g.rating,
            deviation: g.deviation,
            ..Default::default()
        }
    }
}

impl From<StickoRating> for Glicko2Rating {
    fn from(s: StickoRating) -> Self {
        Self {
            rating: s.rating,
            deviation: s.deviation,
            ..Default::default()
        }
    }
}

#[derive(Clone, Copy, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// Constants used in the Glicko-2 calculations.
pub struct Glicko2Config {
    /// The tau constant constrains the change in volatility over time.
    /// To cite Mark Glickman himself: "Reasonable choices are between 0.3 and 1.2".
    /// Smaller values mean less change in volatility and vice versa.  
    /// The default value here is `0.5`.
    pub tau: f64,
    /// The convergence tolerance value, the smaller the value the more accurate the volatility calculations.  
    /// The default value is `0.000_001`, as suggested in [the paper (page 3)](http://www.glicko.net/glicko/glicko2.pdf).  
    /// Do not set this to a negative value.
    pub convergence_tolerance: f64,
}

impl Glicko2Config {
    #[must_use]
    /// Initialise a new `Glicko2Config` with a tau value of `0.5` and a convergence tolerance of `0.000_001`.
    pub const fn new() -> Self {
        Self {
            tau: 0.5,
            convergence_tolerance: 0.000_001,
        }
    }
}

impl Default for Glicko2Config {
    fn default() -> Self {
        Self::new()
    }
}

/// Calculates the [`Glicko2Rating`]s of two players based on their old ratings, deviations, volatilities, and the outcome of the game.
///
/// For the original version, please see [`Glicko`](crate::glicko).
///
/// Takes in two players as [`Glicko2Rating`]s, an [`Outcome`](Outcomes), and a [`Glicko2Config`].
///
/// Instead of the traditional way of calculating the Glicko-2 for only one player only using a list of results,
/// we are calculating the Glicko-2 rating for two players at once, like in the Elo calculation,
/// to make it easier to see instant results.
///
/// For the traditional way of calculating a Glicko-2 rating please see [`glicko2_rating_period`].
///
/// The outcome of the match is in the perspective of `player_one`.
/// This means [`Outcomes::WIN`] is a win for `player_one` and [`Outcomes::LOSS`] is a win for `player_two`.
///
/// # Examples
/// ```
/// use skillratings::{
///     glicko2::{glicko2, Glicko2Config, Glicko2Rating},
///     Outcomes,
/// };
///
/// let player_one = Glicko2Rating {
///     rating: 1500.0,
///     deviation: 350.0,
///     volatility: 0.06,
/// };
/// let player_two = Glicko2Rating {
///     rating: 1500.0,
///     deviation: 350.0,
///     volatility: 0.06,
/// };
///
/// let outcome = Outcomes::WIN;
///
/// let config = Glicko2Config::new();
///
/// let (new_one, new_two) = glicko2(&player_one, &player_two, &outcome, &config);
///
/// assert!((new_one.rating.round() - 1662.0).abs() < f64::EPSILON);
/// assert!((new_one.deviation.round() - 290.0).abs() < f64::EPSILON);
/// assert!((new_one.volatility - 0.05999967537233814).abs() < f64::EPSILON);
///
/// assert!((new_two.rating.round() - 1338.0).abs() < f64::EPSILON);
/// assert!((new_two.deviation.round() - 290.0).abs() < f64::EPSILON);
/// assert!((new_two.volatility - 0.05999967537233814).abs() < f64::EPSILON);
/// ```
#[must_use]
pub fn glicko2(
    player_one: &Glicko2Rating,
    player_two: &Glicko2Rating,
    outcome: &Outcomes,
    config: &Glicko2Config,
) -> (Glicko2Rating, Glicko2Rating) {
    // First we need to convert the ratings into the glicko-2 scale.
    let player_one_rating = (player_one.rating - 1500.0) / 173.7178;
    let player_two_rating = (player_two.rating - 1500.0) / 173.7178;

    // Same with the deviation.
    let player_one_deviation = player_one.deviation / 173.7178;
    let player_two_deviation = player_two.deviation / 173.7178;

    let outcome1 = outcome.to_chess_points();
    let outcome2 = 1.0 - outcome1;

    // We always need the deviation of the opponent in the g function.
    let g1 = g_value(player_two_deviation);
    let g2 = g_value(player_one_deviation);

    let e1 = e_value(player_one_rating, player_two_rating, g1);
    let e2 = e_value(player_two_rating, player_one_rating, g2);

    let v1 = v_value(g1, e1);
    let v2 = v_value(g2, e2);

    let player_one_new_volatility = new_volatility(
        player_one.volatility,
        delta_value(outcome1, v1, g1, e1).powi(2),
        player_one_deviation.powi(2),
        v1,
        config.tau,
        config.convergence_tolerance,
    );
    let player_two_new_volatility = new_volatility(
        player_two.volatility,
        delta_value(outcome2, v2, g2, e2).powi(2),
        player_two_deviation.powi(2),
        v2,
        config.tau,
        config.convergence_tolerance,
    );

    let new_deviation1 = new_deviation(player_one_deviation, player_one_new_volatility, v1);
    let new_deviation2 = new_deviation(player_two_deviation, player_two_new_volatility, v2);

    let new_rating1 = new_rating(player_one_rating, new_deviation1, outcome1, g1, e1);
    let new_rating2 = new_rating(player_two_rating, new_deviation2, outcome2, g2, e2);

    // We return the new values, converted back to the original scale.
    let player_one_new = Glicko2Rating {
        rating: new_rating1.mul_add(173.7178, 1500.0),
        deviation: new_deviation1 * 173.7178,
        volatility: player_one_new_volatility,
    };
    let player_two_new = Glicko2Rating {
        rating: new_rating2.mul_add(173.7178, 1500.0),
        deviation: new_deviation2 * 173.7178,
        volatility: player_two_new_volatility,
    };

    (player_one_new, player_two_new)
}

/// The "traditional" way of calculating a [`Glicko2Rating`] of a player in a rating period.
///
/// Note that in this case, all of the matches are considered to be played at once.  
/// This means that the player will not get updated in-between matches, as you might expect.  
/// This will result in *slightly* different results than if you were to use the [`glicko2`] function in a loop.
///
/// Takes in a player as an [`Glicko2Rating`] and their results as a Vec of tuples containing the opponent as an [`Glicko2Rating`],
/// the outcome of the game as an [`Outcome`](Outcomes) and a [`Glicko2Config`].
///
/// The outcome of the match is in the perspective of the player.
/// This means [`Outcomes::WIN`] is a win for the player and [`Outcomes::LOSS`] is a win for the opponent.
///
/// If the player's results are empty, the player's rating deviation will automatically be decayed using [`decay_deviation`].
///
/// # Examples
/// ```
/// use skillratings::{
///     glicko2::{glicko2_rating_period, Glicko2Config, Glicko2Rating},
///     Outcomes,
/// };
///
/// let player = Glicko2Rating {
///     rating: 1500.0,
///     deviation: 200.0,
///     volatility: 0.06,
/// };
///
/// let opponent1 = Glicko2Rating {
///     rating: 1400.0,
///     deviation: 30.0,
///     volatility: 0.06,
/// };
///
/// let opponent2 = Glicko2Rating {
///     rating: 1550.0,
///     deviation: 100.0,
///     volatility: 0.06,
/// };
///
/// let opponent3 = Glicko2Rating {
///     rating: 1700.0,
///     deviation: 300.0,
///     volatility: 0.06,
/// };
///
/// let results = vec![
///     (opponent1, Outcomes::WIN),
///     (opponent2, Outcomes::LOSS),
///     (opponent3, Outcomes::LOSS),
/// ];
///
/// let new_player = glicko2_rating_period(&player, &results, &Glicko2Config::new());
///
/// assert!((new_player.rating.round() - 1464.0).abs() < f64::EPSILON);
/// assert!((new_player.deviation.round() - 152.0).abs() < f64::EPSILON);
/// assert!((new_player.volatility - 0.059995984286488495).abs() < f64::EPSILON);
/// ```
#[must_use]
pub fn glicko2_rating_period(
    player: &Glicko2Rating,
    results: &Vec<(Glicko2Rating, Outcomes)>,
    config: &Glicko2Config,
) -> Glicko2Rating {
    if results.is_empty() {
        return decay_deviation(player);
    }

    let player_rating = (player.rating - 1500.0) / 173.7178;
    let player_deviation = player.deviation / 173.7178;

    let v = results
        .iter()
        .map(|r| {
            let g = g_value(r.0.deviation / 173.7178);

            let e = e_value(player_rating, (r.0.rating - 1500.0) / 173.7178, g);

            g.powi(2) * e * (1.0 - e)
        })
        .sum::<f64>()
        .recip();

    let scores = (results.iter().map(|r| {
        let g = g_value(r.0.deviation / 173.7178);

        let e = e_value(player_rating, (r.0.rating - 1500.0) / 173.7178, g);

        let s = r.1.to_chess_points();

        g * (s - e)
    }))
    .sum::<f64>();

    let delta = v * scores;

    let new_volatility = new_volatility(
        player.volatility,
        delta.powi(2),
        player_deviation.powi(2),
        v,
        config.tau,
        config.convergence_tolerance,
    );

    let new_deviation = new_deviation(player_deviation, new_volatility, v);

    let new_rating = new_deviation.powi(2).mul_add(scores, player_rating);

    Glicko2Rating {
        rating: new_rating.mul_add(173.7178, 1500.0),
        deviation: new_deviation * 173.7178,
        volatility: new_volatility,
    }
}

/// Calculates the expected outcome of two players based on glicko-2.
///
/// Takes in two players as [`Glicko2Rating`]s and returns the probability of victory for each player as an [`f64`] between 1.0 and 0.0.  
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// # Examples
/// ```
/// use skillratings::glicko2::{expected_score, Glicko2Rating};
///
/// let player_one = Glicko2Rating {
///     rating: 2500.0,
///     deviation: 41.0,
///     volatility: 0.06,
/// };
/// let player_two = Glicko2Rating {
///     rating: 1950.0,
///     deviation: 320.0,
///     volatility: 0.06,
/// };
/// let (exp_one, exp_two) = expected_score(&player_one, &player_two);
/// assert!(((exp_one * 100.0).round() - 90.0).abs() < f64::EPSILON);
/// assert!(((exp_two * 100.0).round() - 10.0).abs() < f64::EPSILON);
/// ```
#[must_use]
pub fn expected_score(player_one: &Glicko2Rating, player_two: &Glicko2Rating) -> (f64, f64) {
    // First we need to convert the ratings into the glicko-2 scale.
    let player_one_rating = (player_one.rating - 1500.0) / 173.7178;
    let player_two_rating = (player_two.rating - 1500.0) / 173.7178;

    // Same with the deviation.
    let player_one_deviation = player_one.deviation / 173.7178;
    let player_two_deviation = player_two.deviation / 173.7178;

    let a1 = g_value(player_two_deviation.hypot(player_one_deviation))
        * (player_one_rating - player_two_rating);

    let exp_one = (1.0 + (-a1).exp()).recip();
    let exp_two = 1.0 - exp_one;

    (exp_one, exp_two)
}

/// Decays a Rating Deviation Value for a player, if they missed playing in a certain rating period.
///
/// The length of the rating period and thus the number of missed periods per player is something to decide and track yourself.
///
/// Takes in a player as a [`Glicko2Rating`] and returns the decayed [`Glicko2Rating`].
///
/// # Examples
/// ```
/// use skillratings::glicko2::{decay_deviation, Glicko2Rating};
///
/// let player_one = Glicko2Rating {
///     rating: 2720.0,
///     deviation: 41.3,
///     volatility: 0.06,
/// };
///
/// let player_one_decay = decay_deviation(&player_one);
///
/// assert!((player_one_decay.deviation.round() - 43.0).abs() < f64::EPSILON);
/// ```
#[must_use]
pub fn decay_deviation(player: &Glicko2Rating) -> Glicko2Rating {
    let player_deviation = player.deviation / 173.7178;
    let new_player_deviation = player_deviation.hypot(player.volatility);

    Glicko2Rating {
        rating: player.rating,
        deviation: (new_player_deviation * 173.7178).min(350.0),
        volatility: player.volatility,
    }
}

#[must_use]
/// The 95% confidence interval of the lowest to highest rating.
///
/// The system is 95% sure that the "true skill" of the player is in-between these values.
///
/// Takes in a player as a [`Glicko2Rating`] and returns two [`f64`]s that describe the lowest and highest rating.
///
/// # Examples
/// ```
/// use skillratings::glicko2::{confidence_interval, Glicko2Rating};
///
/// let player = Glicko2Rating {
///     rating: 2250.0,
///     deviation: 79.0,
///     volatility: 0.0598,
/// };
///
/// let (interval_low, interval_high) = confidence_interval(&player);
///
/// assert!(interval_low.round() - 2095.0 < f64::EPSILON);
/// assert!(interval_high.round() - 2405.0 < f64::EPSILON);
/// ```
pub fn confidence_interval(player: &Glicko2Rating) -> (f64, f64) {
    (
        // Seems like there is no mul_sub function.
        player.rating - 1.96 * player.deviation,
        1.96f64.mul_add(player.deviation, player.rating),
    )
}

fn g_value(deviation: f64) -> f64 {
    (1.0 + ((3.0 * deviation.powi(2)) / (PI.powi(2))))
        .sqrt()
        .recip()
}

fn e_value(rating: f64, opponent_rating: f64, g: f64) -> f64 {
    (1.0 + (-g * (rating - opponent_rating)).exp()).recip()
}

fn v_value(g: f64, e: f64) -> f64 {
    (g.powi(2) * e * (1.0 - e)).recip()
}

fn delta_value(outcome: f64, v: f64, g: f64, e: f64) -> f64 {
    v * (g * (outcome - e))
}

fn f_value(
    x: f64,
    delta_square: f64,
    deviation_square: f64,
    v: f64,
    volatility: f64,
    tau: f64,
) -> f64 {
    let i = (x.exp() * (delta_square - deviation_square - v - x.exp()))
        / (2.0 * (deviation_square + v + x.exp()).powi(2));

    let j = (x - volatility.powi(2).ln()) / tau.powi(2);

    i - j
}

fn new_volatility(
    old_volatility: f64,
    delta_squared: f64,
    deviation_squared: f64,
    v: f64,
    tau: f64,
    convergence_tolerance: f64,
) -> f64 {
    let mut a = old_volatility.powi(2).ln();
    let mut b = if delta_squared > deviation_squared + v {
        (delta_squared - deviation_squared - v).ln()
    } else {
        let mut k = 1.0;
        while f_value(
            a - k * tau,
            delta_squared,
            deviation_squared,
            v,
            old_volatility,
            tau,
        ) < 0.0
        {
            k += 1.0;
        }
        a - k * tau
    };

    let mut fa = f_value(a, delta_squared, deviation_squared, v, old_volatility, tau);
    let mut fb = f_value(b, delta_squared, deviation_squared, v, old_volatility, tau);

    // 0.000001 is the convergence tolerance suggested by Mark Glickman.
    while (b - a).abs() > convergence_tolerance {
        let c = a + ((a - b) * fa / (fb - fa));
        let fc = f_value(c, delta_squared, deviation_squared, v, old_volatility, tau);

        if fc * fb <= 0.0 {
            a = b;
            fa = fb;
        } else {
            fa /= 2.0;
        }

        b = c;
        fb = fc;
    }

    (a / 2.0).exp()
}

fn new_deviation(deviation: f64, new_volatility: f64, v: f64) -> f64 {
    let pre_deviation = deviation.hypot(new_volatility);

    ((pre_deviation.powi(2).recip()) + (v.recip()))
        .sqrt()
        .recip()
}

fn new_rating(rating: f64, new_deviation: f64, outcome: f64, g_value: f64, e_value: f64) -> f64 {
    (new_deviation.powi(2) * g_value).mul_add(outcome - e_value, rating)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_equal_glicko2() {
        let player1 = Glicko2Rating {
            rating: 1520.0,
            deviation: 350.0,
            volatility: 0.06,
        };

        let player2 = Glicko2Rating {
            rating: 1420.0,
            deviation: 350.0,
            volatility: 0.06,
        };

        let config = Glicko2Config {
            //convergence_tolerance: 0.000_000_000_000_001,
            ..Default::default()
        };

        let (player1new, player2new) = glicko2(&player1, &player2, &Outcomes::WIN, &config);

        assert!((player1new.rating.round() - 1653.0).abs() < f64::EPSILON);
        assert!((player1new.deviation.round() - 292.0).abs() < f64::EPSILON);

        assert!((player2new.rating.round() - 1287.0).abs() < f64::EPSILON);
        assert!((player2new.deviation.round() - 292.0).abs() < f64::EPSILON);
    }

    #[test]
    fn not_equal_deviation_draw() {
        let player1 = Glicko2Rating {
            rating: 1600.0,
            deviation: 350.0,
            volatility: 0.06,
        };

        let player2 = Glicko2Rating {
            rating: 1500.0,
            deviation: 50.0,
            volatility: 0.06,
        };

        let (player1new, player2new) = glicko2(
            &player1,
            &player2,
            &Outcomes::DRAW,
            &Glicko2Config::default(),
        );

        assert!((player1new.rating.round() - 1550.0).abs() < f64::EPSILON);
        assert!((player1new.deviation.round() - 253.0).abs() < f64::EPSILON);

        assert!((player2new.rating.round() - 1501.0).abs() < f64::EPSILON);
        assert!((player2new.deviation.round() - 51.0).abs() < f64::EPSILON);
    }

    #[test]
    /// This test is taken directly from the official glicko2 example.
    /// <http://www.glicko.net/glicko/glicko2.pdf>
    fn test_glicko2() {
        let player = Glicko2Rating {
            rating: 1500.0,
            deviation: 200.0,
            volatility: 0.06,
        };

        let opponent_one = Glicko2Rating {
            rating: 1400.0,
            deviation: 30.0,
            volatility: 0.06,
        };

        let (player, opponent_one) = glicko2(
            &player,
            &opponent_one,
            &Outcomes::WIN,
            &Glicko2Config::new(),
        );

        assert!((player.rating.round() - 1564.0).abs() < f64::EPSILON);
        assert!((player.deviation.round() - 175.0).abs() < f64::EPSILON);

        assert!((opponent_one.rating.round() - 1398.0).abs() < f64::EPSILON);
        assert!((opponent_one.deviation.round() - 32.0).abs() < f64::EPSILON);

        let opponent_two = Glicko2Rating {
            rating: 1550.0,
            deviation: 100.0,
            volatility: 0.06,
        };

        let (player, _) = glicko2(
            &player,
            &opponent_two,
            &Outcomes::LOSS,
            &Glicko2Config::new(),
        );

        let opponent_three = Glicko2Rating {
            rating: 1700.0,
            deviation: 300.0,
            volatility: 0.06,
        };

        let (player, _) = glicko2(
            &player,
            &opponent_three,
            &Outcomes::LOSS,
            &Glicko2Config::new(),
        );

        assert!((player.rating.round() - 1464.0).abs() < f64::EPSILON);
        assert!((player.deviation.round() - 152.0).abs() < f64::EPSILON);
        assert!((player.volatility - 0.059_997_514_049_860_735).abs() < f64::EPSILON);
    }

    #[test]
    fn test_glicko2_rating_period() {
        let player = Glicko2Rating {
            rating: 1500.0,
            deviation: 200.0,
            volatility: 0.06,
        };

        let opponent_one = Glicko2Rating {
            rating: 1400.0,
            deviation: 30.0,
            volatility: 0.06,
        };

        let opponent_two = Glicko2Rating {
            rating: 1550.0,
            deviation: 100.0,
            volatility: 0.06,
        };

        let opponent_three = Glicko2Rating {
            rating: 1700.0,
            deviation: 300.0,
            volatility: 0.06,
        };

        let results = vec![
            (opponent_one, Outcomes::WIN),
            (opponent_two, Outcomes::LOSS),
            (opponent_three, Outcomes::LOSS),
        ];

        let new_player = glicko2_rating_period(&player, &results, &Glicko2Config::new());

        assert!((new_player.rating.round() - 1464.0).abs() < f64::EPSILON);
        assert!(((new_player.deviation * 100.0).round() - 15152.0).abs() < f64::EPSILON);
        assert!((new_player.volatility - 0.059_995_984_286_488_495).abs() < f64::EPSILON);

        let player = Glicko2Rating {
            rating: 1250.0,
            deviation: 95.0,
            volatility: 0.06,
        };

        let results: Vec<(Glicko2Rating, Outcomes)> = Vec::new();

        let new_player = glicko2_rating_period(&player, &results, &Glicko2Config::new());

        assert!((new_player.deviation.round() - 96.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_expected_score() {
        let player_one = Glicko2Rating {
            rating: 1500.0,
            deviation: 350.0,
            volatility: 0.06,
        };

        let player_two = Glicko2Rating {
            rating: 1500.0,
            deviation: 350.0,
            volatility: 0.06,
        };

        let (exp_one, exp_two) = expected_score(&player_one, &player_two);

        assert!((exp_one * 100.0 - 50.0).abs() < f64::EPSILON);
        assert!((exp_two * 100.0 - 50.0).abs() < f64::EPSILON);

        let player_three = Glicko2Rating {
            rating: 2000.0,
            deviation: 50.0,
            volatility: 0.06,
        };

        let player_four = Glicko2Rating {
            rating: 1780.0,
            deviation: 150.0,
            volatility: 0.06,
        };

        let (exp_three, exp_four) = expected_score(&player_three, &player_four);

        assert!(((exp_three * 100.0).round() - 76.0).abs() < f64::EPSILON);
        assert!(((exp_four * 100.0).round() - 24.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_decay() {
        let player_one = Glicko2Rating {
            rating: 1500.0,
            deviation: 350.0,
            volatility: 0.06,
        };

        let player_two = Glicko2Rating {
            rating: 1250.0,
            deviation: 95.0,
            volatility: 0.06,
        };

        let player_three = Glicko2Rating {
            rating: 2250.0,
            deviation: 35.0,
            volatility: 0.059_998,
        };

        let player_one_decayed = decay_deviation(&player_one);
        let player_one_decayed_2 = decay_deviation(&player_one_decayed);

        let player_two_decayed = decay_deviation(&player_two);

        let player_three_decayed = decay_deviation(&player_three);
        let player_three_decayed_2 = decay_deviation(&player_three_decayed);

        assert!((player_one_decayed.deviation.round() - 350.0).abs() < f64::EPSILON);
        assert!((player_one_decayed_2.deviation.round() - 350.0).abs() < f64::EPSILON);
        assert!((player_two_decayed.deviation.round() - 96.0).abs() < f64::EPSILON);
        assert!((player_three_decayed.deviation.round() - 37.0).abs() < f64::EPSILON);
        assert!((player_three_decayed_2.deviation.round() - 38.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_single_rp() {
        let player = Glicko2Rating {
            rating: 1200.0,
            deviation: 25.0,
            volatility: 0.05999,
        };
        let opponent = Glicko2Rating {
            rating: 1500.0,
            deviation: 34.0,
            volatility: 0.05923,
        };

        let config = Glicko2Config::new();

        let (np, _) = glicko2(&player, &opponent, &Outcomes::WIN, &config);

        let rp = glicko2_rating_period(&player, &vec![(opponent, Outcomes::WIN)], &config);

        assert_eq!(rp, np);
    }

    #[test]
    fn test_confidence_interval() {
        let player = Glicko2Rating {
            rating: 1500.0,
            deviation: 30.0,
            volatility: 0.06,
        };

        let ci = confidence_interval(&player);

        assert!((ci.0.round() - 1441.0).abs() < f64::EPSILON);
        assert!((ci.1.round() - 1559.0).abs() < f64::EPSILON);
    }

    #[test]
    fn negative_tau() {
        let mut player = Glicko2Rating {
            rating: 2250.0,
            deviation: 3100.0,
            volatility: 0.07,
        };

        let mut opponent = Glicko2Rating {
            rating: 2250.0,
            deviation: 41.0,
            volatility: 0.1,
        };

        let config = Glicko2Config {
            tau: -10.0,
            convergence_tolerance: 0.000_001,
        };

        (player, opponent) = glicko2(&player, &opponent, &Outcomes::WIN, &config);

        assert!((player.rating.round() - 2596.0).abs() < f64::EPSILON);
        assert!((opponent.rating.round() - 2249.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_lose_streak() {
        let mut player = Glicko2Rating::new();

        let mut opponent = Glicko2Rating::default();

        for _ in 0..6 {
            (player, opponent) =
                glicko2(&player, &opponent, &Outcomes::LOSS, &Glicko2Config::new());
        }

        (player, opponent) = glicko2(&player, &opponent, &Outcomes::WIN, &Glicko2Config::new());

        assert!((player.rating.round() - 1397.0).abs() < f64::EPSILON);
        assert!((player.deviation.round() - 212.0).abs() < f64::EPSILON);
        assert!(((player.volatility * 1_000_000.0).round() - 60_004.0).abs() < f64::EPSILON);

        assert!((opponent.rating.round() - 1603.0).abs() < f64::EPSILON);
        assert!((opponent.deviation.round() - 212.0).abs() < f64::EPSILON);
        assert!(((opponent.volatility * 1_000_000.0).round() - 60_004.0).abs() < f64::EPSILON);

        let mut player = Glicko2Rating::new();

        let mut opponent = Glicko2Rating::new();

        for _ in 0..25 {
            (player, opponent) =
                glicko2(&player, &opponent, &Outcomes::LOSS, &Glicko2Config::new());
        }

        (player, opponent) = glicko2(&player, &opponent, &Outcomes::WIN, &Glicko2Config::new());

        assert!((player.rating.round() - 1248.0).abs() < f64::EPSILON);
        assert!((player.deviation.round() - 176.0).abs() < f64::EPSILON);
        assert!(((player.volatility * 1_000_000.0).round() - 60_001.0).abs() < f64::EPSILON);

        assert!((opponent.rating.round() - 1752.0).abs() < f64::EPSILON);
        assert!((opponent.deviation.round() - 176.0).abs() < f64::EPSILON);
        assert!(((opponent.volatility * 1_000_000.0).round() - 60_001.0).abs() < f64::EPSILON);
    }

    #[test]
    fn glicko_conversion() {
        let glicko2_player = Glicko2Rating::new();

        let glicko1_player = GlickoRating::from(glicko2_player);

        assert_eq!(glicko1_player, GlickoRating::new());

        let other_glicko2_player = Glicko2Rating::from(GlickoRating {
            rating: 350.0,
            deviation: 40.0,
        });

        assert!((other_glicko2_player.rating - 350.0).abs() < f64::EPSILON);
        assert!((other_glicko2_player.volatility - 0.06).abs() < f64::EPSILON);
    }
}
