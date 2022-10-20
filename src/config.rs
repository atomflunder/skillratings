//! Contains structs to configure key variables used in the different rating algorithms.
//!
//! Not every algorithm needs a config for its calculations.

#[derive(Clone, Copy, Debug)]
/// Constants used in the Elo calculations.
pub struct EloConfig {
    /// The k-value is the maximum amount of rating change from a single match.
    /// In chess, k-values from 40 to 10 are used, with the most common being 32, 24, 16 or 10.
    /// The higher the number, the more volatile the ranking.  
    /// Here the default is 32.
    pub k: f64,
}

impl EloConfig {
    #[must_use]
    /// Initialize a new `EloConfig` with a k value of `32.0`.
    pub const fn new() -> Self {
        Self { k: 32.0 }
    }
}

impl Default for EloConfig {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Copy, Debug)]
/// Constants used in the Glicko calculations.
pub struct GlickoConfig {
    /// The c value describes how much the rating deviation should decay in each step.
    /// The higher the value, the more the rating deviation will decay.  
    /// In [the paper](http://www.glicko.net/glicko/glicko.pdf) a value of
    /// `63.2` seems to be a suggested value, so that is the default here.
    pub c: f64,
}

impl GlickoConfig {
    #[must_use]
    /// Initialize a new `GlickoConfig` with a c value of `63.2`
    pub const fn new() -> Self {
        Self { c: 63.2 }
    }
}

impl Default for GlickoConfig {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Copy, Debug)]
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
    /// Initialize a new `Glicko2Config` with a tau value of `0.5` and a convergence tolerance of `0.000_001`.
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

#[derive(Clone, Copy, Debug)]
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
    pub default_dynamics: f64,
}

impl TrueSkillConfig {
    #[must_use]
    /// Initialize a new `TrueSkillConfig` with a draw probability of `0.1`,
    /// a beta value of `(25 / 3) * 0.5 ≈ 4.167` and a default dynamics value of 25 / 300 ≈ `0.0833`.
    pub fn new() -> Self {
        Self {
            draw_probability: 0.1,
            beta: (25.0 / 3.0) * 0.5,
            default_dynamics: 25.0 / 300.0,
        }
    }
}

impl Default for TrueSkillConfig {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Copy, Debug)]
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
    /// Initialize a new `WengLinConfig` with a beta value of 25 / 6 ≈ `4.167`
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

#[derive(Clone, Copy, Debug)]
/// Constants used in the Sticko calculations.
///
/// If the `h`, `beta`, `lambda` and `gamma` parameters are set to `0.0`,
/// this will behave exactly like the [`Glicko`](crate::glicko::glicko) calculations.
pub struct StickoConfig {
    /// Controls player deviations across time.  
    /// The higher this number, the higher the deviation is going to be.  
    /// By default set to `10.0`.  
    /// If you want to mimic the [`GlickoConfig`], set this to `0.0`.
    /// Do not set this to a negative value.
    pub h: f64,
    /// A bonus parameter, which gives a rating boost for just participating.  
    /// Note that setting this to a positive number will create rating inflation over time.  
    /// By default set to `0.0`.  
    /// If you want to mimic the [`GlickoConfig`], set this to `0.0`.
    /// Do not set this to a negative value.
    pub beta: f64,
    /// The neighborhood parameter, which shrinks player ratings towards their opponent.  
    /// By default set to `2.0`.  
    /// If you want to mimic the [`GlickoConfig`], set this to `0.0`.
    /// Do not set this to a negative value.
    pub lambda: f64,
    /// The advantage parameter of the first player.  
    /// If your game is biased towards player one set this to a positive number,
    /// or set this to a negative number if the second player has an advantage.  
    /// With this you could represent the advantage of playing white in chess,
    /// or home-team advantage in sports like football and so on.  
    /// In chess, a value of `30.0` seems to be about correct.  
    /// By default set to `0.0`.  
    /// If you want to mimic the [`GlickoConfig`], set this to `0.0`.
    pub gamma: f64,
    /// The c value describes how much the rating deviation should decay in each step.
    /// The higher the value, the more the rating deviation will decay.  
    /// This is similar to the c value in [`GlickoConfig`].
    /// Keep in mind this needs to be set lower than the c in the [`GlickoConfig`] if the h value here is not equal to zero.  
    /// By default set to `10.0`.
    /// If you want to mimic the [`GlickoConfig`] set this to `63.2`.
    pub c: f64,
}

impl StickoConfig {
    #[must_use]
    /// Initialize a new `StickoConfig` with a h value of `10.0`, a beta value of `0.0`,
    /// a lambda value of `2.0` and a gamma value of `0.0`.
    pub const fn new() -> Self {
        Self {
            h: 10.0,
            beta: 0.0,
            lambda: 2.0,
            gamma: 0.0,
            c: 10.0,
        }
    }
}

impl Default for StickoConfig {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Copy, Debug)]
/// Constants used in the EGF Calculations.
pub struct EGFConfig {
    /// The [handicap](https://en.wikipedia.org/wiki/Handicapping_in_Go), of the perspective of player one.  
    /// As a general rule, one handicap point is about equal to a 100 rating point difference.  
    ///
    /// If player one has a handicap in the game,
    /// you can set this number to the amount of handicap stones given to the opponent.  
    /// If player two is the one with the handicap, set this number to the negative amount of stones given.  
    /// If an equal game is played, this value should be 0.0.  
    /// For example, if player two has a handicap of 4 points (player one starts with 4 stones), set this number to -4.0.  
    ///
    /// The maximum number should not exceed 9.0 or -9.0.  
    /// By default set to 0.0.
    pub handicap: f64,
}

impl EGFConfig {
    #[must_use]
    /// Initializes a new `EGFConfig` with a handicap value of `0.0`.
    pub const fn new() -> Self {
        Self { handicap: 0.0 }
    }
}

impl Default for EGFConfig {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Copy, Debug)]
/// Constants used in the Glicko-Boost calculations.
///
/// If the `eta` parameter is set to `0.0`,
/// this will behave exactly like the [`Glicko`](crate::glicko::glicko) calculations.
pub struct GlickoBoostConfig {
    /// The advantage parameter of the first player.  
    /// If your game is biased towards player one set this to a positive number,
    /// or set this to a negative number if the second player has an advantage.  
    /// With this you could represent the advantage of playing white in chess,
    /// or home-team advantage in sports like football and so on.  
    /// In chess, a value of `30.0` seems to be about correct.  
    /// By default set to `0.0`.  
    /// If you want to mimic the [`GlickoConfig`], set this to `0.0`.
    pub eta: f64,
    /// The "exceptional performance" threshold.  
    /// For outstanding performances, the rating deviation of the player will get boosted by the b values.
    /// By default set to `1.96`, which is approximately equal to 2.5% of performances.  
    /// The higher this value, the harder it is to reach the threshold.  
    /// If you want to mimic the [`GlickoConfig`], set this to `0.0`.
    pub k: f64,
    /// The rating deviation boost factors. A tuple of 2 [`f64`]s.
    /// The first value is multiplicative, the second additive.  
    /// By default set to 0.20139 and 17.5.  
    /// If k is set to 0, these will do nothing.  
    /// If you want to mimic the [`GlickoConfig`], set both of these to `0.0`.
    pub b: (f64, f64),
    /// The rating deviation increase factors. A tuple of 5 [`f64`]s.
    /// These values regulate the rating deviation increase of player's who have not played in a rating period.  
    /// By default set to 5.83733, -1.75374e-04, -7.080124e-05, 0.001733792, and 0.00026706.
    pub alpha: (f64, f64, f64, f64, f64),
}

impl GlickoBoostConfig {
    #[must_use]
    /// Initialize a new `GlickoBoostConfig` with a eta value of 30.0, a k value of 1.96,
    /// b values of 0.20139 and 17.5, and alpha values of 5.83733, -1.75374e-04, -7.080124e-05, 0.001733792, 0.00026706.
    pub const fn new() -> Self {
        Self {
            eta: 30.0,
            k: 1.96,
            b: (0.20139, 17.5),
            alpha: (
                5.837_33,
                -1.753_74e-04,
                -7.080_124e-05,
                0.001_733_792,
                0.000_267_06,
            ),
        }
    }
}

impl Default for GlickoBoostConfig {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Copy, Debug)]
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
    /// Initialize a new `USCFConfig` with a t value of 14.0.
    pub const fn new() -> Self {
        Self { t: 14.0 }
    }
}

impl Default for USCFConfig {
    fn default() -> Self {
        Self::new()
    }
}
