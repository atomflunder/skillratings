//! Contains structs to configure key variables used in the different rating algorithms.

/// Constants used in the Elo calculation.
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

/// Constants used in the Glicko calculation.
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

/// Constants used in the Glicko-2 calculation.
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

/// Constants used in the TrueSkill calculation.
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

/// Constants used in the Weng calculations.
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
    /// Initialize a new `WengConfig` with a beta value of 25 / 6 ≈ `4.167`
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
