/// The elo rating of a player.
///
/// The default rating is 1000.0.
#[derive(Copy, Clone, Debug)]
pub struct EloRating {
    /// The player's Elo rating number, by default 1000.0.
    pub rating: f64,
}

impl EloRating {
    /// Initialise a new `EloRating` with a rating of 1000.0.
    #[must_use]
    pub const fn new() -> Self {
        Self { rating: 1000.0 }
    }
}

impl Default for EloRating {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
/// The glicko rating for a player. **For the glicko-2 rating, please see [`Glicko2Rating`]**.
///
/// The default rating is 1500.0.  
/// The default deviation is 350.0.
pub struct GlickoRating {
    /// The player's Glicko rating number, by default 1500.0.
    pub rating: f64,
    /// The player's Glicko deviation number, by default 350.0.
    pub deviation: f64,
}

impl GlickoRating {
    #[must_use]
    /// Initialise a new `GlickoRating` with a rating of 1500.0 and a deviation of 350.0.
    pub const fn new() -> Self {
        Self {
            rating: 1500.0,
            deviation: 350.0,
        }
    }
}

impl Default for GlickoRating {
    fn default() -> Self {
        Self::new()
    }
}

/// The glicko-2 rating of a player.
///
/// The default rating is 1500.0.  
/// The default deviation is 350.0.  
/// The default volatility is 0.06.
#[derive(Copy, Clone, Debug, PartialEq)]
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

#[derive(Copy, Clone, Debug, PartialEq)]
/// The DWZ (Deutsche Wertungszahl) rating for a player.
///
/// Note there is no default value for this,
/// you either have to convert an existing [`EloRating`]
/// using `DWZRating::from(EloRating { ... })`,
/// or use the [`crate::dwz::get_first_dwz`] function.
///
/// The age is the actual age of the player, if unsure or unavailable set this to `>25`.  
/// Using `from` will set the age to 26.
pub struct DWZRating {
    /// The player's DWZ rating number.
    pub rating: f64,
    /// The player's DWZ index, how many "events" they have completed.
    pub index: usize,
    /// The age of the player, if uncertain or unavailable set this to `>25`.
    pub age: usize,
}

impl From<EloRating> for DWZRating {
    fn from(e: EloRating) -> Self {
        Self {
            rating: e.rating,
            index: 6,
            age: 26,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
/// The `TrueSkill` rating of a player.
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
    /// Initialise a new `TrueSkillRating` with a rating of 25.0, and an uncertainty of 25/3 ≈ 8.33.
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
