/// The elo rating of a player.
///
/// The default rating is 1000.0.
#[derive(Copy, Clone, Debug)]
pub struct EloRating {
    /// The player's Elo rating number, by default 1000.0.
    pub rating: f64,
}

impl EloRating {
    /// Initialise a new EloRating with a rating of 1000.0.
    pub fn new() -> Self {
        EloRating { rating: 1000.0 }
    }
}

impl Default for EloRating {
    fn default() -> Self {
        EloRating::new()
    }
}

/// The glicko-2 rating of a player.
///
/// The default rating is 1500.0.  
/// The default deviation is 350.0.  
/// The default volatility is 0.06.
#[derive(Copy, Clone, Debug)]
pub struct GlickoRating {
    /// The player's Glicko-2 rating number, by default 1500.0.
    pub rating: f64,
    /// The player's Glicko-2 deviation number, by default 350.0.
    pub deviation: f64,
    /// The player's Glicko-2 volatility number, by default 0.06.
    pub volatility: f64,
}

impl GlickoRating {
    /// Initialise a new GlickoRating with a rating of 1500.0, a deviation of 350.0 and a volatility of 0.06.
    pub fn new() -> Self {
        GlickoRating {
            rating: 1500.0,
            deviation: 350.0,
            volatility: 0.06,
        }
    }
}

impl Default for GlickoRating {
    fn default() -> Self {
        Self::new()
    }
}
