//! Structs for initializing a player's rating for the different rating algorithms used.

/// The Elo rating of a player.
///
/// The default rating is 1000.0.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct EloRating {
    /// The player's Elo rating number, by default 1000.0.
    pub rating: f64,
}

impl EloRating {
    /// Initialize a new `EloRating` with a rating of 1000.0.
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

impl From<IngoRating> for EloRating {
    fn from(i: IngoRating) -> Self {
        Self {
            rating: 2840.0 - 8.0 * i.rating,
        }
    }
}

impl From<DWZRating> for EloRating {
    fn from(d: DWZRating) -> Self {
        Self { rating: d.rating }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
/// The Glicko rating for a player.
///
/// For the Glicko-2 rating, please see [`Glicko2Rating`].
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
    /// Initialize a new `GlickoRating` with a rating of 1500.0 and a deviation of 350.0.
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

impl From<Glicko2Rating> for GlickoRating {
    fn from(g: Glicko2Rating) -> Self {
        Self {
            rating: g.rating,
            deviation: g.deviation,
        }
    }
}

impl From<StickoRating> for GlickoRating {
    fn from(s: StickoRating) -> Self {
        Self {
            rating: s.rating,
            deviation: s.deviation,
        }
    }
}

/// The Glicko-2 rating of a player.
///
/// For the Glicko rating, please see [`GlickoRating`].
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
    /// Initialize a new `Glicko2Rating` with a rating of 1500.0, a deviation of 350.0 and a volatility of 0.06.
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

impl From<StickoRating> for Glicko2Rating {
    fn from(s: StickoRating) -> Self {
        Self {
            rating: s.rating,
            deviation: s.deviation,
            ..Default::default()
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
/// The DWZ (Deutsche Wertungszahl) rating for a player.
///
/// The age is the actual age of the player, if unsure or unavailable set this to `>25`.  
/// Converting from an `EloRating` or using `DWZRating::default()` will set the age to 26.
///
/// The default rating is 1000.0.
pub struct DWZRating {
    /// The player's DWZ rating number, by default 1000.0.
    pub rating: f64,
    /// The player's DWZ index, how many "events" they have completed.
    pub index: usize,
    /// The age of the player, if uncertain or unavailable set this to `>25`.
    pub age: usize,
}

impl DWZRating {
    #[must_use]
    /// Initialize a new `DWZRating` with a rating of 1000.0, an index of 1 and the specified age.  
    /// The age is the actual age of the player, if unsure or unavailable set this to `>25`.
    pub const fn new(age: usize) -> Self {
        Self {
            rating: 1000.0,
            index: 1,
            age,
        }
    }
}

impl Default for DWZRating {
    fn default() -> Self {
        Self::new(26)
    }
}

impl From<EloRating> for DWZRating {
    fn from(e: EloRating) -> Self {
        Self {
            rating: e.rating,
            // Recommended according to Wikipedia.
            index: 6,
            ..Default::default()
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
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
    /// Initialize a new TrueSkillRating with a rating of 25.0, and an uncertainty of 25/3 ≈ 8.33.
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

impl From<WengLinRating> for TrueSkillRating {
    fn from(w: WengLinRating) -> Self {
        Self {
            rating: w.rating,
            uncertainty: w.uncertainty,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
/// The Ingo rating of a player.
///
/// Note that unlike in the other systems, a lower score is better than a higher score.  
/// Negative values are possible.
///
/// The age is the actual age of the player, if unsure or unavailable set this to `>25`.  
/// Converting from an `EloRating` or using `IngoRating::default()` will set the age to 26.
///
/// The default rating is 230.0.
pub struct IngoRating {
    /// The rating value for a player, by default 230.0.
    /// Note that a lower rating is more desirable.
    pub rating: f64,
    /// The age of the player, if uncertain or unavailable set this to `>25`.
    pub age: usize,
}

impl IngoRating {
    #[must_use]
    /// Initialize a new `IngoRating` with a rating of 230.0 and the given age.  
    /// The age is the actual age of the player, if unsure or unavailable set this to `>25`.
    pub const fn new(age: usize) -> Self {
        Self { rating: 230.0, age }
    }
}

impl Default for IngoRating {
    fn default() -> Self {
        Self::new(26)
    }
}

impl From<EloRating> for IngoRating {
    fn from(e: EloRating) -> Self {
        Self {
            rating: 355.0 - (e.rating / 8.0),
            ..Default::default()
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
/// The Weng-Lin rating of a player.
///
/// Similar to [`TrueSkillRating`].
///
/// The default rating is 25.0.  
/// The default uncertainty is 25/3 ≈ 8.33.
pub struct WengLinRating {
    /// The rating value (mu) of the WengLinRating, by default 25.0.
    pub rating: f64,
    /// The uncertainty value (sigma) of the WengLinRating, by default 25/3 ≈ 8.33.
    pub uncertainty: f64,
}

impl WengLinRating {
    #[must_use]
    /// Initialize a new WengLinRating with a rating of 25.0, and an uncertainty of 25/3 ≈ 8.33.
    pub fn new() -> Self {
        Self {
            rating: 25.0,
            uncertainty: 25.0 / 3.0,
        }
    }
}

impl Default for WengLinRating {
    fn default() -> Self {
        Self::new()
    }
}

impl From<TrueSkillRating> for WengLinRating {
    fn from(t: TrueSkillRating) -> Self {
        Self {
            rating: t.rating,
            uncertainty: t.uncertainty,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
/// The Sticko rating of a player.
///
/// Similar to [`GlickoRating`].
///
/// The default rating is 1500.0.
/// The default deviation is 350.0.
pub struct StickoRating {
    /// The player's Sticko rating number, by default 1500.0.
    pub rating: f64,
    /// The player's Sticko deviation number, by default 350.0.
    pub deviation: f64,
}

impl StickoRating {
    #[must_use]
    /// Initialize a new `GlickoRating` with a rating of 1500.0 and a deviation of 350.0.
    pub const fn new() -> Self {
        Self {
            rating: 1500.0,
            deviation: 350.0,
        }
    }
}

impl Default for StickoRating {
    fn default() -> Self {
        Self::new()
    }
}

impl From<GlickoRating> for StickoRating {
    fn from(g: GlickoRating) -> Self {
        Self {
            rating: g.rating,
            deviation: g.deviation,
        }
    }
}

impl From<Glicko2Rating> for StickoRating {
    fn from(g: Glicko2Rating) -> Self {
        Self {
            rating: g.rating,
            deviation: g.deviation,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
/// The EGF (European Go Federation) Rating for a player.
///
/// If the player has a Go rank or similar,
/// you can set the rating value manually approximately according to
/// [this inofficial comparison table](https://forums.online-go.com/t/go-ranks-vs-chess-ratings/41594/42).  
/// Keep in mind that here, the lowest possible rating is -900.0.
///
/// The default rating is 0.0.
pub struct EGFRating {
    /// The player's EGF rating number, by default 0.0.
    pub rating: f64,
}

impl EGFRating {
    #[must_use]
    /// Initialize a new `EGFRating` with a rating of 0.0.
    pub const fn new() -> Self {
        Self { rating: 0.0 }
    }
}

impl Default for EGFRating {
    fn default() -> Self {
        Self::new()
    }
}
