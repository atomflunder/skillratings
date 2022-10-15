//! The possible outcomes of a match.

/// The possible outcomes for a match: Win, Draw, Loss.
///
/// Note that this is always from the perspective of player one.  
/// That means a win is a win for player one and a loss is a win for player two.

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Outcomes {
    /// A win, from player_one's perspective.
    WIN,
    /// A loss, from player_one's perspective.
    LOSS,
    /// A draw.
    DRAW,
}

impl Outcomes {
    #[must_use]
    /// Converts the outcome of the match into the points used in chess (1 = Win, 0.5 = Draw, 0 = Loss).
    ///
    /// Used internally in several rating algorithms, but some, like TrueSkill, have their own conversion.
    pub const fn to_chess_points(self) -> f64 {
        // Could set the visibility to crate level, but maybe someone has a use for it, who knows.
        match self {
            Self::WIN => 1.0,
            Self::DRAW => 0.5,
            Self::LOSS => 0.0,
        }
    }
}
