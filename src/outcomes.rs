/// The possible outcomes for a match: Win, Draw, Loss.
/// Note that this is always from the perspective of player one.
/// So a win is a win for player one and a loss is a win for player two.

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Outcomes {
    /// A win, from player_one's perspective.
    WIN,
    /// A loss, from player_one's perspective.
    LOSS,
    /// A draw.
    DRAW,
}
