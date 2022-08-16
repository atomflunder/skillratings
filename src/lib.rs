#![warn(missing_docs)]
#![warn(
    clippy::pedantic,
    clippy::nursery,
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::as_conversions
)]
#![allow(clippy::module_name_repetitions)]

//! skillratings provides functions on calculating a player's skill rating in 1v1 games.  
//!
//! We calculate the result instantly instead of after a certain rating period, and only for player-vs-player matches, no teams.
//!
//! Currently we support these skill rating systems:
//! [`Elo`](https://en.wikipedia.org/wiki/Elo_rating_system),
//! [`DWZ`](https://en.wikipedia.org/wiki/Deutsche_Wertungszahl),
//! [`Ingo`](https://de.wikipedia.org/wiki/Ingo-Zahl),
//! [`TrueSkill`](https://en.wikipedia.org/wiki/TrueSkill),
//! [`Glicko`](https://en.wikipedia.org/wiki/Glicko_rating_system)
//! and [`Glicko-2`](https://en.wikipedia.org/wiki/Glicko-2).

/// Module for calculating a player's skill rating using `Elo`.
pub mod elo;

/// Module for calculating a player's skill rating using `DWZ (Deutsche Wertungszahl)`.
pub mod dwz;

/// Module for calculating a player's skill rating using `Ingo`.
pub mod ingo;

/// Module for calculating a player's skill rating using `Glicko`.
pub mod glicko;

/// Module for calculating a player's skill rating using `Glicko-2`.
pub mod glicko2;

/// Module for calculating a player's skill rating using `TrueSkill`.
pub mod trueskill;

/// Module for the outcome of the matches.
pub mod outcomes;

/// Module for initialising a player's skill rating in the different rating methods.
pub mod rating;
