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

/// Contains structs to configure key variables used in the different rating algorithms.
pub mod config;

/// Calculate a player's skill rating using the `Elo` algorithm.
pub mod elo;

/// Calculate a player's skill rating using the `DWZ (Deutsche Wertungszahl)` algorithm.
pub mod dwz;

/// Calculate a player's skill rating using the `Ingo` algorithm.
pub mod ingo;

/// Calculate a player's skill rating using the `Glicko` algorithm.
pub mod glicko;

/// Calculate a player's skill rating using the `Glicko-2` algorithm.
pub mod glicko2;

/// Calculate a player's skill rating using the `TrueSkill` algorithm.
pub mod trueskill;

/// Contains possible outcomes of a match.
pub mod outcomes;

/// Contains structs to initialize player's ratings in the format of the different rating algorithms used.
pub mod rating;
