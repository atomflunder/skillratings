#![warn(missing_docs)]

//! skillratings provides functions on calculating a player's skill rating in 1v1 games.  
//! Currently we support the two major skill rating systems:
//! [Elo](https://en.wikipedia.org/wiki/Elo_rating_system) and [Glicko-2](https://en.wikipedia.org/wiki/Glicko-2).

/// Module for calculating a player's skill rating using Elo.
pub mod elo;

/// Module for calculating a player's skill rating using Glicko-2.
pub mod glicko2;

/// Module for the outcome of the matches.
pub mod outcomes;

/// Module for initialising a player's skill rating with either Elo or Glicko-2.
pub mod rating;
