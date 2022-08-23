#![warn(missing_docs)]
#![warn(
    clippy::pedantic,
    clippy::nursery,
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::as_conversions
)]
#![allow(clippy::module_name_repetitions)]

//! Skillratings provides functions on calculating a player's skill rating in 1v1 games.
//!
//! Currently we support these skill rating systems:
//! [`Elo`](https://en.wikipedia.org/wiki/Elo_rating_system),
//! [`DWZ`](https://en.wikipedia.org/wiki/Deutsche_Wertungszahl),
//! [`Ingo`](https://de.wikipedia.org/wiki/Ingo-Zahl),
//! [`TrueSkill`](https://en.wikipedia.org/wiki/TrueSkill),
//! [`Glicko`](https://en.wikipedia.org/wiki/Glicko_rating_system)
//! and [`Glicko-2`](https://en.wikipedia.org/wiki/Glicko-2).
//!
//! You can use this crate to calculate results for two players instantly,
//! or for one player in a rating period with the algorithms mentioned above.
//!
//! Read the linked wikipedia articles for more information about them and their advantages and disadvantages.

/// Contains structs to configure key variables used in the different rating algorithms.
pub mod config;

/// The `Elo` algorithm, the most used and recognised rating system and the gold-standard in standard chess.
pub mod elo;

/// The `DWZ (Deutsche Wertungszahl)` algorithm used in the german chess leagues alongside Elo.  
/// DWZ continues to be enhanced over the years, while having similar scores to Elo.
pub mod dwz;

/// The `Ingo` algorithm, the predecessor of DWZ and one of the first rating algorihms invented in 1947.  
/// Note that a lower score here is more desirable.
pub mod ingo;

/// Calculate a player's skill rating using the `Glicko` algorithm, developed by Mark Glickman as an improvement on Elo.
pub mod glicko;

/// Calculate a player's skill rating using the `Glicko-2` algorithm, an improvement on Glicko and widely used in online games,
/// like Counter Strike: Global Offensive, Team Fortress 2, Splatoon 2 and most online chess platforms.
pub mod glicko2;

/// Calculate a player's skill rating using the `TrueSkill` algorithm, developed by Microsoft for Halo 3.
/// Used in the Halo games, the Forza Games, Tom Clancy's: Rainbow Six Siege, and most Xbox Live games.  
/// Unlike the other rating algorithms, `TrueSkill` supports teams.  
/// **Caution:** `TrueSkill` is patented, so if you have a commercial project, it is recommended to use another algorihm included here.
pub mod trueskill;

/// Contains possible outcomes of a match.
pub mod outcomes;

/// Contains structs to initialize player's ratings in the format of the different rating algorithms used.
pub mod rating;
