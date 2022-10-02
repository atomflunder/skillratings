#![warn(missing_docs)]
#![warn(
    clippy::pedantic,
    clippy::nursery,
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::as_conversions
)]
#![allow(clippy::module_name_repetitions, clippy::doc_markdown, clippy::ptr_arg)]

//! Skillratings provides functions on calculating a player's skill rating.
//!
//! Currently we support these skill rating systems:  
//! - **[`Elo`](crate::elo)**
//! - **[`Glicko`](crate::glicko)**
//! - **[`Glicko-2`](crate::glicko2)**
//! - **[`TrueSkill`](crate::trueskill)**
//! - **[`Weng-Lin`](crate::weng_lin)**
//! - **[`DWZ (Deutsche Wertungszahl)`](crate::dwz)**
//! - **[`Ingo`](crate::ingo)**
//!
//! You can use this crate to calculate results for two players instantly,
//! or for one player in a rating period with the algorithms mentioned above.
//!
//! Head over to the modules above or below for more information about the specific rating algorithms, their advantages and disadvantages.
//!
//! # Installation
//!
//! If you are on Rust 1.62 or higher use `cargo add` to install the latest version:
//!
//! ```
//! cargo add skillratings
//! ```
//!
//! Alternatively, you can add the following to your `Cargo.toml` file manually:
//!
//! ```toml
//! [dependencies]
//! skillratings = "0.13"
//! ```
//!
//! # Quickstart
//!
//! This is a very quick example of how to use this crate based on the *Glicko-2* rating system.  
//! For a quickstart on the other rating systems and for more advanced usage of those,
//! such as rating team-based games, or rating periods please check out the modules that are linked above.
//!
//! ```
//! use skillratings::{
//!     glicko2::glicko2, outcomes::Outcomes, rating::Glicko2Rating, config::Glicko2Config
//! };
//!
//! // Initialise a new player rating
//! let player_one = Glicko2Rating::new();
//!
//! // Or you can initialise it with your own values of course.
//! // Imagine these numbers being pulled from a database.
//! let (some_rating, some_deviation, some_volatility) = (1325.0, 230.0, 0.05932);
//! let player_two = Glicko2Rating{
//!     rating: some_rating,
//!     deviation: some_deviation,
//!     volatility: some_volatility,
//! };
//!
//! // The outcome of the match is from the perspective of player one.
//! let outcome = Outcomes::WIN;
//!
//! // The config allows you to specify certain values in the Glicko-2 calculation.
//! let config = Glicko2Config::new();
//!
//! // The glicko2 function will calculate the new ratings for both players and return them.
//! let (new_player_one, new_player_two) = glicko2(&player_one, &player_two, &outcome, &config);
//! ```

pub mod config;
pub mod dwz;
pub mod elo;
pub mod glicko;
pub mod glicko2;
pub mod ingo;
pub mod outcomes;
pub mod rating;
pub mod trueskill;
pub mod weng_lin;
