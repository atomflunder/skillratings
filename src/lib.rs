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
//! **[`Elo`](crate::elo)**,
//! **[`Glicko`](crate::glicko)**,
//! **[`Glicko-2`](crate::glicko2)**,
//! **[`TrueSkill`](crate::trueskill)**,
//! **[`Weng-Lin`](crate::weng_lin)**,
//! **[`DWZ (Deutsche Wertungszahl)`](crate::dwz)**,
//! and **[`Ingo`](crate::ingo)**.
//!
//! You can use this crate to calculate results for two players instantly,
//! or for one player in a rating period with the algorithms mentioned above.
//!
//! Head over to the modules above or below for more information about the specific rating algorithms, their advantages and disadvantages.
//!
//! # Installation
//!
//! Add the following to your `Cargo.toml` file:  
//! ```toml
//! [dependencies]
//! skillratings = "0.12.0"
//! ```
//!
//! # Examples and Usage
//!
//! Check out the `Examples` section for every function contained in the modules to see how they can be used.

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
