#![warn(
    missing_docs,
    clippy::pedantic,
    clippy::nursery,
    clippy::unwrap_used,
    clippy::expect_used
)]
#![allow(
    // This is turned off because of the rating values in the structs
    clippy::module_name_repetitions,
    // "TrueSkill" shows up as a false positive otherwise
    clippy::doc_markdown,
    // Need to cast usizes to f64s where precision is not that important, also there seems to be no good alternative.
    clippy::cast_precision_loss,
)]

//! Skillratings provides a collection of well-known (and lesser known) skill rating algorithms, that allow you to assess a player's skill level instantly.  
//! You can easily calculate skill ratings instantly in 1vs1 matches, Team vs Team matches, or in tournaments / rating periods.  
//! This library is incredibly lightweight (no dependencies by default), user-friendly, and of course, *blazingly fast*.  
//!
//! Currently we support these skill rating systems:  
//! - **[`Elo`](crate::elo)**
//! - **[`Glicko`](crate::glicko)**
//! - **[`Glicko-2`](crate::glicko2)**
//! - **[`TrueSkill`](crate::trueskill)**
//! - **[`Weng-Lin`](crate::weng_lin)**
//! - **[`FIFA Men's World Ranking`](crate::fifa)**
//! - **[`Sticko`](crate::sticko)**
//! - **[`Glicko-Boost`](crate::glicko_boost)**
//! - **[`USCF (US Chess Federation)`](crate::uscf)**
//! - **[`EGF (European Go Federation)`](crate::egf)**
//! - **[`DWZ (Deutsche Wertungszahl)`](crate::dwz)**
//! - **[`Ingo`](crate::ingo)**
//!
//! Most of these are known from their usage in chess and various other games.  
//! Click on the documentation for the modules linked above for more information about the specific rating algorithms, and their advantages and disadvantages.
//!
//! # Installation
//!
//! If you are on Rust 1.62 or higher use `cargo add` to install the latest version:
//!
//! ```bash
//! cargo add skillratings
//! ```
//!
//! Alternatively, you can add the following to your `Cargo.toml` file manually:
//!
//! ```toml
//! [dependencies]
//! skillratings = "0.21"
//! ```
//!
//! ## Serde support
//!
//! Serde support is gated behind the `serde` feature. You can enable it like so:
//!
//! Using `cargo add`:
//!
//! ```bash
//! cargo add skillratings --features serde
//! ```
//!
//! By editing `Cargo.toml` manually:
//!
//! ```toml
//! [dependencies]
//! skillratings = {version = "0.21", features = ["serde"]}
//! ```
//!
//! # Usage and Examples
//!
//! Below you can find some basic examples of the use cases of this crate.  
//! There are many more rating algorithms available with lots of useful functions that are not covered here.  
//! For more information head over to the modules linked above or below.
//!
//! ### Player-vs-Player
//!
//! Every rating algorithm included here can be used to rate 1v1 games.  
//! We use *Glicko-2* in this example here.
//!
//! ```rust
//! use skillratings::{
//!     glicko2::{glicko2, Glicko2Config, Glicko2Rating},
//!     Outcomes,
//! };
//!
//! // Initialise a new player rating.
//! // The default values are: 1500.0, 350.0, and 0.06.
//! let player_one = Glicko2Rating::new();
//!
//! // Or you can initialise it with your own values of course.
//! // Imagine these numbers being pulled from a database.
//! let (some_rating, some_deviation, some_volatility) = (1325.0, 230.0, 0.05932);
//! let player_two = Glicko2Rating {
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
//!
//! // The first players rating increased by ~112 points.
//! assert_eq!(new_player_one.rating.round(), 1612.0);
//! ```
//!
//! ### Team-vs-Team
//!
//! Some algorithms like TrueSkill or Weng-Lin allow you to rate team-based games as well.  
//! This example shows a 3v3 game using *TrueSkill*.
//!
//! ```rust
//! use skillratings::{
//!     trueskill::{trueskill_teams, TrueSkillConfig, TrueSkillRating},
//!     Outcomes,
//! };
//!
//! // We initialise Team One as a Vec of multiple TrueSkillRatings.
//! let team_one = vec![
//!     TrueSkillRating {
//!         rating: 33.3,
//!         uncertainty: 3.3,
//!     },
//!     TrueSkillRating {
//!         rating: 25.1,
//!         uncertainty: 1.2,
//!     },
//!     TrueSkillRating {
//!         rating: 43.2,
//!         uncertainty: 2.0,
//!     },
//! ];
//!
//! // Team Two will be made up of 3 new players, for simplicity.
//! // Note that teams do not necessarily have to be the same size.
//! let team_two = vec![
//!     TrueSkillRating::new(),
//!     TrueSkillRating::new(),
//!     TrueSkillRating::new(),
//! ];
//!
//! // The outcome of the match is from the perspective of team one.
//! let outcome = Outcomes::LOSS;
//!
//! // The config allows you to specify certain values in the TrueSkill calculation.
//! let config = TrueSkillConfig::new();
//!
//! // The trueskill_teams function will calculate the new ratings for both teams and return them.
//! let (new_team_one, new_team_two) = trueskill_teams(&team_one, &team_two, &outcome, &config);
//!
//! // The rating of the first player on team one decreased by around ~1.2 points.
//! assert_eq!(new_team_one[0].rating.round(), 32.0);
//! ```
//!
//! ### Expected outcome
//!
//! Every rating algorithm has an `expected_score` function that you can use to predict the outcome of a game.  
//! This example is using *Glicko* (*not Glicko-2!*) to demonstrate.
//!
//! ```rust
//! use skillratings::glicko::{expected_score, GlickoRating};
//!
//! // Initialise a new player rating.
//! // The default values are: 1500.0, and 350.0.
//! let player_one = GlickoRating::new();
//!
//! // Initialising a new rating with custom numbers.
//! let player_two = GlickoRating {
//!     rating: 1812.0,
//!     deviation: 195.0,
//! };
//!
//! // The expected_score function will return two floats between 0 and 1 for each player.
//! // A value of 1 means guaranteed victory, 0 means certain loss.
//! // Values near 0.5 mean draws are likely to occur.
//! let (exp_one, exp_two) = expected_score(&player_one, &player_two);
//!
//! // The expected score for player one is ~0.25.
//! // If these players would play 100 games, player one is expected to score around 25 points.
//! // (Win = 1 point, Draw = 0.5, Loss = 0)
//! assert_eq!((exp_one * 100.0).round(), 25.0);
//! ```
//!
//! ### Rating period
//!
//! Every rating algorithm included here has a `..._rating_period` that allows you to calculate a player's new rating using a list of results.  
//! This can be useful in tournaments, or if you only update ratings at the end of a certain rating period, as the name suggests.  
//! We are using the *Elo* rating algorithm in this example.
//!
//! ```rust
//! use skillratings::{
//!     elo::{elo_rating_period, EloConfig, EloRating},
//!     Outcomes,
//! };
//!
//! // We initialise a new Elo Rating here.
//! let player = EloRating { rating: 1402.1 };
//!
//! // We need a list of results to pass to the elo_rating_period function.
//! let mut results = Vec::new();
//!
//! // And then we populate the list with tuples containing the opponent,
//! // and the outcome of the match from our perspective.
//! results.push((EloRating::new(), Outcomes::WIN));
//! results.push((EloRating { rating: 954.0 }, Outcomes::DRAW));
//! results.push((EloRating::new(), Outcomes::LOSS));
//!
//! // The elo_rating_period function calculates the new rating for the player and returns it.
//! let new_player = elo_rating_period(&player, &results, &EloConfig::new());
//!
//! // The rating of the player decreased by around ~40 points.
//! assert_eq!(new_player.rating.round(), 1362.0);
//! ```

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

pub mod dwz;
pub mod egf;
pub mod elo;
pub mod fifa;
pub mod glicko;
pub mod glicko2;
pub mod glicko_boost;
pub mod ingo;
pub mod sticko;
pub mod trueskill;
pub mod uscf;
pub mod weng_lin;

/// The possible outcomes for a match: Win, Draw, Loss.
///
/// Note that this is always from the perspective of player one.  
/// That means a win is a win for player one and a loss is a win for player two.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
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

/// Outcome for a free-for-all match or a match that involves more than two teams.
///
/// Every team is assigned a rank, depending on their placement. The lower the rank, the better.  
/// If two or more teams tie with each other, assign them the same rank.
///
/// For example: Team A takes 1st place, Team C takes 2nd place, Team B takes 3rd place,
/// and Teams D and E tie with each other and both take the 4th place.  
/// In that case you would assign Team A = 1, Team B = 3, Team C = 2, Team D = 4, and Team E = 4.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct MultiTeamOutcome(usize);

impl MultiTeamOutcome {
    #[must_use]
    #[inline]
    /// Makes a new `MultiTeamOutcome` from a given rank.
    pub const fn new(rank: usize) -> Self {
        Self(rank)
    }

    #[must_use]
    #[inline]
    /// Returns the rank that corresponds to this `MultiTeamOutcome`.
    pub const fn rank(self) -> usize {
        self.0
    }
}

impl From<usize> for MultiTeamOutcome {
    #[must_use]
    #[inline]
    fn from(v: usize) -> Self {
        Self(v)
    }
}

impl From<MultiTeamOutcome> for usize {
    #[must_use]
    #[inline]
    fn from(v: MultiTeamOutcome) -> Self {
        v.0
    }
}
