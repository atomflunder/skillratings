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
//! - **[`Sticko`](crate::sticko)**
//! - **[`Glicko-Boost`](crate::glicko_boost)**
//! - **[`EGF (European Go Federation)`](crate::egf)**
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
//! ```bash
//! cargo add skillratings
//! ```
//!
//! Alternatively, you can add the following to your `Cargo.toml` file manually:
//!
//! ```toml
//! [dependencies]
//! skillratings = "0.16"
//! ```
//!
//! # Examples
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
//!     glicko2::glicko2, outcomes::Outcomes, rating::Glicko2Rating, config::Glicko2Config
//! };
//!
//! // Initialise a new player rating.
//! // The default values are: 1500.0, 350.0, and 0.06.
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
//!     trueskill::trueskill_teams,
//!     outcomes::Outcomes,
//!     rating::TrueSkillRating,
//!     config::TrueSkillConfig,
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
//! use skillratings::{glicko::expected_score, rating::GlickoRating};
//!
//! // Initialise a new player rating.
//! // The default values are: 1500.0, and 350.0.
//! let player_one = GlickoRating::new();
//!
//! // Initialising a new rating with custom numbers.
//! let player_two = GlickoRating{
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
//!     elo::elo_rating_period, outcomes::Outcomes, rating::EloRating, config::EloConfig
//! };
//!
//! // We initialise a new Elo Rating here.
//! let player = EloRating {
//!     rating: 1402.1,
//! };
//!
//! // We need a list of results to pass to the elo_rating_period function.
//! let mut results = Vec::new();
//!
//! // And then we populate the list with tuples containing the opponent,
//! // and the outcome of the match from our perspective.
//! results.push((EloRating::new(), Outcomes::WIN));
//! results.push((EloRating {rating: 954.0}, Outcomes::DRAW));
//! results.push((EloRating::new(), Outcomes::LOSS));
//!
//! // The elo_rating_period function calculates the new rating for the player and returns it.
//! let new_player = elo_rating_period(&player, &results, &EloConfig::new());
//!
//! // The rating of the player decreased by around ~40 points.
//! assert_eq!(new_player.rating.round(), 1362.0);
//! ```

pub mod config;
pub mod dwz;
pub mod egf;
pub mod elo;
pub mod glicko;
pub mod glicko2;
pub mod glicko_boost;
pub mod ingo;
pub mod outcomes;
pub mod rating;
pub mod sticko;
pub mod trueskill;
pub mod weng_lin;
