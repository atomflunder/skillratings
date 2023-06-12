# Changelog

This is a broad overview of the changes that have been made over the lifespan of this library.

## v0.25.0 - 2023-06-04

- Add Rating, RatingSystem, RatingPeriodSystem, TeamRatingSystem, and MultiTeamRatingSystem traits

## v0.24.0 - 2023-01-01

- Renamed `match_quality_teams` to `match_quality_two_teams` for consistency

## v0.23.0 - 2022-12-31

- Added `expected_score_multi_team` function for `weng_lin`
- Renamed `expected_score_teams` -> `expected_score_two_teams` for both `weng_lin` and `trueskill`

## v0.22.0 - 2022-12-19

- Added `weng_lin_multi_team`, and `MultiTeamOutcome` struct
- Renamed `weng_lin_teams` -> `weng_lin_two_teams`
- Renamed `trueskill_teams` -> `trueskill_two_teams`

## v0.21.1 - 2022-12-17

- Perform Step 1 of Glicko calculation
- Perform Step 6 of Glicko-Boost calculation

## v0.21.0 - 2022-11-24

- Return a more descriptive error on `get_first_dwz` function instead of returning None

## v0.20.0 - 2022-11-06

- Added FIFA rating algorithm (Men)

## v0.19.2 - 2022-10-27

- Change every function that takes a Vec to now take a Slice

## v0.19.1 - 2022-10-26

- Add more From implementations for rating structs

## v0.19.0 - 2022-10-23

- Add optional serde support

## v0.18.0 - 2022-10-23

- Major restructuring of ratings, configs and outcomes
    - Ratings and Configs now reside in the rating algorithms files
    - Outcomes now reside in the lib.rs file

## v0.17.0 - 2022-10-21

- Added USCF rating algorithms

## v0.16.0 - 2022-10-19

- Added Glicko-Boost algorithm
- Added boolean parameter to results tuple in Sticko rating period function to indicate advantages
- Calculate Sticko rating period function like Glicko and Glicko-2

## v0.15.2 - 2022-10-16

- Calculating Glicko rating period and Glicko-2 rating period properly now

## v0.15.1 - 2022-10-15

- Increase performance and readability of some functions
- Added `to_chess_points()` function to `Outcomes` enum
- Fix slight inaccuracy (<0.01%) in Glicko-2 volatility calculations

## v0.15.0 - 2022-10-13

- Add EGF rating algorithm

## v0.14.0 - 2022-10-13

- Add Sticko rating algorithm

## v0.13.4 - 2022-10-03

- Improve performance of some functions

## v0.13.3 - 2022-10-03

- Add Apache/MIT dual license
- Add some GitHub templates

## v0.13.2 - 2022-10-03

- Implement more From traits for Ratings
- Overhaul examples

## v0.13.1 - 2022-10-01

- Add some benchmarks
- Overhaul documentation

## v0.13.0 - 2022-09-25

- All functions now use references instead of taking ownership unnecessarily

## v0.12.0 - 2022-08-26

- Add Weng-Lin (A Bayesian Approximation Method for Online Ranking) calculations
- Return original teams when a team is empty in `trueskill::trueskill_teams`

## v0.11.0 - 2022-08-26

- Add `new` and `default` implementations for `DWZRating`
- `new` implementation for `IngoRating` now requires an age to be set

## v0.10.2 - 2022-08-25

- Fix typos

## v0.10.1 - 2022-08-25

- Overhauled documentation

## v0.10.0 - 2022-08-24

- Add team calculations for TrueSkill
    - These include `trueskill::trueskill_teams`, `trueskill::expected_score_teams`, `trueskill::match_quality::teams`

## v0.9.2 - 2022-08-23

- Fix major bug in TrueSkill draw calculations
    - Would just give you your old rating if you had a draw, works properly now

## v0.9.1 - 2022-08-20

- Improved performance of `rating_period` functions
- Improved docs main page

## v0.9.0 - 2022-08-20

- Add `rating_period` functions, these allow to get a single rating from a list of results.
    - The following functions have been added: `dwz::dwz_rating_period`, `elo::elo_rating_period`, `glicko::glicko_rating_period`, `glicko2::glicko2_rating_period`, `ingo::ingo_rating_period`, `trueskill::trueskill_rating_period`.

## v0.8.0 - 2022-08-19

- Add config structs: `EloConfig`, `GlickoConfig`, `Glicko2Config`, `TrueSkillConfig`
    - These allow you to change some values used in the algorithm to further customise the behaviour
    - The following functions require a config now: `elo::elo`, `glicko::decay_deviation`, `glicko2::glicko2`, `trueskill::trueskill`, `trueskill::match_quality`, `trueskill::expected_score`
- Fix some spelling issues

## v0.7.2 - 2022-08-18

- Implement eq for Outcomes
- Refactor some loops

## v0.7.1 - 2022-08-17

- Replace `.unwrap_or()` usage in DWZ calculations

## v0.7.0 - 2022-08-16

- Add Ingo calculations

## v0.6.0 - 2022-08-16

- Add TrueSkill calculations

## v0.5.0 - 2022-08-14

- Added DWZ calculations

## v0.4.1 - 2022-08-13

- Fix major bug in glicko-2 volatility calculations
    - This would yield a wildly incorrect solution when the players came back from a "losing streak"

## v0.4.0 - 2022-08-13

- Added Glicko (1) calculations
- Renamed old GlickoRating to Glicko2Rating
    - New GlickoRating is for the Glicko System now
- Added confidence_interval function to Glicko2 and Glicko

## v0.3.1 - 2022-08-12

- Fix some clippy issues

## v0.3.0 - 2022-08-12

- elo::expected_score now takes in two EloRatings instead of f64
- Added default implementation for GlickoRating and EloRating
- Improved CI
- Improved docs

## v0.2.0 - 2022-08-10

- Added rating deviation decay function for Glicko-2

## v0.1.0 - 2022-08-09

- Initial release
- Includes Glicko-2 and Elo functions to calculate scores and expected outcomes