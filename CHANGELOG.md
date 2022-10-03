# Changelog

This is a broad overview of the changes that have been made over the lifespan of this library.

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
    - These allow you to change some values used in the algorithm to further customise the behavior
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