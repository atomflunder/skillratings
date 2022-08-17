# Changelog

This is a broad overview of the changes that have been made over the lifespan of this library.

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