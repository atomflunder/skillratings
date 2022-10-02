# skillratings

[![](https://img.shields.io/crates/v/skillratings)](https://crates.io/crates/skillratings)
[![](https://img.shields.io/docsrs/skillratings)](https://docs.rs/skillratings/)
[![](https://codecov.io/gh/atomflunder/skillratings/branch/master/graph/badge.svg?token=JFSA86GAX1)](https://codecov.io/gh/atomflunder/skillratings)
[![](https://img.shields.io/crates/d/skillratings)](https://crates.io/crates/skillratings)

Skillratings allows you to calculate a player's skill instantly, or after tournaments/rating periods, using a variety of well known (and lesser known) skill rating algorithms.  
This library is incredibly lightweight, user-friendly, and of course, *blazingly fast*.

Currently supported algorithms:

- [Elo](#elo-rating-system)
- [Glicko](#glicko-rating-system)
- [Glicko-2](#glicko-2-rating-system)
- [TrueSkill](#trueskill-rating-system)
- [Weng-Lin (Bayesian Approxmation Method)](#weng-lin-rating-system)
- [DWZ (Deutsche Wertungszahl)](#dwz-deutsche-wertungszahl-rating-system)
- [Ingo](#ingo-rating-system)

These are mainly known from their usage in chess and online games.

## Installation

If you are on Rust 1.62 or higher use `cargo add` to install the latest version:

```
cargo add skillratings
```

Alternatively, you can add the following to your `Cargo.toml` file manually:

```toml
[dependencies]
skillratings = "0.13"
```

## Basic Usage

**Quick disclaimer:** Below are the most basic use cases for each supported algorithm, in a 1-vs-1 format.  
Each rating algorithm has *many* more associated functions, for example getting a rating using a list of outcomes, or getting the expected scores of a match.

Head over [to the documentation](https://docs.rs/skillratings/) for more information and examples.

### Elo rating system

- [Documentation](https://docs.rs/skillratings/latest/skillratings/elo/index.html)
- [Wikipedia article](https://en.wikipedia.org/wiki/Elo_rating_system)

```rust
use skillratings::{
    elo::elo, outcomes::Outcomes, rating::EloRating, config::EloConfig
};

// Initialise a new player rating.
let player_one = EloRating::new();

// Or you can initialise it with your own values of course.
// Imagine these numbers being pulled from a database.
let some_rating = 1325.0;
let player_two = EloRating{
    rating: some_rating,
};

// The outcome of the match is from the perspective of player one.
let outcome = Outcomes::WIN;

// The config allows you to specify certain values in the Elo calculation.
let config = EloConfig::new();

// The elo function will calculate the new ratings for both players and return them.
let (new_player_one, new_player_two) = elo(&player_one, &player_two, &outcome, &config);
```

### Glicko rating system

- [Documentation](https://docs.rs/skillratings/latest/skillratings/glicko/index.html)
- [Wikipedia article](https://en.wikipedia.org/wiki/Glicko_rating_system)

```rust
use skillratings::{
    glicko::glicko, outcomes::Outcomes, rating::GlickoRating
};

// Initialise a new player rating.
let player_one = GlickoRating::new();

// Or you can initialise it with your own values of course.
// Imagine these numbers being pulled from a database.
let (some_rating, some_deviation) = (1325.0, 230.0);
let player_two = GlickoRating{
    rating: some_rating,
    deviation: some_deviation,
};

// The outcome of the match is from the perspective of player one.
let outcome = Outcomes::WIN;

// The glicko function will calculate the new ratings for both players and return them.
let (new_player_one, new_player_two) = glicko(&player_one, &player_two, &outcome);
```

### Glicko-2 rating system

- [Documentation](https://docs.rs/skillratings/latest/skillratings/glicko2/index.html)
- [Wikipedia article](https://en.wikipedia.org/wiki/Glicko-2)

```rust
use skillratings::{
    glicko2::glicko2, outcomes::Outcomes, rating::Glicko2Rating, config::Glicko2Config
};

// Initialise a new player rating.
let player_one = Glicko2Rating::new();

// Or you can initialise it with your own values of course.
// Imagine these numbers being pulled from a database.
let (some_rating, some_deviation, some_volatility) = (1325.0, 230.0, 0.05932);
let player_two = Glicko2Rating{
    rating: some_rating,
    deviation: some_deviation,
    volatility: some_volatility,
};

// The outcome of the match is from the perspective of player one.
let outcome = Outcomes::WIN;

// The config allows you to specify certain values in the Glicko-2 calculation.
let config = Glicko2Config::new();

// The glicko2 function will calculate the new ratings for both players and return them.
let (new_player_one, new_player_two) = glicko2(&player_one, &player_two, &outcome, &config);
```

### TrueSkill rating system

- [Documentation](https://docs.rs/skillratings/latest/skillratings/trueskill/index.html)
- [Wikipedia article](https://en.wikipedia.org/wiki/TrueSkill)

**Caution regarding usage of TrueSkill**: 
Microsoft permits only Xbox Live games or non-commercial projects to use TrueSkill(TM). 
If your project is commercial, you should use another rating system included here.

```rust
use skillratings::{
    trueskill::trueskill, outcomes::Outcomes, rating::TrueSkillRating, config::TrueSkillConfig
};

// Initialise a new player rating.
let player_one = TrueSkillRating::new();

// Or you can initialise it with your own values of course.
// Imagine these numbers being pulled from a database.
let (some_rating, some_uncertainty) = (34.2, 2.3);
let player_two = TrueSkillRating{
    rating: some_rating,
    uncertainty: some_uncertainty,
};

// The outcome of the match is from the perspective of player one.
let outcome = Outcomes::WIN;

// The config allows you to specify certain values in the TrueSkill calculation.
let config = TrueSkillConfig::new();

// The trueskill function will calculate the new ratings for both players and return them.
let (new_player_one, new_player_two) = trueskill(&player_one, &player_two, &outcome, &config);
```

## Weng-Lin rating system

(A Bayesian Approximation Method for Online Ranking)

- [Documentation](https://docs.rs/skillratings/latest/skillratings/weng_lin/index.html)
- [Original Paper (PDF)](https://jmlr.csail.mit.edu/papers/volume12/weng11a/weng11a.pdf)

```rust
use skillratings::{
    weng_lin::weng_lin, outcomes::Outcomes, rating::WengLinRating, config::WengLinConfig
};

// Initialise a new player rating.
let player_one = WengLinRating::new();

// Or you can initialise it with your own values of course.
// Imagine these numbers being pulled from a database.
let (some_rating, some_uncertainty) = (41.2, 2.12);
let player_two = WengLinRating{
    rating: some_rating,
    uncertainty: some_uncertainty,
};

// The outcome of the match is from the perspective of player one.
let outcome = Outcomes::WIN;

// The config allows you to specify certain values in the Weng-Lin calculation.
let config = WengLinConfig::new();

// The weng_lin function will calculate the new ratings for both players and return them.
let (new_player_one, new_player_two) = weng_lin(&player_one, &player_two, &outcome, &config);
```

### DWZ (Deutsche Wertungszahl) rating system

- [Documentation](https://docs.rs/skillratings/latest/skillratings/dwz/index.html)
- [Wikipedia article](https://en.wikipedia.org/wiki/Deutsche_Wertungszahl)

```rust
use skillratings::{
    dwz::dwz, outcomes::Outcomes, rating::DWZRating
};

// Initialise a new player rating.
// We need to set the actual age for the player,
// if you are unsure what to set here, choose something that is greater than 25.
let player_one = DWZRating::new(19);

// Or you can initialise it with your own values of course.
// Imagine these numbers being pulled from a database.
let (some_rating, some_index, some_age) = (1325.0, 51, 27);
let player_two = DWZRating{
    rating: some_rating,
    index: some_index,
    age: some_age,
};

// The outcome of the match is from the perspective of player one.
let outcome = Outcomes::WIN;

// The dwz function will calculate the new ratings for both players and return them.
let (new_player_one, new_player_two) = dwz(&player_one, &player_two, &outcome);
```

### Ingo rating system

- [Documentation](https://docs.rs/skillratings/latest/skillratings/ingo/index.html)
- [Wikipedia article (in german, no english version available)](https://de.wikipedia.org/wiki/Ingo-Zahl)

```rust
use skillratings::{
    ingo::ingo, outcomes::Outcomes, rating::IngoRating
};

// Initialise a new player rating.
// We need to set the actual age for the player,
// if you are unsure what to set here, choose something that is greater than 25.
let player_one = IngoRating::new(19);

// Or you can initialise it with your own values of course.
// Imagine these numbers being pulled from a database.
let (some_rating, some_age) = (150.4, 23);
let player_two = IngoRating{
    rating: some_rating,
    age: some_age,
};

// The outcome of the match is from the perspective of player one.
let outcome = Outcomes::WIN;

// The ingo function will calculate the new ratings for both players and return them.
let (new_player_one, new_player_two) = ingo(&player_one, &player_two, &outcome);
```

## License

This project is licensed under the [MIT License](/LICENSE).
