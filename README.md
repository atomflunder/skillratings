# skillratings

[![](https://img.shields.io/crates/v/skillratings)](https://crates.io/crates/skillratings)
[![](https://img.shields.io/docsrs/skillratings)](https://docs.rs/skillratings/)
[![](https://codecov.io/gh/atomflunder/skillratings/branch/master/graph/badge.svg?token=JFSA86GAX1)](https://codecov.io/gh/atomflunder/skillratings)
[![](https://img.shields.io/crates/d/skillratings)](https://crates.io/crates/skillratings)

Skillratings allows you to calculate the player's skill instantly, or after tournaments/rating periods with a list of results using a variety of known and lesser known skill rating algorithms.

Currently supported algorithms:

- [Elo](#elo-rating-system)
- [Glicko](#glicko-rating-system)
- [Glicko-2](#glicko-2-rating-system)
- [TrueSkill](#trueskill-rating-system)
- [Weng-Lin (Bayesian Approxmation Method)](#weng-lin-rating-system)
- [DWZ (Deutsche Wertungszahl)](#dwz-deutsche-wertungszahl-rating-system)
- [Ingo](#ingo-rating-system)

They are mainly known from their usage in chess and online games.

## Installation

Add the following to your `Cargo.toml` file:

```toml
[dependencies]
skillratings = "0.13.0"
```

## Basic Usage

Below is the most basic use case for each supported algorithm, in a 1-vs-1 format.  
Each rating algorithm has many more associated functions, for example getting a rating using a list of outcomes or getting expected scores.

Head over [to the documentation](https://docs.rs/skillratings/) for more information.

### Elo rating system

- [Documentation](https://docs.rs/skillratings/latest/skillratings/elo/index.html)
- [Wikipedia article](https://en.wikipedia.org/wiki/Elo_rating_system)

```rust
extern crate skillratings;

use skillratings::{
    elo::elo, outcomes::Outcomes, rating::EloRating, config::EloConfig
};

let player_one = EloRating { rating: 1000.0 };
let player_two = EloRating { rating: 1000.0 };

// The outcome is from the perspective of player one.
let outcome = Outcomes::WIN;

// The config allows you to change certain adjustable values in the algorithms.
let config = EloConfig::new();

let (player_one_new, player_two_new) = elo(player_one, player_two, outcome, &config);
assert!((player_one_new.rating - 1016.0).abs() < f64::EPSILON);
assert!((player_two_new.rating - 984.0).abs() < f64::EPSILON);
```

### Glicko rating system

- [Documentation](https://docs.rs/skillratings/latest/skillratings/glicko/index.html)
- [Wikipedia article](https://en.wikipedia.org/wiki/Glicko_rating_system)

```rust
use skillratings::{
    config::Glicko2Config, glicko::glicko, outcomes::Outcomes, rating::GlickoRating,
};


let player_one = GlickoRating {
    rating: 1500.0,
    deviation: 350.0,
};
let player_two = GlickoRating {
    rating: 1500.0,
    deviation: 350.0,
};

// The outcome is from the perspective of player one.
let outcome = Outcomes::WIN;

// The config allows you to change certain adjustable values in the algorithms.
let config = GlickoConfig::new();

let (player_one_new, player_two_new) = glicko(player_one, player_two, outcome, &config);

assert!((player_one_new.rating.round() - 1662.0).abs() < f64::EPSILON);
assert!((player_one_new.deviation.round() - 290.0).abs() < f64::EPSILON);

assert!((player_two_new.rating.round() - 1338.0).abs() < f64::EPSILON);
assert!((player_two_new.deviation.round() - 290.0).abs() < f64::EPSILON);
```

### Glicko-2 rating system

- [Documentation](https://docs.rs/skillratings/latest/skillratings/glicko2/index.html)
- [Wikipedia article](https://en.wikipedia.org/wiki/Glicko-2)

```rust
extern crate skillratings;

use skillratings::{
    glicko2::glicko2, outcomes::Outcomes, rating::Glicko2Rating, config::Glicko2Config
};

let player_one = Glicko2Rating { 
    rating: 1500.0, 
    deviation: 350.0, 
    volatility: 0.06 
};
let player_two = Glicko2Rating { 
    rating: 1500.0, 
    deviation: 350.0, 
    volatility: 0.06 
};

// The outcome is from the perspective of player one.
let outcome = Outcomes::WIN;

// The config allows you to change certain adjustable values in the algorithms.
let config = Glicko2Config::new();

let (player_one_new, player_two_new) = glicko2(player_one, player_two, outcome, &config);

assert!((player_one_new.rating.round() - 1662.0).abs() < f64::EPSILON);
assert!((player_one_new.deviation.round() - 290.0).abs() < f64::EPSILON);

assert!((player_two_new.rating.round() - 1338.0).abs() < f64::EPSILON);
assert!((player_two_new.deviation.round() - 290.0).abs() < f64::EPSILON);
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

let player_one = TrueSkillRating{
    rating: 25.0,
    uncertainty: 8.333,
};
let player_two = TrueSkillRating {
    rating: 30.0,
    uncertainty: 1.2,
};

// The config allows you to change certain adjustable values in the algorithms.
let config = TrueSkillConfig::new();

// The outcome is from the perspective of player one.
let outcome = Outcomes::WIN;

let (p1, p2) = trueskill(player_one, player_two, outcome, &config);

assert!(((p1.rating * 100.0).round() - 3300.0).abs() < f64::EPSILON);
assert!(((p1.uncertainty * 100.0).round() - 597.0).abs() < f64::EPSILON);

assert!(((p2.rating * 100.0).round() - 2983.0).abs() < f64::EPSILON);
assert!(((p2.uncertainty * 100.0).round() - 120.0).abs() < f64::EPSILON);
```

## Weng-Lin rating system

(A Bayesian Approximation Method for Online Ranking)

- [Documentation](https://docs.rs/skillratings/latest/skillratings/weng_lin/index.html)
- [Original Paper (PDF)](https://jmlr.csail.mit.edu/papers/volume12/weng11a/weng11a.pdf)

```rust
use skillratings::{
    rating::WengLinRating, weng_lin::weng_lin, outcomes::Outcomes, config::WengLinConfig
};

let player_one = WengLinRating {
    rating: 42.0,
    uncertainty: 1.3,
};
let player_two = WengLinRating {
    rating: 25.0,
    uncertainty: 8.333,
};

// The config allows you to change certain adjustable values in the algorithms.
let config = WengLinConfig::new();

// The outcome is from the perspective of player one.
let outcome = Outcomes::WIN;

let (player_one, player_two) = weng_lin(player_one, player_two, outcome, &config);

assert!(((player_one.rating * 100.0).round() - 4203.0).abs() < f64::EPSILON);
assert!(((player_one.uncertainty * 100.0).round() - 130.0).abs() < f64::EPSILON);
assert!(((player_two.rating * 100.0).round() - 2391.0).abs() < f64::EPSILON);
assert!(((player_two.uncertainty * 100.0).round() - 803.0).abs() < f64::EPSILON);
```

### DWZ (Deutsche Wertungszahl) rating system

- [Documentation](https://docs.rs/skillratings/latest/skillratings/dwz/index.html)
- [Wikipedia article](https://en.wikipedia.org/wiki/Deutsche_Wertungszahl)

```rust
use skillratings::{dwz::dwz, outcomes::Outcomes, rating::DWZRating};

let player_one = DWZRating {
    rating: 1500.0,
    index: 42,
    // The actual age of the player, if unavailable set this to >25.
    // The lower the age, the more the rating will fluctuate.
    age: 42,
};
let player_two = DWZRating {
    rating: 1500.0,
    index: 12,
    age: 12,
};

// The outcome is from the perspective of player one.
let outcome = Outcomes::WIN;

let (player_one_new, player_two_new) = dwz(player_one, player_two, outcome);

assert!((player_one_new.rating.round() - 1519.0).abs() < f64::EPSILON);
assert_eq!(player_one_new.index, 43);

assert!((player_two_new.rating.round() - 1464.0).abs() < f64::EPSILON);
assert_eq!(player_two_new.index, 13);
```

### Ingo rating system

- [Documentation](https://docs.rs/skillratings/latest/skillratings/ingo/index.html)
- [Wikipedia article (in german, no english version available)](https://de.wikipedia.org/wiki/Ingo-Zahl)

```rust
use skillratings::{ingo::ingo, outcomes::Outcomes, rating::IngoRating};

let player_one = IngoRating {
    // Note that a lower rating is more desirable.
    rating: 130.0,
    // The actual age of the player, if unavailable set this to >25.
    // The lower the age, the more the rating will fluctuate.
    age: 40,
};
let player_two = IngoRating {
    rating: 160.0,
    age: 40,
};

// The outcome is from the perspective of player one.
let outcome = Outcomes::WIN;

let (p1, p2) = ingo(player_one, player_two, outcome);

assert!((p1.rating.round() - 129.0).abs() < f64::EPSILON);
assert!((p2.rating.round() - 161.0).abs() < f64::EPSILON);
```

## License

This project is licensed under the [MIT License](/LICENSE).
