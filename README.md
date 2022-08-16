# skillratings

[![](https://img.shields.io/crates/v/skillratings)](https://crates.io/crates/skillratings)
[![](https://img.shields.io/docsrs/skillratings)](https://docs.rs/skillratings/)
[![](https://codecov.io/gh/atomflunder/skillratings/branch/master/graph/badge.svg?token=JFSA86GAX1)](https://codecov.io/gh/atomflunder/skillratings)
[![](https://img.shields.io/crates/d/skillratings)](https://crates.io/crates/skillratings)

Calculate a player's skill rating **in 1v1 matches** instantly using [Elo](https://en.wikipedia.org/wiki/Elo_rating_system), [TrueSkill](https://en.wikipedia.org/wiki/TrueSkill), [DWZ](https://en.wikipedia.org/wiki/Deutsche_Wertungszahl), [Glicko](https://en.wikipedia.org/wiki/Glicko_rating_system) and [Glicko-2](https://en.wikipedia.org/wiki/Glicko-2) algorithms known from their usage in chess and online games.  

Skillratings is only for calculating 1v1 matches, teams are not supported. Also we calculate the results instantly, instead of at the end of every rating period.

## Installation

Add the following to your `Cargo.toml` file:

```toml
[dependencies]
skillratings = "0.6.0"
```

## Usage

For a detailed guide on how to use this crate, head over [to the documentation](https://docs.rs/skillratings/).

### Elo rating system
```rust
extern crate skillratings;

use skillratings::{elo::elo, outcomes::Outcomes, rating::EloRating};

let player_one = EloRating { rating: 1000.0 };
let player_two = EloRating { rating: 1000.0 };

// The outcome is from the perspective of player one.
let outcome = Outcomes::WIN;

let (player_one_new, player_two_new) = elo(player_one, player_two, outcome, 32.0);
assert!((player_one_new.rating - 1016.0).abs() < f64::EPSILON);
assert!((player_two_new.rating - 984.0).abs() < f64::EPSILON);
```

### Glicko rating system

```rust
use skillratings::{glicko::glicko, outcomes::Outcomes, rating::GlickoRating};

let player_one = GlickoRating {
    rating: 1500.0,
    deviation: 350.0,
};
let player_two = GlickoRating {
    rating: 1500.0,
    deviation: 350.0,
};

let outcome = Outcomes::WIN;

let (player_one_new, player_two_new) = glicko(player_one, player_two, outcome);

assert!((player_one_new.rating.round() - 1662.0).abs() < f64::EPSILON);
assert!((player_one_new.deviation.round() - 290.0).abs() < f64::EPSILON);

assert!((player_two_new.rating.round() - 1338.0).abs() < f64::EPSILON);
assert!((player_two_new.deviation.round() - 290.0).abs() < f64::EPSILON);
```

### Glicko-2 rating system

```rust
extern crate skillratings;

use skillratings::{glicko2::glicko2, outcomes::Outcomes, rating::Glicko2Rating};

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

let outcome = Outcomes::WIN;

let (player_one_new, player_two_new) = glicko2(player_one, player_two, outcome, 0.5);

assert!((player_one_new.rating.round() - 1662.0).abs() < f64::EPSILON);
assert!((player_one_new.deviation.round() - 290.0).abs() < f64::EPSILON);

assert!((player_two_new.rating.round() - 1338.0).abs() < f64::EPSILON);
assert!((player_two_new.deviation.round() - 290.0).abs() < f64::EPSILON);
```

### TrueSkill rating system

**Caution regarding usage of TrueSkill**: 
Microsoft permits only Xbox Live games or non-commercial projects to use TrueSkill(TM). 
If your project is commercial, you should use another rating system included here.

```rust
use skillratings::{trueskill::trueskill, outcomes::Outcomes, rating::TrueSkillRating};

// Initialises a player with `rating` set to 25.0 and `uncertainty` set to (25.0 / 3).
let player_one = TrueSkillRating::new();
let player_two = TrueSkillRating {
    rating: 30.0,
    uncertainty: 1.2,
};

let (p1, p2) = trueskill(player_one, player_two, Outcomes::WIN);

assert!(((p1.rating * 100.0).round() - 3300.0).abs() < f64::EPSILON);
assert!(((p1.uncertainty * 100.0).round() - 597.0).abs() < f64::EPSILON);

assert!(((p2.rating * 100.0).round() - 2983.0).abs() < f64::EPSILON);
assert!(((p2.uncertainty * 100.0).round() - 120.0).abs() < f64::EPSILON);
```

### DWZ rating system

```rust
use skillratings::{dwz::dwz, outcomes::Outcomes, rating::DWZRating};

let player_one = DWZRating {
    rating: 1500.0,
    index: 42,
    age: 42,
};
let player_two = DWZRating {
    rating: 1500.0,
    index: 12,
    age: 12,
};

let outcome = Outcomes::WIN;

let (player_one_new, player_two_new) = dwz(player_one, player_two, outcome);

assert!((player_one_new.rating.round() - 1519.0).abs() < f64::EPSILON);
assert_eq!(player_one_new.index, 43);

assert!((player_two_new.rating.round() - 1464.0).abs() < f64::EPSILON);
assert_eq!(player_two_new.index, 13);
```

## License

This project is licensed under the [MIT License](/LICENSE).
