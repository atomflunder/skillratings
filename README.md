# skillratings

[![](https://img.shields.io/crates/v/skillratings)](https://crates.io/crates/skillratings)
[![](https://img.shields.io/docsrs/skillratings)](https://docs.rs/skillratings/)
[![](https://codecov.io/gh/atomflunder/skillratings/branch/master/graph/badge.svg?token=JFSA86GAX1)](https://codecov.io/gh/atomflunder/skillratings)
[![](https://img.shields.io/crates/d/skillratings)](https://crates.io/crates/skillratings)

Calculate a player's skill level using [Elo](https://en.wikipedia.org/wiki/Elo_rating_system), [Glicko](https://en.wikipedia.org/wiki/Glicko_rating_system) and [Glicko-2](https://en.wikipedia.org/wiki/Glicko-2) algorithms known from their usage in chess and other games.  

## Installation

Add the following to your `Cargo.toml` file:

```toml
[dependencies]
skillratings = "0.4.1"
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

Instead of the traditional way of calculating the Glicko for only one player only using a list of results, we are calculating the Glicko rating for two players at once, like in the Elo calculation, to make it easier to see instant results.

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

The same as above is true here, we calculate the results instantly for both players.

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

## License

This project is licensed under the [MIT License](/LICENSE).
