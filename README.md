# skillratings

[![](https://img.shields.io/crates/v/skillratings)](https://crates.io/crates/skillratings)
[![](https://img.shields.io/docsrs/skillratings)](https://docs.rs/skillratings/)
[![](https://img.shields.io/crates/d/skillratings)](https://crates.io/crates/skillratings)

Calculate a player's skill level using [Elo](https://en.wikipedia.org/wiki/Elo_rating_system) and [Glicko-2](https://en.wikipedia.org/wiki/Glicko_rating_system#Glicko-2_algorithm) algorithms known from their usage in chess and other games.  

## Installation

Add the following to your `Cargo.toml` file:

```toml
[dependencies]
skillratings = "0.1.0"
```

## Usage

For a detailed guide on how to use this crate, head over [to the documentation](https://docs.rs/skillratings/).

### Elo rating system
```rust
extern crate skillratings;

use skillratings;

let player_one = skillratings::rating::EloRating { rating: 1000.0 };
let player_two = skillratings::rating::EloRating { rating: 1000.0 };

// The outcome is from the perspective of player one.
let outcome = skillratings::outcomes::Outcomes::WIN;

let (player_one_new, player_two_new) = skillratings::elo::elo(player_one, player_two, outcome, 32.0);
assert_eq!(player_one_new.rating, 1016.0);
assert_eq!(player_two_new.rating, 984.0);
```

### Glicko-2 rating system

Instead of the traditional way of calculating the Glicko-2 for only one player only using a list of results, we are calculating the Glicko-2 rating for two players at once, like in the Elo calculation, to make it easier to see instant results.

```rust
extern crate skillratings;

use skillratings;

let player_one = skillratings::rating::GlickoRating { 
    rating: 1500.0, 
    deviation: 350.0, 
    volatility: 0.06 
};
let player_two = skillratings::rating::GlickoRating { 
    rating: 1500.0, 
    deviation: 350.0, 
    volatility: 0.06 
};

let outcome = skillratings::outcomes::Outcomes::WIN;

let (player_one_new, player_two_new) = skillratings::glicko2::glicko2(player_one, player_two, outcome, 0.5);

assert_eq!(player_one_new.rating.round(), 1662.0);
assert_eq!(player_one_new.deviation.round(), 290.0);

assert_eq!(player_two_new.rating.round(), 1338.0);
assert_eq!(player_two_new.deviation.round(), 290.0);
```

## License

This project is licensed under the [MIT License](/LICENSE).
