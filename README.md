# skillratings

[![](https://img.shields.io/crates/v/skillratings)](https://crates.io/crates/skillratings)
[![](https://img.shields.io/docsrs/skillratings)](https://docs.rs/skillratings/)
[![](https://codecov.io/gh/atomflunder/skillratings/branch/master/graph/badge.svg?token=JFSA86GAX1)](https://codecov.io/gh/atomflunder/skillratings)
[![](https://img.shields.io/crates/d/skillratings)](https://crates.io/crates/skillratings)

Skillratings provides a collection of well-known (and lesser known) skill rating algorithms, that allow you to assess a player's skill level instantly.  
You can easily calculate skill ratings instantly in 1 vs 1 matches, Team vs Team Matches, Free-For-Alls, Multiple-Team Matches, or in Tournaments / Rating Periods.  
This library is incredibly lightweight (no dependencies by default), user-friendly, and of course, _blazingly fast_.

Currently supported algorithms:

-   [Elo](https://docs.rs/skillratings/latest/skillratings/elo/)
-   [Glicko](https://docs.rs/skillratings/latest/skillratings/glicko/)
-   [Glicko-2](https://docs.rs/skillratings/latest/skillratings/glicko2/)
-   [TrueSkill](https://docs.rs/skillratings/latest/skillratings/trueskill/)
-   [Weng-Lin (OpenSkill)](https://docs.rs/skillratings/latest/skillratings/weng_lin/)
-   [FIFA Men's World Ranking](https://docs.rs/skillratings/latest/skillratings/fifa/)
-   [Sticko (Stephenson Rating System)](https://docs.rs/skillratings/latest/skillratings/sticko/)
-   [Glicko-Boost](https://docs.rs/skillratings/latest/skillratings/glicko_boost/)
-   [USCF (US Chess Federation Ratings)](https://docs.rs/skillratings/latest/skillratings/uscf/)
-   [EGF (European Go Federation Ratings)](https://docs.rs/skillratings/latest/skillratings/egf/)
-   [DWZ (Deutsche Wertungszahl)](https://docs.rs/skillratings/latest/skillratings/dwz/)
-   [Ingo](https://docs.rs/skillratings/latest/skillratings/ingo/)

Most of these are known from their usage in online multiplayer games.  
Click on the documentation for the modules linked above for more information about the specific rating algorithms, and their advantages and disadvantages.

## Table of Contents

-   [Installation](#installation)
    -   [Serde Support](#serde-support)
-   [Usage and Examples](#usage-and-examples)
    -   [Player vs. Player](#player-vs-player)
    -   [Team vs. Team](#team-vs-team)
    -   [Free-For-Alls and Multiple Teams](#free-for-alls-and-multiple-teams)
    -   [Expected Outcome](#expected-outcome)
    -   [Rating Period](#rating-period)
    -   [Switching between different rating systems](#switching-between-different-rating-systems)
-   [Contributing](#contributing)
-   [License](#license)

## Installation

If you are on Rust 1.62 or higher use `cargo add` to install the latest version:

```bash
cargo add skillratings
```

Alternatively, you can add the following to your `Cargo.toml` file manually:

```toml
[dependencies]
skillratings = "0.28"
```

### Serde support

Serde support is gated behind the `serde` feature. You can enable it like so:

Using `cargo add`:

```bash
cargo add skillratings --features serde
```

By editing `Cargo.toml` manually:

```toml
[dependencies]
skillratings = {version = "0.28", features = ["serde"]}
```

## Usage and Examples

Below you can find some basic examples of the use cases of this crate.  
There are many more rating algorithms available with lots of useful functions that are not covered here.  
For more information, please head over [to the documentation](https://docs.rs/skillratings/).

### Player-vs-Player

Every rating algorithm included here can be used to rate 1v1 games.  
We use _Glicko-2_ in this example here.

```rust
use skillratings::{
    glicko2::{glicko2, Glicko2Config, Glicko2Rating},
    Outcomes,
};

// Initialise a new player rating.
// The default values are: 1500, 350, and 0.06.
let player_one = Glicko2Rating::new();

// Or you can initialise it with your own values of course.
// Imagine these numbers being pulled from a database.
let (some_rating, some_deviation, some_volatility) = (1325.0, 230.0, 0.05932);
let player_two = Glicko2Rating {
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

// The first players rating increased by ~112 points.
assert_eq!(new_player_one.rating.round(), 1612.0);
```

### Team-vs-Team

Some algorithms like TrueSkill or Weng-Lin allow you to rate team-based games as well.  
This example shows a 3v3 game using _TrueSkill_.

```rust
use skillratings::{
    trueskill::{trueskill_two_teams, TrueSkillConfig, TrueSkillRating},
    Outcomes,
};

// We initialise Team One as a Vec of multiple TrueSkillRatings.
// The default values for the rating are: 25, 25/3 ≈ 8.33.
let team_one = vec![
    TrueSkillRating {
        rating: 33.3,
        uncertainty: 3.3,
    },
    TrueSkillRating {
        rating: 25.1,
        uncertainty: 1.2,
    },
    TrueSkillRating {
        rating: 43.2,
        uncertainty: 2.0,
    },
];

// Team Two will be made up of 3 new players, for simplicity.
// Note that teams do not necessarily have to be the same size.
let team_two = vec![
    TrueSkillRating::new(),
    TrueSkillRating::new(),
    TrueSkillRating::new(),
];

// The outcome of the match is from the perspective of team one.
let outcome = Outcomes::LOSS;

// The config allows you to specify certain values in the TrueSkill calculation.
let config = TrueSkillConfig::new();

// The trueskill_two_teams function will calculate the new ratings for both teams and return them.
let (new_team_one, new_team_two) = trueskill_two_teams(&team_one, &team_two, &outcome, &config);

// The rating of the first player on team one decreased by around ~1.2 points.
assert_eq!(new_team_one[0].rating.round(), 32.0);
```

### Free-For-Alls and Multiple Teams

The Weng-Lin and TrueSkill algorithms also support rating matches with multiple Teams.  
Here is an example showing a 3-Team game with 3 players each.

```rust
use skillratings::{
    weng_lin::{weng_lin_multi_team, WengLinConfig, WengLinRating},
    MultiTeamOutcome,
};

// Initialise the teams as Vecs of WengLinRatings.
// Note that teams do not necessarily have to be the same size.
// The default values for the rating are: 25, 25/3 ≈ 8.33.
let team_one = vec![
    WengLinRating {
        rating: 25.1,
        uncertainty: 5.0,
    },
    WengLinRating {
        rating: 24.0,
        uncertainty: 1.2,
    },
    WengLinRating {
        rating: 18.0,
        uncertainty: 6.5,
    },
];

let team_two = vec![
    WengLinRating {
        rating: 44.0,
        uncertainty: 1.2,
    },
    WengLinRating {
        rating: 32.0,
        uncertainty: 2.0,
    },
    WengLinRating {
        rating: 12.0,
        uncertainty: 3.2,
    },
];

// Using the default rating for team three for simplicity.
let team_three = vec![
    WengLinRating::new(),
    WengLinRating::new(),
    WengLinRating::new(),
];

// Every team is assigned a rank, depending on their placement. The lower the rank, the better.
// If two or more teams tie with each other, assign them the same rank.
let rating_groups = vec![
    (&team_one[..], MultiTeamOutcome::new(1)),      // team one takes the 1st place.
    (&team_two[..], MultiTeamOutcome::new(3)),      // team two takes the 3rd place.
    (&team_three[..], MultiTeamOutcome::new(2)),    // team three takes the 2nd place.
];

// The weng_lin_multi_team function will calculate the new ratings for all teams and return them.
let new_teams = weng_lin_multi_team(&rating_groups, &WengLinConfig::new());

// The rating of the first player of team one increased by around ~2.9 points.
assert_eq!(new_teams[0][0].rating.round(), 28.0);
```

### Expected outcome

Every rating algorithm has an `expected_score` function that you can use to predict the outcome of a game.  
This example is using _Glicko_ (_not Glicko-2!_) to demonstrate.

```rust
use skillratings::glicko::{expected_score, GlickoRating};

// Initialise a new player rating.
// The default values are: 1500, and 350.
let player_one = GlickoRating::new();

// Initialising a new rating with custom numbers.
let player_two = GlickoRating {
    rating: 1812.0,
    deviation: 195.0,
};

// The expected_score function will return two floats between 0 and 1 for each player.
// A value of 1 means guaranteed victory, 0 means certain loss.
// Values near 0.5 mean draws are likely to occur.
let (exp_one, exp_two) = expected_score(&player_one, &player_two);

// The expected score for player one is ~0.25.
// If these players would play 100 games, player one is expected to score around 25 points.
// (Win = 1 point, Draw = 0.5, Loss = 0)
assert_eq!((exp_one * 100.0).round(), 25.0);
```

### Rating period

Every rating algorithm included here has a `..._rating_period` that allows you to calculate a player's new rating using a list of results.  
This can be useful in tournaments, or if you only update ratings at the end of a certain rating period, as the name suggests.  
We are using the _Elo_ rating algorithm in this example.

```rust
use skillratings::{
    elo::{elo_rating_period, EloConfig, EloRating},
    Outcomes,
};

// We initialise a new Elo Rating here.
// The default rating value is 1000.
let player = EloRating { rating: 1402.1 };

// We need a list of results to pass to the elo_rating_period function.
let mut results = Vec::new();

// And then we populate the list with tuples containing the opponent,
// and the outcome of the match from our perspective.
results.push((EloRating::new(), Outcomes::WIN));
results.push((EloRating { rating: 954.0 }, Outcomes::DRAW));
results.push((EloRating::new(), Outcomes::LOSS));

// The elo_rating_period function calculates the new rating for the player and returns it.
let new_player = elo_rating_period(&player, &results, &EloConfig::new());

// The rating of the player decreased by around ~40 points.
assert_eq!(new_player.rating.round(), 1362.0);
```

### Switching between different rating systems

If you want to switch between different rating systems, for example to compare results or to do scientific analyisis,
we provide Traits to make switching as easy and fast as possible.  
All you have to do is provide the right Config for your rating system.

_**Disclaimer:**_ For more accurate and fine-tuned calculations it is recommended that you use the rating system modules directly.  
The Traits are primarily meant to be used for comparisions between systems.

In the following example, we are using the `RatingSystem` (1v1) Trait with Glicko-2:

```rust
use skillratings::{
    glicko2::{Glicko2, Glicko2Config},
    Outcomes, Rating, RatingSystem,
 ;

// Initialise a new player rating with a rating value and uncertainty value.
// Not every rating system has an uncertainty value, so it may be discarded.
// Some rating systems might consider other values too (volatility, age, matches played etc.).
// If that is the case, we will use the default values for those.
let player_one = Rating::new(Some(1200.0), Some(120.0));
// Some rating systems might use widely different scales for measuring a player's skill.
// So if you always want the default values for every rating system, use None instead.
let player_two = Rating::new(None, None);

// The config needs to be specific to the rating system.
// When you swap rating systems, make sure to update the config.
let config = Glicko2Config::new();

// We want to rate 1v1 matches here so we are using the `RatingSystem` trait.
// You may also need to use a type annotation here for the compiler.
let rating_system: Glicko2 = RatingSystem::new(config);

// The outcome of the match is from the perspective of player one.
let outcome = Outcomes::WIN;

// Calculate the expected score of the match.
let expected_score = rating_system.expected_score(&player_one, &player_two);
// Calculate the new ratings.
let (new_one, new_two) = rating_system.rate(&player_one, &player_two, &outcome);

// After that, access new ratings and uncertainties with the functions below.
assert_eq!(new_one.rating().round(), 1241.0);
// Note that because not every rating system has an uncertainty value,
// the uncertainty function returns an Option<f64>.
assert_eq!(new_one.uncertainty().unwrap().round(), 118.0);
```

## Contributing

Contributions of any kind are always welcome!

Found a bug or have a feature request? [Submit a new issue](https://github.com/atomflunder/skillratings/issues/).  
Alternatively, [open a pull request](https://github.com/atomflunder/skillratings/pulls) if you want to add features or fix bugs.  
Leaving other feedback is of course also appreciated.

Thanks to everyone who takes their time to contribute.

## License

This project is licensed under either the [MIT License](/LICENSE-MIT), or the [Apache License, Version 2.0](/LICENSE-APACHE).

