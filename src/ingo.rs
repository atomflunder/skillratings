//! The Ingo algorithm, the predecessor of DWZ and one of the first rating algorihms invented in 1947.  
//! Sometimes still used in Xiangqi ("Chinese Chess").
//!
//! Unlike with the other rating systems, with Ingo a lower rating is more desirable,
//! and negative values are possible, though unlikely.  
//! A player with an Ingo rating of 0 has an equivalent Elo rating of 2840, and an Ingo rating of -1 equals 2848 Elo.
//!
//! # More Information
//!
//! - [Wikipedia Article (German, no english version available)](https://de.wikipedia.org/wiki/Ingo-Zahl)
//! - [Archive of Ingo Ratings (German)](https://www.schachbund.de/ingo-spiegel.html)
//! - [Ingo Rules (German, PDF Download)](https://www.schachbund.de/ingo-spiegel.html?file=files/dsb/historie/ingo-spiegel/Ingo-Regeln.pdf&cid=28120)

use crate::{outcomes::Outcomes, rating::IngoRating};

#[must_use]
/// Calculates the [`IngoRating`]s of two players based on their ratings, and the outcome of the game.
///
/// Takes in two players as [`IngoRating`]s, and an [`Outcome`](Outcomes).
///
/// Instead of the traditional way of calculating the Ingo for only one player only using a list of results,
/// we are calculating the Ingo rating for two players at once, like in the Elo calculation,
/// to make it easier to see instant results.
///
/// The outcome of the match is in the perspective of `player_one`.
/// This means [`Outcomes::WIN`] is a win for `player_one` and [`Outcomes::LOSS`] is a win for `player_two`.
///
/// A lower Ingo rating is more desirable, and while negative values are possible,
/// a player with an Ingo rating of 0 has an equivalent Elo rating of 2840.
///
/// # Examples
/// ```
/// use skillratings::{ingo::ingo, outcomes::Outcomes, rating::IngoRating};
///
/// let player_one = IngoRating {
///     rating: 130.0,
///     age: 40,
/// };
/// let player_two = IngoRating {
///     rating: 160.0,
///     age: 40,
/// };
///
/// let (p1, p2) = ingo(&player_one, &player_two, &Outcomes::WIN);
///
/// assert!((p1.rating.round() - 129.0).abs() < f64::EPSILON);
/// assert!((p2.rating.round() - 161.0).abs() < f64::EPSILON);
/// ```
pub fn ingo(
    player_one: &IngoRating,
    player_two: &IngoRating,
    outcome: &Outcomes,
) -> (IngoRating, IngoRating) {
    let score1 = match outcome {
        Outcomes::WIN => 1.0,
        Outcomes::DRAW => 0.5,
        Outcomes::LOSS => 0.0,
    };

    let score2 = 1.0 - score1;

    let perf1 = performance(player_two.rating, score1);
    let perf2 = performance(player_one.rating, score2);

    // Similar to the DWZ algorithm, we use the age of the player to get the development coefficient.
    let development1 = match player_one.age {
        usize::MIN..=20 => 10.0,
        21..=25 => 15.0,
        _ => 20.0,
    };

    let development2 = match player_two.age {
        usize::MIN..=20 => 10.0,
        21..=25 => 15.0,
        _ => 20.0,
    };

    let new_rating1 = perf1.mul_add(1.0, player_one.rating * development1) / (1.0 + development1);
    let new_rating2 = perf2.mul_add(1.0, player_two.rating * development2) / (1.0 + development2);

    (
        IngoRating {
            rating: new_rating1,
            age: player_one.age,
        },
        IngoRating {
            rating: new_rating2,
            age: player_two.age,
        },
    )
}

#[allow(clippy::as_conversions, clippy::cast_precision_loss)]
#[must_use]
/// The "traditional" way of calculating a [`IngoRating`] of a player in a rating period.
///
/// Takes in a player as an [`IngoRating`] and their results as a Vec of tuples containing the opponent as an [`IngoRating`],
/// and the outcome of the game as an [`Outcome`](Outcomes)
///
/// The outcome of the match is in the perspective of the player.
/// This means [`Outcomes::WIN`] is a win for the player and [`Outcomes::LOSS`] is a win for the opponent.
///
/// # Examples
/// ```
/// use skillratings::{ingo::ingo_rating_period, rating::IngoRating, outcomes::Outcomes};
///
/// let player_one = IngoRating {
///     rating: 130.0,
///     age: 40,
/// };
///
/// let player_two = IngoRating {
///     rating: 160.0,
///     age: 40,
/// };
///
/// let player_three = IngoRating {
///     rating: 160.0,
///     age: 40,
/// };
///
/// let player_four = IngoRating {
///     rating: 55.0,
///     age: 40,
/// };
///
/// let player_five = IngoRating {
///     rating: 90.0,
///     age: 40,
/// };
///
/// let results = vec![
///     (player_two, Outcomes::WIN),
///     (player_three, Outcomes::DRAW),
///     (player_four, Outcomes::WIN),
///     (player_five, Outcomes::LOSS),
/// ];
///
/// let p1 = ingo_rating_period(&player_one, &results);
///
/// assert!((p1.rating.round() - 126.0).abs() < f64::EPSILON);
/// ```
pub fn ingo_rating_period(
    player: &IngoRating,
    results: &Vec<(IngoRating, Outcomes)>,
) -> IngoRating {
    let development = match player.age {
        usize::MIN..=20 => 10.0,
        21..=25 => 15.0,
        _ => 20.0,
    };

    let average_points = results
        .iter()
        .map(|r| match r.1 {
            Outcomes::WIN => 1.0,
            Outcomes::DRAW => 0.5,
            Outcomes::LOSS => 0.0,
        })
        .sum::<f64>()
        / results.len() as f64;

    let average_opponent_rating =
        results.iter().map(|r| r.0.rating).sum::<f64>() / results.len() as f64;

    let performance = performance(average_opponent_rating, average_points);

    let new_rating = performance.mul_add(results.len() as f64, player.rating * development)
        / (results.len() as f64 + development);

    IngoRating {
        rating: new_rating,
        age: player.age,
    }
}

#[must_use]
/// Calculates the expected outcome of two players based on Ingo.
///
/// Takes in two players as [`IngoRating`]s and returns the probability of victory for each player as an [`f64`] between 1.0 and 0.0.  
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// # Examples
/// ```
/// use skillratings::{ingo::expected_score, outcomes::Outcomes, rating::IngoRating};
///
/// let player_one = IngoRating {
///     rating: 130.0,
///     age: 40,
/// };
/// let player_two = IngoRating {
///     rating: 160.0,
///     age: 40,
/// };
///
/// let (exp1, exp2) = expected_score(&player_one, &player_two);
///
/// assert!(((exp1 * 100.0).round() - 80.0).abs() < f64::EPSILON);
/// assert!(((exp2 * 100.0).round() - 20.0).abs() < f64::EPSILON);
///
/// assert!((exp1 + exp2 - 1.0).abs() < f64::EPSILON);
/// ```
pub fn expected_score(player_one: &IngoRating, player_two: &IngoRating) -> (f64, f64) {
    let exp_one = 0.5 + (player_two.rating - player_one.rating) / 100.0;

    (exp_one, 1.0 - exp_one)
}

fn performance(average_rating: f64, score: f64) -> f64 {
    average_rating - (100.0 * score - 50.0)
}

#[allow(unused_imports)]
mod tests {
    use crate::rating::EloRating;

    use super::*;

    #[test]
    fn test_ingo() {
        let player_one = IngoRating {
            rating: 130.0,
            age: 40,
        };
        let player_two = IngoRating {
            rating: 160.0,
            age: 40,
        };

        let (p1, p2) = ingo(&player_one, &player_two, &Outcomes::WIN);

        assert!((p1.rating.round() - 129.0).abs() < f64::EPSILON);
        assert!((p2.rating.round() - 161.0).abs() < f64::EPSILON);

        let (p1, p2) = ingo(&player_one, &player_two, &Outcomes::LOSS);

        assert!((p1.rating.round() - 134.0).abs() < f64::EPSILON);
        assert!((p2.rating.round() - 156.0).abs() < f64::EPSILON);

        let young_player = IngoRating {
            rating: 230.0,
            age: 7,
        };

        let old_player = IngoRating {
            rating: 109.0,
            age: 78,
        };

        let (yp, op) = ingo(&young_player, &old_player, &Outcomes::DRAW);

        assert!((yp.rating.round() - 219.0).abs() < f64::EPSILON);
        assert!((op.rating.round() - 115.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_ingo_rating_period() {
        let player_one = IngoRating {
            rating: 130.0,
            age: 40,
        };
        let player_two = IngoRating {
            rating: 160.0,
            age: 40,
        };

        let results = vec![(player_two, Outcomes::WIN)];

        let p1 = ingo_rating_period(&player_one, &results);

        assert!((p1.rating.round() - 129.0).abs() < f64::EPSILON);

        let player_two = IngoRating {
            rating: 160.0,
            age: 40,
        };

        let player_three = IngoRating {
            rating: 160.0,
            age: 40,
        };

        let player_four = IngoRating {
            rating: 55.0,
            age: 40,
        };

        let player_five = IngoRating {
            rating: 90.0,
            age: 40,
        };

        let results = vec![
            (player_two, Outcomes::WIN),
            (player_three, Outcomes::DRAW),
            (player_four, Outcomes::WIN),
            (player_five, Outcomes::LOSS),
        ];

        let p1 = ingo_rating_period(&player_one, &results);

        assert!((p1.rating.round() - 126.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_expected_score() {
        let player_one = IngoRating {
            rating: 130.0,
            age: 40,
        };
        let player_two = IngoRating {
            rating: 160.0,
            age: 40,
        };

        let (exp1, exp2) = expected_score(&player_one, &player_two);

        assert!(((exp1 * 100.0).round() - 80.0).abs() < f64::EPSILON);
        assert!(((exp2 * 100.0).round() - 20.0).abs() < f64::EPSILON);

        assert!((exp1 + exp2 - 1.0).abs() < f64::EPSILON);
    }

    #[test]
    fn ingo_elo_conv() {
        let elo_player = EloRating::new();

        let ingo_player = IngoRating::from(elo_player);

        assert!((ingo_player.rating - 230.0).abs() < f64::EPSILON);
        assert_eq!(ingo_player.age, 26);

        let ingo_player = IngoRating::default();

        let elo_player = EloRating::from(ingo_player);

        assert!((elo_player.rating - 1000.0).abs() < f64::EPSILON);
    }
}
