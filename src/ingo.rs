use crate::{outcomes::Outcomes, rating::IngoRating};

#[must_use]
/// Calculates the Ingo ratings of two players based on their ratings, and the outcome of the game.
///
/// Takes in two players and the outcome of the game.
///
/// Instead of the traditional way of calculating the Ingo for only one player only using a list of results,
/// we are calculating the Ingo rating for two players at once, like in the Elo calculation,
/// to make it easier to see instant results.
///
/// The outcome of the match is in the perspective of `player_one`.
/// This means `Outcomes::WIN` is a win for `player_one` and `Outcomes::LOSS` is a win for `player_two`.
///
/// A lower Ingo rating is more desirable, and while negative values are possible,
/// a player with an Ingo rating of 0 has an equivalent Elo rating of 2840.
///
/// # Example
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
/// let (p1, p2) = ingo(player_one, player_two, Outcomes::WIN);
///
/// assert!((p1.rating.round() - 129.0).abs() < f64::EPSILON);
/// assert!((p2.rating.round() - 161.0).abs() < f64::EPSILON);
/// ```
///
/// # More:
/// [Wikipedia Article on the Ingo system (in german, no english version available)](https://de.wikipedia.org/wiki/Ingo-Zahl).
pub fn ingo(
    player_one: IngoRating,
    player_two: IngoRating,
    outcome: Outcomes,
) -> (IngoRating, IngoRating) {
    let score1 = match outcome {
        Outcomes::WIN => 1.0,
        Outcomes::DRAW => 0.5,
        Outcomes::LOSS => 0.0,
    };

    let score2 = 1.0 - score1;

    let perf1 = performance(player_two, score1);
    let perf2 = performance(player_one, score2);

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

#[must_use]
/// Calculates the expected outcome of two players based on Ingo.
///
/// Takes in two players and returns the probability of victory for each player.  
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// # Example
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
/// let (exp1, exp2) = expected_score(player_one, player_two);
///
/// assert!(((exp1 * 100.0).round() - 80.0).abs() < f64::EPSILON);
/// assert!(((exp2 * 100.0).round() - 20.0).abs() < f64::EPSILON);
///
/// assert!((exp1 + exp2 - 1.0).abs() < f64::EPSILON);
/// ```
pub fn expected_score(player_one: IngoRating, player_two: IngoRating) -> (f64, f64) {
    let exp_one = 0.5 + (player_two.rating - player_one.rating) / 100.0;

    (exp_one, 1.0 - exp_one)
}

fn performance(opponent: IngoRating, score: f64) -> f64 {
    opponent.rating - (100.0 * score - 50.0)
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

        let (p1, p2) = ingo(player_one, player_two, Outcomes::WIN);

        assert!((p1.rating.round() - 129.0).abs() < f64::EPSILON);
        assert!((p2.rating.round() - 161.0).abs() < f64::EPSILON);

        let (p1, p2) = ingo(player_one, player_two, Outcomes::LOSS);

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

        let (yp, op) = ingo(young_player, old_player, Outcomes::DRAW);

        assert!((yp.rating.round() - 219.0).abs() < f64::EPSILON);
        assert!((op.rating.round() - 115.0).abs() < f64::EPSILON);
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

        let (exp1, exp2) = expected_score(player_one, player_two);

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
