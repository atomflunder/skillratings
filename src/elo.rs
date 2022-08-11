use crate::outcomes::Outcomes;
use crate::rating::EloRating;

/// Calculates the elo scores of two players based on their ratings and the outcome of the game.
///
/// Takes in two players, the outcome of the game and the k-value.
///
/// The outcome of the match is in the perspective of player_one.
/// This means `Outcomes::WIN` is a win for player_one and `Outcomes::LOSS` is a win for player_two.
///
/// The k-value is the maximum amount of rating change from a single match.
/// In chess, k-values from 40 to 10 are used, with the most common being 32, 24 or 16.
/// The higher the number, the more volatile the ranking.
///
/// # Example
/// ```
/// use skillratings::{elo::elo, outcomes::Outcomes, rating::EloRating};
///
/// let player_one = EloRating { rating: 1000.0 };
/// let player_two = EloRating { rating: 1000.0 };
///
/// let outcome = Outcomes::WIN;
///
/// let (player_one_new, player_two_new) = elo(player_one, player_two, outcome, 32.0);
///
/// assert_eq!(player_one_new.rating, 1016.0);
/// assert_eq!(player_two_new.rating, 984.0);
/// ```
///
/// # More
/// [Wikipedia Article on the Elo system](https://en.wikipedia.org/wiki/Elo_rating_system).
pub fn elo(
    player_one: EloRating,
    player_two: EloRating,
    outcome: Outcomes,
    k: f64,
) -> (EloRating, EloRating) {
    let (one_expected, two_expected) = expected_score(player_one, player_two);

    let o = match outcome {
        Outcomes::WIN => 1.0,
        Outcomes::LOSS => 0.0,
        Outcomes::DRAW => 0.5,
    };

    let one_new_elo = player_one.rating + k * (o - one_expected);
    let two_new_elo = player_two.rating + k * ((1.0 - o) - two_expected);

    (
        EloRating {
            rating: one_new_elo,
        },
        EloRating {
            rating: two_new_elo,
        },
    )
}

/// Calculates the expected score of two players based on their elo rating.
/// Meant for usage in the elo function, but you can also use it to predict games yourself.
///
/// Takes in two elo scores and returns the expected score of each player.
/// A score of 1.0 means certain win, a score of 0.0 means certain loss, and a score of 0.5 is a draw.
///
/// #Example
/// ```
/// use skillratings::{elo::expected_score, rating::EloRating};
///
/// let player_one = EloRating { rating: 1320.0 };
/// let player_two = EloRating { rating: 1217.0 };
///
/// let (winner_exp, loser_exp) = expected_score(player_one, player_two);
///
/// assert_eq!((winner_exp * 100.0).round(), 64.0);
/// assert_eq!((loser_exp * 100.0).round(), 36.0);
/// ```
pub fn expected_score(player_one: EloRating, player_two: EloRating) -> (f64, f64) {
    (
        1.0 / (1.0 + 10_f64.powf((player_two.rating - player_one.rating) / 400.0)),
        1.0 / (1.0 + 10_f64.powf((player_one.rating - player_two.rating) / 400.0)),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_elo() {
        let (winner_new_elo, loser_new_elo) = elo(
            EloRating { rating: 1000.0 },
            EloRating { rating: 1000.0 },
            Outcomes::WIN,
            32.0,
        );
        assert_eq!(winner_new_elo.rating, 1016.0);
        assert_eq!(loser_new_elo.rating, 984.0);

        let (winner_new_elo, loser_new_elo) = elo(
            EloRating { rating: 1000.0 },
            EloRating { rating: 1000.0 },
            Outcomes::LOSS,
            32.0,
        );
        assert_eq!(winner_new_elo.rating, 984.0);
        assert_eq!(loser_new_elo.rating, 1016.0);

        let (winner_new_elo, loser_new_elo) = elo(
            EloRating { rating: 1000.0 },
            EloRating { rating: 1000.0 },
            Outcomes::DRAW,
            32.0,
        );
        assert_eq!(winner_new_elo.rating, 1000.0);
        assert_eq!(loser_new_elo.rating, 1000.0);

        let (winner_new_elo, loser_new_elo) = elo(
            EloRating { rating: 500.0 },
            EloRating { rating: 1500.0 },
            Outcomes::WIN,
            32.0,
        );
        assert_eq!(winner_new_elo.rating.round(), 532.0);
        assert_eq!(loser_new_elo.rating.round(), 1468.0);
    }

    #[test]
    fn test_expected_score() {
        let player_one = EloRating { rating: 1000.0 };
        let player_two = EloRating { rating: 1000.0 };

        let (winner_expected, loser_expected) = expected_score(player_one, player_two);

        assert_eq!(winner_expected, 0.5);
        assert_eq!(loser_expected, 0.5);

        let player_one = EloRating { rating: 2251.0 };
        let player_two = EloRating { rating: 1934.0 };

        let (winner_expected, loser_expected) = expected_score(player_one, player_two);

        assert_eq!((winner_expected * 100.0).round(), 86.0);
        assert_eq!((loser_expected * 100.0).round(), 14.0);

        assert_eq!(winner_expected + loser_expected, 1.0);
    }
}
