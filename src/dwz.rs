use std::collections::HashMap;

use crate::{outcomes::Outcomes, rating::DWZRating};

#[must_use]
/// Calculates the DWZ (Deutsche Wertungszahl) ratings of two players based on their ratings, index, age and outcome of the game.
///
/// Takes in two players and the outcome of the game.
///
/// Instead of the traditional way of calculating the DWZ for only one player only using a list of results,
/// we are calculating the DWZ rating for two players at once, like in the Elo calculation,
/// to make it easier to see instant results.
///
/// The outcome of the match is in the perspective of `player_one`.
/// This means `Outcomes::WIN` is a win for `player_one` and `Outcomes::LOSS` is a win for `player_two`.
///
/// # Example
/// ```
/// use skillratings::{dwz::dwz, outcomes::Outcomes, rating::DWZRating};
///
/// let player_one = DWZRating {
///     rating: 1500.0,
///     index: 42,
///     age: 42,
/// };
/// let player_two = DWZRating {
///     rating: 1500.0,
///     index: 12,
///     age: 12,
/// };
///
/// let outcome = Outcomes::WIN;
///
/// let (player_one_new, player_two_new) = dwz(player_one, player_two, outcome);
///
/// assert!((player_one_new.rating.round() - 1519.0).abs() < f64::EPSILON);
/// assert_eq!(player_one_new.index, 43);
///
/// assert!((player_two_new.rating.round() - 1464.0).abs() < f64::EPSILON);
/// assert_eq!(player_two_new.index, 13);
/// ```
///
/// # More
/// [Wikipedia Article on DWZ](https://en.wikipedia.org/wiki/Deutsche_Wertungszahl)
pub fn dwz(
    player_one: DWZRating,
    player_two: DWZRating,
    outcome: Outcomes,
) -> (DWZRating, DWZRating) {
    let outcome1 = match outcome {
        Outcomes::WIN => 1.0,
        Outcomes::DRAW => 0.5,
        Outcomes::LOSS => 0.0,
    };
    let outcome2 = 1.0 - outcome1;

    let (exp1, exp2) = expected_score(player_one, player_two);

    let r1 = new_rating(
        player_one.rating,
        e_value(
            player_one.rating,
            player_one.age,
            outcome1,
            exp1,
            player_one.index,
        ),
        outcome1,
        exp1,
    );
    let r2 = new_rating(
        player_two.rating,
        e_value(
            player_two.rating,
            player_two.age,
            outcome2,
            exp2,
            player_two.index,
        ),
        outcome2,
        exp2,
    );

    (
        DWZRating {
            rating: r1,
            index: player_one.index + 1,
            age: player_one.age,
        },
        {
            DWZRating {
                rating: r2,
                index: player_two.index + 1,
                age: player_two.age,
            }
        },
    )
}

#[must_use]
/// Calculates the expected outcome of two players based on DWZ.
///
/// Takes in two players and returns the probability of victory for each player.  
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// # Example
/// ```
/// use skillratings::{dwz::expected_score, rating::DWZRating};
///
/// let player_one = DWZRating {
///     rating: 1900.0,
///     index: 42,
///     age: 42,
/// };
/// let player_two = DWZRating {
///     rating: 1500.0,
///     index: 12,
///     age: 12,
/// };
///
/// let (exp_one, exp_two) = expected_score(player_one, player_two);
///
///
/// assert!(((exp_one * 100.0).round() - 91.0).abs() < f64::EPSILON);
/// assert!(((exp_two * 100.0).round() - 9.0).abs() < f64::EPSILON);
/// ```
pub fn expected_score(player_one: DWZRating, player_two: DWZRating) -> (f64, f64) {
    (
        (1.0 + 10.0_f64.powf((player_two.rating - player_one.rating) / 400.0)).recip(),
        (1.0 + 10.0_f64.powf((player_one.rating - player_two.rating) / 400.0)).recip(),
    )
}

#[allow(clippy::as_conversions, clippy::cast_precision_loss)]
#[must_use]
/// Gets a proper first DWZ rating.
///
/// If you do not have enough opponents, and have an [`crate::rating::EloRating`]
/// consider using `DWZRating::from(EloRating { ... })`.
///
/// Takes in the player's age and their results as a Vec of tuples containing the opponent and the outcome.
/// If the actual player's age is unavailable or unknown, choose something `>25`.
///
/// This only returns a DWZ rating if the results include at least 5 matches,
/// and you dont have a 100% or a 0% win record. Otherwise it will return `None`.
///
/// # Example
/// ```
/// use skillratings::{dwz::get_first_dwz, outcomes::Outcomes, rating::DWZRating};
///
/// let o1 = DWZRating {
///     rating: 1300.0,
///     index: 23,
///     age: 17,
/// };
/// let o2 = DWZRating {
///     rating: 1540.0,
///     index: 2,
///     age: 29,
/// };
/// let o3 = DWZRating {
///     rating: 1200.0,
///     index: 10,
///     age: 7,
/// };
/// let o4 = DWZRating {
///     rating: 1290.0,
///     index: 76,
///     age: 55,
/// };
/// let o5 = DWZRating {
///     rating: 1400.0,
///     index: 103,
///     age: 11,
/// };
///
/// let player = get_first_dwz(
///     26,
///     &vec![
///         (o1, Outcomes::WIN),
///         (o2, Outcomes::DRAW),
///         (o3, Outcomes::LOSS),
///         (o4, Outcomes::WIN),
///         (o5, Outcomes::WIN),
///     ],
/// )
/// .unwrap();
///
/// assert!((player.rating - 1491.0).abs() < f64::EPSILON);
/// assert_eq!(player.index, 1);
/// ```
///
/// # More
/// [Probability Table](https://www.schachbund.de/wertungsordnung-anhang-2-tabellen/articles/wertungsordnung-anhang-21-wahrscheinlichkeitstabelle.html)
pub fn get_first_dwz(player_age: usize, results: &Vec<(DWZRating, Outcomes)>) -> Option<DWZRating> {
    if results.len() < 5 {
        return None;
    }

    let mut points: f64 = 0.0;

    for r in results {
        if r.1 == Outcomes::WIN {
            points += 1.0;
        } else if r.1 == Outcomes::DRAW {
            points += 0.5;
        }
    }

    if (points - results.len() as f64).abs() < f64::EPSILON || points == 0.0 {
        return None;
    }

    let mut cumulative_ratings = 0.0;

    for r in results {
        cumulative_ratings += r.0.rating;
    }

    let rc = cumulative_ratings / results.len() as f64;

    #[allow(clippy::cast_possible_truncation)]
    let p = ((points / results.len() as f64) * 100.0).round() as i64;

    // We need to look up the points value in a lookup table:
    // <https://www.schachbund.de/wertungsordnung-anhang-2-tabellen/articles/wertungsordnung-anhang-21-wahrscheinlichkeitstabelle.html>
    // There seems to be no real way to solve this in a better way, sorry.
    // At least we only need one half of this table.
    let probability_table = HashMap::from([
        (0, -728.),
        (1, -614.),
        (2, -555.),
        (3, -513.),
        (4, -480.),
        (5, -453.),
        (6, -429.),
        (7, -408.),
        (8, -389.),
        (9, -371.),
        (10, -355.),
        (11, -340.),
        (12, -326.),
        (13, -312.),
        (14, -300.),
        (15, -288.),
        (16, -276.),
        (17, -265.),
        (18, -254.),
        (19, -244.),
        (20, -234.),
        (21, -224.),
        (22, -214.),
        (23, -205.),
        (24, -196.),
        (25, -187.),
        (26, -178.),
        (27, -170.),
        (28, -161.),
        (29, -153.),
        (30, -145.),
        (31, -137.),
        (32, -129.),
        (33, -121.),
        (34, -113.),
        (35, -106.),
        (36, -98.),
        (37, -91.),
        (38, -83.),
        (39, -76.),
        (40, -69.),
        (41, -61.),
        (42, -54.),
        (43, -47.),
        (44, -40.),
        (45, -32.),
        (46, -25.),
        (47, -18.),
        (48, -11.),
        (49, -4.),
        (50, -0.),
    ]);

    let mut new_rating = if p > 50 {
        let temp = probability_table.get(&(p - 100).abs()).unwrap_or(&0.0);

        f64::abs(*temp) + rc
    } else {
        probability_table.get(&p).unwrap_or(&0.0) + rc
    };

    if new_rating <= 800.0 {
        new_rating = 700.0 + (new_rating / 8.0);
    }

    Some(DWZRating {
        rating: new_rating,
        index: 1,
        age: player_age,
    })
}

/// Getting the fundamental e0 value.
///
/// The variable j is dependent on the age of the player. From wikipedia:  
///
/// "Teenagers up to 20 years: `j = 5.0`, junior adults (21 – 25 years): `j = 10.0`, over-25-year-olds: `j = 15.0`"
fn e0_value(rating: f64, j: f64) -> f64 {
    (rating / 1000.0).powi(4) + j
}

fn e_value(rating: f64, age: usize, score: f64, expected_score: f64, index: usize) -> f64 {
    let j = match age {
        usize::MIN..=20 => 5.0,
        21..=25 => 10.0,
        _ => 15.0,
    };

    let e0 = e0_value(rating, j);

    let a = if age < 20 && score >= expected_score {
        rating / 2000.0
    } else {
        1.0
    };

    let b = if rating < 1300.0 && score <= expected_score {
        ((1300.0 - rating) / 150.0_f64).exp_m1()
    } else {
        0.0
    };

    let mut e = a.mul_add(e0, b);

    if e <= 5.0 {
        e = 5.0;
    } else if b == 0.0 {
        #[allow(clippy::cast_precision_loss, clippy::as_conversions)]
        if e >= 30.0_f64.min(5.0 * index as f64) {
            e = 30.0_f64.min(5.0 * index as f64);
        }
    } else if e >= 150.0 {
        e = 150.0;
    }

    e
}

fn new_rating(old_rating: f64, e: f64, score: f64, expected_score: f64) -> f64 {
    (800.0 / (e + 1.0)).mul_add(score - expected_score, old_rating)
}

mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    /// The results here have been double-checked with the unofficial DWZ Calculator: <http://www.wertungszahl.de/>.
    fn test_dwz() {
        let mut player_one = DWZRating {
            rating: 1530.0,
            index: 22,
            age: 26,
        };

        let mut player_two = DWZRating {
            rating: 1930.0,
            index: 103,
            age: 39,
        };

        (player_one, player_two) = dwz(player_one, player_two, Outcomes::WIN);

        assert!((player_one.rating.round() - 1564.0).abs() < f64::EPSILON);
        assert_eq!(player_one.index, 23);

        assert!((player_two.rating.round() - 1906.0).abs() < f64::EPSILON);
        assert_eq!(player_two.index, 104);

        (player_one, player_two) = dwz(player_one, player_two, Outcomes::DRAW);

        assert!((player_one.rating.round() - 1578.0).abs() < f64::EPSILON);
        assert_eq!(player_one.index, 24);

        assert!((player_two.rating.round() - 1895.0).abs() < f64::EPSILON);
        assert_eq!(player_two.index, 105);

        player_two.age = 12;

        (player_one, player_two) = dwz(player_one, player_two, Outcomes::LOSS);

        assert!((player_one.rating.round() - 1573.0).abs() < f64::EPSILON);
        assert_eq!(player_one.index, 25);

        assert!((player_two.rating.round() - 1901.0).abs() < f64::EPSILON);
        assert_eq!(player_two.index, 106);
    }

    #[test]
    fn test_large_delta() {
        let mut really_good_player = DWZRating {
            rating: 3210.0,
            index: 143,
            age: 25,
        };

        let mut really_bad_player = DWZRating {
            rating: 90.0,
            index: 1,
            age: 12,
        };

        (really_good_player, really_bad_player) =
            dwz(really_good_player, really_bad_player, Outcomes::WIN);

        assert!((really_good_player.rating.round() - 3210.0).abs() < f64::EPSILON);
        assert_eq!(really_good_player.index, 144);

        assert!((really_bad_player.rating.round() - 90.0).abs() < f64::EPSILON);
        assert_eq!(really_bad_player.index, 2);

        really_bad_player.rating = 1.0;
        really_good_player.rating = 32_477_324_874_238.0;

        (really_good_player, really_bad_player) =
            dwz(really_good_player, really_bad_player, Outcomes::WIN);

        assert!((really_good_player.rating.round() - 32_477_324_874_238.0).abs() < f64::EPSILON);

        assert!((really_bad_player.rating.round() - 1.0).abs() < f64::EPSILON);

        really_good_player.rating = 2.0;
        really_good_player.age = 5;

        really_bad_player.rating = 1.0;
        really_bad_player.age = 5;

        (really_good_player, really_bad_player) =
            dwz(really_good_player, really_bad_player, Outcomes::LOSS);

        assert!((really_good_player.rating.round() + 1.0).abs() < f64::EPSILON);
        assert!((really_bad_player.rating.round() - 68.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_expected_score() {
        let player_one = DWZRating {
            rating: 1530.0,
            index: 22,
            age: 26,
        };

        let player_two = DWZRating {
            rating: 1930.0,
            index: 103,
            age: 39,
        };

        let (exp1, exp2) = expected_score(player_one, player_two);

        assert!(((exp1 * 100.0).round() - 9.0).abs() < f64::EPSILON);
        assert!(((exp2 * 100.0).round() - 91.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_first_dwz() {
        let o1 = DWZRating {
            rating: 1300.0,
            index: 23,
            age: 17,
        };

        let o2 = DWZRating {
            rating: 1540.0,
            index: 2,
            age: 29,
        };

        let o3 = DWZRating {
            rating: 1200.0,
            index: 10,
            age: 7,
        };

        let o4 = DWZRating {
            rating: 1290.0,
            index: 76,
            age: 55,
        };

        let o5 = DWZRating {
            rating: 1400.0,
            index: 103,
            age: 11,
        };

        #[allow(clippy::unwrap_used)]
        let player = get_first_dwz(
            26,
            &vec![
                (o1, Outcomes::WIN),
                (o2, Outcomes::DRAW),
                (o3, Outcomes::LOSS),
                (o4, Outcomes::WIN),
                (o5, Outcomes::WIN),
            ],
        )
        .unwrap();

        assert!((player.rating - 1491.0).abs() < f64::EPSILON);
        assert_eq!(player.index, 1);

        let all_win_player = get_first_dwz(
            17,
            &vec![
                (o1, Outcomes::WIN),
                (o2, Outcomes::WIN),
                (o3, Outcomes::WIN),
                (o4, Outcomes::WIN),
                (o5, Outcomes::WIN),
            ],
        );

        assert_eq!(all_win_player, None);

        let all_lose_player = get_first_dwz(
            17,
            &vec![
                (o1, Outcomes::LOSS),
                (o2, Outcomes::LOSS),
                (o3, Outcomes::LOSS),
                (o4, Outcomes::LOSS),
                (o5, Outcomes::LOSS),
            ],
        );

        assert_eq!(all_lose_player, None);

        let less_than_5 = get_first_dwz(
            32,
            &vec![
                (o1, Outcomes::LOSS),
                (o2, Outcomes::WIN),
                (o3, Outcomes::DRAW),
                (o4, Outcomes::LOSS),
            ],
        );

        assert_eq!(less_than_5, None);
    }

    #[test]
    fn test_new_dwz_bad_players() {
        let o1 = DWZRating {
            rating: 1300.0,
            index: 23,
            age: 17,
        };

        let o2 = DWZRating {
            rating: 1540.0,
            index: 2,
            age: 29,
        };

        let o3 = DWZRating {
            rating: 1200.0,
            index: 10,
            age: 7,
        };

        let o4 = DWZRating {
            rating: 1290.0,
            index: 76,
            age: 55,
        };

        let o5 = DWZRating {
            rating: 1400.0,
            index: 103,
            age: 11,
        };

        #[allow(clippy::unwrap_used)]
        let bad_player = get_first_dwz(
            26,
            &vec![
                (o1, Outcomes::LOSS),
                (o2, Outcomes::DRAW),
                (o3, Outcomes::LOSS),
                (o4, Outcomes::LOSS),
                (o5, Outcomes::LOSS),
            ],
        )
        .unwrap();

        assert!((bad_player.rating.round() - 991.0).abs() < f64::EPSILON);
        assert_eq!(bad_player.index, 1);

        let o4 = DWZRating {
            rating: 430.0,
            index: 76,
            age: 55,
        };

        let o5 = DWZRating {
            rating: 520.0,
            index: 103,
            age: 11,
        };

        #[allow(clippy::unwrap_used)]
        let really_bad_player = get_first_dwz(
            26,
            &vec![
                (o1, Outcomes::LOSS),
                (o2, Outcomes::DRAW),
                (o3, Outcomes::LOSS),
                (o4, Outcomes::LOSS),
                (o5, Outcomes::LOSS),
                (o3, Outcomes::LOSS),
                (o4, Outcomes::LOSS),
                (o5, Outcomes::LOSS),
                (o3, Outcomes::LOSS),
                (o4, Outcomes::LOSS),
                (o5, Outcomes::LOSS),
                (o4, Outcomes::LOSS),
                (o5, Outcomes::LOSS),
                (o4, Outcomes::LOSS),
                (o5, Outcomes::LOSS),
                (o4, Outcomes::LOSS),
                (o5, Outcomes::LOSS),
                (o4, Outcomes::LOSS),
                (o5, Outcomes::LOSS),
            ],
        )
        .unwrap();

        assert!((really_bad_player.rating.round() - 722.0).abs() < f64::EPSILON);
        assert_eq!(really_bad_player.index, 1);
    }

    #[test]
    fn test_from_elo() {
        use crate::rating::EloRating;

        let player_one = EloRating { rating: 1200.0 };

        let player_one_dwz = DWZRating::from(player_one);

        assert!((player_one_dwz.rating.round() - 1200.0).abs() < f64::EPSILON);
        assert_eq!(player_one_dwz.index, 6);
        assert_eq!(player_one_dwz.age, 26);
    }
}
