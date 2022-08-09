use crate::{outcomes::Outcomes, rating::GlickoRating};
use std::f64::consts::PI;

/// Calculates the glicko-2 scores of two players based on their ratings, deviations, and the outcome of the game.
///
/// Takes in two players, the outcome of the game and a tau constant.
///
/// The outcome of the match is in the perspective of player_one.
/// This means `Outcomes::WIN` is a win for player_one and `Outcomes::LOSS` is a win for player_two.
///
/// The tau constant constrains the change in volatility over time.
/// To cite Mark Glickman himself: "Reasonable choices are between 0.3 and 1.2".
/// Smaller values mean less change in volatility and vice versa.
/// The most common choice seems to be 0.5.  
/// In any case, pick one value and **stick to your choice** in your system's calculations.
///
/// # Example
/// ```
/// use skillratings;
///
/// let player_one = skillratings::rating::GlickoRating { rating: 1500.0, deviation: 350.0, volatility: 0.06 };
/// let player_two = skillratings::rating::GlickoRating { rating: 1500.0, deviation: 350.0, volatility: 0.06 };
///
/// let outcome = skillratings::outcomes::Outcomes::WIN;
///
/// let (player_one_new, player_two_new) = skillratings::glicko2::glicko2(player_one, player_two, outcome, 0.5);
///
/// assert_eq!(player_one_new.rating.round(), 1662.0);
/// assert_eq!(player_one_new.deviation.round(), 290.0);
/// assert_eq!(player_one_new.volatility, 0.05999578094735206);
///
/// assert_eq!(player_two_new.rating.round(), 1338.0);
/// assert_eq!(player_two_new.deviation.round(), 290.0);
/// assert_eq!(player_two_new.volatility, 0.05999578094735206);
/// ```
///
/// # More
/// [Wikipedia Article on the Glicko-2 system](https://en.wikipedia.org/wiki/Glicko-2).
/// [Example of the Glicko-2 system](http://www.glicko.net/glicko/glicko2.pdf).
pub fn glicko2(
    player_one: GlickoRating,
    player_two: GlickoRating,
    outcome: Outcomes,
    tau: f64,
) -> (GlickoRating, GlickoRating) {
    // First we need to convert the ratings into the glicko-2 scale.
    let player_one_rating = (player_one.rating - 1500.0) / 173.7178;
    let player_two_rating = (player_two.rating - 1500.0) / 173.7178;

    // Same with the deviation.
    let player_one_deviation = player_one.deviation / 173.7178;
    let player_two_deviation = player_two.deviation / 173.7178;

    let outcome1 = match outcome {
        Outcomes::WIN => 1.0,
        Outcomes::DRAW => 0.5,
        Outcomes::LOSS => 0.0,
    };
    let outcome2 = 1.0 - outcome1;

    // We always need the deviation of the opponent in the g function.
    let g1 = g_value(player_two_deviation);
    let g2 = g_value(player_one_deviation);

    let e1 = e_value(player_one_rating, player_two_rating, g1);
    let e2 = e_value(player_two_rating, player_one_rating, g2);

    let v1 = v_value(g1, e1);
    let v2 = v_value(g2, e2);

    let player_one_new_volatility = new_volatility(
        player_one.volatility,
        delta_value(outcome1, v1, g1, e1),
        player_one_deviation,
        v1,
        tau,
    );
    let player_two_new_volatility = new_volatility(
        player_two.volatility,
        delta_value(outcome2, v2, g2, e2),
        player_two_deviation,
        v2,
        tau,
    );

    let new_deviation1 = new_deviation(
        new_pre_deviation(player_one_deviation, player_one_new_volatility),
        v1,
    );
    let new_deviation2 = new_deviation(
        new_pre_deviation(player_two_deviation, player_two_new_volatility),
        v2,
    );

    let new_rating1 = new_rating(player_one_rating, new_deviation1, outcome1, g1, e1);
    let new_rating2 = new_rating(player_two_rating, new_deviation2, outcome2, g2, e2);

    let player_one_new = GlickoRating {
        rating: (new_rating1 * 173.7178) + 1500.0,
        deviation: new_deviation1 * 173.7178,
        volatility: player_one_new_volatility,
    };
    let player_two_new = GlickoRating {
        rating: (new_rating2 * 173.7178) + 1500.0,
        deviation: new_deviation2 * 173.7178,
        volatility: player_two_new_volatility,
    };

    (player_one_new, player_two_new)
}

/// Calculates the expected outcome of two players based on glicko-2, assuming no draws.
///
/// Takes in two players and returns the probability of victory for each player,
/// again, without taking draws into calculation.  
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// # Example
/// ```
/// use skillratings;
///
/// let player_one = skillratings::rating::GlickoRating { rating: 2500.0, deviation: 41.0, volatility: 0.06 };
/// let player_two = skillratings::rating::GlickoRating { rating: 1950.0, deviation: 320.0, volatility: 0.06 };
///
/// let (exp_one, exp_two) = skillratings::glicko2::expected_score(player_one, player_two);
///
/// assert_eq!((exp_one * 100.0).round(), 90.0);
/// assert_eq!((exp_two * 100.0).round(), 10.0);
/// ```
pub fn expected_score(player_one: GlickoRating, player_two: GlickoRating) -> (f64, f64) {
    // First we need to convert the ratings into the glicko-2 scale.
    let player_one_rating = (player_one.rating - 1500.0) / 173.7178;
    let player_two_rating = (player_two.rating - 1500.0) / 173.7178;

    // Same with the deviation.
    let player_one_deviation = player_one.deviation / 173.7178;
    let player_two_deviation = player_two.deviation / 173.7178;

    let a1 = a_value(
        player_one_deviation,
        player_two_deviation,
        player_one_rating,
        player_two_rating,
    );
    let a2 = a_value(
        player_two_deviation,
        player_one_deviation,
        player_two_rating,
        player_one_rating,
    );

    (
        (1.0 + (-1.0 * a1).exp()).recip(),
        (1.0 + (-1.0 * a2).exp()).recip(),
    )
}

/// The g value of the glicko-2 calculation.
/// For more information, see: http://www.glicko.net/glicko/glicko2.pdf
fn g_value(deviation: f64) -> f64 {
    (1.0 + ((3.0 * deviation.powf(2.0)) / (PI.powf(2.0))))
        .sqrt()
        .recip()
}

/// The E value of the glicko-2 calculation.
/// For more information, see: http://www.glicko.net/glicko/glicko2.pdf
fn e_value(rating: f64, opponent_rating: f64, g: f64) -> f64 {
    (1.0 + (-1.0 * g * (rating - opponent_rating)).exp()).recip()
}

/// The v value of the glicko-2 calculation.
/// For more information, see: http://www.glicko.net/glicko/glicko2.pdf
fn v_value(g: f64, e: f64) -> f64 {
    (g.powf(2.0) * e * (1.0 - e)).recip()
}

/// The ∆ value of the glicko-2 calculation.
/// For more information, see: http://www.glicko.net/glicko/glicko2.pdf
fn delta_value(outcome: f64, v: f64, g: f64, e: f64) -> f64 {
    v * (g * (outcome - e))
}

/// The f(x) value of the glicko-2 calculation.
/// For more information, see: http://www.glicko.net/glicko/glicko2.pdf
fn f_value(
    x: f64,
    delta_square: f64,
    deviation_square: f64,
    v: f64,
    volatility: f64,
    tau: f64,
) -> f64 {
    let one = {
        let i = x.exp() * (delta_square - deviation_square - v - x.exp());
        let j = 2.0 * (deviation_square + v + x.exp());
        i / j
    };

    let two = {
        let i = x - volatility.powf(2.0).ln();
        let j = tau.powf(2.0);
        i / j
    };

    one - two
}

/// The A value of the expected_outcome function, based on glicko-2,
/// slightly modified to produce an expected outcome score.  
/// Not found in the original paper.
fn a_value(deviation: f64, opponent_deviation: f64, rating: f64, opponent_rating: f64) -> f64 {
    g_value((opponent_deviation.powf(2.0) + deviation.powf(2.0)).sqrt())
        * (rating - opponent_rating)
}

/// The σ' value of the glicko-2 calculation.
/// For more information, see: http://www.glicko.net/glicko/glicko2.pdf
fn new_volatility(old_volatility: f64, delta: f64, deviation: f64, v: f64, tau: f64) -> f64 {
    let mut a = old_volatility.powf(2.0).ln();
    let delta_squared = delta.powf(2.0);
    let deviation_squared = deviation.powf(2.0);
    let mut b = if delta_squared > deviation_squared + v {
        delta_squared - deviation_squared - v
    } else {
        let mut k = 1.0;
        while f_value(
            a - k * tau,
            delta_squared,
            deviation,
            v,
            old_volatility,
            tau,
        ) < 0.0
        {
            k += 1.0;
        }
        a - k * tau
    };

    let mut fa = f_value(a, delta_squared, deviation_squared, v, old_volatility, tau);
    let mut fb = f_value(b, delta_squared, deviation_squared, v, old_volatility, tau);

    // 0.000001 is the convergence tolerance suggested by Mark Glickman.
    while (b - a).abs() > 0.000001 {
        let c = a + ((a - b) * fa / (fb - fa));
        let fc = f_value(c, delta_squared, deviation_squared, v, old_volatility, tau);

        if fc * fb <= 0.0 {
            a = b;
            fa = fb;
        } else {
            fa /= 2.0;
        }

        b = c;
        fb = fc;
    }

    (a / 2.0).exp()
}

/// The φ* value of the glicko-2 calculation.
/// For more information, see: http://www.glicko.net/glicko/glicko2.pdf
fn new_pre_deviation(deviation: f64, new_volatility: f64) -> f64 {
    (deviation.powf(2.0) + new_volatility.powf(2.0)).sqrt()
}

/// The φ' value of the glicko-2 calculation.
/// For more information, see: http://www.glicko.net/glicko/glicko2.pdf
fn new_deviation(pre_deviation: f64, v: f64) -> f64 {
    ((pre_deviation.powf(2.0).recip()) + (v.recip()))
        .sqrt()
        .recip()
}

/// The µ' value of the glicko-2 calculation.
/// For more information, see: http://www.glicko.net/glicko/glicko2.pdf
fn new_rating(rating: f64, new_deviation: f64, outcome: f64, g_value: f64, e_value: f64) -> f64 {
    rating + (new_deviation.powf(2.0) * g_value * (outcome - e_value))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::outcomes::Outcomes;

    #[test]
    fn test_equal_glicko2() {
        let player1 = GlickoRating {
            rating: 1520.0,
            deviation: 350.0,
            volatility: 0.06,
        };

        let player2 = GlickoRating {
            rating: 1420.0,
            deviation: 350.0,
            volatility: 0.06,
        };

        let (player1new, player2new) = glicko2(player1, player2, Outcomes::WIN, 0.5);

        assert_eq!(player1new.rating.round(), 1653.0);
        assert_eq!(player1new.deviation.round(), 292.0);

        assert_eq!(player2new.rating.round(), 1287.0);
        assert_eq!(player2new.deviation.round(), 292.0);
    }

    #[test]
    fn not_equal_deviation_draw() {
        let player1 = GlickoRating {
            rating: 1600.0,
            deviation: 350.0,
            volatility: 0.06,
        };

        let player2 = GlickoRating {
            rating: 1500.0,
            deviation: 50.0,
            volatility: 0.06,
        };

        let (player1new, player2new) = glicko2(player1, player2, Outcomes::DRAW, 0.5);

        assert_eq!(player1new.rating.round(), 1550.0);
        assert_eq!(player1new.deviation.round(), 253.0);

        assert_eq!(player2new.rating.round(), 1501.0);
        assert_eq!(player2new.deviation.round(), 51.0);
    }

    #[test]
    /// This test is taken directly from the official glicko2 example.
    /// http://www.glicko.net/glicko/glicko2.pdf
    fn test_glicko2() {
        let player = GlickoRating {
            rating: 1500.0,
            deviation: 200.0,
            volatility: 0.06,
        };

        let opponent_one = GlickoRating {
            rating: 1400.0,
            deviation: 30.0,
            volatility: 0.06,
        };

        let (player, opponent_one) = glicko2(player, opponent_one, Outcomes::WIN, 0.5);

        assert_eq!(player.rating.round(), 1564.0);
        assert_eq!(player.deviation.round(), 175.0);

        assert_eq!(opponent_one.rating.round(), 1398.0);
        assert_eq!(opponent_one.deviation.round(), 32.0);

        let opponent_two = GlickoRating {
            rating: 1550.0,
            deviation: 100.0,
            volatility: 0.06,
        };

        let (player, _) = glicko2(player, opponent_two, Outcomes::LOSS, 0.5);

        let opponent_three = GlickoRating {
            rating: 1700.0,
            deviation: 300.0,
            volatility: 0.06,
        };

        let (player, _) = glicko2(player, opponent_three, Outcomes::LOSS, 0.5);

        assert_eq!(player.rating.round(), 1464.0);
        assert_eq!(player.deviation.round(), 152.0);
        assert_eq!(player.volatility, 0.059982355058921626);
    }

    #[test]
    fn test_expected_score() {
        let player_one = GlickoRating {
            rating: 1500.0,
            deviation: 350.0,
            volatility: 0.06,
        };

        let player_two = GlickoRating {
            rating: 1500.0,
            deviation: 350.0,
            volatility: 0.06,
        };

        let (exp_one, exp_two) = expected_score(player_one, player_two);

        assert_eq!(exp_one * 100.0, 50.0);
        assert_eq!(exp_two * 100.0, 50.0);

        let player_three = GlickoRating {
            rating: 2000.0,
            deviation: 50.0,
            volatility: 0.06,
        };

        let player_four = GlickoRating {
            rating: 1780.0,
            deviation: 150.0,
            volatility: 0.06,
        };

        let (exp_three, exp_four) = expected_score(player_three, player_four);

        assert_eq!((exp_three * 100.0).round(), 76.0);
        assert_eq!((exp_four * 100.0).round(), 24.0);
    }
}
