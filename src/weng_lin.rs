//! A bayesian approximation method for online ranking. Similar to TrueSkill, but based on a logistical distribution.  
//! Used in games such as Rocket League.
//!
//! Developed by Ruby C. Weng and Chih-Jen Lin.
//! Unlike with the other algorithms, there does not seem to exist a *short* name everyone agrees upon,
//! so we are just calling it Weng-Lin, for short, after the researchers.
//! But the proper name would be `A Bayesian Approximation Method for Online Ranking`.
//!
//! Developed specifically for online games with multiple teams and multiple players,
//! this algorithm aims to be simpler and faster than TrueSkill while yielding similar accuracy.
//!
//! While TrueSkill is based upon a Gaussian distibution, this algorithm is based upon a logistical distribution, the Bradley-Terry model.
//!
//! # More Information
//! - [Original Paper (PDF)](https://jmlr.csail.mit.edu/papers/volume12/weng11a/weng11a.pdf)
//! - [Bradley-Terry model Wikipedia](https://en.wikipedia.org/wiki/Bradley–Terry_model)
//! - [Approximate Bayesian computaion Wikipedia](https://en.wikipedia.org/wiki/Approximate_Bayesian_computation)
//! - [Logistic distribution Wikipedia](https://en.wikipedia.org/wiki/Logistic_distribution)

use crate::{config::WengLinConfig, outcomes::Outcomes, rating::WengLinRating};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
/// Calculates the [`WengLinRating`]s of two players based on their old ratings, uncertainties, and the outcome of the game.
///
/// Takes in two players as [`WengLinRating`]s, an [`Outcome`](Outcomes), and a [`WengLinConfig`].
///
/// The outcome of the match is in the perspective of `player_one`.
/// This means [`Outcomes::WIN`] is a win for `player_one` and [`Outcomes::LOSS`] is a win for `player_two`.
///
/// Similar to [`weng_lin_rating_period`] and [`weng_lin_teams`].
///
/// # Examples
/// ```
/// use skillratings::{
///     rating::WengLinRating, weng_lin::weng_lin, outcomes::Outcomes, config::WengLinConfig
/// };
///
/// let player_one = WengLinRating {
///     rating: 42.0,
///     uncertainty: 1.3,
/// };
/// let player_two = WengLinRating::new();
///
/// let (player_one, player_two) = weng_lin(player_one, player_two, Outcomes::WIN, &WengLinConfig::new());
///
/// assert!(((player_one.rating * 100.0).round() - 4203.0).abs() < f64::EPSILON);
/// assert!(((player_one.uncertainty * 100.0).round() - 130.0).abs() < f64::EPSILON);
/// assert!(((player_two.rating * 100.0).round() - 2391.0).abs() < f64::EPSILON);
/// assert!(((player_two.uncertainty * 100.0).round() - 803.0).abs() < f64::EPSILON);
/// ```
pub fn weng_lin(
    player_one: WengLinRating,
    player_two: WengLinRating,
    outcome: Outcomes,
    config: &WengLinConfig,
) -> (WengLinRating, WengLinRating) {
    let c = 2.0f64
        .mul_add(
            config.beta.powi(2),
            player_one
                .uncertainty
                .mul_add(player_one.uncertainty, player_two.uncertainty.powi(2)),
        )
        .sqrt();

    let (p1, p2) = expected_score(player_one, player_two, config);

    let outcome1 = match outcome {
        Outcomes::WIN => 1.0,
        Outcomes::DRAW => 0.5,
        Outcomes::LOSS => 0.0,
    };

    let outcome2 = match outcome {
        Outcomes::WIN => 0.0,
        Outcomes::DRAW => 0.5,
        Outcomes::LOSS => 1.0,
    };

    let delta1 = (player_one.uncertainty.powi(2) / c) * (outcome1 - p1);
    // You could also set gamma to 1/k, with k being the amount of teams in a match.
    let gamma1 = player_one.uncertainty / c;
    let eta1 = gamma1 * (player_one.uncertainty / c).powi(2) * p1 * p2;

    let delta2 = (player_two.uncertainty.powi(2) / c) * (outcome2 - p2);
    let gamma2 = player_two.uncertainty / c;
    let eta2 = gamma2 * (player_two.uncertainty / c).powi(2) * p2 * p1;

    let new_rating1 = delta1 + player_one.rating;
    let sigma1 = (1.0 - 1.0 * eta1).max(config.uncertainty_tolerance);
    let new_uncertainty1 = (player_one.uncertainty.powi(2) * sigma1).sqrt();

    let new_rating2 = delta2 + player_two.rating;
    let sigma2 = (1.0 - 1.0 * eta2).max(config.uncertainty_tolerance);
    let new_uncertainty2 = (player_two.uncertainty.powi(2) * sigma2).sqrt();

    (
        WengLinRating {
            rating: new_rating1,
            uncertainty: new_uncertainty1,
        },
        WengLinRating {
            rating: new_rating2,
            uncertainty: new_uncertainty2,
        },
    )
}

#[must_use]
/// Calculates a [`WengLinRating`] in a non-traditional way using a rating period,
/// for compatibility with the other algorithms.
///
/// Takes in a player as an [`WengLinRating`] and their results as a Vec of tuples containing the opponent as an [`WengLinRating`],
/// the outcome of the game as an [`Outcome`](Outcomes) and a [`WengLinConfig`].
///
/// The outcome of the match is in the perspective of the player.
/// This means [`Outcomes::WIN`] is a win for the player and [`Outcomes::LOSS`] is a win for the opponent.
///
/// Similar to [`weng_lin`] or [`weng_lin_teams`].
///
/// # Examples
/// ```
/// use skillratings::{
///     rating::WengLinRating, weng_lin::weng_lin_rating_period, outcomes::Outcomes, config::WengLinConfig
/// };
///
/// let player = WengLinRating::new();
///
/// let opponent_one = WengLinRating::new();
/// let opponent_two = WengLinRating {
///     rating: 12.0,
///     uncertainty: 4.2,
/// };
///
/// let player = weng_lin_rating_period(
///     player,
///     &vec![
///         (opponent_one, Outcomes::WIN),
///         (opponent_two, Outcomes::DRAW),
///     ],
///     &WengLinConfig::new(),
/// );
///
/// assert!(((player.rating * 100.0).round() - 2578.0).abs() < f64::EPSILON);
/// assert!(((player.uncertainty * 100.0).round() - 780.0).abs() < f64::EPSILON);
/// ```
pub fn weng_lin_rating_period(
    player: WengLinRating,
    results: &Vec<(WengLinRating, Outcomes)>,
    config: &WengLinConfig,
) -> WengLinRating {
    let mut player = player;

    for (opponent, result) in results {
        let c = 2.0f64
            .mul_add(
                config.beta.powi(2),
                player
                    .uncertainty
                    .mul_add(player.uncertainty, opponent.uncertainty.powi(2)),
            )
            .sqrt();

        let (p1, p2) = expected_score(player, *opponent, config);

        let outcome = match result {
            Outcomes::WIN => 1.0,
            Outcomes::DRAW => 0.5,
            Outcomes::LOSS => 0.0,
        };

        let delta = (player.uncertainty.powi(2) / c) * (outcome - p1);
        let gamma = player.uncertainty / c;
        let eta = gamma * (player.uncertainty / c).powi(2) * p1 * p2;

        let new_rating = delta + player.rating;
        let sigma = (1.0 - 1.0 * eta).max(config.uncertainty_tolerance);
        let new_uncertainty = (player.uncertainty.powi(2) * sigma).sqrt();

        player = WengLinRating {
            rating: new_rating,
            uncertainty: new_uncertainty,
        }
    }

    player
}

#[must_use]
/// Calculates the [`WengLinRating`] of two teams based on their ratings, uncertainties, and the outcome of the game.
///
/// Takes in two teams as a Vec of [`WengLinRating`]s, the outcome of the game as an [`Outcome`](Outcomes) and a [`WengLinConfig`].
///
/// The outcome of the match is in the perspective of `team_one`.
/// This means [`Outcomes::WIN`] is a win for `team_one` and [`Outcomes::LOSS`] is a win for `team_two`.
///
/// Similar to [`weng_lin`].
///
/// # Examples
/// ```
/// use skillratings::{
///     rating::WengLinRating, weng_lin::weng_lin_teams, outcomes::Outcomes, config::WengLinConfig
/// };
///
/// let team_one = vec![
///     WengLinRating::new(),
///     WengLinRating {
///         rating: 30.0,
///         uncertainty: 1.2,
///     },
///     WengLinRating {
///         rating: 21.0,
///         uncertainty: 6.5,
///     },
/// ];
///
/// let team_two = vec![
///     WengLinRating::default(),
///     WengLinRating {
///         rating: 41.0,
///         uncertainty: 1.4,
///     },
///     WengLinRating {
///         rating: 19.2,
///         uncertainty: 4.3,
///     },
/// ];
///
/// let (team_one, team_two) =
///     weng_lin_teams(team_one, team_two, Outcomes::WIN, &WengLinConfig::new());
///
/// assert!(((team_one[0].rating * 100.0).round() - 2790.0).abs() < f64::EPSILON);
/// assert!(((team_one[1].rating * 100.0).round() - 3006.0).abs() < f64::EPSILON);
/// assert!(((team_one[2].rating * 100.0).round() - 2277.0).abs() < f64::EPSILON);
///
/// assert!(((team_two[0].rating * 100.0).round() - 2210.0).abs() < f64::EPSILON);
/// assert!(((team_two[1].rating * 100.0).round() - 4092.0).abs() < f64::EPSILON);
/// assert!(((team_two[2].rating * 100.0).round() - 1843.0).abs() < f64::EPSILON);
/// ```
pub fn weng_lin_teams(
    team_one: Vec<WengLinRating>,
    team_two: Vec<WengLinRating>,
    outcome: Outcomes,
    config: &WengLinConfig,
) -> (Vec<WengLinRating>, Vec<WengLinRating>) {
    if team_one.is_empty() || team_two.is_empty() {
        return (team_one, team_two);
    }
    let team_one_uncertainties = team_one.iter().map(|p| p.uncertainty.powi(2)).sum::<f64>();
    let team_two_uncertainties = team_two.iter().map(|p| p.uncertainty.powi(2)).sum::<f64>();

    let c = 2.0f64
        .mul_add(
            config.beta.powi(2),
            team_one_uncertainties + team_two_uncertainties,
        )
        .sqrt();

    let (p1, p2) = expected_score_teams(team_one.clone(), team_two.clone(), config);

    let outcome1 = match outcome {
        Outcomes::WIN => 1.0,
        Outcomes::DRAW => 0.5,
        Outcomes::LOSS => 0.0,
    };

    let outcome2 = match outcome {
        Outcomes::WIN => 0.0,
        Outcomes::DRAW => 0.5,
        Outcomes::LOSS => 1.0,
    };

    let delta1 = (team_one_uncertainties / c) * (outcome1 - p1);
    let gamma1 = team_one_uncertainties.sqrt() / c;
    let eta1 = gamma1 * (team_one_uncertainties.sqrt() / c).powi(2) * p1 * p2;

    let delta2 = (team_two_uncertainties / c) * (outcome2 - p2);
    let gamma2 = team_two_uncertainties.sqrt() / c;
    let eta2 = gamma2 * (team_two_uncertainties.sqrt() / c).powi(2) * p2 * p1;

    let mut new_team_one = Vec::new();
    let mut new_team_two = Vec::new();

    for player in team_one {
        let new_rating =
            (player.uncertainty.powi(2) / team_one_uncertainties).mul_add(delta1, player.rating);
        let sigma = (1.0 - (player.uncertainty.powi(2) / team_one_uncertainties) * eta1)
            .max(config.uncertainty_tolerance);

        let new_uncertainty = (player.uncertainty.powi(2) * sigma).sqrt();

        new_team_one.push(WengLinRating {
            rating: new_rating,
            uncertainty: new_uncertainty,
        });
    }

    for player in team_two {
        let new_rating =
            (player.uncertainty.powi(2) / team_two_uncertainties).mul_add(delta2, player.rating);
        let sigma = (1.0 - (player.uncertainty.powi(2) / team_two_uncertainties) * eta2)
            .max(config.uncertainty_tolerance);

        let new_uncertainty = (player.uncertainty.powi(2) * sigma).sqrt();

        new_team_two.push(WengLinRating {
            rating: new_rating,
            uncertainty: new_uncertainty,
        });
    }

    (new_team_one, new_team_two)
}

#[must_use]
/// Calculates the expected outcome of two players based on the Bradley-Terry model.
///
/// Takes in two players as [`WengLinRating`]s and a [`WengLinConfig`],
/// and returns the probability of victory for each player as an [`f64`] between 1.0 and 0.0.
///
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// Similar to [`expected_score_teams`].
///
/// # Examples
/// ```
/// use skillratings::{
///     rating::WengLinRating, weng_lin::expected_score, config::WengLinConfig
/// };
///
/// let p1 = WengLinRating {
///     rating: 42.0,
///     uncertainty: 2.1,
/// };
/// let p2 = WengLinRating {
///     rating: 31.0,
///     uncertainty: 1.2,
/// };
///
/// let (exp1, exp2) = expected_score(p1, p2, &WengLinConfig::new());
///
/// assert!((exp1 + exp2 - 1.0).abs() < f64::EPSILON);
///
/// assert!(((exp1 * 100.0).round() - 85.0).abs() < f64::EPSILON);
/// ```
pub fn expected_score(
    player_one: WengLinRating,
    player_two: WengLinRating,
    config: &WengLinConfig,
) -> (f64, f64) {
    let c = 2.0f64
        .mul_add(
            config.beta.powi(2),
            player_one
                .uncertainty
                .mul_add(player_one.uncertainty, player_two.uncertainty.powi(2)),
        )
        .sqrt();

    let e1 = (player_one.rating / c).exp();
    let e2 = (player_two.rating / c).exp();

    (e1 / (e1 + e2), e2 / (e1 + e2))
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
/// Calculates the expected outcome of two teams based on the Bradley-Terry model.
///
/// Takes in two teams as Vec of [`WengLinRating`]s and a [`WengLinConfig`],
/// and returns the probability of victory for each player as an [`f64`] between 1.0 and 0.0.  
///
/// 1.0 means a certain victory for the player, 0.0 means certain loss.
/// Values near 0.5 mean a draw is likely to occur.
///
/// Similar to [`expected_score`].
///
/// # Examples
/// ```
/// use skillratings::{
///     rating::WengLinRating, weng_lin::expected_score_teams, config::WengLinConfig
/// };
///
/// let team_one = vec![
///     WengLinRating {
///         rating: 42.0,
///         uncertainty: 2.1,
///     },
///     WengLinRating::new(),
///     WengLinRating {
///         rating: 12.0,
///         uncertainty: 3.2,
///     },
///
/// ];
/// let team_two = vec![
///     WengLinRating {
///         rating: 31.0,
///         uncertainty: 1.2,
///     },
///     WengLinRating::new(),
///     WengLinRating {
///         rating: 41.0,
///         uncertainty: 1.2,
///     },
/// ];
///
/// let (exp1, exp2) = expected_score_teams(team_one, team_two, &WengLinConfig::new());
///
/// assert!((exp1 + exp2 - 1.0).abs() < f64::EPSILON);
///
/// assert!(((exp1 * 100.0).round() - 21.0).abs() < f64::EPSILON);
/// ```
pub fn expected_score_teams(
    team_one: Vec<WengLinRating>,
    team_two: Vec<WengLinRating>,
    config: &WengLinConfig,
) -> (f64, f64) {
    let team_one_ratings = team_one.iter().map(|p| p.rating).sum::<f64>();
    let team_two_ratings = team_two.iter().map(|p| p.rating).sum::<f64>();

    let team_one_uncertainties = team_one.iter().map(|p| p.uncertainty.powi(2)).sum::<f64>();
    let team_two_uncertainties = team_two.iter().map(|p| p.uncertainty.powi(2)).sum::<f64>();

    let c = 2.0f64
        .mul_add(
            config.beta.powi(2),
            team_one_uncertainties + team_two_uncertainties,
        )
        .sqrt();

    let e1 = (team_one_ratings / c).exp();
    let e2 = (team_two_ratings / c).exp();

    (e1 / (e1 + e2), e2 / (e1 + e2))
}

mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_weng() {
        let p1 = WengLinRating::new();
        let p2 = WengLinRating::new();

        let (t1, t2) = weng_lin(p1, p2, Outcomes::WIN, &WengLinConfig::new());

        assert!((t1.rating - 27.635_231_383_473_65).abs() < f64::EPSILON);
        assert!((t1.uncertainty - 8.065_506_316_323_548).abs() < f64::EPSILON);
        assert!((t2.rating - 22.364_768_616_526_35).abs() < f64::EPSILON);
        assert!((t2.uncertainty - 8.065_506_316_323_548).abs() < f64::EPSILON);

        let p1 = WengLinRating {
            rating: 42.0,
            uncertainty: 1.3,
        };
        let p2 = WengLinRating::new();

        let (t1, t2) = weng_lin(p1, p2, Outcomes::WIN, &WengLinConfig::new());

        assert!((t1.rating - 42.026_412_401_802_894).abs() < f64::EPSILON);
        assert!((t1.uncertainty - 1.299_823_053_277_078_3).abs() < f64::EPSILON);
        assert!((t2.rating - 23.914_677_769_440_46).abs() < f64::EPSILON);
        assert!((t2.uncertainty - 8.029_022_445_649_298).abs() < f64::EPSILON);

        let (t1, t2) = weng_lin(p1, p2, Outcomes::LOSS, &WengLinConfig::new());

        assert!((t1.rating - 41.862_153_998_286_94).abs() < f64::EPSILON);
        assert!((t1.uncertainty - 1.299_823_053_277_078_3).abs() < f64::EPSILON);
        assert!((t2.rating - 30.664_283_436_598_35).abs() < f64::EPSILON);
        assert!((t2.uncertainty - 8.029_022_445_649_298).abs() < f64::EPSILON);

        let (t1, t2) = weng_lin(p1, p2, Outcomes::DRAW, &WengLinConfig::new());

        assert!((t1.rating - 41.944_283_200_044_92).abs() < f64::EPSILON);
        assert!((t1.uncertainty - 1.299_823_053_277_078_3).abs() < f64::EPSILON);
        assert!((t2.rating - 27.289_480_603_019_403).abs() < f64::EPSILON);
        assert!((t2.uncertainty - 8.029_022_445_649_298).abs() < f64::EPSILON);
    }

    #[test]
    #[allow(clippy::cognitive_complexity)]
    fn test_weng_teams() {
        let t1 = vec![
            WengLinRating::new(),
            WengLinRating {
                rating: 30.0,
                uncertainty: 1.2,
            },
            WengLinRating {
                rating: 21.0,
                uncertainty: 6.5,
            },
        ];

        let t2 = vec![
            WengLinRating::default(),
            WengLinRating {
                rating: 41.0,
                uncertainty: 1.4,
            },
            WengLinRating {
                rating: 19.2,
                uncertainty: 4.3,
            },
        ];

        let (nt1, nt2) =
            weng_lin_teams(t1.clone(), t2.clone(), Outcomes::WIN, &WengLinConfig::new());

        assert!((nt1[0].rating - 27.904_443_970_057_24).abs() < f64::EPSILON);
        assert!((nt1[1].rating - 30.060_226_550_163_108).abs() < f64::EPSILON);
        assert!((nt1[2].rating - 22.767_063_711_382_825).abs() < f64::EPSILON);

        assert!((nt2[0].rating - 22.095_556_029_942_76).abs() < f64::EPSILON);
        assert!((nt2[1].rating - 40.918_024_973_389_1).abs() < f64::EPSILON);
        assert!((nt2[2].rating - 18.426_674_366_308_44).abs() < f64::EPSILON);

        assert!((nt1[0].uncertainty - 8.138_803_466_450_47).abs() < f64::EPSILON);
        assert!((nt1[1].uncertainty - 1.199_425_779_255_630_7).abs() < f64::EPSILON);
        assert!((nt1[2].uncertainty - 6.408_113_466_768_933).abs() < f64::EPSILON);

        assert!((nt2[0].uncertainty - 8.160_155_338_979_159).abs() < f64::EPSILON);
        assert!((nt2[1].uncertainty - 1.399_187_149_975_365_4).abs() < f64::EPSILON);
        assert!((nt2[2].uncertainty - 4.276_389_807_576_043).abs() < f64::EPSILON);

        let (nt1, nt2) = weng_lin_teams(
            t1.clone(),
            t2.clone(),
            Outcomes::DRAW,
            &WengLinConfig::new(),
        );

        assert!((nt1[0].rating - 25.652_558_832_338_293).abs() < f64::EPSILON);
        assert!((nt1[1].rating - 30.013_531_459_947_366).abs() < f64::EPSILON);
        assert!((nt1[2].rating - 21.397_016_793_594_62).abs() < f64::EPSILON);

        assert!((nt2[0].rating - 24.347_441_167_661_707).abs() < f64::EPSILON);
        assert!((nt2[1].rating - 40.981_582_179_516_08).abs() < f64::EPSILON);
        assert!((nt2[2].rating - 19.026_252_295_536_935).abs() < f64::EPSILON);

        // The uncertainties do not change.
        assert!((nt1[0].uncertainty - 8.138_803_466_450_47).abs() < f64::EPSILON);

        let (nt1, nt2) = weng_lin_teams(t1, t2, Outcomes::LOSS, &WengLinConfig::default());

        assert!((nt1[0].rating - 23.400_673_694_619_35).abs() < f64::EPSILON);
        assert!((nt1[1].rating - 29.966_836_369_731_627).abs() < f64::EPSILON);
        assert!((nt1[2].rating - 20.026_969_875_806_41).abs() < f64::EPSILON);

        assert!((nt2[0].rating - 26.599_326_305_380_65).abs() < f64::EPSILON);
        assert!((nt2[1].rating - 41.045_139_385_643_06).abs() < f64::EPSILON);
        assert!((nt2[2].rating - 19.625_830_224_765_43).abs() < f64::EPSILON);
    }

    #[test]
    fn test_empty_team() {
        let t1 = vec![WengLinRating::new()];
        let t2 = Vec::new();

        let (nt1, nt2) = weng_lin_teams(
            t1.clone(),
            t2.clone(),
            Outcomes::DRAW,
            &WengLinConfig::new(),
        );

        assert_eq!(t1, nt1);
        assert_eq!(t2, nt2);
    }

    #[test]
    fn test_expected() {
        let p1 = WengLinRating::new();
        let p2 = WengLinRating::new();

        let (exp1, exp2) = expected_score(p1, p2, &WengLinConfig::new());

        assert!((exp1 - exp2).abs() < f64::EPSILON);

        let p1 = WengLinRating {
            rating: 42.0,
            uncertainty: 2.1,
        };
        let p2 = WengLinRating {
            rating: 31.0,
            uncertainty: 1.2,
        };

        let (exp1, exp2) = expected_score(p1, p2, &WengLinConfig::new());

        assert!((exp1 + exp2 - 1.0).abs() < f64::EPSILON);

        assert!((exp1 - 0.849_021_123_412_260_5).abs() < f64::EPSILON);
        assert!((exp2 - 0.150_978_876_587_739_42).abs() < f64::EPSILON);
    }

    #[test]
    fn test_expected_teams() {
        let p1 = vec![WengLinRating::new()];
        let p2 = vec![WengLinRating::new()];

        let (exp1, exp2) = expected_score_teams(p1, p2, &WengLinConfig::new());

        assert!((exp1 - exp2).abs() < f64::EPSILON);

        let mut p1 = vec![WengLinRating {
            rating: 42.0,
            uncertainty: 2.1,
        }];
        let mut p2 = vec![WengLinRating {
            rating: 31.0,
            uncertainty: 1.2,
        }];

        let (exp1, exp2) = expected_score_teams(p1.clone(), p2.clone(), &WengLinConfig::new());

        assert!((exp1 + exp2 - 1.0).abs() < f64::EPSILON);

        assert!((exp1 - 0.849_021_123_412_260_5).abs() < f64::EPSILON);
        assert!((exp2 - 0.150_978_876_587_739_42).abs() < f64::EPSILON);

        p1.push(WengLinRating::new());
        p1.push(WengLinRating {
            rating: 12.0,
            uncertainty: 3.2,
        });

        p2.push(WengLinRating {
            rating: 41.0,
            uncertainty: 1.2,
        });

        let (exp1, exp2) = expected_score_teams(p1.clone(), p2.clone(), &WengLinConfig::new());

        assert!((exp1 + exp2 - 1.0).abs() < f64::EPSILON);

        // Even if they are better, team two is a player down.
        assert!((exp1 - 0.653_518_078_332_893_4).abs() < f64::EPSILON);

        p2.push(WengLinRating::new());

        let (exp1, _) = expected_score_teams(p1, p2, &WengLinConfig::new());

        assert!((exp1 - 0.213_836_440_502_453_18).abs() < f64::EPSILON);
    }

    #[test]
    fn test_rating_period() {
        let player = WengLinRating::new();

        let opponent_one = WengLinRating::new();
        let opponent_two = WengLinRating {
            rating: 12.0,
            uncertainty: 4.2,
        };

        let (normal_player, _) =
            weng_lin(player, opponent_one, Outcomes::WIN, &WengLinConfig::new());
        let (normal_player, _) = weng_lin(
            normal_player,
            opponent_two,
            Outcomes::DRAW,
            &WengLinConfig::new(),
        );
        let (normal_player, _) = weng_lin(
            normal_player,
            opponent_two,
            Outcomes::LOSS,
            &WengLinConfig::new(),
        );

        let rating_player = weng_lin_rating_period(
            player,
            &vec![
                (opponent_one, Outcomes::WIN),
                (opponent_two, Outcomes::DRAW),
                (opponent_two, Outcomes::LOSS),
            ],
            &WengLinConfig::new(),
        );

        assert!((normal_player.rating - rating_player.rating).abs() < f64::EPSILON);
        assert!((normal_player.uncertainty - rating_player.uncertainty).abs() < f64::EPSILON);
    }
}
