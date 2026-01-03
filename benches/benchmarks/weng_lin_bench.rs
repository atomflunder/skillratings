use skillratings::{
    weng_lin::{
        expected_score, expected_score_two_teams, weng_lin, weng_lin_multi_team,
        weng_lin_rating_period, weng_lin_two_teams, WengLinConfig, WengLinRating,
    },
    MultiTeamOutcome, Outcomes,
};

use criterion::{criterion_group, criterion_main, Criterion};
use std::hint::black_box;

pub fn weng_lin_benchmark(c: &mut Criterion) {
    let player_one = WengLinRating {
        rating: 38.12,
        uncertainty: 6.2,
    };
    let player_two = WengLinRating {
        rating: 22.101,
        uncertainty: 3.2,
    };
    let outcome = Outcomes::WIN;
    let config = WengLinConfig::new();

    c.bench_function("WengLin 1v1", |b| {
        b.iter(|| {
            weng_lin(
                black_box(&player_one),
                black_box(&player_two),
                black_box(&outcome),
                black_box(&config),
            )
        })
    });
}

pub fn weng_lin_two_teams_benchmark(c: &mut Criterion) {
    let team_one = vec![
        WengLinRating {
            rating: 32.1,
            uncertainty: 4.233,
        },
        WengLinRating {
            rating: 41.01,
            uncertainty: 1.34,
        },
        WengLinRating {
            rating: 32.1,
            uncertainty: 4.233,
        },
        WengLinRating {
            rating: 41.01,
            uncertainty: 1.34,
        },
    ];
    let team_two = vec![
        WengLinRating {
            rating: 29.1,
            uncertainty: 4.233,
        },
        WengLinRating {
            rating: 12.01,
            uncertainty: 1.34,
        },
        WengLinRating {
            rating: 9.1,
            uncertainty: 4.233,
        },
        WengLinRating {
            rating: 53.01,
            uncertainty: 1.34,
        },
    ];

    let outcome = Outcomes::WIN;
    let config = WengLinConfig::new();

    c.bench_function("WengLin 4v4", |b| {
        b.iter(|| {
            weng_lin_two_teams(
                black_box(&team_one),
                black_box(&team_two),
                black_box(&outcome),
                black_box(&config),
            )
        })
    });
}

pub fn weng_lin_multi_team_benchmark(c: &mut Criterion) {
    let team_one = [
        WengLinRating {
            rating: 32.1,
            uncertainty: 4.233,
        },
        WengLinRating {
            rating: 41.01,
            uncertainty: 1.34,
        },
        WengLinRating {
            rating: 32.1,
            uncertainty: 4.233,
        },
        WengLinRating {
            rating: 41.01,
            uncertainty: 1.34,
        },
    ];
    let team_two = [
        WengLinRating {
            rating: 29.1,
            uncertainty: 4.233,
        },
        WengLinRating {
            rating: 12.01,
            uncertainty: 1.34,
        },
        WengLinRating {
            rating: 9.1,
            uncertainty: 4.233,
        },
        WengLinRating {
            rating: 53.01,
            uncertainty: 1.34,
        },
    ];
    let team_three = [
        WengLinRating {
            rating: 13.1,
            uncertainty: 4.233,
        },
        WengLinRating {
            rating: 25.01,
            uncertainty: 1.34,
        },
        WengLinRating {
            rating: 43.1,
            uncertainty: 4.233,
        },
        WengLinRating {
            rating: 50.0,
            uncertainty: 1.34,
        },
    ];

    let game = vec![
        (&team_one[..], MultiTeamOutcome::new(2)),
        (&team_two[..], MultiTeamOutcome::new(1)),
        (&team_three[..], MultiTeamOutcome::new(3)),
    ];

    let config = WengLinConfig::new();

    c.bench_function("WengLin 4v4v4", |b| {
        b.iter(|| weng_lin_multi_team(black_box(&game), black_box(&config)))
    });
}

pub fn expected_wenglin(c: &mut Criterion) {
    let player_one = WengLinRating {
        rating: 32.1,
        uncertainty: 4.233,
    };
    let player_two = WengLinRating {
        rating: 41.01,
        uncertainty: 1.34,
    };

    let config = WengLinConfig::new();

    c.bench_function("WengLin 1v1 Expected Score", |b| {
        b.iter(|| {
            expected_score(
                black_box(&player_one),
                black_box(&player_two),
                black_box(&config),
            )
        });
    });
}

pub fn expected_wenglin_teams(c: &mut Criterion) {
    let team_one = vec![
        WengLinRating {
            rating: 32.1,
            uncertainty: 4.233,
        },
        WengLinRating {
            rating: 41.01,
            uncertainty: 1.34,
        },
        WengLinRating {
            rating: 32.1,
            uncertainty: 4.233,
        },
        WengLinRating {
            rating: 41.01,
            uncertainty: 1.34,
        },
    ];
    let team_two = vec![
        WengLinRating {
            rating: 29.1,
            uncertainty: 4.233,
        },
        WengLinRating {
            rating: 12.01,
            uncertainty: 1.34,
        },
        WengLinRating {
            rating: 9.1,
            uncertainty: 4.233,
        },
        WengLinRating {
            rating: 53.01,
            uncertainty: 1.34,
        },
    ];

    let config = WengLinConfig::new();

    c.bench_function("WengLin 4v4 Expected Score", |b| {
        b.iter(|| {
            expected_score_two_teams(
                black_box(&team_one),
                black_box(&team_two),
                black_box(&config),
            )
        });
    });
}

pub fn rating_period_wenglin(c: &mut Criterion) {
    let player = WengLinRating {
        rating: 8.3,
        uncertainty: 2.2,
    };

    let results = vec![
        (
            WengLinRating {
                rating: 3.2,
                uncertainty: 2.1,
            },
            Outcomes::WIN,
        ),
        (
            WengLinRating {
                rating: 6.2,
                uncertainty: 2.1,
            },
            Outcomes::DRAW,
        ),
        (
            WengLinRating {
                rating: 9.2,
                uncertainty: 2.1,
            },
            Outcomes::LOSS,
        ),
        (
            WengLinRating {
                rating: 12.2,
                uncertainty: 2.1,
            },
            Outcomes::WIN,
        ),
        (
            WengLinRating {
                rating: 15.2,
                uncertainty: 2.1,
            },
            Outcomes::DRAW,
        ),
        (
            WengLinRating {
                rating: 18.2,
                uncertainty: 2.1,
            },
            Outcomes::LOSS,
        ),
        (
            WengLinRating {
                rating: 21.2,
                uncertainty: 2.1,
            },
            Outcomes::WIN,
        ),
        (
            WengLinRating {
                rating: 24.2,
                uncertainty: 2.1,
            },
            Outcomes::DRAW,
        ),
        (
            WengLinRating {
                rating: 27.2,
                uncertainty: 2.1,
            },
            Outcomes::LOSS,
        ),
        (
            WengLinRating {
                rating: 30.2,
                uncertainty: 2.1,
            },
            Outcomes::LOSS,
        ),
    ];

    let config = WengLinConfig::new();

    c.bench_function("WengLin Rating Period 10 Players", |b| {
        b.iter(|| {
            weng_lin_rating_period(black_box(&player), black_box(&results), black_box(&config))
        });
    });
}

criterion_group!(
    benches,
    weng_lin_benchmark,
    weng_lin_two_teams_benchmark,
    weng_lin_multi_team_benchmark,
    expected_wenglin,
    expected_wenglin_teams,
    rating_period_wenglin,
);
criterion_main!(benches);
