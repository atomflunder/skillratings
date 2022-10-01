use skillratings::{
    config::WengLinConfig,
    outcomes::Outcomes,
    rating::WengLinRating,
    weng_lin::{weng_lin, weng_lin_teams},
};

use criterion::{black_box, criterion_group, criterion_main, Criterion};

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

pub fn weng_lin_team_benchmark(c: &mut Criterion) {
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
            weng_lin_teams(
                black_box(&team_one),
                black_box(&team_two),
                black_box(&outcome),
                black_box(&config),
            )
        })
    });
}

criterion_group!(benches, weng_lin_benchmark, weng_lin_team_benchmark);
criterion_main!(benches);
