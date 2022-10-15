use skillratings::{
    config::Glicko2Config,
    glicko2::{expected_score, glicko2, glicko2_rating_period},
    outcomes::Outcomes,
    rating::Glicko2Rating,
};

use criterion::{black_box, criterion_group, criterion_main, Criterion};

pub fn glicko2_bench(c: &mut Criterion) {
    let player_one = Glicko2Rating {
        rating: 1520.3,
        deviation: 120.5,
        volatility: 0.059,
    };
    let player_two = Glicko2Rating {
        rating: 990.2,
        deviation: 241.1,
        volatility: 0.05932,
    };
    let outcome = Outcomes::WIN;
    let config = Glicko2Config::new();

    c.bench_function("Glicko-2 1v1", |b| {
        b.iter(|| {
            glicko2(
                black_box(&player_one),
                black_box(&player_two),
                black_box(&outcome),
                black_box(&config),
            )
        })
    });
}

pub fn expected_glicko2(c: &mut Criterion) {
    let player_one = Glicko2Rating {
        rating: 1520.3,
        deviation: 120.5,
        volatility: 0.059,
    };
    let player_two = Glicko2Rating {
        rating: 990.2,
        deviation: 241.1,
        volatility: 0.05932,
    };

    c.bench_function("Glicko-2 Expected Score", |b| {
        b.iter(|| expected_score(black_box(&player_one), black_box(&player_two)));
    });
}

pub fn rating_period_glicko2(c: &mut Criterion) {
    let player = Glicko2Rating {
        rating: 832.1,
        deviation: 320.0,
        volatility: 0.0599,
    };

    let results = vec![
        (
            Glicko2Rating {
                rating: 291.1,
                deviation: 130.0,
                volatility: 0.0599,
            },
            Outcomes::WIN,
        ),
        (
            Glicko2Rating {
                rating: 391.1,
                deviation: 130.0,
                volatility: 0.0599,
            },
            Outcomes::DRAW,
        ),
        (
            Glicko2Rating {
                rating: 491.1,
                deviation: 130.0,
                volatility: 0.0599,
            },
            Outcomes::LOSS,
        ),
        (
            Glicko2Rating {
                rating: 591.1,
                deviation: 130.0,
                volatility: 0.0599,
            },
            Outcomes::WIN,
        ),
        (
            Glicko2Rating {
                rating: 691.1,
                deviation: 130.0,
                volatility: 0.0599,
            },
            Outcomes::DRAW,
        ),
        (
            Glicko2Rating {
                rating: 791.1,
                deviation: 130.0,
                volatility: 0.0599,
            },
            Outcomes::LOSS,
        ),
        (
            Glicko2Rating {
                rating: 891.1,
                deviation: 130.0,
                volatility: 0.0599,
            },
            Outcomes::WIN,
        ),
        (
            Glicko2Rating {
                rating: 991.1,
                deviation: 130.0,
                volatility: 0.0599,
            },
            Outcomes::DRAW,
        ),
        (
            Glicko2Rating {
                rating: 1091.1,
                deviation: 130.0,
                volatility: 0.0599,
            },
            Outcomes::LOSS,
        ),
        (
            Glicko2Rating {
                rating: 1191.1,
                deviation: 130.0,
                volatility: 0.0599,
            },
            Outcomes::LOSS,
        ),
    ];

    let config = Glicko2Config::new();

    c.bench_function("Glicko-2 Rating Period 10 Players", |b| {
        b.iter(|| {
            glicko2_rating_period(black_box(&player), black_box(&results), black_box(&config))
        });
    });
}

criterion_group!(
    benches,
    glicko2_bench,
    expected_glicko2,
    rating_period_glicko2
);
criterion_main!(benches);
