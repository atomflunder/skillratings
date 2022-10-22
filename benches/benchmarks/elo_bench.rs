use skillratings::{
    elo::{elo, elo_rating_period, expected_score, EloConfig, EloRating},
    Outcomes,
};

use criterion::{black_box, criterion_group, criterion_main, Criterion};

pub fn elo_benchmark(c: &mut Criterion) {
    let player_one = EloRating { rating: 1520.3 };
    let player_two = EloRating { rating: 990.2 };
    let outcome = Outcomes::WIN;
    let config = EloConfig::new();

    c.bench_function("Elo 1v1", |b| {
        b.iter(|| {
            elo(
                black_box(&player_one),
                black_box(&player_two),
                black_box(&outcome),
                black_box(&config),
            )
        })
    });
}

pub fn expected_elo(c: &mut Criterion) {
    let player_one = EloRating { rating: 1520.3 };
    let player_two = EloRating { rating: 990.2 };

    c.bench_function("Elo Expected Score", |b| {
        b.iter(|| expected_score(black_box(&player_one), black_box(&player_two)));
    });
}

pub fn rating_period_elo(c: &mut Criterion) {
    let player = EloRating { rating: 832.1 };

    let results = vec![
        (EloRating { rating: 291.1 }, Outcomes::WIN),
        (EloRating { rating: 391.1 }, Outcomes::DRAW),
        (EloRating { rating: 491.1 }, Outcomes::LOSS),
        (EloRating { rating: 591.1 }, Outcomes::WIN),
        (EloRating { rating: 691.1 }, Outcomes::DRAW),
        (EloRating { rating: 791.1 }, Outcomes::LOSS),
        (EloRating { rating: 891.1 }, Outcomes::WIN),
        (EloRating { rating: 991.1 }, Outcomes::DRAW),
        (EloRating { rating: 1091.1 }, Outcomes::LOSS),
        (EloRating { rating: 1191.1 }, Outcomes::LOSS),
    ];

    let config = EloConfig::new();

    c.bench_function("Elo Rating Period 10 Players", |b| {
        b.iter(|| elo_rating_period(black_box(&player), black_box(&results), black_box(&config)));
    });
}

criterion_group!(benches, elo_benchmark, expected_elo, rating_period_elo);
criterion_main!(benches);
