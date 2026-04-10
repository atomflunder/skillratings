use skillratings::{
    yuksel::{expected_score, yuksel, yuksel_rating_period, YukselConfig, YukselRating},
    Outcomes,
};

use criterion::{criterion_group, criterion_main, Criterion};
use std::hint::black_box;

pub fn yuksel_benchmark(c: &mut Criterion) {
    let player_one = YukselRating {
        rating: 1520.3,
        ..YukselRating::new()
    };
    let player_two = YukselRating {
        rating: 990.2,
        ..YukselRating::new()
    };
    let outcome = Outcomes::WIN;
    let config = YukselConfig::new();

    c.bench_function("Yuksel 1v1 (zero-sum)", |b| {
        b.iter(|| {
            yuksel(
                black_box(&player_one),
                black_box(&player_two),
                black_box(&outcome),
                black_box(&config),
            )
        })
    });
}

pub fn expected_yuksel(c: &mut Criterion) {
    let player_one = YukselRating {
        rating: 1520.3,
        ..YukselRating::new()
    };
    let player_two = YukselRating {
        rating: 990.2,
        ..YukselRating::new()
    };

    c.bench_function("Yuksel Expected Score", |b| {
        b.iter(|| expected_score(black_box(&player_one), black_box(&player_two)));
    });
}

pub fn rating_period_yuksel(c: &mut Criterion) {
    let player = YukselRating {
        rating: 832.1,
        ..YukselRating::new()
    };

    let results = vec![
        (YukselRating { rating: 291.1, ..YukselRating::new() }, Outcomes::WIN),
        (YukselRating { rating: 391.1, ..YukselRating::new() }, Outcomes::DRAW),
        (YukselRating { rating: 491.1, ..YukselRating::new() }, Outcomes::LOSS),
        (YukselRating { rating: 591.1, ..YukselRating::new() }, Outcomes::WIN),
        (YukselRating { rating: 691.1, ..YukselRating::new() }, Outcomes::DRAW),
        (YukselRating { rating: 791.1, ..YukselRating::new() }, Outcomes::LOSS),
        (YukselRating { rating: 891.1, ..YukselRating::new() }, Outcomes::WIN),
        (YukselRating { rating: 991.1, ..YukselRating::new() }, Outcomes::DRAW),
        (YukselRating { rating: 1091.1, ..YukselRating::new() }, Outcomes::LOSS),
        (YukselRating { rating: 1191.1, ..YukselRating::new() }, Outcomes::LOSS),
    ];

    let config = YukselConfig::new();

    c.bench_function("Yuksel Rating Period 10 Players", |b| {
        b.iter(|| {
            yuksel_rating_period(black_box(&player), black_box(&results), black_box(&config))
        });
    });
}

criterion_group!(benches, yuksel_benchmark, expected_yuksel, rating_period_yuksel);
criterion_main!(benches);
