use skillratings::{config::EloConfig, elo::elo, outcomes::Outcomes, rating::EloRating};

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

criterion_group!(benches, elo_benchmark);
criterion_main!(benches);
