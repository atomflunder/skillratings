use skillratings::{
    config::Glicko2Config, glicko2::glicko2, outcomes::Outcomes, rating::Glicko2Rating,
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

criterion_group!(benches, glicko2_bench);
criterion_main!(benches);
