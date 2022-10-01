use skillratings::{
    config::TrueSkillConfig,
    outcomes::Outcomes,
    rating::TrueSkillRating,
    trueskill::{trueskill, trueskill_teams},
};

use criterion::{black_box, criterion_group, criterion_main, Criterion};

pub fn trueskill_benchmark(c: &mut Criterion) {
    let player_one = TrueSkillRating {
        rating: 32.1,
        uncertainty: 4.233,
    };
    let player_two = TrueSkillRating {
        rating: 41.01,
        uncertainty: 1.34,
    };
    let outcome = Outcomes::WIN;
    let config = TrueSkillConfig::new();

    c.bench_function("TrueSkill 1v1", |b| {
        b.iter(|| {
            trueskill(
                black_box(&player_one),
                black_box(&player_two),
                black_box(&outcome),
                black_box(&config),
            )
        })
    });
}

pub fn trueskill_team_benchmark(c: &mut Criterion) {
    let team_one = vec![
        TrueSkillRating {
            rating: 32.1,
            uncertainty: 4.233,
        },
        TrueSkillRating {
            rating: 41.01,
            uncertainty: 1.34,
        },
        TrueSkillRating {
            rating: 32.1,
            uncertainty: 4.233,
        },
        TrueSkillRating {
            rating: 41.01,
            uncertainty: 1.34,
        },
    ];
    let team_two = vec![
        TrueSkillRating {
            rating: 29.1,
            uncertainty: 4.233,
        },
        TrueSkillRating {
            rating: 12.01,
            uncertainty: 1.34,
        },
        TrueSkillRating {
            rating: 9.1,
            uncertainty: 4.233,
        },
        TrueSkillRating {
            rating: 53.01,
            uncertainty: 1.34,
        },
    ];

    let outcome = Outcomes::WIN;
    let config = TrueSkillConfig::new();

    c.bench_function("TrueSkill 4v4", |b| {
        b.iter(|| {
            trueskill_teams(
                black_box(&team_one),
                black_box(&team_two),
                black_box(&outcome),
                black_box(&config),
            )
        })
    });
}

criterion_group!(benches, trueskill_benchmark, trueskill_team_benchmark);
criterion_main!(benches);
