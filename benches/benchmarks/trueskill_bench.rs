use skillratings::{
    trueskill::{
        expected_score, expected_score_two_teams, trueskill, trueskill_rating_period,
        trueskill_two_teams, TrueSkillConfig, TrueSkillRating,
    },
    Outcomes,
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
            trueskill_two_teams(
                black_box(&team_one),
                black_box(&team_two),
                black_box(&outcome),
                black_box(&config),
            )
        })
    });
}

pub fn expected_trueskill(c: &mut Criterion) {
    let player_one = TrueSkillRating {
        rating: 32.1,
        uncertainty: 4.233,
    };
    let player_two = TrueSkillRating {
        rating: 41.01,
        uncertainty: 1.34,
    };

    let config = TrueSkillConfig::new();

    c.bench_function("TrueSkill 1v1 Expected Score", |b| {
        b.iter(|| {
            expected_score(
                black_box(&player_one),
                black_box(&player_two),
                black_box(&config),
            )
        });
    });
}

pub fn expected_trueskill_teams(c: &mut Criterion) {
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

    let config = TrueSkillConfig::new();

    c.bench_function("TrueSkill 4v4 Expected Score", |b| {
        b.iter(|| {
            expected_score_two_teams(
                black_box(&team_one),
                black_box(&team_two),
                black_box(&config),
            )
        });
    });
}

pub fn rating_period_trueskill(c: &mut Criterion) {
    let player = TrueSkillRating {
        rating: 8.3,
        uncertainty: 2.2,
    };

    let results = vec![
        (
            TrueSkillRating {
                rating: 3.2,
                uncertainty: 2.1,
            },
            Outcomes::WIN,
        ),
        (
            TrueSkillRating {
                rating: 6.2,
                uncertainty: 2.1,
            },
            Outcomes::DRAW,
        ),
        (
            TrueSkillRating {
                rating: 9.2,
                uncertainty: 2.1,
            },
            Outcomes::LOSS,
        ),
        (
            TrueSkillRating {
                rating: 12.2,
                uncertainty: 2.1,
            },
            Outcomes::WIN,
        ),
        (
            TrueSkillRating {
                rating: 15.2,
                uncertainty: 2.1,
            },
            Outcomes::DRAW,
        ),
        (
            TrueSkillRating {
                rating: 18.2,
                uncertainty: 2.1,
            },
            Outcomes::LOSS,
        ),
        (
            TrueSkillRating {
                rating: 21.2,
                uncertainty: 2.1,
            },
            Outcomes::WIN,
        ),
        (
            TrueSkillRating {
                rating: 24.2,
                uncertainty: 2.1,
            },
            Outcomes::DRAW,
        ),
        (
            TrueSkillRating {
                rating: 27.2,
                uncertainty: 2.1,
            },
            Outcomes::LOSS,
        ),
        (
            TrueSkillRating {
                rating: 30.2,
                uncertainty: 2.1,
            },
            Outcomes::LOSS,
        ),
    ];

    let config = TrueSkillConfig::new();

    c.bench_function("TrueSkill Rating Period 10 Players", |b| {
        b.iter(|| {
            trueskill_rating_period(black_box(&player), black_box(&results), black_box(&config))
        });
    });
}

criterion_group!(
    benches,
    trueskill_benchmark,
    trueskill_team_benchmark,
    expected_trueskill,
    expected_trueskill_teams,
    rating_period_trueskill,
);
criterion_main!(benches);
