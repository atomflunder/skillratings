use criterion::criterion_main;

mod benchmarks;

criterion_main! {
    benchmarks::elo_bench::benches,
    benchmarks::glicko2_bench::benches,
    benchmarks::trueskill_bench::benches,
    benchmarks::weng_lin_bench::benches,
}
