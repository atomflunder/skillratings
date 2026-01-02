use crate::trueskill::factor_graph::{
    LikelihoodFactor, PriorFactor, SumFactor, TruncateFactor, Variable,
};
use crate::trueskill::gaussian::Gaussian;
use crate::trueskill::math::{v_draw, v_non_draw, w_draw, w_non_draw};
use crate::trueskill::{draw_margin, TrueSkillRating};
use crate::MultiTeamOutcome;
use std::cell::RefCell;
use std::rc::Rc;

#[allow(clippy::too_many_arguments)]
pub fn run_schedule(
    rating_vars: &[Rc<RefCell<Variable>>],
    perf_vars: &[Rc<RefCell<Variable>>],
    team_perf_vars: &[Rc<RefCell<Variable>>],
    team_diff_vars: &[Rc<RefCell<Variable>>],
    team_sizes: &[usize],
    sorted_teams_and_ranks: &[(&[TrueSkillRating], MultiTeamOutcome)],
    flattened_ratings: &[TrueSkillRating],
    flattened_weights: &[f64],
    tau: f64,
    beta: f64,
    draw_probability: f64,
    min_delta: f64,
) -> Vec<PriorFactor> {
    assert!((min_delta > 0.0), "min_delta must be greater than 0");

    let mut id = 0;

    let mut rating_layer = build_rating_layer(rating_vars, flattened_ratings, tau, id);
    id += rating_layer.len();
    let mut perf_layer = build_perf_layer(rating_vars, perf_vars, beta, id);
    id += perf_layer.len();
    let mut team_perf_layer =
        build_team_perf_layer(team_perf_vars, perf_vars, flattened_weights, team_sizes, id);
    id += team_perf_layer.len();

    for factor in &mut rating_layer {
        factor.down();
    }
    for factor in &mut perf_layer {
        factor.down();
    }
    for factor in &mut team_perf_layer {
        factor.down();
    }

    let team_diff_layer = build_team_diff_layer(team_diff_vars, team_perf_vars, id);
    let team_diff_len = team_diff_layer.len();
    id += team_diff_len;

    let trunc_layer = build_trunc_layer(
        team_diff_vars,
        sorted_teams_and_ranks,
        draw_probability,
        beta,
        id,
    );

    let mut delta: f64;
    for _ in 0..10 {
        if team_diff_len == 1 {
            team_diff_layer[0].down();
            delta = trunc_layer[0].up();
        } else {
            delta = 0.0;
            for x in 0..(team_diff_len - 1) {
                team_diff_layer[x].down();
                delta = delta.max(trunc_layer[x].up());
                team_diff_layer[x].up(1);
            }
            for x in (1..team_diff_len).rev() {
                team_diff_layer[x].down();
                delta = delta.max(trunc_layer[x].up());
                team_diff_layer[x].up(0);
            }
        }
        if delta <= min_delta {
            break;
        }
    }

    team_diff_layer[0].up(0);
    team_diff_layer[team_diff_len - 1].up(1);
    for f in &mut team_perf_layer {
        for x in 0..f.terms_len() {
            f.up(x);
        }
    }
    for f in &mut perf_layer {
        f.up();
    }

    rating_layer
}

pub fn build_rating_layer(
    rating_vars: &[Rc<RefCell<Variable>>],
    flattened_ratings: &[TrueSkillRating],
    tau: f64,
    starting_id: usize,
) -> Vec<PriorFactor> {
    let mut v = Vec::with_capacity(rating_vars.len());
    let mut i = starting_id;
    for (var, rating) in rating_vars.iter().zip(flattened_ratings) {
        v.push(PriorFactor::new(
            i,
            Rc::clone(var),
            Gaussian::with_mu_sigma(rating.rating, rating.uncertainty),
            tau,
        ));
        i += 1;
    }

    v
}

pub fn build_perf_layer(
    rating_vars: &[Rc<RefCell<Variable>>],
    perf_vars: &[Rc<RefCell<Variable>>],
    beta: f64,
    starting_id: usize,
) -> Vec<LikelihoodFactor> {
    let beta_sq = beta.powi(2);
    let mut v = Vec::with_capacity(rating_vars.len());
    let mut i = starting_id;
    for (rating_var, perf_var) in rating_vars.iter().zip(perf_vars) {
        v.push(LikelihoodFactor::new(
            i,
            Rc::clone(rating_var),
            Rc::clone(perf_var),
            beta_sq,
        ));
        i += 1;
    }

    v
}

pub fn build_team_perf_layer(
    team_perf_vars: &[Rc<RefCell<Variable>>],
    perf_vars: &[Rc<RefCell<Variable>>],
    flattened_weights: &[f64],
    team_sizes: &[usize],
    starting_id: usize,
) -> Vec<SumFactor> {
    let mut v = Vec::with_capacity(team_perf_vars.len());
    let mut i = starting_id;
    for (team, team_perf_var) in team_perf_vars.iter().enumerate() {
        let start = if team > 0 { team_sizes[team - 1] } else { 0 };

        let end = team_sizes[team];
        let child_perf_vars = perf_vars[start..end].to_vec();
        let coeffs = flattened_weights[start..end].to_vec();

        v.push(SumFactor::new(
            i,
            Rc::clone(team_perf_var),
            child_perf_vars,
            coeffs,
        ));
        i += 1;
    }

    v
}

pub fn build_team_diff_layer(
    team_diff_vars: &[Rc<RefCell<Variable>>],
    team_perf_vars: &[Rc<RefCell<Variable>>],
    starting_id: usize,
) -> Vec<SumFactor> {
    let mut v = Vec::with_capacity(team_diff_vars.len());
    let mut i = starting_id;
    for (team, team_diff_var) in team_diff_vars.iter().enumerate() {
        v.push(SumFactor::new(
            i,
            Rc::clone(team_diff_var),
            team_perf_vars[team..(team + 2)].to_vec(),
            vec![1.0, -1.0],
        ));
        i += 1;
    }

    v
}

pub fn build_trunc_layer(
    team_diff_vars: &[Rc<RefCell<Variable>>],
    sorted_teams_and_ranks: &[(&[TrueSkillRating], MultiTeamOutcome)],
    draw_probability: f64,
    beta: f64,
    starting_id: usize,
) -> Vec<TruncateFactor> {
    let mut v = Vec::with_capacity(team_diff_vars.len());
    let mut i = starting_id;
    for (x, team_diff_var) in team_diff_vars.iter().enumerate() {
        let size = sorted_teams_and_ranks[x..(x + 2)]
            .iter()
            .map(|v| v.0.len() as f64)
            .sum();
        let draw_margin = draw_margin(draw_probability, beta, size);
        let v_func: Box<dyn Fn(f64, f64, f64) -> f64> =
            if sorted_teams_and_ranks[x].1 == sorted_teams_and_ranks[x + 1].1 {
                Box::new(v_draw)
            } else {
                Box::new(v_non_draw)
            };
        let w_func: Box<dyn Fn(f64, f64, f64) -> f64> =
            if sorted_teams_and_ranks[x].1 == sorted_teams_and_ranks[x + 1].1 {
                Box::new(w_draw)
            } else {
                Box::new(w_non_draw)
            };

        v.push(TruncateFactor::new(
            i,
            Rc::clone(team_diff_var),
            v_func,
            w_func,
            draw_margin,
        ));
        i += 1;
    }

    v
}
