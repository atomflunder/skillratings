use crate::trueskill::erf::{cdf, pdf};

pub fn v_non_draw(difference: f64, draw_margin: f64, c: f64) -> f64 {
    let diff_c = difference / c;
    let draw_c = draw_margin / c;

    let norm = cdf(diff_c - draw_c, 0.0, 1.0);

    if norm < 2.222_758_749e-162 {
        -diff_c + draw_c
    } else {
        pdf(diff_c - draw_c, 0.0, 1.0) / norm
    }
}

pub fn w_non_draw(difference: f64, draw_margin: f64, c: f64) -> f64 {
    let diff_c = difference / c;
    let draw_c = draw_margin / c;

    let norm = cdf(diff_c - draw_c, 0.0, 1.0);

    if norm < 2.222_758_749e-162 {
        if diff_c < 0.0 {
            return 1.0;
        }
        return 0.0;
    }

    let v = v_non_draw(difference, draw_margin, c);

    v * (v + (diff_c) - (draw_c))
}

pub fn v_draw(difference: f64, draw_margin: f64, c: f64) -> f64 {
    let diff_c = difference / c;
    let draw_c = draw_margin / c;
    let diff_c_abs = diff_c.abs();

    let norm = cdf(draw_c - diff_c_abs, 0.0, 1.0) - cdf(-draw_c - diff_c_abs, 0.0, 1.0);

    if norm < 2.222_758_749e-162 {
        if diff_c < 0.0 {
            return -diff_c - draw_c;
        }
        return -diff_c + draw_c;
    }

    let x = pdf(-draw_c - diff_c_abs, 0.0, 1.0) - pdf(draw_c - diff_c_abs, 0.0, 1.0);

    if diff_c < 0.0 {
        -x / norm
    } else {
        x / norm
    }
}

pub fn w_draw(difference: f64, draw_margin: f64, c: f64) -> f64 {
    let diff_c = difference / c;
    let draw_c = draw_margin / c;
    let diff_c_abs = diff_c.abs();

    let norm = cdf(draw_c - diff_c_abs, 0.0, 1.0) - cdf(-draw_c - diff_c_abs, 0.0, 1.0);

    if norm < 2.222_758_749e-162 {
        return 1.0;
    }

    let v = v_draw(difference, draw_margin, c);

    let p1 = pdf(draw_c - diff_c_abs, 0.0, 1.0);
    let p2 = pdf(-draw_c - diff_c_abs, 0.0, 1.0);

    v.mul_add(
        v,
        (draw_c - diff_c_abs).mul_add(p1, -(-draw_c - diff_c_abs) * p2) / norm,
    )
}

pub fn new_rating(
    rating: f64,
    uncertainty: f64,
    v: f64,
    c: f64,
    dynamics_factor: f64,
    rank_multiplier: f64,
) -> f64 {
    let mean_multiplier = uncertainty.mul_add(uncertainty, dynamics_factor.powi(2)) / c;

    (rank_multiplier * mean_multiplier).mul_add(v, rating)
}

pub fn new_uncertainty(uncertainty: f64, c: f64, w: f64, dynamics_factor: f64) -> f64 {
    let variance = uncertainty.mul_add(uncertainty, dynamics_factor.powi(2));
    let dev_multiplier = variance / c.powi(2);

    (variance * w.mul_add(-dev_multiplier, 1.0)).sqrt()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_wv_edge_cases() {
        let w = w_non_draw(f64::NEG_INFINITY, 0.0, 1.0);

        assert!((w - 1.0).abs() < f64::EPSILON);

        let v = v_non_draw(f64::NEG_INFINITY, 0.0, 1.0);

        assert!(v == f64::INFINITY);

        let w2 = w_draw(f64::NEG_INFINITY, 0.0, 1.0);

        assert!((w2 - 1.0).abs() < f64::EPSILON);

        let v2 = v_draw(f64::NEG_INFINITY, 0.0, 1.0);

        assert!(v2 == f64::INFINITY);

        let w3 = w_non_draw(1.0, f64::INFINITY, 1.0);

        assert!((w3 - 0.0).abs() < f64::EPSILON);

        let v3 = v_draw(f64::INFINITY, f64::MAX, 1.0);

        assert!(v3 == f64::NEG_INFINITY);
    }
}
