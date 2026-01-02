/// The following functions could have been imported from some math crate,
/// but in order to keep this crate dependency-free, we implement them ourselves.
/// For more information:
/// - https://en.wikipedia.org/wiki/Error_function#Complementary_error_function
/// - https://en.wikipedia.org/wiki/Error_function#Cumulative_distribution_function
/// - https://en.wikipedia.org/wiki/Probability_density_function
use std::f64::consts::{FRAC_1_SQRT_2, FRAC_2_SQRT_PI, PI, SQRT_2};

/// The complementary error function.
pub fn erfc(x: f64) -> f64 {
    let z = x.abs();
    let t = (1.0 + z / 2.0).recip();

    // I know this looks dumb but clippy insists that mul_add increases performance.
    let r = t * t
        .mul_add(
            t.mul_add(
                t.mul_add(
                    t.mul_add(
                        t.mul_add(
                            t.mul_add(
                                t.mul_add(
                                    t.mul_add(t.mul_add(0.170_872_77, -0.822_152_23), 1.488_515_87),
                                    -1.135_203_98,
                                ),
                                0.278_868_07,
                            ),
                            -0.186_288_06,
                        ),
                        0.096_784_18,
                    ),
                    0.374_091_96,
                ),
                1.000_023_68,
            ),
            (-z).mul_add(z, -1.265_512_23),
        )
        .exp();

    if x < 0.0 {
        2.0 - r
    } else {
        r
    }
}

#[allow(clippy::excessive_precision)]
/// The inverse of the complementary error function.
pub fn inverse_erfc(y: f64) -> f64 {
    if y >= 2.0 {
        return -100.0;
    } else if y <= 0.0 {
        return 100.0;
    }

    // If y = 0.xyz
    let zero_point = y < 1.0;

    // Overwriting y if it is equal to 1.xyz
    let y = if zero_point { y } else { 2.0 - y };

    let t = (-2.0 * (y / 2.0).ln()).sqrt();

    let mut x = -FRAC_1_SQRT_2
        * (t.mul_add(0.27061, 2.30753) / t.mul_add(t.mul_add(0.04481, 0.99229), 1.0) - t);

    for _ in 0..2 {
        let err = erfc(x) - y;
        x += err / FRAC_2_SQRT_PI.mul_add((-(x.powi(2))).exp(), -x * err);
    }

    if zero_point {
        x
    } else {
        -x
    }
}

/// The cumulative distribution function.
pub fn cdf(x: f64, mu: f64, sigma: f64) -> f64 {
    // In our case, mu and sigma will always be 0.0 and 1.0.
    0.5 * erfc(-(x - mu) / (sigma * SQRT_2))
}

/// The inverse of the cumulative distribution function.
pub fn inverse_cdf(x: f64, mu: f64, sigma: f64) -> f64 {
    (sigma * SQRT_2).mul_add(-inverse_erfc(2.0 * x), mu)
}

/// The probability density function.
pub fn pdf(x: f64, mu: f64, sigma: f64) -> f64 {
    ((2.0 * PI).sqrt() * sigma.abs()).recip() * (-(((x - mu) / sigma.abs()).powi(2) / 2.0)).exp()
}

#[cfg(test)]
mod tests {
    use super::*;

    // Some tests to validate the math.
    #[test]
    fn test_erfc() {
        let err = erfc(0.5);

        assert!((err - 0.479_500).abs() < 0.000_001);
    }

    #[test]
    fn test_inverse_erfc() {
        let err = inverse_erfc(0.5);

        assert!((err - 0.476_936).abs() < 0.000_001);

        let err = inverse_erfc(3.0);

        assert!((err + 100.0).abs() < f64::EPSILON);

        let err = inverse_erfc(-1.0);

        assert!((err - 100.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_cdf() {
        let dist = cdf(5.5, 12.0, 5.0);

        assert!((dist - 0.096_800).abs() < 0.000_001);

        let dist_inf = cdf(f64::INFINITY, 0.0, 1.0);

        assert!((dist_inf - 1.0).abs() < f64::EPSILON);

        let dist_neg_inf = cdf(f64::NEG_INFINITY, 0.0, 1.0);

        assert!((dist_neg_inf - 0.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_inverse_cdf() {
        let dist = inverse_cdf(0.055, 12.0, 5.0);

        assert!((dist - 4.009_034).abs() < 0.000_001);
    }

    #[test]
    fn test_pdf() {
        let p = pdf(2.5, 0.0, 1.0);

        assert!((p - 0.017_528).abs() < 0.000_001);
    }
}
