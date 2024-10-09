use std::cmp::Ordering;
use std::ops::{Div, Mul};

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Gaussian {
    pub pi: f64,
    pub tau: f64,
}

impl Gaussian {
    #[allow(clippy::float_cmp)]
    pub fn with_mu_sigma(mu: f64, sigma: f64) -> Self {
        assert_ne!(sigma, 0.0, "Sigma cannot be equal to 0.0");

        let pi = sigma.powi(-2);
        Self { pi, tau: pi * mu }
    }

    pub const fn with_pi_tau(pi: f64, tau: f64) -> Self {
        Self { pi, tau }
    }

    pub fn mu(&self) -> f64 {
        if self.pi == 0.0 {
            return 0.0;
        }

        self.tau / self.pi
    }

    pub fn sigma(&self) -> f64 {
        if self.pi == 0.0 {
            return f64::INFINITY;
        }

        self.pi.recip().sqrt()
    }
}

impl Mul for Gaussian {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self {
            pi: self.pi + rhs.pi,
            tau: self.tau + rhs.tau,
        }
    }
}

impl Div for Gaussian {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self {
            pi: self.pi - rhs.pi,
            tau: self.tau - rhs.tau,
        }
    }
}

impl PartialOrd for Gaussian {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.mu().partial_cmp(&other.mu())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gaussian_edge_cases() {
        let g1 = Gaussian::with_pi_tau(0.0, 0.0);

        assert!(g1.sigma() == f64::INFINITY);
        assert!(g1.mu() == 0.0);
    }

    #[test]
    fn test_gaussian_ordering() {
        let g1 = Gaussian::with_mu_sigma(1.0, 1.0);
        let g2 = Gaussian::with_mu_sigma(2.0, 1.0);

        assert!(g1 < g2);
    }

    #[test]
    #[should_panic(expected = "Sigma cannot be equal to 0.0")]
    fn test_invalid_sigma() {
        let g1 = Gaussian::with_mu_sigma(0.0, 0.0);

        g1.mu();
    }
}
