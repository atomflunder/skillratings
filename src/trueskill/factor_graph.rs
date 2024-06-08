use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::gaussian::Gaussian;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Variable {
    pub gaussian: Gaussian,
    messages: HashMap<u32, Gaussian>,
}

impl Variable {
    pub fn new() -> Self {
        Self {
            gaussian: Gaussian::default(),
            messages: HashMap::new(),
        }
    }

    fn set(&mut self, val: Gaussian) -> f64 {
        let delta = self.delta(val);

        self.gaussian.pi = val.pi;
        self.gaussian.tau = val.tau;

        delta
    }

    fn update_message(&mut self, factor_id: u32, message: Gaussian) -> f64 {
        let old_message = self.messages[&factor_id];
        let v = self.messages.entry(factor_id).or_default();
        *v = message;

        self.set(self.gaussian / old_message * message)
    }

    fn update_value(&mut self, factor_id: u32, val: Gaussian) -> f64 {
        let old_message = self.messages[&factor_id];
        let v = self.messages.entry(factor_id).or_default();
        *v = val * old_message / self.gaussian;

        self.set(val)
    }

    fn delta(&self, other: Gaussian) -> f64 {
        let pi_delta = (self.gaussian.pi - other.pi).abs();
        if pi_delta.is_infinite() {
            return 0.0;
        }

        (self.gaussian.tau - other.tau).abs().max(pi_delta.sqrt())
    }
}

pub struct PriorFactor {
    id: u32,
    pub variable: Rc<RefCell<Variable>>,
    val: Gaussian,
    dynamic: f64,
}

impl PriorFactor {
    pub fn new(id: u32, variable: Rc<RefCell<Variable>>, val: Gaussian, dynamic: f64) -> Self {
        variable.borrow_mut().messages.entry(id).or_default();

        Self {
            id,
            variable,
            val,
            dynamic,
        }
    }

    pub fn down(&mut self) -> f64 {
        let sigma = self.val.sigma().hypot(self.dynamic);
        let value = Gaussian::with_mu_sigma(self.val.mu(), sigma);
        self.variable.borrow_mut().update_value(self.id, value)
    }
}

pub struct LikelihoodFactor {
    id: u32,
    mean: Rc<RefCell<Variable>>,
    value: Rc<RefCell<Variable>>,
    variance: f64,
}

impl LikelihoodFactor {
    pub fn new(
        id: u32,
        mean: Rc<RefCell<Variable>>,
        value: Rc<RefCell<Variable>>,
        variance: f64,
    ) -> Self {
        mean.borrow_mut().messages.entry(id).or_default();
        value.borrow_mut().messages.entry(id).or_default();

        Self {
            id,
            mean,
            value,
            variance,
        }
    }

    pub fn down(&mut self) -> f64 {
        let msg = {
            let mean = self.mean.borrow();
            mean.gaussian / mean.messages[&self.id]
        };
        let a = self.calc_a(msg);
        self.value
            .borrow_mut()
            .update_message(self.id, Gaussian::with_pi_tau(a * msg.pi, a * msg.tau))
    }

    pub fn up(&mut self) -> f64 {
        let msg = {
            let value = self.value.borrow();
            value.gaussian / value.messages[&self.id]
        };
        let a = self.calc_a(msg);
        self.mean
            .borrow_mut()
            .update_message(self.id, Gaussian::with_pi_tau(a * msg.pi, a * msg.tau))
    }

    fn calc_a(&self, gaussian: Gaussian) -> f64 {
        self.variance.mul_add(gaussian.pi, 1.0).recip()
    }
}

pub struct SumFactor {
    id: u32,
    sum: Rc<RefCell<Variable>>,
    terms: Vec<Rc<RefCell<Variable>>>,
    coeffs: Vec<f64>,
}

impl SumFactor {
    pub fn new(
        id: u32,
        sum: Rc<RefCell<Variable>>,
        terms: Vec<Rc<RefCell<Variable>>>,
        coeffs: Vec<f64>,
    ) -> Self {
        sum.borrow_mut().messages.entry(id).or_default();
        for term in &terms {
            term.borrow_mut().messages.entry(id).or_default();
        }

        Self {
            id,
            sum,
            terms,
            coeffs,
        }
    }

    pub fn down(&mut self) -> f64 {
        let msgs: Vec<Gaussian> = self
            .terms
            .iter()
            .map(|term| term.borrow().messages[&self.id])
            .collect();
        self.update(&self.sum, &self.terms, &msgs, &self.coeffs)
    }

    pub fn up(&mut self, index: usize) -> f64 {
        let coeff = self.coeffs[index];
        let mut coeffs = Vec::new();
        for (x, c) in self.coeffs.iter().enumerate() {
            if coeff == 0.0 {
                coeffs.push(0.0);
            } else if x == index {
                coeffs.push(coeff.recip());
            } else {
                coeffs.push(-(*c) / coeff);
            }
        }

        let mut vals = self.terms.clone();
        vals[index] = self.sum.clone();
        let msgs: Vec<Gaussian> = vals
            .iter()
            .map(|val| val.borrow().messages[&self.id])
            .collect();

        self.update(&self.terms[index], &vals, &msgs, &coeffs)
    }

    #[inline]
    pub fn terms_len(&self) -> usize {
        self.terms.len()
    }

    fn update(
        &self,
        var: &Rc<RefCell<Variable>>,
        vals: &[Rc<RefCell<Variable>>],
        msgs: &[Gaussian],
        coeffs: &[f64],
    ) -> f64 {
        let mut pi_inv = 0.0_f64;
        let mut mu = 0.0;

        for ((val, msg), coeff) in vals.iter().zip(msgs).zip(coeffs) {
            let div = val.borrow().gaussian / *msg;
            mu += coeff * div.mu();
            if pi_inv.is_infinite() {
                continue;
            }

            if div.pi == 0.0 {
                pi_inv = f64::INFINITY;
            } else {
                pi_inv += coeff.powi(2) / div.pi;
            }
        }

        let pi = pi_inv.recip();
        let tau = pi * mu;

        var.borrow_mut()
            .update_message(self.id, Gaussian::with_pi_tau(pi, tau))
    }
}

pub struct TruncateFactor {
    id: u32,
    variable: Rc<RefCell<Variable>>,
    v_func: Box<dyn Fn(f64, f64) -> f64>,
    w_func: Box<dyn Fn(f64, f64) -> f64>,
    draw_margin: f64,
}

impl TruncateFactor {
    pub fn new(
        id: u32,
        variable: Rc<RefCell<Variable>>,
        v_func: Box<dyn Fn(f64, f64) -> f64>,
        w_func: Box<dyn Fn(f64, f64) -> f64>,
        draw_margin: f64,
    ) -> Self {
        variable.borrow_mut().messages.entry(id).or_default();

        Self {
            id,
            variable,
            v_func,
            w_func,
            draw_margin,
        }
    }

    pub fn up(&mut self) -> f64 {
        let div = {
            let variable = self.variable.borrow();
            variable.gaussian / variable.messages[&self.id]
        };
        let pi_sqrt = div.pi.sqrt();
        let arg_1 = div.tau / pi_sqrt;
        let arg_2 = self.draw_margin * pi_sqrt;
        let v = (self.v_func)(arg_1, arg_2);
        let w = (self.w_func)(arg_1, arg_2);
        let denom = 1.0 - w;

        let pi = div.pi / denom;
        let tau = pi_sqrt.mul_add(v, div.tau) / denom;

        self.variable
            .borrow_mut()
            .update_value(self.id, Gaussian::with_pi_tau(pi, tau))
    }
}
