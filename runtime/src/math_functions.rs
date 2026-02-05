//! Basic mathematical functions for Kraken runtime.
//!
//! Provides fundamental math operations that can be called from Kraken code.

use std::ffi::c_double;

/// Absolute value of a number
#[no_mangle]
pub extern "C" fn kraken_abs(x: c_double) -> c_double {
    x.abs()
}

/// Square root
#[no_mangle]
pub extern "C" fn kraken_sqrt(x: c_double) -> c_double {
    x.sqrt()
}

/// Power function
#[no_mangle]
pub extern "C" fn kraken_pow(base: c_double, exp: c_double) -> c_double {
    base.powf(exp)
}

/// Natural logarithm
#[no_mangle]
pub extern "C" fn kraken_log(x: c_double) -> c_double {
    x.ln()
}

/// Base-10 logarithm
#[no_mangle]
pub extern "C" fn kraken_log10(x: c_double) -> c_double {
    x.log10()
}

/// Exponential function (e^x)
#[no_mangle]
pub extern "C" fn kraken_exp(x: c_double) -> c_double {
    x.exp()
}

/// Sine function
#[no_mangle]
pub extern "C" fn kraken_sin(x: c_double) -> c_double {
    x.sin()
}

/// Cosine function
#[no_mangle]
pub extern "C" fn kraken_cos(x: c_double) -> c_double {
    x.cos()
}

/// Tangent function
#[no_mangle]
pub extern "C" fn kraken_tan(x: c_double) -> c_double {
    x.tan()
}

/// Arc sine
#[no_mangle]
pub extern "C" fn kraken_asin(x: c_double) -> c_double {
    x.asin()
}

/// Arc cosine
#[no_mangle]
pub extern "C" fn kraken_acos(x: c_double) -> c_double {
    x.acos()
}

/// Arc tangent
#[no_mangle]
pub extern "C" fn kraken_atan(x: c_double) -> c_double {
    x.atan()
}

/// Two-argument arc tangent
#[no_mangle]
pub extern "C" fn kraken_atan2(y: c_double, x: c_double) -> c_double {
    y.atan2(x)
}

/// Ceiling function
#[no_mangle]
pub extern "C" fn kraken_ceil(x: c_double) -> c_double {
    x.ceil()
}

/// Floor function
#[no_mangle]
pub extern "C" fn kraken_floor(x: c_double) -> c_double {
    x.floor()
}

/// Round to nearest integer
#[no_mangle]
pub extern "C" fn kraken_round(x: c_double) -> c_double {
    x.round()
}

/// Truncate to integer
#[no_mangle]
pub extern "C" fn kraken_trunc(x: c_double) -> c_double {
    x.trunc()
}

/// Minimum of two numbers
#[no_mangle]
pub extern "C" fn kraken_min(a: c_double, b: c_double) -> c_double {
    a.min(b)
}

/// Maximum of two numbers
#[no_mangle]
pub extern "C" fn kraken_max(a: c_double, b: c_double) -> c_double {
    a.max(b)
}

/// Hyperbolic sine
#[no_mangle]
pub extern "C" fn kraken_sinh(x: c_double) -> c_double {
    x.sinh()
}

/// Hyperbolic cosine
#[no_mangle]
pub extern "C" fn kraken_cosh(x: c_double) -> c_double {
    x.cosh()
}

/// Hyperbolic tangent
#[no_mangle]
pub extern "C" fn kraken_tanh(x: c_double) -> c_double {
    x.tanh()
}

/// Mathematical constants
pub mod constants {
    use std::ffi::c_double;

    /// Pi constant
    #[no_mangle]
    pub static KRAKEN_PI: c_double = std::f64::consts::PI;

    /// Euler's number
    #[no_mangle]
    pub static KRAKEN_E: c_double = std::f64::consts::E;

    /// Square root of 2
    #[no_mangle]
    pub static KRAKEN_SQRT_2: c_double = std::f64::consts::SQRT_2;

    /// Natural log of 2
    #[no_mangle]
    pub static KRAKEN_LN_2: c_double = std::f64::consts::LN_2;

    /// Natural log of 10
    #[no_mangle]
    pub static KRAKEN_LN_10: c_double = std::f64::consts::LN_10;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_abs() {
        assert_eq!(kraken_abs(-5.0), 5.0);
        assert_eq!(kraken_abs(5.0), 5.0);
        assert_eq!(kraken_abs(0.0), 0.0);
    }

    #[test]
    fn test_sqrt() {
        assert_eq!(kraken_sqrt(4.0), 2.0);
        assert_eq!(kraken_sqrt(9.0), 3.0);
        assert_eq!(kraken_sqrt(0.0), 0.0);
    }

    #[test]
    fn test_pow() {
        assert_eq!(kraken_pow(2.0, 3.0), 8.0);
        assert_eq!(kraken_pow(10.0, 2.0), 100.0);
        assert_eq!(kraken_pow(5.0, 0.0), 1.0);
    }

    #[test]
    fn test_log() {
        assert!((kraken_log(std::f64::consts::E) - 1.0).abs() < 1e-10);
        assert!((kraken_log10(100.0) - 2.0).abs() < 1e-10);
    }

    #[test]
    fn test_exp() {
        assert!((kraken_exp(1.0) - std::f64::consts::E).abs() < 1e-10);
        assert_eq!(kraken_exp(0.0), 1.0);
    }

    #[test]
    fn test_trig() {
        let pi_4 = std::f64::consts::PI / 4.0;
        assert!((kraken_sin(pi_4) - std::f64::consts::FRAC_1_SQRT_2).abs() < 1e-10);
        assert!((kraken_cos(pi_4) - std::f64::consts::FRAC_1_SQRT_2).abs() < 1e-10);
        assert!((kraken_tan(pi_4) - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_inverse_trig() {
        assert!((kraken_asin(1.0) - std::f64::consts::FRAC_PI_2).abs() < 1e-10);
        assert!((kraken_acos(0.0) - std::f64::consts::FRAC_PI_2).abs() < 1e-10);
        assert!((kraken_atan(1.0) - std::f64::consts::FRAC_PI_4).abs() < 1e-10);
    }

    #[test]
    fn test_atan2() {
        assert!((kraken_atan2(1.0, 1.0) - std::f64::consts::FRAC_PI_4).abs() < 1e-10);
        assert!((kraken_atan2(1.0, 0.0) - std::f64::consts::FRAC_PI_2).abs() < 1e-10);
    }

    #[test]
    fn test_rounding() {
        assert_eq!(kraken_ceil(3.2), 4.0);
        assert_eq!(kraken_floor(3.8), 3.0);
        assert_eq!(kraken_round(3.5), 4.0);
        assert_eq!(kraken_trunc(3.9), 3.0);
    }

    #[test]
    fn test_min_max() {
        assert_eq!(kraken_min(3.0, 5.0), 3.0);
        assert_eq!(kraken_max(3.0, 5.0), 5.0);
    }

    #[test]
    fn test_hyperbolic() {
        let x = 1.0;
        assert!((kraken_sinh(x) - x.sinh()).abs() < 1e-10);
        assert!((kraken_cosh(x) - x.cosh()).abs() < 1e-10);
        assert!((kraken_tanh(x) - x.tanh()).abs() < 1e-10);
    }
}
