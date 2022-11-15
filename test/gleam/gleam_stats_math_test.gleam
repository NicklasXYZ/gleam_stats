//// Small examples ALSO used in the docs...

import gleam_stats/math
import gleam_stats/stats
import gleeunit
import gleeunit/should
import gleam/result
import gleam/io

pub fn main() {
  gleeunit.main()
}

pub fn math_round_test() {
  math.round(0.4444, 2)
  |> should.equal(0.44)

  math.round(0.4445, 2)
  |> should.equal(0.44)

  math.round(0.4455, 2)
  |> should.equal(0.45)

  math.round(0.4555, 2)
  |> should.equal(0.46)
}

pub fn math_factorial_test() {
  // Invalid input gives an error
  math.factorial(-1)
  |> should.be_error()

  // Valid input returns a result
  math.factorial(0)
  |> should.equal(Ok(1))

  math.factorial(1)
  |> should.equal(Ok(1))

  math.factorial(2)
  |> should.equal(Ok(2))

  math.factorial(3)
  |> should.equal(Ok(6))

  math.factorial(4)
  |> should.equal(Ok(24))
}

pub fn math_combination_test() {
  // Invalid input gives an error
  // Error on: n = -1 < 0
  math.combination(-1, 1)
  |> should.be_error()

  // Valid input returns a result
  math.combination(4, 0)
  |> should.equal(Ok(1))

  math.combination(4, 4)
  |> should.equal(Ok(1))

  math.combination(4, 2)
  |> should.equal(Ok(6))

  math.combination(7, 5)
  |> should.equal(Ok(21))
  // NOTE: Tests with the 'combination' function that produce values that
  // exceed precision of the JavaScript 'Number' primitive will result in
  // errors
}

pub fn math_permutation_test() {
  // Invalid input gives an error
  // Error on: n = -1 < 0
  math.permutation(-1, 1)
  |> should.be_error()

  // Valid input returns a result
  math.permutation(4, 0)
  |> should.equal(Ok(1))

  math.permutation(4, 4)
  |> should.equal(Ok(1))

  math.permutation(4, 2)
  |> should.equal(Ok(12))
}

pub fn math_gammainc_test() {
  // Invalid input gives an error
  // 1st arg is invalid
  math.gammainc(-1.0, 1.0)
  |> should.be_error()

  // 2nd arg is invalid
  math.gammainc(1.0, -1.0)
  |> should.be_error()

  // Valid input returns a result
  math.gammainc(1., 0.)
  |> result.unwrap(-999.)
  |> stats.isclose(0.0, 0.0, 0.01)
  |> should.be_true()

  math.gammainc(1., 2.)
  |> result.unwrap(-999.)
  |> stats.isclose(0.864664716763387308106, 0.0, 0.01)
  |> should.be_true()

  math.gammainc(2., 3.)
  |> result.unwrap(-999.)
  |> stats.isclose(0.8008517265285442280826, 0.0, 0.01)
  |> should.be_true()

  math.gammainc(3., 4.)
  |> result.unwrap(-999.)
  |> stats.isclose(1.523793388892911312363, 0.0, 0.01)
  |> should.be_true()
}

pub fn math_sin_test() {
  // Check that the function agrees, at some arbitrary input
  // points, with known function values
  math.sin(0.0)
  |> should.equal(0.0)

  math.sin(0.5 *. math.pi())
  |> should.equal(1.0)

  assert Ok(tol) = math.pow(-10., -6.)
  math.sin(0.5)
  |> stats.isclose(0.479425, 0.0, tol)
  |> should.be_true()
}

pub fn math_asin_test() {
  // Check that the function agrees, at some arbitrary input
  // points, with known function values
  math.asin(0.0)
  |> should.equal(Ok(0.0))

  assert Ok(tol) = math.pow(-10., -6.)
  assert Ok(result) = math.asin(0.5)
  result
  |> stats.isclose(0.523598, 0.0, tol)
  |> should.be_true()

  // Check that we get an error when the function is evaluated
  // outside its domain
  math.asin(1.1)
  |> should.be_error()

  math.asin(-1.1)
  |> should.be_error()
}

pub fn math_sinh_test() {
  // Check that the function agrees, at some arbitrary input
  // points, with known function values
  math.sinh(0.0)
  |> should.equal(0.0)

  assert Ok(tol) = math.pow(-10., -6.)
  math.sinh(0.5)
  |> stats.isclose(0.521095, 0.0, tol)
  |> should.be_true()
  // An (overflow) error might occur when given an input
  // value that will result in a too large output value
  // e.g. math.sinh(1000.0) but this is a property of the
  // runtime.
}

pub fn math_asinh_test() {
  // Check that the function agrees, at some arbitrary input
  // points, with known function values
  math.asinh(0.0)
  |> should.equal(0.0)

  assert Ok(tol) = math.pow(-10., -6.)
  math.asinh(0.5)
  |> stats.isclose(0.481211, 0.0, tol)
  |> should.be_true()
}

pub fn math_cos_test() {
  // Check that the function agrees, at some arbitrary input
  // points, with known function values
  math.cos(0.0)
  |> should.equal(1.0)

  math.cos(math.pi())
  |> should.equal(-1.0)

  assert Ok(tol) = math.pow(-10., -6.)
  math.cos(0.5)
  |> stats.isclose(0.877582, 0.0, tol)
  |> should.be_true()
}

pub fn math_acos_test() {
  // Check that the function agrees, at some arbitrary input
  // points, with known function values
  math.acos(1.0)
  |> should.equal(Ok(0.0))

  assert Ok(tol) = math.pow(-10., -6.)
  assert Ok(result) = math.acos(0.5)
  result
  |> stats.isclose(1.047197, 0.0, tol)
  |> should.be_true()

  // Check that we get an error when the function is evaluated
  // outside its domain
  math.acos(1.1)
  |> should.be_error()

  math.acos(-1.1)
  |> should.be_error()
}

pub fn math_cosh_test() {
  // Check that the function agrees, at some arbitrary input
  // points, with known function values
  math.cosh(0.0)
  |> should.equal(1.0)

  assert Ok(tol) = math.pow(-10., -6.)
  math.cosh(0.5)
  |> stats.isclose(1.127625, 0.0, tol)
  |> should.be_true()
  // An (overflow) error might occur when given an input
  // value that will result in a too large output value
  // e.g. math.cosh(1000.0) but this is a property of the
  // runtime.
}

pub fn math_acosh_test() {
  // Check that the function agrees, at some arbitrary input
  // points, with known function values
  math.acosh(1.0)
  |> should.equal(Ok(0.0))

  assert Ok(tol) = math.pow(-10., -6.)
  assert Ok(result) = math.acosh(5.)
  result
  |> stats.isclose(2.292431, 0.0, tol)
  |> should.be_true()

  // Check that we get an error when the function is evaluated
  // outside its domain
  math.acosh(0.0)
  |> should.be_error()
}

pub fn math_tan_test() {
  // Check that the function agrees, at some arbitrary input
  // points, with known function values
  math.tan(0.0)
  |> should.equal(0.0)

  assert Ok(tol) = math.pow(-10., -6.)
  math.tan(0.5)
  |> stats.isclose(0.546302, 0.0, tol)
  |> should.be_true()
}

pub fn math_atan_test() {
  // Check that the function agrees, at some arbitrary input
  // points, with known function values
  math.atan(0.0)
  |> should.equal(0.0)

  assert Ok(tol) = math.pow(-10., -6.)
  math.atan(0.5)
  |> stats.isclose(0.463647, 0.0, tol)
  |> should.be_true()
}

pub fn math_tanh_test() {
  // Check that the function agrees, at some arbitrary input
  // points, with known function values
  math.tanh(0.0)
  |> should.equal(0.0)

  math.tanh(25.0)
  |> should.equal(1.0)

  math.tanh(-25.0)
  |> should.equal(-1.0)

  assert Ok(tol) = math.pow(-10., -6.)
  math.tanh(0.5)
  |> stats.isclose(0.462117, 0.0, tol)
  |> should.be_true()
}

pub fn math_atanh_test() {
  // Check that the function agrees, at some arbitrary input
  // points, with known function values
  math.atanh(0.0)
  |> should.equal(Ok(0.0))

  assert Ok(tol) = math.pow(-10., -6.)
  assert Ok(result) = math.atanh(0.5)
  result
  |> stats.isclose(0.549306, 0.0, tol)
  |> should.be_true()

  // Check that we get an error when the function is evaluated
  // outside its domain
  math.atanh(1.0)
  |> should.be_error()

  math.atanh(2.0)
  |> should.be_error()

  math.atanh(1.0)
  |> should.be_error()

  math.atanh(-2.0)
  |> should.be_error()
}

pub fn math_atan2_test() {
  // Check that the function agrees, at some arbitrary input
  // points, with known function values
  math.atan2(0.0, 0.0)
  |> should.equal(0.0)

  math.atan2(0.0, 1.0)
  |> should.equal(0.0)

  // Check atan2(y=1.0, x=0.5)
  // Should be equal to atan(y / x) for any x > 0 and any y
  math.atan2(1.0, 0.5)
  |> should.equal(math.atan(1.0 /. 0.5))

  // Check atan2(y=2.0, x=-1.5)
  // Should be equal to pi + atan(y / x) for any x < 0 and y >= 0
  math.atan2(2.0, -1.5)
  |> should.equal(math.pi() +. math.atan(2.0 /. -1.5))

  // Check atan2(y=-2.0, x=-1.5)
  // Should be equal to atan(y / x) - pi for any x < 0 and y < 0
  math.atan2(-2.0, -1.5)
  |> should.equal(math.atan(-2.0 /. -1.5) -. math.pi())

  // Check atan2(y=1.5, x=0.0)
  // Should be equal to pi/2 for x = 0 and any y > 0
  math.atan2(1.5, 0.0)
  |> should.equal(math.pi() /. 2.)

  // Check atan2(y=-1.5, x=0.0)
  // Should be equal to -pi/2 for x = 0 and any y < 0
  math.atan2(-1.5, 0.0)
  |> should.equal(-1. *. math.pi() /. 2.)
}

pub fn math_exp_test() {
  // Check that the function agrees, at some arbitrary input
  // points, with known function values
  math.exp(0.0)
  |> should.equal(1.0)

  assert Ok(tol) = math.pow(-10., -6.)
  math.exp(0.5)
  |> stats.isclose(1.648721, 0.0, tol)
  |> should.be_true()
  // An (overflow) error might occur when given an input
  // value that will result in a too large output value
  // e.g. math.exp(1000.0) but this is a property of the
  // runtime.
}

pub fn math_log_test() {
  // Check that the function agrees, at some arbitrary input
  // points, with known function values
  math.log(1.0)
  |> should.equal(Ok(0.0))

  assert Ok(tol) = math.pow(-10., -6.)
  assert Ok(result) = math.log(0.5)
  result
  |> stats.isclose(-0.693147, 0.0, tol)
  |> should.be_true()

  // Check that we get an error when the function is evaluated
  // outside its domain
  math.log(-1.0)
  |> should.be_error()
}

pub fn math_log2_test() {
  // Check that the function agrees, at some arbitrary input
  // points, with known function values
  math.log2(1.0)
  |> should.equal(Ok(0.0))

  math.log2(2.0)
  |> should.equal(Ok(1.0))

  assert Ok(tol) = math.pow(-10., -6.)
  assert Ok(result) = math.log2(5.)
  result
  |> stats.isclose(2.321928, 0.0, tol)
  |> should.be_true()

  // Check that we get an error when the function is evaluated
  // outside its domain
  math.log2(-1.0)
  |> should.be_error()
}

pub fn math_log10_test() {
  // Check that the function agrees, at some arbitrary input
  // points, with known function values
  math.log10(1.0)
  |> should.equal(Ok(0.0))

  math.log10(10.0)
  |> should.equal(Ok(1.0))

  assert Ok(tol) = math.pow(-10., -6.)
  assert Ok(result) = math.log10(50.)
  result
  |> stats.isclose(1.698970, 0.0, tol)
  |> should.be_true()

  // Check that we get an error when the function is evaluated
  // outside its domain
  math.log10(-1.0)
  |> should.be_error()
}

pub fn to_radians_test() {
  math.to_radians(0.0)
  |> should.equal(0.0)

  math.to_radians(360.)
  |> should.equal(2. *. math.pi())
}

pub fn to_degrees_test() {
  math.to_degrees(0.0)
  |> should.equal(0.0)

  math.to_degrees(2. *. math.pi())
  |> should.equal(360.)
}

pub fn math_ceil_test() {
  math.ceil(0.1)
  |> should.equal(1.0)

  math.ceil(0.9)
  |> should.equal(1.0)
}

pub fn math_floor_test() {
  math.floor(0.1)
  |> should.equal(0.0)

  math.floor(0.9)
  |> should.equal(0.0)
}

pub fn math_pow_test() {
  math.pow(2.0, 2.0)
  |> should.equal(Ok(4.0))

  math.pow(-5.0, 3.0)
  |> should.equal(Ok(-125.0))

  math.pow(10.5, 0.0)
  |> should.equal(Ok(1.0))

  math.pow(16.0, 0.5)
  |> should.equal(Ok(4.0))

  math.pow(2.0, -1.0)
  |> should.equal(Ok(0.5))

  math.pow(2.0, -1.0)
  |> should.equal(Ok(0.5))

  // math.pow(-1.0, 0.5) is equivalent to float.square_root(-1.0)
  // and should return an error as an imaginary number would otherwise
  // have to be returned
  math.pow(-1.0, 0.5)
  |> should.be_error()

  // Check another case with a negative base and fractional exponent
  math.pow(-1.5, 1.5)
  |> should.be_error()

  // math.pow(0.0, -1.0) is equivalent to 1. /. 0 and is expected
  // to be an error
  math.pow(0.0, -1.0)
  |> should.be_error()

  // Check that a negative base and exponent is fine as long as the
  // exponent is not fractional
  math.pow(-2.0, -1.0)
  |> should.equal(Ok(-0.5))
}

pub fn math_sign_test() {
  math.sign(100.0)
  |> should.equal(1.0)

  math.sign(0.0)
  |> should.equal(0.0)

  math.sign(-100.0)
  |> should.equal(-1.0)
}

pub fn math_beta_test() {
  io.debug("TODO: Implement tests for 'math.beta'.")
}

pub fn math_erf_test() {
  io.debug("TODO: Implement tests for 'math.erf'.")
}

pub fn math_gamma_test() {
  io.debug("TODO: Implement tests for 'math.gamma'.")
}

pub fn absolute_int_difference_test() {
  math.absolute_int_difference(0, 0)
  |> should.equal(0)

  math.absolute_int_difference(1, 2)
  |> should.equal(1)

  math.absolute_int_difference(2, 1)
  |> should.equal(1)

  math.absolute_int_difference(-1, 0)
  |> should.equal(1)

  math.absolute_int_difference(0, -1)
  |> should.equal(1)

  math.absolute_int_difference(10, 20)
  |> should.equal(10)

  math.absolute_int_difference(-10, -20)
  |> should.equal(10)

  math.absolute_int_difference(-10, 10)
  |> should.equal(20)
}

pub fn absolute_float_difference_test() {
  math.absolute_float_difference(0.0, 0.0)
  |> should.equal(0.0)

  math.absolute_float_difference(1.0, 2.0)
  |> should.equal(1.0)

  math.absolute_float_difference(2.0, 1.0)
  |> should.equal(1.0)

  math.absolute_float_difference(-1.0, 0.0)
  |> should.equal(1.0)

  math.absolute_float_difference(0.0, -1.0)
  |> should.equal(1.0)

  math.absolute_float_difference(10.0, 20.0)
  |> should.equal(10.0)

  math.absolute_float_difference(-10.0, -20.0)
  |> should.equal(10.0)

  math.absolute_float_difference(-10.5, 10.5)
  |> should.equal(21.0)
}
