//// Small examples ALSO used in the docs...

// import gleam/should
import gleam_stats/math
import gleam_stats/stats
import gleeunit
import gleeunit/should
import gleam/result

pub fn main() {
  gleeunit.main()
}

pub fn example_round_test() {
  math.round(0.4444, 2)
  |> should.equal(0.44)
  math.round(0.4445, 2)
  |> should.equal(0.44)
  math.round(0.4455, 2)
  |> should.equal(0.45)
  math.round(0.4555, 2)
  |> should.equal(0.46)
}

pub fn example_factorial_test() {
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

pub fn example_combination_test() {
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
}

pub fn example_permutation_test() {
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

pub fn example_gammainc_test() {
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
