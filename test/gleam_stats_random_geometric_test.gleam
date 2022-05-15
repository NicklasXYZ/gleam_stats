import gleam/pair
import gleam/list
import gleam_stats/generators
import gleam_stats/distributions/geometric
import gleam_stats/stats
import gleeunit
import gleeunit/should
import gleam/int

pub fn main() {
  gleeunit.main()
}

// The relative tolerance
const rtol: Float = 0.025

// The absolute tolerance
const atol: Float = 0.025

// Number of random numbers to generate when validating the 
// population mean and variance of the generated random numbers
const n: Int = 25_000

// Parameters of a geometric distribution (discrete)
// Success probability
const p: Float = 0.5

// Test that the implemented probability mass function (pmf) of a 
// geometric distribution (discrete) is correct by checking equality a
// certain analytically calculated points
pub fn geometric_pmf_test() {
  let xs: List(Int) = [-1, 0, 1, 2, 100]
  //, 20, 40]
  let fxs: List(Float) = [0.0, 0.5, 0.25, 0.125, 0.0]
  //, 0.1253706876195792574436, 0.0]
  let vs: List(#(Int, Float)) = list.zip(xs, fxs)
  vs
  |> list.map(fn(v: #(Int, Float)) -> Bool {
    pair.first(v)
    |> geometric.geometric_pmf(p)
    |> fn(x: Result(Float, String)) {
      case x {
        Ok(x) ->
          x
          |> stats.isclose(pair.second(v), rtol, atol)
        _ -> False
      }
    }
  })
  |> list.all(fn(a: Bool) -> Bool { a })
  |> should.be_true()
}

// Test that the implemented cumulative distribution function (cdf) of a 
// geometric distribution (discrete) is correct by checking equality a
// certain analytically calculated points
pub fn geometric_cdf_test() {
  let xs: List(Int) = [-1, 0, 1, 2, 100]
  //, 20, 40]
  let fxs: List(Float) = [0.0, 0.5, 0.75, 0.875, 1.0]
  let vs: List(#(Int, Float)) = list.zip(xs, fxs)
  vs
  |> list.map(fn(v: #(Int, Float)) -> Bool {
    pair.first(v)
    |> geometric.geometric_cdf(p)
    |> fn(x: Result(Float, String)) {
      case x {
        Ok(x) ->
          x
          |> stats.isclose(pair.second(v), rtol, atol)
        _ -> False
      }
    }
  })
  |> list.all(fn(a: Bool) -> Bool { a })
  |> should.be_true()
}

pub fn geometric_random_test() {
  assert Ok(mean) = geometric.geometric_mean(p)
  assert Ok(variance) = geometric.geometric_variance(p)
  assert Ok(out) =
    generators.seed_pcg32(5, 1)
    |> geometric.geometric_random(p, n)

  // Make sure the population mean of the generated geometric random numbers
  // is close to the analytically calculated mean
  pair.first(out)
  |> list.map(fn(x) { int.to_float(x) })
  |> stats.mean()
  |> fn(x) {
    case x {
      Ok(x) -> stats.isclose(x, mean, rtol, atol)
      _ -> False
    }
  }
  |> should.be_true()

  // Make sure the population variance of the generated geometric random numbers
  // is close to the analytically calculated variance
  pair.first(out)
  |> list.map(fn(x) { int.to_float(x) })
  |> stats.var(1)
  |> fn(x) {
    case x {
      Ok(x) -> stats.isclose(x, variance, rtol, atol)
      _ -> False
    }
  }
  |> should.be_true()
}
