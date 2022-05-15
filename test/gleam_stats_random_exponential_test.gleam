import gleam/pair
import gleam/list
import gleam_stats/generators
import gleam_stats/distributions/exponential
import gleam_stats/stats
import gleeunit
import gleeunit/should

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

// The rate parameter of a exponential distribution (continuous) 
const lambda: Float = 1.0

// Test that the implemented probability density function (pdf) of a 
// exponential distribution (continuous) is correct by checking equality a
// certain analytically calculated points
pub fn exponential_pdf_test() {
  let xs: List(Float) = [0.0, 1.0, 2.0, 3.0, 100.0]
  let fxs: List(Float) = [
    1.0, 0.36787944117144233, 0.1353352832366127, 0.049787068367863944, 0.0,
  ]
  let vs: List(#(Float, Float)) = list.zip(xs, fxs)

  vs
  |> list.map(fn(v: #(Float, Float)) -> Bool {
    pair.first(v)
    |> exponential.exponential_pdf(lambda)
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

// Test that the cumulative distribution function (cdf) of a 
// exponential distribution (continuous) is correct by checking equality a
// certain analytically calculated points
pub fn exponential_cdf_test() {
  let xs: List(Float) = [0.0, 1.0, 2.0, 3.0]
  let fxs: List(Float) = [
    0.0, 0.6321205588285577, 0.8646647167633873, 0.950212931632136, 1.0,
  ]
  let vs: List(#(Float, Float)) = list.zip(xs, fxs)
  vs
  |> list.map(fn(v: #(Float, Float)) -> Bool {
    pair.first(v)
    |> exponential.exponential_cdf(lambda)
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

pub fn exponential_random_test() {
  assert Ok(mean) = exponential.exponential_mean(lambda)
  assert Ok(variance) = exponential.exponential_variance(lambda)
  assert Ok(out) =
    generators.seed_pcg32(5, 1)
    |> exponential.exponential_random(lambda, n)

  // Make sure the population mean of the generated exponential random numbers
  // is close to the analytically calculated mean
  pair.first(out)
  |> stats.mean()
  |> fn(x) {
    case x {
      Ok(x) -> stats.isclose(x, mean, rtol, atol)
      _ -> False
    }
  }
  |> should.be_true()

  // Make sure the population variance of the generated exponential random numbers
  // is close to the analytically calculated variance
  pair.first(out)
  |> stats.var(1)
  |> fn(x) {
    case x {
      Ok(x) -> stats.isclose(x, variance, rtol, atol)
      _ -> False
    }
  }
  |> should.be_true()
}
