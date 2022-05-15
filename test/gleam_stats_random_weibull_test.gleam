import gleam/pair
import gleam/list
import gleam_stats/generators
import gleam_stats/distributions/weibull
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

// The scale parameter of a weibull distribution (continuous) 
const lambda: Float = 1.0

// The shape parameter of a weibull distribution (continuous) 
const k: Float = 5.0

// Test that the implemented probability density function (pdf) of a 
// weibull distribution (continuous) is correct by checking equality a
// certain analytically calculated points
pub fn weibull_pdf_test() {
  let xs: List(Float) = [0.0, 1.0, 100.0]
  let fxs: List(Float) = [0.0, 1.839397206, 0.0]
  let vs: List(#(Float, Float)) = list.zip(xs, fxs)

  vs
  |> list.map(fn(v: #(Float, Float)) -> Bool {
    pair.first(v)
    |> weibull.weibull_pdf(lambda, k)
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
// weibull distribution (continuous) is correct by checking equality a
// certain analytically calculated points
pub fn weibull_cdf_test() {
  let xs: List(Float) = [-100.0, 1.0, 100.0]
  let fxs: List(Float) = [0.0, 0.632120558829, 1.0]
  let vs: List(#(Float, Float)) = list.zip(xs, fxs)
  vs
  |> list.map(fn(v: #(Float, Float)) -> Bool {
    pair.first(v)
    |> weibull.weibull_cdf(lambda, k)
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

pub fn weibull_random_test() {
  assert Ok(mean) = weibull.weibull_mean(lambda, k)
  assert Ok(variance) = weibull.weibull_variance(lambda, k)
  assert Ok(out) =
    generators.seed_pcg32(5, 1)
    |> weibull.weibull_random(lambda, k, n)

  // Make sure the population mean of the generated weibull random numbers
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

  // Make sure the population variance of the generated weibull random numbers
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
