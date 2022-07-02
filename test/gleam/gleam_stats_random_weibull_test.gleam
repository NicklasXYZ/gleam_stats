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
const random_rtol: Float = 0.025

const rtol: Float = 0.0

// The absolute tolerance
const random_atol: Float = 0.025

const atol: Float = 0.000_000_1

// Number of random numbers to generate when validating the 
// sample mean and variance of the generated random numbers
const m: Int = 4_000

// The scale parameter of a weibull distribution (continuous) 
const lambda: Float = 1.0

// The shape parameter of a weibull distribution (continuous) 
const k: Float = 5.0

// Temporary function used instead of 'list.all' from the standard
// library. There is currently a bug with the 'list.all' function
fn all(xs: List(Bool), initial: Bool) -> Bool {
  list.fold(
    xs,
    initial,
    fn(acc: Bool, x: Bool) -> Bool {
      case acc {
        True -> acc && x
        False -> False
      }
    },
  )
}

// Test that the implemented probability density function (pdf) of a 
// weibull distribution (continuous) is correct by checking equality a
// certain analytically calculated points
pub fn weibull_pdf_test() {
  let xs: List(Float) = [-100.0, 0.0, 1.0, 100.0]
  let fxs: List(Float) = [0.0, 0.0, 1.839397206, 0.0]
  let vs: List(#(Float, Float)) = list.zip(xs, fxs)

  vs
  |> list.map(fn(v: #(Float, Float)) -> Bool {
    pair.first(v)
    |> weibull.weibull_pdf(lambda, k)
    |> fn(x: Result(Float, String)) -> Bool {
      case x {
        Ok(x) ->
          x
          |> stats.isclose(pair.second(v), rtol, atol)
        _ -> False
      }
    }
  })
  // |> list.all(fn(a: Bool) -> Bool { a })
  |> all(True)
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
    |> fn(x: Result(Float, String)) -> Bool {
      case x {
        Ok(x) ->
          x
          |> stats.isclose(pair.second(v), rtol, atol)
        _ -> False
      }
    }
  })
  // |> list.all(fn(a: Bool) -> Bool { a })
  |> all(True)
  |> should.be_true()
}

pub fn weibull_random_test() {
  assert Ok(mean) = weibull.weibull_mean(lambda, k)
  assert Ok(variance) = weibull.weibull_variance(lambda, k)
  assert Ok(out) =
    generators.seed_pcg32(5, 1)
    |> weibull.weibull_random(lambda, k, m)

  // Make sure the sample mean of the generated weibull random numbers
  // is close to the analytically calculated mean
  pair.first(out)
  |> stats.mean()
  |> fn(x: Result(Float, String)) -> Bool {
    case x {
      Ok(x) -> stats.isclose(x, mean, random_rtol, random_atol)
      _ -> False
    }
  }
  |> should.be_true()

  // Make sure the sample variance of the generated weibull random numbers
  // is close to the analytically calculated variance
  pair.first(out)
  |> stats.var(1)
  |> fn(x: Result(Float, String)) -> Bool {
    case x {
      Ok(x) -> stats.isclose(x, variance, random_rtol, random_atol)
      _ -> False
    }
  }
  |> should.be_true()
}
