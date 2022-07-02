import gleam/pair
import gleam/list
import gleam_stats/generators
import gleam_stats/distributions/binomial
import gleam_stats/stats
import gleeunit
import gleeunit/should
import gleam/int

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

// Parameters of a binomial distribution (discrete)
// Number of trials
const n: Int = 40

// Probability of a success for each trial
const p: Float = 0.5

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

// Test that the implemented probability mass function (pmf) of a 
// binomial distribution (discrete) is correct by checking equality a
// certain analytically calculated points
pub fn binomial_pmf_test() {
  let xs: List(Int) = [0, 20, 40]
  let fxs: List(Float) = [0.0, 0.1253706876195792574436, 0.0]
  let vs: List(#(Int, Float)) = list.zip(xs, fxs)
  vs
  |> list.map(fn(v: #(Int, Float)) -> Bool {
    pair.first(v)
    |> binomial.binomial_pmf(n, p)
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

// Test that the implemented cumulative distribution function (cdf) of a 
// binomial distribution (discrete) is correct by checking equality a
// certain analytically calculated points
pub fn binomial_cdf_test() {
  let xs: List(Int) = [0, 20, 40]
  let fxs: List(Float) = [0.0, 0.5626853438097896287218, 1.0]
  let vs: List(#(Int, Float)) = list.zip(xs, fxs)
  vs
  |> list.map(fn(v: #(Int, Float)) -> Bool {
    pair.first(v)
    |> binomial.binomial_cdf(n, p)
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

pub fn binomial_random_test() {
  assert Ok(mean) = binomial.binomial_mean(n, p)
  assert Ok(variance) = binomial.binomial_variance(n, p)
  assert Ok(out) =
    generators.seed_pcg32(5, 1)
    |> binomial.binomial_random(n, p, m)
  // Make sure the sample mean of the generated binomial random numbers
  // is close to the analytically calculated mean
  pair.first(out)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.mean()
  |> fn(x: Result(Float, String)) -> Bool {
    case x {
      Ok(x) -> stats.isclose(x, mean, random_rtol, random_atol)
      _ -> False
    }
  }
  |> should.be_true()

  // Make sure the sample variance of the generated binomial random numbers
  // is close to the analytically calculated variance
  pair.first(out)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.var(1)
  |> fn(x: Result(Float, String)) -> Bool {
    case x {
      Ok(x) -> stats.isclose(x, variance, random_rtol, random_atol)
      _ -> False
    }
  }
  |> should.be_true()
}
