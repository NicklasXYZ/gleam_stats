import gleam/pair
import gleam/list
import gleam_stats/generators
import gleam_stats/distributions/negbinomial
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

// Parameters of a negbinomial distribution (discrete)
// Number of failes until the experiment is stopped
const r: Int = 40

// Success probability
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
// negbinomial distribution (discrete) is correct by checking equality a
// certain analytically calculated points
pub fn negbinomial_pmf_test() {
  let xs: List(Int) = [-100, 40, 45, 200]
  let fxs: List(Float) = [0.0, 0.044463939386953616, 0.0351105314186622, 0.0]
  let vs: List(#(Int, Float)) = list.zip(xs, fxs)
  vs
  |> list.map(fn(v: #(Int, Float)) -> Bool {
    pair.first(v)
    |> negbinomial.negbinomial_pmf(r, p)
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
// negbinomial distribution (discrete) is correct by checking equality a
// certain analytically calculated points
pub fn negbinomial_cdf_test() {
  let xs: List(Int) = [-100, 40, 45, 200]
  let fxs: List(Float) = [0.0, 0.5444639393869535, 0.7422871340580496, 1.0]
  let vs: List(#(Int, Float)) = list.zip(xs, fxs)
  vs
  |> list.map(fn(v: #(Int, Float)) -> Bool {
    pair.first(v)
    |> negbinomial.negbinomial_cdf(r, p)
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

pub fn negbinomial_random_test() {
  assert Ok(mean) = negbinomial.negbinomial_mean(r, p)
  assert Ok(variance) = negbinomial.negbinomial_variance(r, p)
  assert Ok(out) =
    generators.seed_pcg32(5, 1)
    |> negbinomial.negbinomial_random(r, p, m)

  // Make sure the sample mean of the generated negbinomial random numbers
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

  // Make sure the sample variance of the generated negbinomial random numbers
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
