import gleam/pair
import gleam/list
import gleam_stats/generators
import gleam_stats/distributions/uniform
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

// The minimum of a uniform distribution (continuos) 
const min: Float = 0.

// The maximum of a uniform distribution (continuos) 
const max: Float = 1.

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
// uniform distribution (continuous) is correct by checking equality a
// certain analytically calculated points
pub fn uniform_pdf_test() {
  let xs: List(Float) = [0.25, 0.5, 0.75, 1.0]
  let fxs: List(Float) = [1.0, 1.0, 1.0, 1.0]
  let vs: List(#(Float, Float)) = list.zip(xs, fxs)

  vs
  |> list.map(fn(v: #(Float, Float)) -> Bool {
    pair.first(v)
    |> uniform.uniform_pdf(min, max)
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
// uniform distribution (continuous) is correct by checking equality a
// certain analytically calculated points
pub fn uniform_cdf_test() {
  let xs: List(Float) = [-1.0, 0.0, 0.5, 1.0, 2.0]
  let fxs: List(Float) = [0.0, 0.0, 0.5, 1.0, 1.0]
  let vs: List(#(Float, Float)) = list.zip(xs, fxs)

  vs
  |> list.map(fn(v: #(Float, Float)) -> Bool {
    pair.first(v)
    |> uniform.uniform_cdf(min, max)
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

pub fn uniform_random_one_test() {
  assert Ok(mean) = uniform.uniform_mean(min, max)
  assert Ok(variance) = uniform.uniform_variance(min, max)
  assert Ok(out) =
    generators.seed_pcg32(5, 1)
    |> uniform.uniform_random(min, max, m)

  // Make sure the generated uniform random numbers are actually within
  // the given min/max bounds
  // pair.first(out)
  // |> list.all(fn(x: Float) -> Bool {
  //   case x <=. max, x >=. min {
  //     True, True -> True
  //     _, _ -> False
  //   }
  // })
  // |> should.be_true()
  pair.first(out)
  |> list.map(fn(x: Float) -> Bool {
    case x <=. max, x >=. min {
      True, True -> True
      _, _ -> False
    }
  })
  |> all(True)
  |> should.be_true()

  // Make sure the sample mean of the generated uniform random numbers is
  // close to the analytically calculated mean
  pair.first(out)
  |> stats.mean()
  |> fn(x: Result(Float, String)) -> Bool {
    case x {
      Ok(x) -> stats.isclose(x, mean, random_rtol, random_atol)
      _ -> False
    }
  }
  |> should.be_true()

  // Make sure the sample variance of the generated uniform random numbers is
  // close to the analytically calculated variance
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

pub fn uniform_random_two_test() {
  let same: Float = 5.0
  assert Ok(mean) = uniform.uniform_mean(same, same)
  assert Ok(variance) = uniform.uniform_variance(same, same)
  assert Ok(out) =
    generators.seed_pcg32(5, 1)
    |> uniform.uniform_random(same, same, m)

  // Make sure the generated uniform random numbers are actually within
  // the given min/max bounds
  // pair.first(out)
  // |> list.all(fn(x) {
  //   case x == same {
  //     True -> True
  //     _ -> False
  //   }
  // })
  // |> should.be_true()
  pair.first(out)
  |> list.map(fn(x: Float) -> Bool {
    case x == same {
      True -> True
      _ -> False
    }
  })
  |> all(True)
  |> should.be_true()

  // Make sure the sample mean of the generated uniform random numbers
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

  // Make sure the sample variance of the generated uniform random numbers
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
