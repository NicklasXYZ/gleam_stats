import gleam/pair
import gleam/list
import gleam/float
import gleam_stats/generators
import gleam_stats/distributions/triangular
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

// The minimum of a triangular distribution (continuos) 
const a: Float = 0.

// The mode of a triangular distribution (continuos)
const c: Float = 0.5

// The maximum of a triangular distribution (continuos)
const b: Float = 1.

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
// triangular distribution (continuous) is correct by checking equality a
// certain analytically calculated points
pub fn triangular_pdf_test() {
  let xs: List(Float) = [a -. 1.0, a, c, b, b +. 1.0]
  let fxs: List(Float) = [
    0.0,
    { a -. a } /. { { b -. a } *. { c -. a } },
    2.0 /. { b -. a },
    2.0 *. { b -. b } /. { { b -. a } *. { b -. c } },
    0.0,
  ]
  let vs: List(#(Float, Float)) = list.zip(xs, fxs)

  vs
  |> list.map(fn(v: #(Float, Float)) -> Bool {
    pair.first(v)
    |> triangular.triangular_pdf(a, b, c)
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
// triangular distribution (continuous) is correct by checking equality a
// certain analytically calculated points
pub fn triangular_cdf_test() {
  let p1: Float = a
  let p2: Float = c
  let p3: Float = c +. { b -. c } /. 2.0
  let p4: Float = b

  let xs: List(Float) = [p1, p2, p3, p4]

  let fxs: List(Float) = [
    0.0,
    float.power(p2 -. a, 2.0) /. { { b -. a } *. { c -. a } },
    1.0 -. float.power(b -. p3, 2.0) /. { { b -. a } *. { b -. c } },
    1.0,
  ]
  let vs: List(#(Float, Float)) = list.zip(xs, fxs)

  vs
  |> list.map(fn(v: #(Float, Float)) -> Bool {
    pair.first(v)
    |> triangular.triangular_cdf(a, b, c)
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

pub fn triangular_random_test() {
  assert Ok(mean) = triangular.triangular_mean(a, b, c)
  assert Ok(variance) = triangular.triangular_variance(a, b, c)
  assert Ok(out) =
    generators.seed_pcg32(5, 1)
    |> triangular.triangular_random(a, b, c, m)

  // Make sure the sample mean of the generated triangular random numbers
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
