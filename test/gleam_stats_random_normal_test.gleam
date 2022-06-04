import gleam/pair
import gleam/list
import gleam_stats/generators
import gleam_stats/distributions/normal
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
// sample mean and variance of the generated random numbers
const n: Int = 25_000

// The mean of a normal distribution (continuous) 
const mu: Float = 0.0

// The scale parameter of a normal distribution (continuous) 
const sigma: Float = 1.0

// Test that the implemented probability density function (pdf) of a 
// normal distribution (continuous) is correct by checking equality a
// certain analytically calculated points
pub fn normal_pdf_test() {
  let xs: List(Float) = [-100.0, 0.0, 100.0]
  let fxs: List(Float) = [0.0, 0.3989422804014327, 0.0]
  let vs: List(#(Float, Float)) = list.zip(xs, fxs)

  vs
  |> list.map(fn(v: #(Float, Float)) -> Bool {
    pair.first(v)
    |> normal.normal_pdf(mu, sigma)
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
// normal distribution (continuous) is correct by checking equality a
// certain analytically calculated points
pub fn normal_cdf_test() {
  let xs: List(Float) = [-100.0, 0.5, 100.0]
  let fxs: List(Float) = [0.0, 0.6914624612740131, 1.0]
  let vs: List(#(Float, Float)) = list.zip(xs, fxs)

  vs
  |> list.map(fn(v: #(Float, Float)) -> Bool {
    pair.first(v)
    |> normal.normal_cdf(mu, sigma)
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

pub fn normal_random_test() {
  assert Ok(mean) = normal.normal_mean(mu, sigma)
  assert Ok(variance) = normal.normal_variance(mu, sigma)
  assert Ok(out) =
    generators.seed_pcg32(5, 1)
    |> normal.normal_random(mu, sigma, n)

  // Check that the correct number of elements is actually returned
  pair.first(out)
  |> list.length()
  |> should.equal(n)

  // Make sure the sample mean of the generated normal random numbers
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

  // Make sure the sample variance of the generated normal random numbers
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
