//// Functions related to discrete negative binomial random variables.
////
//// ---
////
//// * **Available Functions**
////   * [`negbinomial_mean`](#negbinomial_mean)
////   * [`negbinomial_variance`](#negbinomial_variance)
////   * [`negbinomial_pmf`](#negbinomial_pmf)
////   * [`negbinomial_cdf`](#negbinomial_cdf)
////   * [`negbinomial_random`](#negbinomial_random)

import gleam/list
import gleam/iterator.{Iterator}
import gleam/float
import gleam/int
import gleam/pair
import gleam_stats/math
import gleam_stats/distributions/geometric

fn check_negbinomial_parameters(r: Int, p: Float) -> Result(Bool, String) {
  case r > 0 {
    False ->
      "Invalid input argument: r <= 0. Valid input is r > 0."
      |> Error
    True ->
      case 0.0 <=. p && p <=. 1.0 {
        False ->
          "Invalid input argument: p < 0 or p > 1. Valid input is 0 <= p <= 1."
          |> Error
        True ->
          True
          |> Ok
      }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Analytically compute the mean of a discrete negative binomial 
/// distribution with parameters 'r' > 0 (number of failures until the experiment
/// is stopped) and 'p' in the interval [0, 1] (the success probability in each 
/// experiment).
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn negbinomial_mean(r: Int, p: Float) -> Result(Float, String) {
  case check_negbinomial_parameters(r, p) {
    Error(string) ->
      string
      |> Error
    _ ->
      int.to_float(r) *. p /. { 1.0 -. p }
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Analytically compute the variance of a discrete negative binomial 
/// distribution with parameters 'r' > 0 (number of failures until the experiment
/// is stopped) and 'p' in the interval [0, 1] (the success probability in each 
/// experiment).
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn negbinomial_variance(r: Int, p: Float) -> Result(Float, String) {
  case check_negbinomial_parameters(r, p) {
    Error(string) ->
      string
      |> Error
    _ ->
      int.to_float(r) *. p /. float.power(1.0 -. p, 2.0)
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Evaluate the probability mass function (pmf) of a discrete negative binomial 
/// distribution with parameters 'r' > 0 (number of failures until the experiment
/// is stopped) and 'p' in the interval [0, 1] (the success probability in each 
/// experiment).
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/negbinomial
///     import gleeunit/should
///
///     pub fn example() {
///       let r: Float = 40.
///       let p: Float = 0.5
///       // For illustrational purposes, evaluate the pmf at the 
///       // point -100.0
///       negbinomial.negbinomial_pmf(-100.0, r, p) |> should.equal(Ok(0.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn negbinomial_pmf(x: Int, r: Int, p: Float) -> Result(Float, String) {
  case check_negbinomial_parameters(r, p) {
    Error(string) ->
      string
      |> Error
    _ ->
      case x >= 0 && x <= r {
        True -> {
          assert Ok(c) = math.combination(x + r - 1, x)
          int.to_float(c) *. float.power(1.0 -. p, int.to_float(r)) *. float.power(
            p,
            int.to_float(x),
          )
          |> Ok
        }
        _ ->
          0.0
          |> Ok
      }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Evaluate, at a certain point, the cumulative distribution function (cdf) of a 
/// discrete negative binomial distribution with parameters 'r' > 0 (number of 
/// failures until the experiment is stopped) and 'p' in the interval [0, 1] (the 
/// success probability in each experiment).
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/negbinomial
///     import gleeunit/should
///
///     pub fn example() {
///       let r: Float = 40.
///       let p: Float = 0.5
///       // For illustrational purposes, evaluate the cdf at the 
///       // point -100.0
///       negbinomial.negbinomial_cdf(-100.0, r, p) |> should.equal(Ok(0.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn negbinomial_cdf(x: Int, r: Int, p: Float) -> Result(Float, String) {
  case check_negbinomial_parameters(r, p) {
    Error(string) ->
      string
      |> Error
    _ ->
      case x < 0 {
        True ->
          0.0
          |> Ok
        False ->
          case x >= 0 && x <= r {
            True ->
              list.range(0, x + 1)
              |> list.fold(
                0.0,
                fn(acc: Float, i: Int) {
                  let v: Float = int.to_float(i)
                  assert Ok(c) = math.combination(i + r - 1, i)
                  acc +. int.to_float(c) *. float.power(
                    1.0 -. p,
                    int.to_float(r),
                  ) *. float.power(p, v)
                },
              )
              |> Ok
            False ->
              1.0
              |> Ok
          }
      }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generate 'm' random numbers from a discrete negative binomial distribution
/// with parameters 'r' > 0 (number of failures until the experiment is stopped)
/// and 'p' in the interval [0, 1] (the success probability in each experiment).
/// 
/// The random numbers are generated using the inverse transform method.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import gleam_stats/generator
///     import gleam_stats/distributions/negbinomial
///
///     pub fn example() {
///       let seed: Int = 5
///       let seq: Int = 1
///       let r: Float = 40.
///       let p: Float = 0.5
///       assert Ok(out) =
///         generators.seed_pcg32(seed)
///         |> negbinomial.negbinomial_random(r, p, 5_000)
///       let rands: List(Float) = pair.first(out)
///       let stream: Iterator(Int) = pair.second(out)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn negbinomial_random(
  stream: Iterator(Int),
  r: Int,
  p: Float,
  m: Int,
) -> Result(#(List(Int), Iterator(Int)), String) {
  case check_negbinomial_parameters(r, p) {
    Error(string) ->
      string
      |> Error
    _ ->
      case m > 0 {
        False ->
          "Invalid input arugment: m < 0. Valid input is m > 0."
          |> Error
        True -> {
          // Take out 'm' integers from the stream of pseudo-random numbers and generate 
          // uniform random numbers.
          assert Ok(out) = geometric.geometric_random(stream, p, r * m)
          // Transform each batch of 'm' bernoulli distributed random numbers to a negative binomial
          // distributed random number
          let numbers: List(Int) =
            pair.first(out)
            |> list.window(r)
            |> list.map(fn(x: List(Int)) -> Int {
              x
              |> list.fold(0, fn(a: Int, b: Int) -> Int { a + b })
            })
          // Then return a tuple consisting of a list of negative binomial random numbers
          // and the stream of pseudo-random numbers where the 'm' integers have been dropped
          // from the stream.
          #(numbers, pair.second(out))
          |> Ok
        }
      }
  }
}
