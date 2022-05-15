//// Functions related to discrete bernoulli random variables.
////
//// ---
////
//// * **Available Functions**
////   * [`bernoulli_mean`](#bernoulli_mean)
////   * [`bernoulli_variance`](#bernoulli_variance)
////   * [`bernoulli_pmf`](#bernoulli_pmf)
////   * [`bernoulli_cdf`](#bernoulli_cdf)
////   * [`bernoulli_random`](#bernoulli_random)

import gleam/list
import gleam/iterator.{Iterator}
import gleam/pair
import gleam_stats/distributions/uniform

fn check_bernoulli_parameters(p: Float) -> Result(Bool, String) {
  case 0.0 <=. p && p <=. 1.0 {
    False ->
      "Invalid input argument: p < 0 or p > 1. Valid input is 0 <= p <= 1."
      |> Error
    True ->
      True
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Analytically compute the mean of a discrete bernoulli distribution with parameter
/// 'p' in the interval [0, 1] (the success probability of a trial).
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn bernoulli_mean(p: Float) -> Result(Float, String) {
  case check_bernoulli_parameters(p) {
    Error(string) ->
      string
      |> Error
    _ ->
      p
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Analytically compute the variance of a discrete bernoulli distribution with parameter
/// 'p' in the interval [0, 1] (the success probability of a trial).
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn bernoulli_variance(p: Float) -> Result(Float, String) {
  case check_bernoulli_parameters(p) {
    Error(string) ->
      string
      |> Error
    _ ->
      p *. { 1.0 -. p }
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Evaluate the probability mass function (pmf) of a discrete bernoulli distribution with
/// parameter 'p' in the interval [0, 1] (the success probability of a trial).
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/bernoulli
///
///     pub fn example() {
///       let p: Float = 0.5
///       // For illustrational purposes, evaluate the pmf at the 
///       // point -100.0
///       bernoulli.bernoulli_pmf(-100.0, p) |> should.equal(0.0)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn bernoulli_pmf(x: Int, p: Float) -> Result(Float, String) {
  case check_bernoulli_parameters(p) {
    Error(string) ->
      string
      |> Error
    _ ->
      case x == 0 || x == 1 {
        True ->
          case x {
            0 ->
              1.0 -. p
              |> Ok
            1 ->
              p
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
/// Evaluate, at a certain point, the cumulative distribution function (cdf) of a discrete
/// bernoulli distribution with parameter 'p' in the interval [0, 1] (the success probability
/// of a trial).
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/bernoulli
///
///     pub fn example() {
///       let p: Float = 0.5
///       // For illustrational purposes, evaluate the cdf at the 
///       // point -100.0
///       bernoulli.bernoulli_cdf(-100.0, p) |> should.equal(0.0)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn bernoulli_cdf(x: Int, p: Float) -> Result(Float, String) {
  case check_bernoulli_parameters(p) {
    Error(string) ->
      string
      |> Error
    _ ->
      case x < 0 {
        True ->
          0.0
          |> Ok
        False ->
          case 0 <= x && x < 1 {
            True ->
              1.0 -. p
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
/// Generate 'm' random numbers from a discrete bernoulli distribution with parameter 
/// 'p' in the interval [0, 1] (the success probability of a trial).
/// 
/// The random numbers are generated using the inverse transform method.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import gleam_stats/generator
///     import gleam_stats/distributions/bernoulli
///
///     pub fn example() {
///       let seed: Int = 5
///       let seq: Int = 1
///       let p: Float = 0.5
///       assert Ok(out) =
///         generators.seed_pcg32(seed)
///         |> bernoulli.bernoulli_random(p, 5_000)
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
pub fn bernoulli_random(
  stream: Iterator(Int),
  p: Float,
  m: Int,
) -> Result(#(List(Int), Iterator(Int)), String) {
  case check_bernoulli_parameters(p) {
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
          assert Ok(out) = uniform.uniform_random(stream, 0., 1., m)
          // Transform the 'm' continuous uniform random numbers to bernoulli random numbers
          let numbers: List(Int) =
            pair.first(out)
            |> list.map(fn(x: Float) -> Int {
              case x <=. p {
                True -> 1
                False -> 0
              }
            })
          // Then return a tuple consisting of a list of bernoulli random numbers
          // and the stream of pseudo-random numbers where the 'm' integers have been dropped
          // from the stream.
          #(numbers, pair.second(out))
          |> Ok
        }
      }
  }
}
