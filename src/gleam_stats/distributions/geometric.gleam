//// Functions related to discrete geometric random variables.
////
//// ---
////
//// * **Available Functions**
////   * [`geometric_mean`](#geometric_mean)
////   * [`geometric_variance`](#geometric_variance)
////   * [`geometric_pmf`](#geometric_pmf)
////   * [`geometric_cdf`](#geometric_cdf)
////   * [`geometric_random`](#geometric_random)

import gleam/list
import gleam/iterator.{Iterator}
import gleam/float
import gleam/int
import gleam/pair
import gleam_stats/math.{log}
import gleam_stats/distributions/uniform

fn check_geometric_parameters(p: Float) -> Result(Bool, String) {
  case 0.0 <. p && p <=. 1.0 {
    False ->
      "Invalid input argument: p <= 0 or p > 1. Valid input is 0 < p <= 1."
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
/// Analytically compute the mean of a discrete geometric distribution with parameter
/// 'p' in the interval (0, 1] (the success probability).
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn geometric_mean(p: Float) -> Result(Float, String) {
  case check_geometric_parameters(p) {
    Error(string) ->
      string
      |> Error
    _ ->
      { 1.0 -. p } /. p
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Analytically compute the variance of a discrete geometric distribution with parameter
/// 'p' in the interval (0, 1] (the success probability).
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn geometric_variance(p: Float) -> Result(Float, String) {
  case check_geometric_parameters(p) {
    Error(string) ->
      string
      |> Error
    _ ->
      { 1.0 -. p } /. float.power(p, 2.0)
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Evaluate the probability mass function (pmf) of a discrete geometric distribution
/// with with parameter 'p' in the interval (0, 1] (the success probability).
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/geometric
///     import gleeunit/should
///
///     pub fn example() {
///       let p: Float = 0.5
///       // For illustrational purposes, evaluate the pmf at the 
///       // point -100.0
///       geometric.geometric_pmf(-100.0, r, p) |> should.equal(Ok(0.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn geometric_pmf(x: Int, p: Float) -> Result(Float, String) {
  case check_geometric_parameters(p) {
    Error(string) ->
      string
      |> Error
    _ ->
      case x >= 0 {
        True ->
          float.power(1.0 -. p, int.to_float(x)) *. p
          |> Ok
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
/// of a discrete geometric distribution with with parameter 'p' in the interval (0, 1]
/// (the success probability).
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/geometric
///     import gleeunit/should
///
///     pub fn example() {
///       let p: Float = 0.5
///       // For illustrational purposes, evaluate the cdf at the 
///       // point -100.0
///       geometric.geometric_cdf(-100.0, r, p) |> should.equal(Ok(0.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn geometric_cdf(x: Int, p: Float) -> Result(Float, String) {
  case check_geometric_parameters(p) {
    Error(string) ->
      string
      |> Error
    _ ->
      1.0 -. float.power(1.0 -. p, int.to_float(x) +. 1.0)
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generate 'm' random numbers from a discrete geometric distribution with with 
/// parameter 'p' in the interval (0, 1] (the success probability).
/// 
/// The random numbers are generated using the inverse transform method.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import gleam_stats/generator
///     import gleam_stats/distributions/geometric
///
///     pub fn example() {
///       let seed: Int = 5
///       let seq: Int = 1
///       let p: Float = 0.5
///       assert Ok(out) =
///         generators.seed_pcg32(seed)
///         |> geometric.geometric_random(r, p, 5_000)
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
pub fn geometric_random(
  stream: Iterator(Int),
  p: Float,
  m: Int,
) -> Result(#(List(Int), Iterator(Int)), String) {
  case check_geometric_parameters(p) {
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
          // Transform the 'm' continuous uniform random numbers to geometric distributed 
          // random numbers
          let numbers: List(Int) =
            pair.first(out)
            |> list.map(fn(x) {
              float.round(float.floor(log(x) /. log(1. -. p)))
            })
          // Then return a tuple consisting of a list of geometric random numbers
          // and the stream of pseudo-random numbers where the 'm' integers have been dropped
          // from the stream.
          #(numbers, pair.second(out))
          |> Ok
        }
      }
  }
}
