//// Functions related to continuous uniform random variables.
////
//// ---
////
//// * **Available Functions**
////   * [`uniform_mean`](#uniform_mean)
////   * [`uniform_variance`](#uniform_variance)
////   * [`uniform_pdf`](#uniform_pdf)
////   * [`uniform_cdf`](#uniform_cdf)
////   * [`uniform_random`](#uniform_random)

import gleam/list
import gleam/iterator.{Iterator}
import gleam/float
import gleam/int
import gleam/pair
import gleam_stats/generators.{mask_32, take_randints}

fn check_uniform_parameters(a: Float, b: Float) -> Result(Bool, String) {
  case a <=. b {
    False ->
      "Invalid input arugment: a > b. Valid input is a <= b."
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
/// Analytically compute the mean of a continuous uniform random variable   
/// that takes values in the interval '[a, b]'.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn uniform_mean(a: Float, b: Float) -> Result(Float, String) {
  case check_uniform_parameters(a, b) {
    Error(string) ->
      string
      |> Error
    _ ->
      { a +. b } /. 2.
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Analytically compute the variance of a continuous uniform random variable   
/// that takes values in the interval '[a, b]'.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn uniform_variance(a: Float, b: Float) -> Result(Float, String) {
  case check_uniform_parameters(a, b) {
    Error(string) ->
      string
      |> Error
    _ ->
      float.power(b -. a, 2.) /. 12.
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Evaluate the probability density function (pdf) of a continuous uniform random 
/// variable that takes values in the interval '[a, b]'.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/uniform
///
///     pub fn example() {
///       let a: Float = 0.
///       let b: Float = 1.
///       // For illustrational purposes, evaluate the pdf at points 
///       // 0.0 and 1.0
///       uniform.uniform_cdf(0.0, a, b) |> should.equal(Ok(0.5))
///       uniform.uniform_cdf(1.0, a, b) |> should.equal(Ok(1.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn uniform_pdf(x: Float, a: Float, b: Float) -> Result(Float, String) {
  case check_uniform_parameters(a, b) {
    Error(string) ->
      string
      |> Error
    _ ->
      case x >=. a && x <=. b {
        True ->
          1.0 /. { b -. a }
          |> Ok
        False ->
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
/// continuous uniform random variable that takes values in the interval '[a, b]'.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/uniform
///
///     pub fn example() {
///       let a: Float = 0.
///       let b: Float = 1.
///       // For illustrational purposes, evaluate the cdf at points
///       // 0.0 and 1.0
///       uniform.uniform_cdf(0.0, a, b) |> should.equal(Ok(0.0))
///       uniform.uniform_cdf(1.0, a, b) |> should.equal(Ok(1.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn uniform_cdf(x: Float, a: Float, b: Float) -> Result(Float, String) {
  case check_uniform_parameters(a, b) {
    Error(string) ->
      string
      |> Error
    _ ->
      // Check if x falls into interval (-inf, a)
      case x <. a {
        True ->
          0.0
          |> Ok
        False ->
          // Check if x falls into interval [a, b]
          case x >=. a && x <=. b {
            True ->
              { x -. a } /. { b -. a }
              |> Ok
            // Finally, if we arrive here x must fall into interval (b, inf)
            False ->
              case x >. b {
                _ ->
                  1.0
                  |> Ok
              }
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
/// Generate 'm' random numbers in the interval '[a, b]' from a 
/// continuous uniform distribution.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import gleam_stats/generator
///     import gleam_stats/distributions/uniform
///
///     pub fn example() {
///       let seed: Int = 5
///       let seq: Int = 1
///       // Min value
///       let a: Float = 0.
///       // Max value
///       let b: Float = 1.
///       assert Ok(out) =
///         generators.seed_pcg32(seed)
///         |> uniform.uniform_random(a, b, 5_000)
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
pub fn uniform_random(
  stream: Iterator(Int),
  a: Float,
  b: Float,
  m: Int,
) -> Result(#(List(Float), Iterator(Int)), String) {
  case check_uniform_parameters(a, b) {
    Error(string) ->
      string
      |> Error
    _ ->
      case m > 0 {
        False -> Error("Invalid input arugment: m < 0. Valid input is m > 0.")
        True -> {
          // Take out 'm' integers from the stream of pseudo-random numbers.
          assert Ok(out) = take_randints(stream, m)
          // Transform the 'm' integers to continuous uniform random numbers in an interval.
          let numbers: List(Float) =
            pair.first(out)
            |> list.map(fn(x) {
              b *. { int.to_float(x) /. int.to_float(mask_32) } +. a
            })
          // Then return a tuple consisting of a list of continuous uniform random numbers
          // and the stream of pseudo-random numbers where the 'm' integers have been dropped
          // from the stream.
          #(numbers, pair.second(out))
          |> Ok
        }
      }
  }
}
