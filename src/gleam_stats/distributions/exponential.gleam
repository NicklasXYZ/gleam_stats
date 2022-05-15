//// Functions related to continuous exponential random variables.
////
//// ---
////
//// * **Available Functions**
////   * [`exponential_mean`](#exponential_mean)
////   * [`exponential_variance`](#exponential_variance)
////   * [`exponential_pdf`](#exponential_pdf)
////   * [`exponential_cdf`](#exponential_cdf)
////   * [`exponential_random`](#exponential_random)

import gleam/list
import gleam/iterator.{Iterator}
import gleam/float
import gleam/pair
import gleam_stats/math.{exp, log}
import gleam_stats/distributions/uniform

fn check_exponential_parameters(lambda: Float) {
  case lambda >. 0.0 {
    False ->
      "Invalid input argument: lambda <= 0. Valid input is lambda > 0."
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
/// Analytically compute the mean of a continuous exponential random variable   
/// with given rate parameter 'lambda' > 0.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn exponential_mean(lambda: Float) -> Result(Float, String) {
  case check_exponential_parameters(lambda) {
    Error(string) ->
      string
      |> Error
    _ ->
      1.0 /. lambda
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Analytically compute the variance of a continuous exponential random variable   
/// with given rate parameter 'lambda' > 0.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn exponential_variance(lambda: Float) -> Result(Float, String) {
  case check_exponential_parameters(lambda) {
    Error(string) ->
      string
      |> Error
    _ ->
      1.0 /. float.power(lambda, 2.0)
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Evaluate the probability density function (pdf) of a continuous exponential random
/// variable with given rate parameter 'lambda' > 0.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/exponential
///
///     pub fn example() {
///       let lambda: Float = 1.
///       // For illustrational purposes, evaluate the pdf at the 
///       // point -100.0
///       exponential.exponential_pdf(-100.0, lambda) |> should.equal(0.0)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn exponential_pdf(x: Float, lambda: Float) -> Result(Float, String) {
  case check_exponential_parameters(lambda) {
    Error(string) ->
      string
      |> Error
    _ ->
      case x >=. 0.0 {
        True ->
          lambda *. exp(-1.0 *. lambda *. x)
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
/// continuous exponential random variable with given rate parameter 'lambda' > 0.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/exponential
///
///     pub fn example() {
///       let lambda: Float = 1.
///       // For illustrational purposes, evaluate the cdf at the 
///       // point -100.0
///       exponential.exponential_cdf(-100.0, lambda) |> should.equal(0.0)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn exponential_cdf(x: Float, lambda: Float) -> Result(Float, String) {
  case check_exponential_parameters(lambda) {
    Error(string) ->
      string
      |> Error
    _ ->
      case x >=. 0.0 {
        True ->
          1.0 -. exp(-1.0 *. lambda *. x)
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
/// Generate 'm' random numbers from a continuous exponential distribution with 
/// with given rate parameter 'lambda' > 0. 
///
/// The random numbers are generated using the inverse transform method.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import gleam_stats/generator
///     import gleam_stats/distributions/exponential
///
///     pub fn example() {
///       let seed: Int = 5
///       let seq: Int = 1
///       let lambda: Float = 1.
///       assert Ok(out) =
///         generators.seed_pcg32(seed, seq)
///         |> exponential.exponential_random(lambda, 5_000)
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
pub fn exponential_random(
  stream: Iterator(Int),
  lambda: Float,
  m: Int,
) -> Result(#(List(Float), Iterator(Int)), String) {
  case check_exponential_parameters(lambda) {
    Error(string) ->
      string
      |> Error
    _ ->
      case m > 0 {
        False -> Error("Invalid input arugment: m < 0. Valid input is m > 0.")
        True -> {
          // Take out 'm' integers from the stream of pseudo-random numbers and generate 
          // uniform random numbers.
          assert Ok(out) = uniform.uniform_random(stream, 0., 1., m)
          // Transform the 'm' continuous uniform random numbers to exponential distributed
          // random numbers.
          let numbers: List(Float) =
            pair.first(out)
            |> list.map(fn(x: Float) -> Float {
              1. /. { -1. *. lambda } *. log(x)
            })
          // Then return a tuple consisting of a list of exponential random numbers
          // and the stream of pseudo-random numbers where the 'm' integers have been dropped
          // from the stream.
          #(numbers, pair.second(out))
          |> Ok
        }
      }
  }
}
