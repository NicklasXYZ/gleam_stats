//// Functions related to continuous weibull random variables.
////
//// ---
////
//// * **Available Functions**
////   * [`weibull_mean`](#weibull_mean)
////   * [`weibull_variance`](#weibull_variance)
////   * [`weibull_pdf`](#weibull_pdf)
////   * [`weibull_cdf`](#weibull_cdf)
////   * [`weibull_random`](#weibull_random)

import gleam/list
import gleam/iterator.{Iterator}
import gleam/float
import gleam/pair
import gleam_stats/math
import gleam_stats/distributions/uniform

fn check_weibull_parameters(lambda: Float, k: Float) {
  case lambda >. 0.0 && k >. 0.0 {
    False ->
      "Invalid input argument: lambda < 0 or k < 0. Valid input is lambda > 0 and k > 0."
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
/// Analytically compute the mean of a continuous weibull random variable   
/// with scale parameter 'lambda' > 0 and shape parameter 'k' > 0.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn weibull_mean(lambda: Float, k: Float) -> Result(Float, String) {
  case check_weibull_parameters(lambda, k) {
    Error(string) ->
      string
      |> Error
    _ ->
      lambda *. math.gamma(1.0 +. 1.0 /. k)
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Analytically compute the variance of a continuous weibull random variable   
/// with scale parameter 'lambda' > 0 and shape parameter 'k' > 0.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn weibull_variance(lambda: Float, k: Float) -> Result(Float, String) {
  case check_weibull_parameters(lambda, k) {
    Error(string) ->
      string
      |> Error
    _ ->
      float.power(lambda, 2.0) *. {
        math.gamma(1.0 +. 2.0 /. k) -. float.power(
          math.gamma(1.0 +. 1.0 /. k),
          2.0,
        )
      }
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Evaluate the probability density function (pdf) of a continuous weibull random 
/// variable with scale parameter 'lambda' > 0 and shape parameter 'k' > 0.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/weibull
///     import gleeunit/should
///
///     pub fn example() {
///       let lambda: Float = 1.
///       let k: Float = 5.
///       // For illustrational purposes, evaluate the pdf at the 
///       // point -100.0
///       weibull.weibull_pdf(-100.0, lambda, k) |> should.equal(Ok(0.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn weibull_pdf(x: Float, lambda: Float, k: Float) -> Result(Float, String) {
  case check_weibull_parameters(lambda, k) {
    Error(string) ->
      string
      |> Error
    _ ->
      case x <. 0.0 {
        True ->
          0.0
          |> Ok
        False ->
          k /. lambda *. float.power(x /. lambda, k -. 1.0) *. math.exp(
            -1.0 *. float.power(x /. lambda, k),
          )
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
/// weibull random variable with scale parameter 'lambda' > 0 and shape parameter 
/// 'k' > 0.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/weibull
///     import gleeunit/should
///
///     pub fn example() {
///       let lambda: Float = 1.
///       let k: Float = 5.
///       // For illustrational purposes, evaluate the cdf at the 
///       // point -100.0
///       weibull.weibull_cdf(-100.0, lambda, k) |> should.equal(Ok(0.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn weibull_cdf(x: Float, lambda: Float, k: Float) -> Result(Float, String) {
  case check_weibull_parameters(lambda, k) {
    Error(string) ->
      string
      |> Error
    _ ->
      case x <. 0.0 {
        True ->
          0.0
          |> Ok
        False ->
          1.0 -. math.exp(-1.0 *. float.power(x /. lambda, k))
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
/// Generate 'm' random numbers from a continuous weibull distribution with scale 
/// parameter 'lambda' > 0 and shape parameter 'k' > 0.
///
/// The random numbers are generated using the inverse transform method.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import gleam_stats/generator
///     import gleam_stats/distributions/weibull
///
///     pub fn example() {
///       let seed: Int = 5
///       let seq: Int = 1
///       let lambda: Float = 1.
///       let k: Float = 5.
///       assert Ok(out) =
///         generators.seed_pcg32(seed, seq)
///         |> weibull.weibull_random(lambda, k, 5_000)
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
pub fn weibull_random(
  stream: Iterator(Int),
  lambda: Float,
  k: Float,
  m: Int,
) -> Result(#(List(Float), Iterator(Int)), String) {
  case check_weibull_parameters(lambda, k) {
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
          // Transform the 'm' continuous uniform random numbers to weibull distributed
          // random numbers.
          let numbers: List(Float) =
            pair.first(out)
            |> list.map(fn(x: Float) -> Float {
              assert Ok(x1) = math.log(1.0 -. x)
              lambda *. float.power(-1.0 *. x1, 1.0 /. k)
            })
          // Then return a tuple consisting of a list of weibull random numbers
          // and the stream of pseudo-random numbers where the 'm' integers have been dropped
          // from the stream.
          #(numbers, pair.second(out))
          |> Ok
        }
      }
  }
}
