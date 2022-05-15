//// Functions related to continuous normal random variables.
////
//// ---
////
//// * **Available Functions**
////   * [`chisquared_mean`](#chisquared_mean)
////   * [`chisquared_variance`](#chisquared_variance)
////   * [`chisquared_pdf`](#chisquared_pdf)
////   * [`chisquared_cdf`](#chisquared_cdf)
////   * [`chisquared_random`](#chisquared_random)

import gleam/list
import gleam/iterator.{Iterator}
import gleam/float
import gleam/pair
import gleam/int
import gleam_stats/math.{exp, gamma, gammainc}
import gleam_stats/distributions/normal

fn check_chisquared_parameters(ddof: Int) -> Result(Bool, String) {
  case ddof > 0 {
    False ->
      "Invalid input argument: ddof < 0. Valid input is ddof > 0."
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
/// Analytically compute the mean of a continuous chi-squared random variable   
/// with given degrees of freedom 'ddof' > 0.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn chisquared_mean(ddof: Int) -> Result(Float, String) {
  case check_chisquared_parameters(ddof) {
    Error(string) ->
      string
      |> Error
    _ ->
      int.to_float(ddof)
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Analytically compute the variance of a continuous chi-squared random variable   
/// with given degrees of freedom 'ddof' > 0.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn chisquared_variance(ddof: Int) -> Result(Float, String) {
  case check_chisquared_parameters(ddof) {
    Error(string) ->
      string
      |> Error
    _ ->
      2.0 *. int.to_float(ddof)
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Evaluate the probability density function (pdf) of a continuous chi-squared 
/// random variable with given degrees of freedom 'ddof' > 0.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/chisquared
///
///     pub fn example() {
///       let ddof: Float = 1.
///       // For illustrational purposes, evaluate the pdf at the 
///       // point -100.0
///       chisquared.chisquared_pdf(-100.0, ddof) |> should.equal(0.0)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn chisquared_pdf(x: Float, ddof: Int) -> Result(Float, String) {
  case check_chisquared_parameters(ddof) {
    Error(string) ->
      string
      |> Error
    _ ->
      case x >. 0.0 {
        True -> {
          let float_ddof: Float = int.to_float(ddof)
          let expr: Float = float_ddof /. 2.0
          let denominator: Float = float.power(2.0, expr) *. gamma(expr)
          let numerator: Float =
            float.power(x, expr -. 1.0) *. exp(-1.0 *. x /. 2.0)
          numerator /. denominator
          |> Ok
        }
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
/// continuous chi-squared random variable with given degrees of freedom 'ddof' > 0.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/chisquared
///
///     pub fn example() {
///       let ddof: Float = 1.
///       // For illustrational purposes, evaluate the cdf at the 
///       // point -100.0
///       chisquared.chisquared_cdf(-100.0, mu, sigma) |> should.equal(0.0)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn chisquared_cdf(x: Float, ddof: Int) -> Result(Float, String) {
  case check_chisquared_parameters(ddof) {
    Error(string) ->
      string
      |> Error
    _ ->
      case x >. 0.0 && ddof == 1 {
        True ->
          do_chisquared_cdf(x, ddof)
          |> Ok
        False ->
          case x >=. 0.0 && ddof > 1 {
            True ->
              do_chisquared_cdf(x, ddof)
              |> Ok
            False ->
              0.0
              |> Ok
          }
      }
  }
}

fn do_chisquared_cdf(x: Float, ddof: Int) -> Float {
  // In the computations below, assume all input arguments
  // have been checked and are valid
  let float_ddof: Float = int.to_float(ddof)
  let expr: Float = float_ddof /. 2.0
  assert Ok(numerator) = gammainc(expr, x /. 2.0)
  let denominator: Float = gamma(expr)
  numerator /. denominator
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generate 'm' random numbers from a continuous chi-squared random variable with
/// given degrees of freedom 'ddof' > 0.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import gleam_stats/generator
///     import gleam_stats/distributions/chisquared
///
///     pub fn example() {
///       let seed: Int = 5
///       let seq: Int = 1
///       let ddof: Float = 1.
///       assert Ok(out) =
///         generators.seed_pcg32(seed, seq)
///         |> chisquared.chisquared_random(ddof, 5_000)
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
pub fn chisquared_random(
  stream: Iterator(Int),
  ddof: Int,
  m: Int,
) -> Result(#(List(Float), Iterator(Int)), String) {
  case check_chisquared_parameters(ddof) {
    Error(string) ->
      string
      |> Error
    _ ->
      case m > 0 {
        False -> Error("Invalid input arugment: m < 0. Valid input is m > 0.")
        True -> {
          // Take out 'ddof' * 'm' integers from the stream of pseudo-random numbers and 
          // generate normal random numbers.
          assert Ok(out) = normal.normal_random(stream, 0.0, 1.0, ddof * m)
          // Transform the 'ddof' * 'm' continuous normal random numbers to 'm' chi-squared
          // distributed random numbers
          let numbers: List(Float) =
            pair.first(out)
            |> list.sized_chunk(ddof)
            |> list.map(fn(x: List(Float)) -> Float {
              x
              |> list.fold(
                0.,
                fn(acc: Float, a: Float) -> Float { a *. a +. acc },
              )
            })
          // Then return a tuple consisting of a list of chisquared normal random numbers
          // and the stream of pseudo-random numbers where the 'm' integers have been dropped
          // from the stream.
          #(numbers, pair.second(out))
          |> Ok
        }
      }
  }
}