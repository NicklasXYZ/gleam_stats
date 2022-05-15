//// Functions related to continuous normal random variables.
////
//// ---
////
//// * **Available Functions**
////   * [`normal_mean`](#normal_mean)
////   * [`normal_variance`](#normal_variance)
////   * [`normal_pdf`](#normal_pdf)
////   * [`normal_cdf`](#normal_cdf)
////   * [`normal_random`](#normal_random)

import gleam/list
import gleam/iterator.{Iterator}
import gleam/float
import gleam/pair
import gleam_stats/math.{cos, erf, exp, log, pi, sin}
import gleam_stats/distributions/uniform

fn check_normal_parameters(mu: Float, sigma: Float) -> Result(Bool, String) {
  case sigma >. 0.0 {
    False ->
      "Invalid input argument: sigma < 0. Valid input is sigma > 0."
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
/// Analytically compute the mean of a continuous normal random variable   
/// with given mean 'mu' and standard deviation 'sigma'.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn normal_mean(mu: Float, sigma: Float) -> Result(Float, String) {
  case check_normal_parameters(mu, sigma) {
    Error(string) ->
      string
      |> Error
    _ ->
      mu
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Analytically compute the variance of a continuous normal random variable   
/// with given mean 'mu' and standard deviation 'sigma'.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn normal_variance(mu: Float, sigma: Float) -> Result(Float, String) {
  case check_normal_parameters(mu, sigma) {
    Error(string) ->
      string
      |> Error
    _ ->
      float.power(sigma, 2.)
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Evaluate the probability density function (pdf) of a continuous normal random 
/// variable with given mean 'mu' and standard deviation 'sigma'.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/normal
///     import gleeunit/should
///
///     pub fn example() {
///       let mean: Float = 0.
///       let sigma: Float = 1.
///       // For illustrational purposes, evaluate the pdf at the 
///       // point -100.0
///       normal.normal_pdf(-100.0, mu, sigma) |> should.equal(Ok(0.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn normal_pdf(x: Float, mu: Float, sigma: Float) -> Result(Float, String) {
  case check_normal_parameters(mu, sigma) {
    Error(string) ->
      string
      |> Error
    _ -> {
      let numexp: Float =
        float.power(x -. mu, 2.0) /. { 2.0 *. float.power(sigma, 2.0) }
      let denominator: Float = sigma *. float.power(2.0 *. pi(), 0.5)
      let numerator: Float = exp(numexp *. -1.0)
      numerator /. denominator
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
/// continuous normal random variable with mean 'mu' and standard deviation 'sigma'.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/normal
///     import gleeunit/should
///
///     pub fn example() {
///       let mean: Float = 0.
///       let sigma: Float = 1.
///       // For illustrational purposes, evaluate the cdf at the 
///       // point -100.0
///       normal.normal_cdf(-100.0, mu, sigma) |> should.equal(Ok(0.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn normal_cdf(x: Float, mu: Float, sigma: Float) -> Result(Float, String) {
  case check_normal_parameters(mu, sigma) {
    Error(string) ->
      string
      |> Error
    _ -> {
      let denominator: Float = sigma *. float.power(2.0, 0.5)
      0.5 *. { 1.0 +. erf({ x -. mu } /. denominator) }
      |> Ok
    }
  }
}

// Use Box-Muller transform to sample from the normal distribution,
// given standard uniform distributed random numbers.
fn box_muller(u1: Float, u2: Float) -> List(Float) {
  assert Ok(x) = float.square_root(-2.0 *. log(u1))
  [x *. cos(2.0 *. pi() *. u2), x *. sin(2.0 *. pi() *. u2)]
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generate 'm' random numbers from a normal distribution with a given mean 
/// 'mu' and standard deviation 'sigma'. 
///
/// The random numbers are generated using Box–Muller transform.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import gleam_stats/generator
///     import gleam_stats/distributions/normal
///
///     pub fn example() {
///       let seed: Int = 5
///       let seq: Int = 1
///       let mean: Float = 0.
///       let std: Float = 1.
///       assert Ok(out) =
///         generators.seed_pcg32(seed, seq)
///         |> normal.normal_random(mean, std, 5_000)
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
pub fn normal_random(
  stream: Iterator(Int),
  mu: Float,
  sigma: Float,
  m: Int,
) -> Result(#(List(Float), Iterator(Int)), String) {
  case check_normal_parameters(mu, sigma) {
    Error(string) ->
      string
      |> Error
    _ ->
      case m > 0 {
        False -> Error("Invalid input arugment: m < 0. Valid input is m > 0.")
        True -> {
          // Take out 'm_even' integers from the stream of pseudo-random numbers 
          // and generate uniform random numbers.
          let m_even: Int = m + m % 2
          assert Ok(out) = uniform.uniform_random(stream, 0., 1., m_even)
          // Transform the 'm_even' continuous uniform random numbers to normal 
          // distributed random numbers
          let numbers: List(Float) =
            pair.first(out)
            |> list.sized_chunk(2)
            |> list.map(fn(x: List(Float)) -> List(Float) {
              case x {
                [u1, u2] ->
                  case box_muller(u1, u2) {
                    [z1, z2] -> [sigma *. z1 +. mu, sigma *. z2 +. mu]
                  }
              }
            })
            |> list.flatten()
            |> fn(x: List(Float)) -> List(Float) {
              // Make sure the returned list has length 'm'
              case m == list.length(x) {
                True -> x
                False -> list.drop(x, 1)
              }
            }
          // Then return a tuple consisting of a list of continuous normal random 
          // numbers and the stream of pseudo-random numbers where the 'm' integers 
          // have been dropped from the stream.
          #(numbers, pair.second(out))
          |> Ok
        }
      }
  }
}
