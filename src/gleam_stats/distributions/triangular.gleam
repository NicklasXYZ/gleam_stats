//// Functions related to continuous triangular random variables.
////
//// ---
////
//// * **Available Functions**
////   * [`triangular_mean`](#triangular_mean)
////   * [`triangular_variance`](#triangular_variance)
////   * [`triangular_pdf`](#triangular_pdf)
////   * [`triangular_cdf`](#triangular_cdf)
////   * [`triangular_random`](#triangular_random)

import gleam/list
import gleam/iterator.{Iterator}
import gleam/float
import gleam/pair
import gleam_stats/distributions/uniform

fn check_triangular_parameters(
  a: Float,
  b: Float,
  c: Float,
) -> Result(Bool, String) {
  case a <=. b {
    False ->
      "Invalid input arugment: a > b. Valid input is a <= b."
      |> Error
    True ->
      case a <=. c {
        False ->
          "Invalid input argument: a > c. Valid input is a <= c <= b."
          |> Error
        True ->
          case c <=. b {
            False ->
              "Invalid input argument: c > b. Valid input is a <= c <= b."
              |> Error
            True ->
              True
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
/// Analytically compute the mean of a continuous triangular random variable   
/// that takes values in the interval '[a, b]' and has mode (a peak at value) 'c'.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn triangular_mean(a: Float, b: Float, c: Float) -> Result(Float, String) {
  case check_triangular_parameters(a, b, c) {
    Error(string) ->
      string
      |> Error
    _ ->
      { a +. b +. c } /. 3.0
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Analytically compute the variance of a continuous triangular random variable   
/// that takes values in the interval '[a, b]' and has mode (a peak at value) 'c'.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn triangular_variance(
  a: Float,
  b: Float,
  c: Float,
) -> Result(Float, String) {
  case check_triangular_parameters(a, b, c) {
    Error(string) ->
      string
      |> Error
    _ ->
      {
        float.power(a, 2.0) +. float.power(b, 2.0) +. float.power(c, 2.0) -. a *. b -. a *. c -. b *. c
      } /. 18.
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Evaluate the probability density function (pdf) of a continuous triangular
/// random variable that takes values in the interval '[a, b]' and has mode (a 
/// peak at value) 'c'.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/uniform
///
///     pub fn example() {
///       // Min value
///       let a: Float = 0.
///       // Max value
///       let b: Float = 1.
///       // The mode of the distribution
///       let c: Float = 0.5
///       // For illustrational purposes, evaluate the pdf at the 
///       // point -100.0
///       triangular.triangular_pdf(-100.0, a, b ,c) |> should.equal(0.0)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn triangular_pdf(
  x: Float,
  a: Float,
  b: Float,
  c: Float,
) -> Result(Float, String) {
  case check_triangular_parameters(a, b, c) {
    Error(string) ->
      string
      |> Error
    _ ->
      case x <. a {
        True ->
          0.0
          |> Ok
        False ->
          case a <=. x && x <. c {
            True ->
              2.0 *. { x -. a } /. { { b -. a } *. { c -. a } }
              |> Ok
            False ->
              case x == c {
                True ->
                  2.0 /. { b -. a }
                  |> Ok
                False ->
                  case c <. x && x <=. b {
                    True ->
                      2.0 *. { b -. x } /. { { b -. a } *. { b -. c } }
                      |> Ok
                    False ->
                      0.0
                      |> Ok
                  }
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
/// Evaluate, at a certain point, the cumulative distribution function (cdf) of a 
/// continuous triangular random variable that takes values in the interval '[a, b]'
/// and has mode (a peak at value) 'c'.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/uniform
///
///     pub fn example() {
///       // Min value
///       let a: Float = 0.
///       // Max value
///       let b: Float = 1.
///       // The mode of the distribution
///       let c: Float = 0.5
///       // For illustrational purposes, evaluate the cdf at the 
///       // point -100.0
///       triangular.triangular_cdf(-100.0, a, b ,c) |> should.equal(0.0)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn triangular_cdf(
  x: Float,
  a: Float,
  b: Float,
  c: Float,
) -> Result(Float, String) {
  case check_triangular_parameters(a, b, c) {
    Error(string) ->
      string
      |> Error
    _ ->
      case x <=. a {
        True ->
          0.0
          |> Ok
        False ->
          case a <. x && x <=. c {
            True ->
              float.power(x -. a, 2.0) /. { { b -. a } *. { c -. a } }
              |> Ok
            False ->
              case c <. x && x <. b {
                True ->
                  1.0 -. float.power(b -. x, 2.0) /. {
                    { b -. a } *. { b -. c }
                  }
                  |> Ok
                False ->
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
/// triangular distribution with mode 'c'.
/// 
/// The random numbers are generated using the inverse transform method.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import gleam_stats/generator
///     import gleam_stats/distributions/triangular
///
///     pub fn example() {
///       let seed: Int = 5
///       let seq: Int = 1
///       // Min value
///       let a: Float = 0.
///       // Max value
///       let b: Float = 1.
///       // The mode of the distribution
///       let c: Float = 0.5
///       assert Ok(out) =
///         generators.seed_pcg32(seed)
///         |> triangular.triangular_random(a, b, c, 5_000)
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
pub fn triangular_random(
  stream: Iterator(Int),
  a: Float,
  b: Float,
  c: Float,
  m: Int,
) -> Result(#(List(Float), Iterator(Int)), String) {
  case check_triangular_parameters(a, b, c) {
    Error(string) -> Error(string)
    _ ->
      case m > 0 {
        False -> Error("Invalid input arugment: m < 0. Valid input is m > 0.")
        True -> {
          let eval: Float = { c -. a } /. { b -. a }
          // Take out 'm' integers from the stream of pseudo-random numbers and generate 
          // uniform random numbers.
          assert Ok(out) = uniform.uniform_random(stream, 0., 1., m)
          // Transform the 'm' continuous uniform random numbers to triangular distributed
          // random numbers.
          let numbers: List(Float) =
            pair.first(out)
            |> list.map(fn(x: Float) -> Float {
              case x <. eval {
                True -> a +. float.power(x *. { b -. a } *. { b -. c }, 0.5)
                False ->
                  b -. float.power({ 1. -. x } *. { b -. a } *. { b -. c }, 0.5)
              }
            })
          // Then return a tuple consisting of a list of continuous triangular random numbers
          // and the stream of pseudo-random numbers where the 'm' integers have been dropped
          // from the stream.
          #(numbers, pair.second(out))
          |> Ok
        }
      }
  }
}
