//// Functions related to continuous chi-squared random variables.
////
//// ---
////
//// * **Available functions**
////   * [`chisquared_mean`](#chisquared_mean)
////   * [`chisquared_variance`](#chisquared_variance)
////   * [`chisquared_pdf`](#chisquared_pdf)
////   * [`chisquared_cdf`](#chisquared_cdf)
////   * [`chisquared_random`](#chisquared_random)

import gleam/iterator.{Iterator}
import gleam/int

if erlang {
  import gleam/list
  import gleam/pair
  import gleam_stats/math
  import gleam_stats/distributions/normal
}

fn check_chisquared_parameters(d: Int) -> Result(Bool, String) {
  case d > 0 {
    False ->
      "Invalid input argument: d < 0. Valid input is d > 0."
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
/// with given degrees of freedom $$d \in \mathbb{N}$$.
///
/// The mean returned is: $$d$$.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn chisquared_mean(d: Int) -> Result(Float, String) {
  case check_chisquared_parameters(d) {
    Error(string) ->
      string
      |> Error
    _ ->
      int.to_float(d)
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
/// with given degrees of freedom $$d \in \mathbb{Z}\_{>0}$$.
///
/// The variance returned is: $$2 \cdot d$$.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn chisquared_variance(d: Int) -> Result(Float, String) {
  case check_chisquared_parameters(d) {
    Error(string) ->
      string
      |> Error
    _ ->
      2.0 *. int.to_float(d)
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Evaluate, at a certain point $$x \in \[0, +\infty\]$$ the probability density function (pdf)
/// of a continuous chi-squared random variable with given degrees of freedom $$d \in \mathbb{Z}\_{>0}$$.
///
/// The pdf is defined as:
///
/// \\[
/// f(x; d) = 
/// \begin{cases}
///  \frac{x^{\frac{d}{2} - 1}\cdot e^{-\frac{x}{2}}}{2^{\frac{d}{2}} \cdot \Gamma\left(\frac{d}{2}\right)} &\text{if } x > 0, \\\\
///  0 &\text{if } x \leq 0.
/// \end{cases}
/// \\]
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/chisquared
///     import gleeunit/should
///
///     pub fn example() {
///       let ddof: Float = 1.
///       // For illustrational purposes, evaluate the pdf at the 
///       // point -100.0
///       chisquared.chisquared_pdf(-100.0, ddof) |> should.equal(Ok(0.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn chisquared_pdf(x: Float, d: Int) -> Result(Float, String) {
  do_chisquared_pdf(x, d)
}

if erlang {
  fn do_chisquared_pdf(x: Float, d: Int) -> Result(Float, String) {
    case check_chisquared_parameters(d) {
      Error(string) ->
        string
        |> Error
      _ ->
        case x >. 0.0 {
          True -> {
            let float_ddof: Float = int.to_float(d)
            let expr: Float = float_ddof /. 2.0
            assert Ok(v1) = math.pow(2.0, expr)
            let denominator: Float = v1 *. math.gamma(expr)
            assert Ok(v2) = math.pow(x, expr -. 1.0)
            let numerator: Float = v2 *. math.exp(-1.0 *. x /. 2.0)
            numerator /. denominator
            |> Ok
          }
          False ->
            0.0
            |> Ok
        }
    }
  }
}

if javascript {
  external fn do_chisquared_pdf(Float, Int) -> Result(Float, String) =
    "../../chisquared.mjs" "chisquared_pdf"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Evaluate, at a certain point $$x \in \[0, +\infty]$$, the cumulative distribution 
/// function (cdf) of a continuous chi-squared random variable with given degrees of 
/// freedom $$d \in \mathbb{Z}\_{>0}$$.
///
/// The cdf is defined as:
///
/// \\[
/// F(x; d) = \frac{\gamma\left(\frac{d}{2}, \frac{x}{2}\right)}{\Gamma\left(\frac{d}{2}\right)}
/// \\]
///
/// In the formula, $$\gamma(\cdot, \cdot)$$ is the lower incomplete gamma function.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/chisquared
///     import gleeunit/should
///
///     pub fn example() {
///       let ddof: Float = 1.
///       // For illustrational purposes, evaluate the cdf at the 
///       // point -100.0
///       chisquared.chisquared_cdf(-100.0, mu, sigma) |> should.equal(Ok(0.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn chisquared_cdf(x: Float, d: Int) -> Result(Float, String) {
  do_chisquared_cdf(x, d)
}

if erlang {
  fn do_chisquared_cdf(x: Float, d: Int) -> Result(Float, String) {
    case check_chisquared_parameters(d) {
      Error(string) ->
        string
        |> Error
      _ ->
        case x >. 0.0 && d == 1 {
          True ->
            chisquared_cdf_helper(x, d)
            |> Ok
          False ->
            case x >=. 0.0 && d > 1 {
              True ->
                chisquared_cdf_helper(x, d)
                |> Ok
              False ->
                0.0
                |> Ok
            }
        }
    }
  }

  fn chisquared_cdf_helper(x: Float, d: Int) -> Float {
    // In the computations below, assume all input arguments
    // have been checked and are valid
    let float_ddof: Float = int.to_float(d)
    let expr: Float = float_ddof /. 2.0
    assert Ok(numerator) = math.gammainc(expr, x /. 2.0)
    let denominator: Float = math.gamma(expr)
    numerator /. denominator
  }
}

if javascript {
  external fn do_chisquared_cdf(Float, Int) -> Result(Float, String) =
    "../../chisquared.mjs" "chisquared_cdf"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generate $$m \in \mathbb{Z}\_{>0}$$ random numbers from a continuous chi-squared
/// distribution with given degrees of freedom $$d \in \mathbb{Z}\_{>0}$$.
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
  d: Int,
  m: Int,
) -> Result(#(List(Float), Iterator(Int)), String) {
  do_chisquared_random(stream, d, m)
}

if erlang {
  fn do_chisquared_random(
    stream: Iterator(Int),
    d: Int,
    m: Int,
  ) -> Result(#(List(Float), Iterator(Int)), String) {
    case check_chisquared_parameters(d) {
      Error(string) ->
        string
        |> Error
      _ ->
        case m > 0 {
          False -> Error("Invalid input arugment: m < 0. Valid input is m > 0.")
          True -> {
            // Take out 'd' * 'm' integers from the stream of pseudo-random numbers and 
            // generate normal random numbers.
            assert Ok(out) = normal.normal_random(stream, 0.0, 1.0, d * m)
            // Transform the 'd' * 'm' continuous normal random numbers to 'm' chi-squared
            // distributed random numbers
            let numbers: List(Float) =
              pair.first(out)
              |> list.sized_chunk(d)
              |> list.map(fn(x: List(Float)) -> Float {
                x
                |> list.fold(
                  0.0,
                  fn(acc: Float, a: Float) -> Float { a *. a +. acc },
                )
              })
            // Then return a tuple consisting of a list of chi-squared normal random numbers
            // and the stream of pseudo-random numbers where the 'm' integers have been dropped
            // from the stream.
            #(numbers, pair.second(out))
            |> Ok
          }
        }
    }
  }
}

if javascript {
  external fn do_chisquared_random(
    Iterator(Int),
    Int,
    Int,
  ) -> Result(#(List(Float), Iterator(Int)), String) =
    "../../chisquared.mjs" "chisquared_random"
}
