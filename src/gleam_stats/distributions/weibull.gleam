//// Functions related to continuous Weibull random variables.
////
//// ---
////
//// * **Available functions**
////   * [`weibull_mean`](#weibull_mean)
////   * [`weibull_variance`](#weibull_variance)
////   * [`weibull_pdf`](#weibull_pdf)
////   * [`weibull_cdf`](#weibull_cdf)
////   * [`weibull_random`](#weibull_random)

import gleam/iterator.{Iterator}
import gleam_stats/math

if erlang {
  import gleam/list
  import gleam/pair
  import gleam_stats/distributions/uniform
}

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
/// Analytically compute the mean of a continuous Weibull random variable   
/// with scale parameter $$\lambda \in \(0, +\infty\)$$ and shape parameter 
/// $$k \in \(0, +\infty\)$$.
///
/// The mean returned is: $$\lambda \cdot \Gamma\left(1 + \frac{1}{k}\right)$$.
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
/// Analytically compute the variance of a continuous Weibull random variable   
/// with scale parameter $$\lambda \in \(0, +\infty\)$$ and shape parameter 
/// $$k \in \(0, +\infty\)$$.
///
/// The variance returned is: $$\lambda^{2} \cdot \left[ \Gamma\left(1 + \frac{2}{k}\right) - \left(\Gamma\left(1 + \frac{1}{k}\right)\right)^{2} \right]$$.
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
    _ -> {
      assert Ok(v1) = math.pow(lambda, 2.0)
      assert Ok(v2) = math.pow(math.gamma(1.0 +. 1.0 /. k), 2.0)
      v1 *. { math.gamma(1.0 +. 2.0 /. k) -. v2 }
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
/// Evaluate, at a certain point $$x \in \[0, +\infty\)$$, the probability density function (pdf) 
/// of a continuous Weibull random variable with scale parameter $$\lambda in \(0,+\infty\)$$ and
/// shape parameter $$k \in \(0, +\infty\)$$.
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
  do_weibull_pdf(x, lambda, k)
}

if erlang {
  fn do_weibull_pdf(x: Float, lambda: Float, k: Float) -> Result(Float, String) {
    case check_weibull_parameters(lambda, k) {
      Error(string) ->
        string
        |> Error
      _ ->
        case x <. 0.0 {
          True ->
            0.0
            |> Ok
          False -> {
            assert Ok(v1) = math.pow(x /. lambda, k -. 1.0)
            assert Ok(v2) = math.pow(x /. lambda, k)
            k /. lambda *. v1 *. math.exp(-1.0 *. v2)
            |> Ok
          }
        }
    }
  }
}

if javascript {
  external fn do_weibull_pdf(Float, Float, Float) -> Result(Float, String) =
    "../../weibull.mjs" "weibull_pdf"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Evaluate, at a certain point $$x \in \(-\infty, +\infty\)$$, the cumulative distribution 
/// function (cdf) of a Weibull random variable with scale parameter $$\lambda in \(0, +\infty\)$$ 
/// and shape parameter $$k \in \(0, +\infty\)$$.
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
  do_weibull_cdf(x, lambda, k)
}

if erlang {
  pub fn do_weibull_cdf(
    x: Float,
    lambda: Float,
    k: Float,
  ) -> Result(Float, String) {
    case check_weibull_parameters(lambda, k) {
      Error(string) ->
        string
        |> Error
      _ ->
        case x <. 0.0 {
          True ->
            0.0
            |> Ok
          False -> {
            assert Ok(v) = math.pow(x /. lambda, k)
            1.0 -. math.exp(-1.0 *. v)
            |> Ok
          }
        }
    }
  }
}

if javascript {
  external fn do_weibull_cdf(Float, Float, Float) -> Result(Float, String) =
    "../../weibull.mjs" "weibull_cdf"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generate $$m \in \mathbb{Z}\_{>0}$$ random numbers from a continuous Weibull distribution with
/// scale parameter $$\lambda \in \(0, +\infty\)$$ and shape parameter $$k \in \(0, +\infty\)$$.
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
  do_weibull_random(stream, lambda, k, m)
}

if erlang {
  fn do_weibull_random(
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
                assert Ok(x2) = math.pow(-1.0 *. x1, 1.0 /. k)
                lambda *. x2
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
}

if javascript {
  external fn do_weibull_random(
    Iterator(Int),
    Float,
    Float,
    Int,
  ) -> Result(#(List(Float), Iterator(Int)), String) =
    "../../weibull.mjs" "weibull_random"
}
