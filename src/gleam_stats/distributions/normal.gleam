////<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.15.6/dist/katex.min.css" integrity="sha384-ZPe7yZ91iWxYumsBEOn7ieg8q/o+qh/hQpSaPow8T6BwALcXSCS6C6fSRPIAnTQs" crossorigin="anonymous">
////<script defer src="https://cdn.jsdelivr.net/npm/katex@0.15.6/dist/katex.min.js" integrity="sha384-ljao5I1l+8KYFXG7LNEA7DyaFvuvSCmedUf6Y6JI7LJqiu8q5dEivP2nDdFH31V4" crossorigin="anonymous"></script>
////<script defer src="https://cdn.jsdelivr.net/npm/katex@0.15.6/dist/contrib/auto-render.min.js" integrity="sha384-+XBljXPPiv+OzfbB3cVmLHf4hdUFHlWNZN5spNQ7rmHTXpd7WvJum6fIACpNNfIR" crossorigin="anonymous"></script>
////<script>
////    document.addEventListener("DOMContentLoaded", function() {
////        renderMathInElement(document.body, {
////          // customised options
////          // • auto-render specific keys, e.g.:
////          delimiters: [
////              {left: '$$', right: '$$', display: false},
////            //   {left: '$', right: '$', display: false},
////            //   {left: '\\(', right: '\\)', display: false},
////              {left: '\\[', right: '\\]', display: true}
////          ],
////          // • rendering keys, e.g.:
////          throwOnError : false
////        });
////    });
////</script>
////<style>
////    .katex { font-size: 1.1em; }
////</style>
////
//// Functions related to continuous normal random variables.
////
//// ---
////
//// * **Available functions**
////   * [`normal_mean`](#normal_mean)
////   * [`normal_variance`](#normal_variance)
////   * [`normal_pdf`](#normal_pdf)
////   * [`normal_cdf`](#normal_cdf)
////   * [`normal_random`](#normal_random)

import gleam/iterator.{Iterator}
import gleam_stats/math

if erlang {
  import gleam/pair
  import gleam/list
  import gleam_stats/distributions/uniform
}

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
/// with given mean $$\mu \in \(-\infty, +\infty\)$$ and standard deviation
/// $$\sigma \in \(-\infty, +\infty\)$$ .
///
/// The mean returned is: $$\mu$$.
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
/// with given mean $$\mu \in \(-\infty, +\infty\)$$ and standard deviation
/// $$\sigma\in \(-\infty, +\infty\)$$ .
///
/// The variance returned is: $$\sigma^{2}$$.
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
    _ -> {
      assert Ok(v) = math.pow(sigma, 2.0)
      v
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
/// Evaluate, at a certain point $$x \in \(-\infty, +\infty\)$$, the probability density function (pdf) of a
/// continuous normal random variable with given mean $$\mu \in \(-\infty, +\infty\)$$ and standard deviation
/// $$\sigma\in \(-\infty, +\infty\)$$ .
///
/// The pdf is defined as:
///
/// \\[
/// f(x; \mu, \sigma) = \frac{1}{\sigma \cdot \left(2 \cdot \pi \right)^{\frac{1}{2}}} \cdot e^{- \frac{1}{2} \cdot \left(\frac{x - \mu}{\sigma}\right)^{2}}
/// \\]
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
///       normal.normal_pdf(-100.0, mu, sigma) 
///       |> should.equal(Ok(0.0))
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
  do_normal_pdf(x, mu, sigma)
}

if erlang {
  fn do_normal_pdf(x: Float, mu: Float, sigma: Float) -> Result(Float, String) {
    case check_normal_parameters(mu, sigma) {
      Error(string) ->
        string
        |> Error
      _ -> {
        assert Ok(v1) = math.pow(x -. mu, 2.0)
        assert Ok(v2) = math.pow(sigma, 2.0)
        let numexp: Float = v1 /. { 2.0 *. v2 }
        assert Ok(v3) = math.pow(2.0 *. math.pi(), 0.5)
        let denominator: Float = sigma *. v3
        let numerator: Float = math.exp(numexp *. -1.0)
        numerator /. denominator
        |> Ok
      }
    }
  }
}

if javascript {
  external fn do_normal_pdf(Float, Float, Float) -> Result(Float, String) =
    "../../normal.mjs" "normal_pdf"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Evaluate, at a certain point $$x \in \(-\infty, \infty\)$$, the cumulative distribution function (cdf)
/// of a continuous normal random variable with mean $$\mu \in \(-\infty, +\infty\)$$ and standard deviation
/// $$\sigma\in \(-\infty, +\infty\)$$.
///
/// The cdf is defined as:
///
/// \\[
/// F(x; \mu, \sigma) = \frac{1}{2} \cdot \left[ 1 + \text{erf}\left(\frac{x - \mu}{\sigma \cdot 2^{\frac{1}{2}}}\right) \right]
/// \\]
///
/// In the formula $$\text{erf}(\dot)$$ is the error function.
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
///       normal.normal_cdf(-100.0, mu, sigma) 
///       |> should.equal(Ok(0.0))
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
  do_normal_cdf(x, mu, sigma)
}

if erlang {
  fn do_normal_cdf(x: Float, mu: Float, sigma: Float) -> Result(Float, String) {
    case check_normal_parameters(mu, sigma) {
      Error(string) ->
        string
        |> Error
      _ -> {
        assert Ok(v) = math.pow(2.0, 0.5)
        let denominator: Float = sigma *. v
        0.5 *. { 1.0 +. math.erf({ x -. mu } /. denominator) }
        |> Ok
      }
    }
  }

  // Use Box-Muller transform to sample from the normal distribution,
  // given standard uniform distributed random numbers.
  fn box_muller(u1: Float, u2: Float) -> List(Float) {
    assert Ok(u) = math.log(u1)
    assert Ok(x) = math.pow(-2.0 *. u, 0.5)
    [
      x *. math.cos(2.0 *. math.pi() *. u2),
      x *. math.sin(2.0 *. math.pi() *. u2),
    ]
  }
}

if javascript {
  external fn do_normal_cdf(Float, Float, Float) -> Result(Float, String) =
    "../../normal.mjs" "normal_cdf"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generate $$m \in \in \mathbb{Z}\_{>0}$$ random numbers from a continuous normal distribution with a given
/// mean $$\mu \in \(-\infty, +\infty\)$$ and standard deviation $$\sigma\in \(-\infty, +\infty\)$$.
///
/// The random numbers are generated using Box–Muller transform.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import gleam_stats/generators
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
  do_normal_random(stream, mu, sigma, m)
}

if erlang {
  fn do_normal_random(
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
}

if javascript {
  external fn do_normal_random(
    Iterator(Int),
    Float,
    Float,
    Int,
  ) -> Result(#(List(Float), Iterator(Int)), String) =
    "../../normal.mjs" "normal_random"
}
