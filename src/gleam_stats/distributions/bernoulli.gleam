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
//// Functions related to discrete Bernoulli random variables.
////
//// ---
////
//// * **Available functions**
////   * [`bernoulli_mean`](#bernoulli_mean)
////   * [`bernoulli_variance`](#bernoulli_variance)
////   * [`bernoulli_pmf`](#bernoulli_pmf)
////   * [`bernoulli_cdf`](#bernoulli_cdf)
////   * [`bernoulli_random`](#bernoulli_random)

import gleam/iterator.{Iterator}

if erlang {
  import gleam/pair
  import gleam/list
  import gleam_stats/distributions/uniform
}

fn check_bernoulli_parameters(p: Float) -> Result(Bool, String) {
  case 0.0 <=. p && p <=. 1.0 {
    False ->
      "Invalid input argument: p < 0 or p > 1. Valid input is 0 <= p <= 1."
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
/// Analytically compute the mean of a discrete Bernoulli random variable with parameter
/// $$p \in \[0, 1\]$$ (the success probability of a trial).
///
/// The mean returned is: $$p$$.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn bernoulli_mean(p: Float) -> Result(Float, String) {
  case check_bernoulli_parameters(p) {
    Error(string) ->
      string
      |> Error
    _ ->
      p
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Analytically compute the variance of a discrete Bernoulli random variable with parameter
/// $$p \in \[0, 1\]$$ (the success probability of a trial).
///
/// The variance returned is: $$p \cdot \(1 - p\)$$.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn bernoulli_variance(p: Float) -> Result(Float, String) {
  case check_bernoulli_parameters(p) {
    Error(string) ->
      string
      |> Error
    _ ->
      p *. { 1.0 -. p }
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Evaluate, at a certain point $$x \in \{0, 1\}$$, the probability mass function (pmf) of a 
/// discrete Bernoulli random variable with parameter $$p \in \[0, 1\]$$ (the success 
/// probability of a trial).
///
/// The pmf is defined as:
///
/// \\[
/// f(x; p) = 
/// \begin{cases}
///  p &\text{if } x = 0, \\\\
///  1 - p &\text{if } x = 1.
/// \end{cases}
/// \\]
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/bernoulli
///     import gleeunit/should
///
///     pub fn example() {
///       let p: Float = 0.5
///       // For illustrational purposes, evaluate the pmf at the 
///       // point -100.0
///       bernoulli.bernoulli_pmf(-100.0, p) 
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
pub fn bernoulli_pmf(x: Int, p: Float) -> Result(Float, String) {
  do_bernoulli_pmf(x, p)
}

if erlang {
  fn do_bernoulli_pmf(x: Int, p: Float) -> Result(Float, String) {
    case check_bernoulli_parameters(p) {
      Error(string) ->
        string
        |> Error
      _ ->
        case x == 0 || x == 1 {
          True ->
            case x {
              0 ->
                1.0 -. p
                |> Ok
              1 ->
                p
                |> Ok
            }
          _ ->
            0.0
            |> Ok
        }
    }
  }
}

if javascript {
  external fn do_bernoulli_pmf(Int, Float) -> Result(Float, String) =
    "../../bernoulli.mjs" "bernoulli_pmf"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Evaluate, at a certain point $$x \in \mathbb{Z}$$, the cumulative distribution function (cdf) of 
/// a discrete Bernoulli random variable with parameter $$p \in \[0, 1\]$$ (the success probability
/// of a trial).
///
/// The cdf is defined as:
///
/// \\[
/// F(x; p) = 
/// \begin{cases}
///  0 &\text{if } x < 0, \\\\
///  1 - p &\text{if } x \leq < 1, \\\\
///  1 &\text{if } x \geq 1.
/// \end{cases}
/// \\]
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/bernoulli
///     import gleeunit/should
///
///     pub fn example() {
///       let p: Float = 0.5
///       // For illustrational purposes, evaluate the cdf at the 
///       // point -100.0
///       bernoulli.bernoulli_cdf(-100.0, p) 
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
pub fn bernoulli_cdf(x: Int, p: Float) -> Result(Float, String) {
  do_bernoulli_cdf(x, p)
}

if erlang {
  fn do_bernoulli_cdf(x: Int, p: Float) -> Result(Float, String) {
    case check_bernoulli_parameters(p) {
      Error(string) ->
        string
        |> Error
      _ ->
        case x < 0 {
          True ->
            0.0
            |> Ok
          False ->
            case 0 <= x && x < 1 {
              True ->
                1.0 -. p
                |> Ok
              False ->
                1.0
                |> Ok
            }
        }
    }
  }
}

if javascript {
  external fn do_bernoulli_cdf(Int, Float) -> Result(Float, String) =
    "../../bernoulli.mjs" "bernoulli_cdf"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generate $$m \in \mathbb{Z}\_{>0}$$ random numbers from a Bernoulli distribution (discrete) with 
/// parameter $$p \in \[0, 1\]$$ (the success probability of a trial).
/// 
/// The random numbers are generated using the inverse transform method.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import gleam_stats/generators
///     import gleam_stats/distributions/bernoulli
///
///     pub fn example() {
///       let seed: Int = 5
///       let seq: Int = 1
///       let p: Float = 0.5
///       assert Ok(out) =
///         generators.seed_pcg32(seed, seq)
///         |> bernoulli.bernoulli_random(p, 5_000)
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
pub fn bernoulli_random(
  stream: Iterator(Int),
  p: Float,
  m: Int,
) -> Result(#(List(Int), Iterator(Int)), String) {
  do_bernoulli_random(stream, p, m)
}

if erlang {
  fn do_bernoulli_random(
    stream: Iterator(Int),
    p: Float,
    m: Int,
  ) -> Result(#(List(Int), Iterator(Int)), String) {
    case check_bernoulli_parameters(p) {
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
            // Transform the 'm' continuous uniform random numbers to bernoulli random numbers
            let numbers: List(Int) =
              pair.first(out)
              |> list.map(fn(x: Float) -> Int {
                case x <=. p {
                  True -> 1
                  False -> 0
                }
              })
            // Then return a tuple consisting of a list of bernoulli random numbers
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
  external fn do_bernoulli_random(
    Iterator(Int),
    Float,
    Int,
  ) -> Result(#(List(Int), Iterator(Int)), String) =
    "../../bernoulli.mjs" "bernoulli_random"
}
