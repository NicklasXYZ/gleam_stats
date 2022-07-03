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
//// Functions related to discrete geometric random variables.
////
//// ---
////
//// * **Available Functions**
////   * [`geometric_mean`](#geometric_mean)
////   * [`geometric_variance`](#geometric_variance)
////   * [`geometric_pmf`](#geometric_pmf)
////   * [`geometric_cdf`](#geometric_cdf)
////   * [`geometric_random`](#geometric_random)

import gleam/iterator.{Iterator}
import gleam_stats/math

if erlang {
  import gleam/list
  import gleam/int
  import gleam/pair
  import gleam/float
  import gleam_stats/distributions/uniform
}

fn check_geometric_parameters(p: Float) -> Result(Bool, String) {
  case 0.0 <. p && p <=. 1.0 {
    False ->
      "Invalid input argument: p <= 0 or p > 1. Valid input is 0 < p <= 1."
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
/// Analytically compute the mean of a discrete geometric random variable with parameter
/// $$p \in \(0, 1\]$$ (the success probability).
///
/// The mean returned is: $$\frac{1 - p}{p}$$.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn geometric_mean(p: Float) -> Result(Float, String) {
  case check_geometric_parameters(p) {
    Error(string) ->
      string
      |> Error
    _ ->
      { 1.0 -. p } /. p
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Analytically compute the variance of a discrete geometric random variable with parameter
/// $$p \in \(0, 1\]$$ (the success probability).
///
/// The variance returned is: $$\frac{1 - p}{p^{2}}$$.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn geometric_variance(p: Float) -> Result(Float, String) {
  case check_geometric_parameters(p) {
    Error(string) ->
      string
      |> Error
    _ -> {
      assert Ok(v) = math.pow(p, 2.0)
      { 1.0 -. p } /. v
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
/// Evaluate, at a certain point $$x \in \mathbb{Z}$$, the probability mass function (pmf) of 
/// a discrete geometric random variable with parameter $$p \in \(0, 1\]$$ (the success probability).
///
/// The pmf is defined as:
///
/// \\[
/// f(x; p) =
/// \begin{cases}
/// \(1 - p\)^{x} \cdot p &\text{if } x \geq 0, \\\\
/// 0 &\text{if } x < 0.
/// \end{cases}
/// \\]
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/geometric
///     import gleeunit/should
///
///     pub fn example() {
///       let p: Float = 0.5
///       // For illustrational purposes, evaluate the pmf at the 
///       // point -100.0
///       geometric.geometric_pmf(-100.0, r, p) 
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
pub fn geometric_pmf(x: Int, p: Float) -> Result(Float, String) {
  do_geometric_pmf(x, p)
}

if erlang {
  fn do_geometric_pmf(x: Int, p: Float) -> Result(Float, String) {
    case check_geometric_parameters(p) {
      Error(string) ->
        string
        |> Error
      _ ->
        case x >= 0 {
          True -> {
            assert Ok(v) = math.pow(1.0 -. p, int.to_float(x))
            v *. p
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
  external fn do_geometric_pmf(Int, Float) -> Result(Float, String) =
    "../../geometric.mjs" "geometric_pmf"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Evaluate, at a certain point $$x \in \mathbb{Z}$$, the cumulative distribution function 
/// (cdf) of a discrete geometric random variable with with parameter $$p \in \(0, 1\]$$
/// (the success probability).
///
/// The cdf is defined as:
///
/// \\[
/// F(x; p) = 
/// \begin{cases}
///  1 - \(1 - p\)^{x + 1} &\text{if } x \geq 0, \\\\
///  0 &\text{if } x < a \text{ or } x < 0.
/// \end{cases} 
/// \\]
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/geometric
///     import gleeunit/should
///
///     pub fn example() {
///       let p: Float = 0.5
///       // For illustrational purposes, evaluate the cdf at the 
///       // point -100.0
///       geometric.geometric_cdf(-100.0, r, p) 
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
pub fn geometric_cdf(x: Int, p: Float) -> Result(Float, String) {
  do_geometric_cdf(x, p)
}

if erlang {
  fn do_geometric_cdf(x: Int, p: Float) -> Result(Float, String) {
    case check_geometric_parameters(p) {
      Error(string) ->
        string
        |> Error
      _ ->
        case x >= 0 {
          True -> {
            assert Ok(v) = math.pow(1.0 -. p, int.to_float(x) +. 1.0)
            1.0 -. v
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
  external fn do_geometric_cdf(Int, Float) -> Result(Float, String) =
    "../../geometric.mjs" "geometric_cdf"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generate $$m \in \mathbb{Z}\_{>0}$$ random numbers from a discrete geometric distribution with 
/// parameter $$p \in \(0, 1\]$$ (the success probability).
/// 
/// The random numbers are generated using the inverse transform method.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import gleam_stats/generators
///     import gleam_stats/distributions/geometric
///
///     pub fn example() {
///       let seed: Int = 5
///       let seq: Int = 1
///       let p: Float = 0.5
///       assert Ok(out) =
///         generators.seed_pcg32(seed, seq)
///         |> geometric.geometric_random(r, p, 5_000)
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
pub fn geometric_random(
  stream: Iterator(Int),
  p: Float,
  m: Int,
) -> Result(#(List(Int), Iterator(Int)), String) {
  do_geometric_random(stream, p, m)
}

if erlang {
  fn do_geometric_random(
    stream: Iterator(Int),
    p: Float,
    m: Int,
  ) -> Result(#(List(Int), Iterator(Int)), String) {
    case check_geometric_parameters(p) {
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
            // Transform the 'm' continuous uniform random numbers to geometric distributed 
            // random numbers
            let numbers: List(Int) =
              pair.first(out)
              |> list.map(fn(x) {
                assert Ok(x1) = math.log(x)
                assert Ok(x2) = math.log(1. -. p)
                float.round(math.floor(x1 /. x2))
              })
            // Then return a tuple consisting of a list of geometric random numbers
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
  external fn do_geometric_random(
    Iterator(Int),
    Float,
    Int,
  ) -> Result(#(List(Int), Iterator(Int)), String) =
    "../../geometric.mjs" "geometric_random"
}
