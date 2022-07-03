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
//// Functions related to discrete negative binomial random variables.
////
//// ---
////
//// * **Available Functions**
////   * [`negbinomial_mean`](#negbinomial_mean)
////   * [`negbinomial_variance`](#negbinomial_variance)
////   * [`negbinomial_pmf`](#negbinomial_pmf)
////   * [`negbinomial_cdf`](#negbinomial_cdf)
////   * [`negbinomial_random`](#negbinomial_random)

import gleam/iterator.{Iterator}
import gleam/int
import gleam_stats/math

if erlang {
  import gleam/list
  import gleam/pair
  import gleam_stats/distributions/geometric
}

fn check_negbinomial_parameters(r: Int, p: Float) -> Result(Bool, String) {
  case r > 0 {
    False ->
      "Invalid input argument: r <= 0. Valid input is r > 0."
      |> Error
    True ->
      case 0.0 <=. p && p <=. 1.0 {
        False ->
          "Invalid input argument: p < 0 or p > 1. Valid input is 0 <= p <= 1."
          |> Error
        True ->
          True
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
/// Analytically compute the mean of a discrete negative binomial random variable with parameters 
/// $$r \in \mathbb{Z}\_{>0}$$ (number of failures until the experiment is stopped) and $$p \in \[0, 1\]$$
/// (the success probability in each experiment).
///
/// The mean returned is: $$\frac{r \cdot p}{1 - p}$$.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn negbinomial_mean(r: Int, p: Float) -> Result(Float, String) {
  case check_negbinomial_parameters(r, p) {
    Error(string) ->
      string
      |> Error
    _ ->
      int.to_float(r) *. p /. { 1.0 -. p }
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Analytically compute the variance of a discrete negative binomial random variiable with parameters 
/// $$r \in \mathbb{Z}\_{>0}$$ (number of failures until the experiment is stopped) and $$p \in \[0, 1\]$$
/// (the success probability in each experiment).
///
/// The variance returned is: $$\frac{r \cdot p}{\(1 - p\)^2}$$.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn negbinomial_variance(r: Int, p: Float) -> Result(Float, String) {
  case check_negbinomial_parameters(r, p) {
    Error(string) ->
      string
      |> Error
    _ -> {
      assert Ok(v) = math.pow(1.0 -. p, 2.0)
      int.to_float(r) *. p /. v
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
/// Evaluate, at a certain point $$x \in \mathbb{Z}$$, the probability mass function (pmf) of a discrete 
/// negative binomial random variable with parameters $$r \in \mathbb{Z}\_{>0}$$ (number of failures until 
/// the experiment is stopped) and $$p \in \[0, 1\]$$ (the success probability in each experiment).
///
/// The pmf is defined as:
///
/// \\[
/// f(x; r, p) = 
/// \begin{cases}
///  \binom{x + r - 1}{x} \cdot \(1 - p\)^{r} \cdot p^{x} &\text{if } x \geq 0, \\\\
///  0 &\text{if } x < 0.
/// \end{cases} 
/// \\]
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/negbinomial
///     import gleeunit/should
///
///     pub fn example() {
///       let r: Float = 40.
///       let p: Float = 0.5
///       // For illustrational purposes, evaluate the pmf at the 
///       // point -100.0
///       negbinomial.negbinomial_pmf(-100.0, r, p) |> should.equal(Ok(0.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn negbinomial_pmf(x: Int, r: Int, p: Float) -> Result(Float, String) {
  do_negbinomial_pmf(x, r, p)
}

if erlang {
  fn do_negbinomial_pmf(x: Int, r: Int, p: Float) -> Result(Float, String) {
    case check_negbinomial_parameters(r, p) {
      Error(string) ->
        string
        |> Error
      _ ->
        case x >= 0 && x + r - 1 > 0 {
          True -> {
            assert Ok(c) = math.combination(x + r - 1, x)
            assert Ok(v1) = math.pow(1.0 -. p, int.to_float(r))
            assert Ok(v2) = math.pow(p, int.to_float(x))
            int.to_float(c) *. v1 *. v2
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
  external fn do_negbinomial_pmf(Int, Int, Float) -> Result(Float, String) =
    "../../negbinomial.mjs" "negbinomial_pmf"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Evaluate, at a certain point $$x \in \mathbb{Z}$$, the cumulative distribution function (cdf) of a 
/// discrete negative binomial random variable with parameters $$r \in \mathbb{Z}\_{>0}$$ (number of 
/// failures until the experiment is stopped) and $$p \in \[0, 1\]$$ (the success probability in each
/// experiment).
///
/// The cdf is defined as:
///
/// \\[
/// F(x; r, p) = 
/// \begin{cases}
///  \sum_{i=0}^{x} \binom{i + r - 1}{i} \cdot \(1 - p\)^{r} \cdot p^{i} &\text{if } x \geq 0, \\\\
///  0 &\text{if } x < 0.
/// \end{cases} 
/// \\]
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/negbinomial
///     import gleeunit/should
///
///     pub fn example() {
///       let r: Float = 40.
///       let p: Float = 0.5
///       // For illustrational purposes, evaluate the cdf at the 
///       // point -100.0
///       negbinomial.negbinomial_cdf(-100.0, r, p) |> should.equal(Ok(0.0))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn negbinomial_cdf(x: Int, r: Int, p: Float) -> Result(Float, String) {
  do_negbinomial_cdf(x, r, p)
}

if erlang {
  fn do_negbinomial_cdf(x: Int, r: Int, p: Float) -> Result(Float, String) {
    case check_negbinomial_parameters(r, p) {
      Error(string) ->
        string
        |> Error
      _ ->
        case x < 0 {
          True ->
            0.0
            |> Ok
          False ->
            case x >= 0 && x + r - 1 > 0 {
              True ->
                list.range(0, x + 1)
                |> list.fold(
                  0.0,
                  fn(acc: Float, i: Int) {
                    assert Ok(c) = math.combination(i + r - 1, i)
                    assert Ok(v1) = math.pow(1.0 -. p, int.to_float(r))
                    assert Ok(v2) = math.pow(p, int.to_float(i))
                    acc +. int.to_float(c) *. v1 *. v2
                  },
                )
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
  external fn do_negbinomial_cdf(Int, Int, Float) -> Result(Float, String) =
    "../../negbinomial.mjs" "negbinomial_cdf"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generate $$m \in \mathbb{Z}\_{>0}$$ random numbers from a discrete negative binomial distribution
/// with parameters $$r \in \mathbb{Z}\_{>0}$$ (number of failures until the experiment is stopped)
/// and $$p \in \[0, 1\]$$ (the success probability in each experiment).
/// 
/// The random numbers are generated using the inverse transform method.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import gleam_stats/generator
///     import gleam_stats/distributions/negbinomial
///
///     pub fn example() {
///       let seed: Int = 5
///       let seq: Int = 1
///       let r: Float = 40.
///       let p: Float = 0.5
///       assert Ok(out) =
///         generators.seed_pcg32(seed, seq)
///         |> negbinomial.negbinomial_random(r, p, 5_000)
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
pub fn negbinomial_random(
  stream: Iterator(Int),
  r: Int,
  p: Float,
  m: Int,
) -> Result(#(List(Int), Iterator(Int)), String) {
  do_negbinomial_random(stream, r, p, m)
}

if erlang {
  fn do_negbinomial_random(
    stream: Iterator(Int),
    r: Int,
    p: Float,
    m: Int,
  ) -> Result(#(List(Int), Iterator(Int)), String) {
    case check_negbinomial_parameters(r, p) {
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
            assert Ok(out) = geometric.geometric_random(stream, p, r * m)
            // Transform each batch of 'm' bernoulli distributed random numbers to a negative binomial
            // distributed random number
            let numbers: List(Int) =
              pair.first(out)
              |> list.window(r)
              |> list.map(fn(x: List(Int)) -> Int {
                x
                |> list.fold(0, fn(a: Int, b: Int) -> Int { a + b })
              })
            // Then return a tuple consisting of a list of negative binomial random numbers
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
  external fn do_negbinomial_random(
    Iterator(Int),
    Int,
    Float,
    Int,
  ) -> Result(#(List(Int), Iterator(Int)), String) =
    "../../negbinomial.mjs" "negbinomial_random"
}
