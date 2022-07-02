//// Functions related to discrete binomial random variables.
////
//// ---
////
//// * **Available functions**
////   * [`binomial_mean`](#binomial_mean)
////   * [`binomial_variance`](#binomial_variance)
////   * [`binomial_pmf`](#binomial_pmf)
////   * [`binomial_cdf`](#binomial_cdf)
////   * [`binomial_random`](#binomial_random)

import gleam/iterator.{Iterator}
import gleam/int

if erlang {
  import gleam/list
  import gleam/pair
  import gleam_stats/math
  import gleam_stats/distributions/bernoulli
}

fn check_binomial_parameters(n: Int, p: Float) -> Result(Bool, String) {
  case n >= 0 {
    False ->
      "Invalid input argument: n < 0. Valid input is n >= 0."
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
/// Analytically compute the mean of a discrete binomial random variable with parameters
/// $$n \in \mathbb{Z}\_{\geq 0}$$ (number of trials) and $$p \in \[0, 1\]$$ (the success probability 
/// in each trial).
///
/// The mean returned is: $$n \cdot p$$.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn binomial_mean(n: Int, p: Float) -> Result(Float, String) {
  case check_binomial_parameters(n, p) {
    Error(string) ->
      string
      |> Error
    _ ->
      int.to_float(n) *. p
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Analytically compute the variance of a discrete binomial random variable with parameters
/// $$n \in \mathbb{Z}\_{\geq 0}$$ (number of trials) and $$p \in \[0, 1\]$$ (the success probability 
/// in each trial).
///
/// The variance returned is: $$n \cdot p \cdot (1 - p)$$.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn binomial_variance(n: Int, p: Float) -> Result(Float, String) {
  case check_binomial_parameters(n, p) {
    Error(string) ->
      string
      |> Error
    _ ->
      int.to_float(n) *. p *. { 1.0 -. p }
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Evaluate, at a certain point $$x \in \mathbb{Z}$$, the probability mass function (pmf) of 
/// a discrete binomial random variable with parameters $$n \in \mathbb{Z}\_{\geq 0}$$ (number of trials)
/// and $$p \in \[0, 1\]$$ (the success probability in each trial).
///
/// The pmf is defined as:
///
/// \\[
/// f(x; n, p) = 
/// \begin{cases}
/// \binom{n}{x} \cdot p^{x} \cdot \(1 - p\)^{n - x} &\text{if } x \geq 0, \\\\
/// 0 &\text{if } x < 0.
/// \end{cases}
/// \\]
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/binomial
///     import gleeunit/should
///
///     pub fn example() {
///       let n: Float = 40.
///       let p: Float = 0.5
///       // For illustrational purposes, evaluate the pmf at the 
///       // point -100.0
///       binomial.binomial_pmf(-100.0, n, p) |> should.equal(0.0)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn binomial_pmf(x: Int, n: Int, p: Float) -> Result(Float, String) {
  do_binomial_pmf(x, n, p)
}

if erlang {
  fn do_binomial_pmf(x: Int, n: Int, p: Float) -> Result(Float, String) {
    case check_binomial_parameters(n, p) {
      Error(string) ->
        string
        |> Error
      _ ->
        case x >= 0 && x <= n {
          True -> {
            assert Ok(c) = math.combination(n, x)
            assert Ok(v1) = math.pow(p, int.to_float(x))
            assert Ok(v2) = math.pow(1.0 -. p, int.to_float(n - x))
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
  external fn do_binomial_pmf(Int, Int, Float) -> Result(Float, String) =
    "../../binomial.mjs" "binomial_pmf"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Evaluate, at a certain point $$x \in \mathbb{Z}$$, the cumulative distribution 
/// function (cdf) of a discrete binomial random variable with parameters $$n \in \mathbb{Z}\_{\geq 0}$$ 
/// (number of trials) and $$p \in \[0, 1\]$$ (the success probability in each trial).
///
/// The cdf is defined as:
///
/// \\[
/// F(x; n, p) = 
/// \begin{cases}
/// \sum_{i=0}^{x} \binom{n}{i} \cdot p^{i} \cdot \(1-p\)^{n-i} &\text{if } x \geq 0,\\\\
/// 0 &\text{if } x < 0.
/// \end{cases}
/// \\]
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/binomial
///     import gleeunit/should
///
///     pub fn example() {
///       let n: Float = 40.
///       let p: Float = 0.5
///       // For illustrational purposes, evaluate the cdf at the 
///       // point -100.0
///       binomial.binomial_cdf(-100.0, n, p) |> should.equal(0.0)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn binomial_cdf(x: Int, n: Int, p: Float) -> Result(Float, String) {
  do_binomial_cdf(x, n, p)
}

if erlang {
  fn do_binomial_cdf(x: Int, n: Int, p: Float) -> Result(Float, String) {
    // TODO: Make it possible to estimate the cdf via the normal distribution (faster for large n)
    // TODO: Make it possible to estimate cdf via the poisson distribution (faster large n and small p)
    case check_binomial_parameters(n, p) {
      Error(string) ->
        string
        |> Error
      _ ->
        case x < 0 {
          True ->
            0.0
            |> Ok
          False ->
            case x >= 0 && x < n {
              True ->
                list.range(0, x + 1)
                |> list.fold(
                  0.0,
                  fn(acc: Float, i: Int) {
                    assert Ok(c) = math.combination(n, i)
                    assert Ok(v1) = math.pow(p, int.to_float(i))
                    assert Ok(v2) = math.pow(1.0 -. p, int.to_float(n - i))
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
  external fn do_binomial_cdf(Int, Int, Float) -> Result(Float, String) =
    "../../binomial.mjs" "binomial_cdf"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generate $$m \in \mathbb{Z}\_{>0}$$ random numbers from a binomial distribution (discrete) with parameters 
/// $$n \in \mathbb{Z}\_{\geq 0}$$ (number of trials) and $$p \in \[0, 1\]$$ (the success 
/// probability in each trial).
/// 
/// The random numbers are generated using the inverse transform method.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import gleam_stats/generator
///     import gleam_stats/distributions/binomial
///
///     pub fn example() {
///       let seed: Int = 5
///       let seq: Int = 1
///       let n: Float = 40.
///       let p: Float = 0.5
///       assert Ok(out) =
///         generators.seed_pcg32(seed, seq)
///         |> binomial.binomial_random(n, p, 5_000)
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
pub fn binomial_random(
  stream: Iterator(Int),
  n: Int,
  p: Float,
  m: Int,
) -> Result(#(List(Int), Iterator(Int)), String) {
  do_binomial_random(stream, n, p, m)
}

if erlang {
  fn do_binomial_random(
    stream: Iterator(Int),
    n: Int,
    p: Float,
    m: Int,
  ) -> Result(#(List(Int), Iterator(Int)), String) {
    case check_binomial_parameters(n, p) {
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
            assert Ok(out) = bernoulli.bernoulli_random(stream, p, n * m)
            // Transform each batch of 'm' bernoulli distributed random numbers to a binomial
            // distributed random number
            let numbers: List(Int) =
              pair.first(out)
              |> list.window(n)
              |> list.map(fn(x: List(Int)) -> Int {
                x
                |> list.fold(0, fn(a: Int, b: Int) -> Int { a + b })
              })
            // Then return a tuple consisting of a list of binomial random numbers
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
  external fn do_binomial_random(
    Iterator(Int),
    Int,
    Float,
    Int,
  ) -> Result(#(List(Int), Iterator(Int)), String) =
    "../../binomial.mjs" "binomial_random"
}
