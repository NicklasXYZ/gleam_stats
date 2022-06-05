//// Functions related to discrete binomial random variables.
////
//// ---
////
//// * **Available Functions**
////   * [`binomial_mean`](#binomial_mean)
////   * [`binomial_variance`](#binomial_variance)
////   * [`binomial_pmf`](#binomial_pmf)
////   * [`binomial_cdf`](#binomial_cdf)
////   * [`binomial_random`](#binomial_random)

import gleam/list
import gleam/iterator.{Iterator}
import gleam/float
import gleam/int
import gleam/pair
import gleam_stats/math
import gleam_stats/distributions/bernoulli

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
/// Analytically compute the mean of a discrete Binomial random variable with parameters
/// $$n > 0$$ (number of trials) and $$p \in \[0, 1\]$$ (the success probability 
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
/// Analytically compute the variance of a discrete Binomial random variable with parameters
/// $$n > 0$$ (number of trials) and $$p \in \[0, 1\]$$ (the success probability 
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
/// Evaluate the probability mass function (pmf) of a discrete Binomial random variable with
/// parameters $$n > 0$$ (number of trials) and $$p \in \[0, 1\]$$ (the success 
/// probability in each trial).
///
/// The pmf is defined as:
///
/// \\[
/// f(x; n, p) =  \binom{n}{x} \cdot p^{x} (1 - p)^{n - x}
/// \\]
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/binomial
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
  case check_binomial_parameters(n, p) {
    Error(string) ->
      string
      |> Error
    _ ->
      case x >= 0 && x <= n {
        True -> {
          assert Ok(c) = math.combination(n, x)
          int.to_float(c) *. float.power(p, int.to_float(x)) *. float.power(
            1.0 -. p,
            int.to_float(n - x),
          )
          |> Ok
        }
        _ ->
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
/// Evaluate, at a certain point $$x \in \mathbb{Z}$$, the cumulative distribution function (cdf) of a 
/// discrete Binomial random variable with parameters $$n > 0$$ (number of trials) and $$p \in \[0, 1\]$$
/// (the success probability in each trial).
///
/// The cdf is defined as:
///
/// \\[
/// F(x; n, p) = \sum_{i=0}^{\lfloor k \rfloor} \binom{n}{i}p^{i}(1-p)^{n-i}
/// \\]
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/distributions/binomial
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
                  let v: Float = int.to_float(i)
                  assert Ok(c) = math.combination(n, i)
                  acc +. int.to_float(c) *. float.power(p, v) *. float.power(
                    1.0 -. p,
                    int.to_float(n - i),
                  )
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

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generate $$m \in \mathbb{N}$$ random numbers from a Binomial distribution (discrete) with parameters 
/// $$n > 0$$ (number of trials) and $$p \in \[0, 1\]$$ (the success probability in each trial).
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
///         generators.seed_pcg32(seed)
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
