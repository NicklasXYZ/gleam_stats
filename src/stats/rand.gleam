//// A module for generating random numbers from common distributions.
////
//// ---
////
//// * **Retrieving numbers from the base-generator**
////   * [`take_randints`](#take_randints)
//// * **Generating random numbers from common distributions**
////   * [`next_uniform`](#next_uniform)
////   * [`next_normal`](#next_normal)
////   * [`next_randint`](#next_randint)
////   * [`next_bern`](#next_bern)
////   * [`next_binom`](#next_binom)
////   * [`next_negbinom`](#next_negbinom)
////   * [`next_geom`](#next_geom)
////   * [`next_exp`](#next_exp)

import gleam/bitwise
import gleam/list
import gleam/iterator.{Iterator}
import gleam/float
import gleam/int
import gleam/pair
import gleam/float
import stats/math
import stats/generators.{mask_32}
import gleam/io

external fn cos(Float) -> Float =
  "math" "cos"

external fn sin(Float) -> Float =
  "math" "sin"

external fn log(Float) -> Float =
  "math" "log"

external fn pi() -> Float =
  "math" "pi"

// Use Box-Muller transform to sample from the normal distribution,
// given standard uniform distributed random numbers.
fn box_muller(u1: Float, u2: Float) {
  case float.square_root(-2. *. log(u1)) {
    Ok(x) -> x *. cos(2. *. pi() *. u2)
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Take out 'm' integers from the raw stream of pseudo-random 
/// numbers produced by a base-iterator.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import stats/generator
///     import stats/rand
///
///     pub fn example() {
///       let seed: Int = 5
///       let out =
///         generators.seed_mt19937(seed)
///         |> rand.take_randints(5_000)
///       let randints: List(Int) = pair.first(out)
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
pub fn take_randints(
  stream: Iterator(Int),
  m: Int,
) -> tuple(List(Int), Iterator(Int)) {
  // Take out 'm' integers from the stream of pseudo-random numbers.
  let numbers: List(Int) =
    stream
    |> iterator.take(m)
    |> iterator.to_list
  tuple(numbers, iterator.drop(stream, m))
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generate 'm' random numbers in the interval '[min, max)' from a 
/// continous uniform distribution ('max' excluded).
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import stats/generator
///     import stats/rand
///
///     pub fn example() {
///       let seed: Int = 5
///       let min: Float = 0.
///       let max: Float = 1.
///       let out =
///         generators.seed_mt19937(seed)
///         |> rand.next_uniform(min, max, 5_000)
///       let randunif: List(Float) = pair.first(out)
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
pub fn next_uniform(
  stream: Iterator(Int),
  min: Float,
  max: Float,
  m: Int,
) -> tuple(List(Float), Iterator(Int)) {
  // Take out 'm' integers from the stream of pseudo-random numbers.
  let out: tuple(List(Int), Iterator(Int)) = take_randints(stream, m)
  // Transform the 'm' integers to continuous uniform random numbers in an interval.
  let numbers: List(Float) =
    pair.first(out)
    |> list.map(fn(x) {
      max *. { int.to_float(x) /. int.to_float(mask_32()) } +. min
    })
  // Then return a tuple consisting of a list of continuous uniform random numbers
  // and the stream of pseudo-random numbers where the 'm' integers have been dropped
  // from the stream.
  tuple(numbers, pair.second(out))
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generate 'm' random numbers from a normal distribution with a given mean and 
/// standard deviation.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import stats/generator
///     import stats/rand
///
///     pub fn example() {
///       let seed: Int = 5
///       let mean: Float = 0.
///       let std: Float = 1.
///       let out =
///         generators.seed_mt19937(seed)
///         |> rand.next_normal(mean, std, 5_000)
///       let randnorm: List(Float) = pair.first(out)
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
pub fn next_normal(
  stream: Iterator(Int),
  mean: Float,
  sigma: Float,
  m: Int,
) -> tuple(List(Float), Iterator(Int)) {
  // Take out 'm' integers from the stream of pseudo-random numbers and generate 
  // uniform random numbers.
  let out: tuple(List(Float), Iterator(Int)) =
    next_uniform(stream, 0., 1., 2 * m)
  // Transform the 'm' continuous uniform random numbers to normal distributed
  // random numbers
  let numbers: List(Float) =
    pair.first(out)
    |> list.window(2)
    |> list.map(fn(x: List(Float)) -> Float {
      case x {
        [u1, u2] -> sigma *. box_muller(u1, u2) +. mean
      }
    })
  tuple(numbers, pair.second(out))
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generate 'm' random numbers in the interval ['min', 'max'] from a discrete 
/// uniform distribution ('max' included).
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import stats/generator
///     import stats/rand
///
///     pub fn example() {
///       let seed: Int = 5
///       let min: Int = 0.
///       let max: Int = 10.
///       let out =
///         generators.seed_mt19937(seed)
///         |> rand.next_randint(min, max, 5_000)
///       let randints: List(Int) = pair.first(out)
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
pub fn next_randint(
  stream: Iterator(Int),
  min: Int,
  max: Int,
  m: Int,
) -> tuple(List(Int), Iterator(Int)) {
  // Pre-compute constant
  let c = int.to_float(max) -. int.to_float(min) +. 1.
  // Take out 'm' integers from the stream of pseudo-random numbers and generate 
  // uniform random numbers.
  let out: tuple(List(Float), Iterator(Int)) = next_uniform(stream, 0., 1., m)
  // Transform the'm' continouous uniform random numbers to discrete uniform 
  // random numbers
  let numbers: List(Int) =
    pair.first(out)
    |> list.map(fn(x: Float) -> Int {
      float.round(int.to_float(min) +. float.floor(c *. x))
    })
  tuple(numbers, pair.second(out))
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generate 'm' random numbers from a bernoulli distribution with parameter
/// 0 ≤ 'p' ≤.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import stats/generator
///     import stats/rand
///
///     pub fn example() {
///       let seed: Int = 5
///       let p: Float = 0.5.
///       let out =
///         generators.seed_mt19937(seed)
///         |> rand.next_bern(p, 5_000)
///       let randberns: List(Int) = pair.first(out)
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
pub fn next_bern(
  stream: Iterator(Int),
  p: Float,
  m: Int,
) -> tuple(List(Int), Iterator(Int)) {
  // Take out 'm' integers from the stream of pseudo-random numbers and generate 
  // uniform random numbers.
  let out: tuple(List(Float), Iterator(Int)) = next_uniform(stream, 0., 1., m)
  // Transform the 'm' continuous uniform random numbers to bernoulli random numbers
  let numbers: List(Int) =
    pair.first(out)
    |> list.map(fn(x: Float) -> Int {
      case x <=. p {
        True -> 1
        False -> 0
      }
    })
  tuple(numbers, pair.second(out))
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generate 'm' random numbers from a binomial distribution with parameters 
/// 0 ≤ 'p' ≤ 1 and 'n' in {0, 1, 2,...}.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import stats/generator
///     import stats/rand
///
///     pub fn example() {
///       let seed: Int = 5
///       let p: Float = 0.5.
///       let n: Int  = 5
///       let out =
///         generators.seed_mt19937(seed)
///         |> rand.next_binom(p, n, 5_000)
///       let randbinoms: List(Int) = pair.first(out)
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
pub fn next_binom(
  stream: Iterator(Int),
  p: Float,
  n: Int,
  m: Int,
) -> tuple(List(Int), Iterator(Int)) {
  // Take out 'm' integers from the stream of pseudo-random numbers and generate 
  // bernoulli random numbers.
  let out: tuple(List(Int), Iterator(Int)) = next_bern(stream, p, n * m)
  // Transform each batch of 'm' bernoulli distributed random numbers to a binomial
  // distributed random number
  let numbers: List(Int) =
    pair.first(out)
    |> list.window(n)
    |> io.debug()
    |> list.map(fn(x: List(Int)) -> Int {
      x
      |> list.fold(0, fn(a: Int, b: Int) -> Int { a + b })
    })
  tuple(numbers, pair.second(out))
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generate 'm' random numbers from a negative binomial distribution with
/// parameters 0 ≤ 'p' ≤ 1 and 'n' in {0, 1, 2,...}.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import stats/generator
///     import stats/rand
///
///     pub fn example() {
///       let seed: Int = 5
///       let p: Float = 0.5.
///       let n: Int  = 5
///       let out =
///         generators.seed_mt19937(seed)
///         |> rand.next_negbinom(p, n, 5_000)
///       let randnegbinoms: List(Int) = pair.first(out)
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
pub fn next_negbinom(
  stream: Iterator(Int),
  p: Float,
  n: Int,
  m: Int,
) -> tuple(List(Int), Iterator(Int)) {
  // Take out 'm' integers from the stream of pseudo-random numbers and generate 
  // geometric distributed random numbers.
  let out: tuple(List(Int), Iterator(Int)) = next_geom(stream, p, n * m)
  // Transform each batch of 'm' geometric distributed random numbers 
  // to a negative binomial distributed random number
  let numbers: List(Int) =
    pair.first(out)
    |> list.window(n)
    |> list.map(fn(x: List(Int)) -> Int {
      x
      |> list.fold(0, fn(a: Int, b: Int) -> Int { a + b })
    })
  tuple(numbers, pair.second(out))
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generate 'm' random numbers from a geometric distribution with parameter 
/// 0 ≤ 'p' ≤ 1.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import stats/generator
///     import stats/rand
///
///     pub fn example() {
///       let seed: Int = 5
///       let p: Float = 0.5.
///       let out =
///         generators.seed_mt19937(seed)
///         |> rand.next_geom(p, 5_000)
///       let randgeoms: List(Int) = pair.first(out)
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
pub fn next_geom(
  stream: Iterator(Int),
  p: Float,
  m: Int,
) -> tuple(List(Int), Iterator(Int)) {
  // Take out 'm' integers from the stream of pseudo-random numbers and generate 
  // uniform random numbers.
  let out: tuple(List(Float), Iterator(Int)) = next_uniform(stream, 0., 1., m)
  // Transform the 'm' continuous uniform random numbers to geometric distributed 
  // random numbers
  let numbers: List(Int) =
    pair.first(out)
    |> list.map(fn(x) { float.round(float.floor(log(x) /. log(1. -. p))) })
  tuple(numbers, pair.second(out))
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generate random numbers from an exponential distribution with parameter 
/// 0 < 'lambda'.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import stats/generator
///     import stats/rand
///
///     pub fn example() {
///       let seed: Int = 5
///       let lambda: Float = 0.5
///       let out =
///         generators.seed_mt19937(seed)
///         |> rand.next_exp(lambda, 5_000)
///       let randexps: List(Float) = pair.first(out)
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
pub fn next_exp(
  stream: Iterator(Int),
  lambda: Float,
  m: Int,
) -> tuple(List(Float), Iterator(Int)) {
  // Take out 'm' integers from the stream of pseudo-random numbers and generate 
  // uniform random numbers.
  let out: tuple(List(Float), Iterator(Int)) = next_uniform(stream, 0., 1., m)
  // Transform the 'm' continuous uniform random numbers to exponential distributed
  // random numbers
  let numbers: List(Float) =
    pair.first(out)
    |> list.map(fn(x: Float) -> Float { 1. /. { -1. *. lambda } *. log(x) })
  tuple(numbers, pair.second(out))
}
