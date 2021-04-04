//// A module for generating random numbers.
////
//// ---
////
//// * **Creating a random number base-generator**
////   * [`seed_mt19937`](#seed_mt19937)

import gleam/bitwise
import gleam/list
import gleam/iterator.{Iterator, Next}
import gleam/pair
import gleam/float

pub fn mask_32() -> Int {
  float.round(float.power(2., 32.)) - 1
}

pub fn mask_64() -> Int {
  float.round(float.power(2., 64.)) - 1
}

/// A type used to encapsulate all parameters used by the Mersenne Twister
/// (MT19937) Pseudo-Random Number Generator (PRNG) algorithm.
type MersenneTwister {
  MersenneTwister(
    // The word size in number of bits
    w: Int,
    // The degree of the recurrence relation
    n: Int,
    // The offset used in the recurrence relation. 1 ≤ m ≤ n
    m: Int,
    // The number of bits of the lower bitmask. 0 ≤ r ≤ w - 1
    r: Int,
    // The coefficients of the rational normal form twist matrix
    a: Int,
    // Mersenne Twister tempering bit shifts/masks
    u: Int,
    d: Int,
    l: Int,
    // Tempering bitmasks
    b: Int,
    c: Int,
    // Tempering bitshifts
    s: Int,
    t: Int,
    // A initialization parameter
    f: Int,
  )
}

// A constant containing the defualt MT19937 parameters
const mt19937 = MersenneTwister(
  w: 32,
  n: 624,
  m: 397,
  r: 31,
  a: 0x9908B0DF,
  u: 11,
  d: 0xFFFFFFFF,
  l: 18,
  b: 0x9D2C5680,
  c: 0xEFC60000,
  s: 7,
  t: 15,
  f: 1812433253,
)

// MT19937 helper function
fn lowest_bits(x: Int, mt: MersenneTwister) -> Int {
  bitwise.and(x, bitwise.shift_left(1, mt.w) - 1)
}

// MT19937 helper function
fn lower_bitmask(mt: MersenneTwister) -> Int {
  bitwise.shift_left(1, mt.r) - 1
}

// MT19937 helper function
fn upper_bitmask(mt: MersenneTwister) -> Int {
  lowest_bits(bitwise.not(lower_bitmask(mt)), mt)
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Use the MT19937 PRNG algorithm to create a base-iterator that yields
/// pseudo-random numbers. This base-iterator can then be used with other
/// methods to generate random numbers from common distributions.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import gstats/generators
///
///     pub fn example () {
///         let seed: Int = 5
///         let stream: Iterator(Int) = generators.seed_mt19937(seed)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn seed_mt19937(seed: Int) -> Iterator(Int) {
  let mt: MersenneTwister = mt19937
  list.range(1, mt.n)
  |> iterator.from_list
  |> iterator.scan(
    seed,
    fn(x: Int, acc: Int) -> Int {
      lowest_bits(
        mt.f * bitwise.exclusive_or(acc, bitwise.shift_right(acc, mt.w - 2)) + x,
        mt,
      )
    },
  )
  |> iterator.to_list
  |> fn(arr) { [seed, ..arr] }
  |> rng(mt)
}

// Copy the random numbers in the given list over into a new list.
// Replace the random number at the given index with the new random number.
fn replace_rn(rn: Int, arr: List(Int), index: Int) -> List(Int) {
  arr
  |> list.index_map(fn(i: Int, x: Int) -> Int {
    case i == index {
      True -> rn
      False -> x
    }
  })
}

// Compute the next random number.
fn next_rn(arr: List(Int), index: Int, mt: MersenneTwister) -> Int {
  let v0: Result(Int, Nil) = list.at(arr, index)
  let v1: Result(Int, Nil) = list.at(arr, { index + 1 } % mt.n)
  let v2: Result(Int, Nil) = list.at(arr, { index + mt.m } % mt.n)
  case v0, v1, v2 {
    Ok(v0), Ok(v1), Ok(v2) -> {
      let v3: Int =
        bitwise.and(v0, upper_bitmask(mt)) + bitwise.and(v1, lower_bitmask(mt))
      let v4: Int = bitwise.shift_right(v3, 1)
      case v3 % 2 == 0 {
        True -> bitwise.exclusive_or(v2, v4)
        False -> bitwise.exclusive_or(v2, bitwise.exclusive_or(v4, mt.a))
      }
    }
  }
}

fn do_twist(arr: List(Int), index: Int, mt: MersenneTwister) -> List(Int) {
  case index >= mt.n {
    True -> arr
    False ->
      next_rn(arr, index, mt)
      |> replace_rn(arr, index)
      |> do_twist(index + 1, mt)
  }
}

// Apply the twist transformation that generates the next batch of random numbers
fn twist(arr, mt: MersenneTwister) -> List(Int) {
  do_twist(arr, 0, mt)
}

// The tempering function that returns a new random number for a given state
fn shout(x: Int, mt: MersenneTwister) -> Int {
  x
  |> fn(y: Int) -> Int {
    bitwise.exclusive_or(y, bitwise.and(bitwise.shift_right(y, mt.u), mt.d))
  }
  |> fn(y: Int) -> Int {
    bitwise.exclusive_or(y, bitwise.and(bitwise.shift_left(y, mt.s), mt.b))
  }
  |> fn(y: Int) -> Int {
    bitwise.exclusive_or(y, bitwise.and(bitwise.shift_left(y, mt.t), mt.c))
  }
  |> fn(y: Int) -> Int { bitwise.exclusive_or(y, bitwise.shift_right(y, mt.l)) }
  |> fn(y: Int) -> Int { lowest_bits(y, mt) }
}

// Given the current state compute the next state and thus the next 
// bacth of random numbers
fn next_state(state: tuple(Int, List(Int)), mt: MersenneTwister) {
  let index: Int = pair.first(state)
  let arr: List(Int) = pair.second(state)
  case list.at(arr, index) {
    Ok(x) -> Next(element: shout(x, mt), accumulator: tuple(index + 1, arr))
  }
}

// Create an iterator that yields pseudo-random numbers
fn rng(arr: List(Int), mt: MersenneTwister) -> Iterator(Int) {
  iterator.unfold(
    tuple(mt.n, arr),
    fn(state: tuple(Int, List(Int))) {
      case pair.first(state) == mt.n {
        True ->
          tuple(
            0,
            pair.second(state)
            |> twist(mt),
          )
          |> next_state(mt)
        False ->
          state
          |> next_state(mt)
      }
    },
  )
}
