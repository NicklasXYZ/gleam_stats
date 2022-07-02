//// A module with all the necessary base-functions for generating random numbers. 
//// Some of the functions listed in the following are able to create a base-iterator that 
//// yields pseudo-random integer numbers. A base-iterator can then used with other functions
//// to generate random numbers from common distributions. See the `gleam_stats/distributions`
//// modules for this functionality. 
////
//// ---
////
//// * **Bit masking-related functions & constants**
////   * [`mask_32`](#mask_32)
////   * [`mask_64`](#mask_64)
////   * [`uint32`](#uint32)
////   * [`uint64`](#uint64)
//// * **Random number base-iterator functions**
////   * [`seed_pcg32`](#seed_pcg32)
////   * [`seed_lcg32`](#seed_lcg32)
//// * **Miscellaneous functions**
////   * [`take_randints`](#take_randints)

import gleam/iterator

if erlang {
  import gleam/bitwise
  import gleam/pair
}

///
/// The value is equal to `float.round(float.power(2., 32.)) - 1`,
/// which is the maximum value for a 32-bit unsigned integer.
///
pub const mask_32: Int = 4294967295

///
/// The value is equal to `float.round(float.power(2., 64.)) - 1`,
/// which is the maximum value for a 64-bit unsigned integer.
///
pub const mask_64: Int = 18446744073709551615

if erlang {
  // A type used to encapsulate all parameters used by the Permuted Congruential 
  // Generator (PCG32).
  type PermutedCongruentialGenerator {
    PermutedCongruentialGenerator(
      int_1: Int,
      int_18: Int,
      int_27: Int,
      int_59: Int,
      int_31: Int,
      multiplier: Int,
    )
  }

  // A constant containing the defualt PCG32 parameters
  const pcg32 = PermutedCongruentialGenerator(
    int_1: 1,
    int_18: 18,
    int_27: 27,
    int_59: 59,
    int_31: 31,
    multiplier: 6364136223846793005,
  )

  type StatePCG =
    #(Int, Int)

  fn pcg32_next_rn(state: StatePCG, pcg: PermutedCongruentialGenerator) -> Int {
    let old_state: Int = pair.first(state)
    let xorshifted: Int =
      uint32(bitwise.shift_right(
        bitwise.exclusive_or(
          bitwise.shift_right(old_state, pcg.int_18),
          old_state,
        ),
        pcg.int_27,
      ))
    let rotation: Int = uint32(bitwise.shift_right(old_state, pcg.int_59))
    uint32(bitwise.or(
      bitwise.shift_right(xorshifted, rotation),
      bitwise.shift_left(xorshifted, bitwise.and(-1 * rotation, pcg.int_31)),
    ))
  }

  fn pcg32_init(
    seed: Int,
    seq: Int,
    pcg: PermutedCongruentialGenerator,
  ) -> StatePCG {
    // Assume that the input seed and seq are unsigned 64-bit integers
    // Keep PRNG state as a (state, increment) tuple
    #(0, bitwise.or(uint64(bitwise.shift_left(seq, pcg32.int_1)), pcg.int_1))
    |> pcg32_next_state(pcg)
    |> fn(state: StatePCG) -> StatePCG {
      let #(s, i) = state
      #(uint64(s + seed), i)
    }
    |> pcg32_next_state(pcg)
  }

  fn pcg32_next_state(
    state: StatePCG,
    pcg: PermutedCongruentialGenerator,
  ) -> StatePCG {
    let #(s, i) = state
    #(uint64(s * pcg.multiplier + i), i)
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// The function creates a base-iterator that uses the Permuted Congruential Generator (PCG32)
/// algorithm to generate random integer numbers. The PCG32 algorithm is a generator of 32-bit 
/// random numbers and uses two 64-bit integer seeds (internal initial state and sequence/stream
/// number). The PCG32 algorithm is fast, space effecient, and have good statitical properties. 
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import gleam_stats/generators
///
///     pub fn example () {
///       // 'seed' is the "current state" of the generator
///       // It can be any 64-bit value.
///       let seed: Int = 5
///       // 'seed_sequence' defines which of the 2^63 possible
///       // random sequences the current state is iterating through
///       // It can be any 64-bit value.
///       let seed_sequence: Int = 5
///       let stream: Iterator(Int) = generators.seed_pcg32(seed, seed_sequence)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn seed_pcg32(seed: Int, seq: Int) -> iterator.Iterator(Int) {
  do_seed_pcg32(seed, seq)
}

if erlang {
  fn do_seed_pcg32(seed: Int, seq: Int) -> iterator.Iterator(Int) {
    // fn do_seed_pcg32(seed: Int, seq: Int) -> Int {
    let pcg: PermutedCongruentialGenerator = pcg32
    pcg32_init(uint64(seed), uint64(seq), pcg)
    |> iterator.unfold(fn(state: StatePCG) -> iterator.Step(Int, StatePCG) {
      let next_rn: Int = pcg32_next_rn(state, pcg)
      let next_state: StatePCG = pcg32_next_state(state, pcg)
      iterator.Next(element: next_rn, accumulator: next_state)
    })
  }
}

if javascript {
  external fn do_seed_pcg32(Int, Int) -> iterator.Iterator(Int) =
    "../generators.mjs" "seed_pcg32"
}

// external fn do_seed_pcg32(Int, Int) -> Int =

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
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
///     import gleam_stats/generators
///
///     pub fn example() {
///       let seed: Int = 5
///       let seed_sequence: Int = 5
///       assert Ok(out) =
///         generators.seed_pcg32(seed, seed_sequence)
///         |> generators.take_randints(5_000)
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
  stream: iterator.Iterator(Int),
  m: Int,
) -> Result(#(List(Int), iterator.Iterator(Int)), String) {
  do_take_randints(stream, m)
}

if erlang {
  fn do_take_randints(
    stream: iterator.Iterator(Int),
    m: Int,
  ) -> Result(#(List(Int), iterator.Iterator(Int)), String) {
    case m > 0 {
      False -> Error("Invalid input arugment: m < 0. Valid input is m > 0.")
      True -> {
        // Take out 'm' integers from the stream of pseudo-random numbers.
        let numbers: List(Int) =
          stream
          |> iterator.take(m)
          |> iterator.to_list
        #(numbers, iterator.drop(stream, m))
        |> Ok
      }
    }
  }
}

if javascript {
  external fn do_take_randints(
    iterator.Iterator(Int),
    Int,
  ) -> Result(#(List(Int), iterator.Iterator(Int)), String) =
    "../generators.mjs" "take_randints"
}

if erlang {
  // A type used to encapsulate all parameters used by the Linear 
  // Congruential Generator (LCG32).
  type LinearCongruentialGenerator {
    LinearCongruentialGenerator(a: Int, c: Int)
  }

  // A constant containing the defualt LCG32 parameters taken from "Numerical Recipes"
  // by William H. Press, Saul A. Teukolsky, William T. Vetterling and Brian P. Flannery.
  const lcg32 = LinearCongruentialGenerator(a: 1664525, c: 1013904223)

  type StateLCG =
    Int

  fn lcg32_init(seed: Int, lcg: LinearCongruentialGenerator) -> StateLCG {
    // Keep PRNG state as a single int
    bitwise.and(lcg.a * seed + lcg.c, mask_32)
  }

  fn lcg32_next_state(
    state: StateLCG,
    lcg: LinearCongruentialGenerator,
  ) -> StateLCG {
    bitwise.and(lcg.a * state + lcg.c, mask_32)
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Create a base-iterator that uses the Linear Congruential Generator (LCG32)
/// algorithm to generate random numbers. The LCG32 algorithm is a generator of
/// 32-bit random numbers and uses a 32-bit integer seed.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/iterator.{Iterator}
///     import gleam_stats/generators
///
///     pub fn example () {
///       let seed: Int = 5
///       let stream: Iterator(Int) = generators.seed_lcg32(seed)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn seed_lcg32(seed: Int) -> iterator.Iterator(Int) {
  do_seed_lcg32(seed)
}

if javascript {
  external fn do_seed_lcg32(Int) -> iterator.Iterator(Int) =
    "../generators.mjs" "seed_lcg32"
}

if erlang {
  fn do_seed_lcg32(seed: Int) -> iterator.Iterator(Int) {
    let lcg: LinearCongruentialGenerator = lcg32
    lcg32_init(seed, lcg)
    |> iterator.unfold(fn(state: StateLCG) -> iterator.Step(Int, StateLCG) {
      let next_state: StateLCG = lcg32_next_state(state, lcg)
      iterator.Next(element: next_state, accumulator: next_state)
    })
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Gleam does not have 32 bit unsigned integers (integers are arbitrary-sized)
/// so use an explicit bit mask to convert integers to 32 bit integers.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn uint32(n: Int) -> Int {
  do_uint32(n)
}

if erlang {
  fn do_uint32(n: Int) -> Int {
    bitwise.and(n, mask_32)
  }
}

if javascript {
  external fn do_uint32(Int) -> Int =
    "../generators.mjs" "uint32"
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Gleam does not have 64 bit unsigned integers (integers are arbitrary-sized)
/// so use an explicit bit mask to convert integers to 64 bit integers.
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn uint64(n: Int) -> Int {
  do_uint64(n)
}

if erlang {
  fn do_uint64(n: Int) -> Int {
    bitwise.and(n, mask_64)
  }
}

if javascript {
  external fn do_uint64(Int) -> Int =
    "../generators.mjs" "uint64"
}
