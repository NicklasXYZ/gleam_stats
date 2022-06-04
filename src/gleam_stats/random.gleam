//// A module containing functions for generating and randomizing data.
////
//// ---
////
//// * **Lists**
////   * [`shuffle`](#shuffle)
//// * **Data**
////   * [`bools`](#bools)

import gleam/list
import gleam/iterator.{Iterator}
import gleam/io
import gleam/pair
import gleam_stats/distributions/bernoulli
import gleam_stats/distributions/uniform

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Given a list as input shuffle and return it (along with the generator
/// that was also passed as input).
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/generators
///     import gleam_stats/random
///
///     pub fn example() {
///       let stream = generators.seed_pcg32(5, 1)
///       let shuffled_list =
///       random.shuffle([1, 10, 100], stream)
///       |> pair.first()
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn shuffle(
  list: List(a),
  stream: Iterator(Int),
) -> #(List(a), Iterator(Int)) {
  let length = list.length(list)
  assert Ok(out) = bernoulli.bernoulli_random(stream, 0.5, length)
  let shuffled_list = do_shuffle(list, length, pair.first(out))
  #(shuffled_list, pair.second(out))
}

fn shuffle_list(a: List(a), b: List(a), c: List(Int)) -> List(a) {
  case a, b, c {
    [], _, _ -> b
    _, [], _ -> a
    [ax, ..ar], [bx, ..br], [cx, ..cr] -> {
      io.debug(cx)
      case cx {
        1 -> [ax, ..shuffle_list(ar, b, cr)]
        0 -> [bx, ..shuffle_list(a, br, cr)]
      }
    }
  }
}

fn do_shuffle(list: List(a), list_length: Int, c: List(Int)) -> List(a) {
  case list_length < 2 {
    True -> list
    False -> {
      let split_length = list_length / 2
      let a_list = list.take(list, split_length)
      let b_list = list.drop(list, split_length)
      let c1_list = list.take(c, split_length)
      let c2_list = list.drop(c, split_length)
      shuffle_list(
        do_shuffle(a_list, split_length, c1_list),
        do_shuffle(b_list, list_length - split_length, c2_list),
        c,
      )
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Generate a list of random boolean values where each outcome (True/False)
/// appears with 50% probability. 
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/generators
///     import gleam_stats/random
///
///     pub fn example() {
///       let stream = generators.seed_pcg32(5, 1)
///       let bools =
///       random.bools(10, stream)
///       |> pair.first()
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn bools(
  m: Int,
  stream: Iterator(Int),
) -> Result(#(List(Bool), Iterator(Int)), String) {
  case m > 0 {
    False ->
      "Invalid input arugment: m < 0. Valid input is m > 0."
      |> Error
    True -> {
      // Take out 'm' integers from the stream of pseudo-random numbers and generate 
      // uniform random numbers.
      assert Ok(out) = uniform.uniform_random(stream, 0., 1., m)
      let bools_list =
        pair.first(out)
        |> list.map(fn(x: Float) -> Bool {
          case x <=. 0.5 {
            True -> True
            False -> False
          }
        })
      #(bools_list, pair.second(out))
      |> Ok
    }
  }
}
