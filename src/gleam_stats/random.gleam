//// A module containing functions for generating and randomizing data.
////
//// ---
////
//// * **Lists**
////   * [`shuffle`](#shuffle)

import gleam/list
import gleam/int
import gleam/float
import gleam/iterator.{Iterator}
import gleam/io
import gleam/pair
import gleam_stats/distributions/bernoulli

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_stats/generators
///     import gleam_stats/random
///
///     pub fn example() {
///       assert stream = generators.seed_pcg32(5, 1)
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
