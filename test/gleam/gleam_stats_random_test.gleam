import gleam/int
import gleam/list
import gleam/pair
import gleam_stats/random
import gleam_stats/generators
import gleam_stats/stats
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// The relative tolerance
const rtol: Float = 0.025

// The absolute tolerance
const atol: Float = 0.025

// Number of random numbers to generate 
const n: Int = 25_000

pub fn example_shuffle_test() {
  let stream = generators.seed_pcg32(5, 1)
  let shuffled_list =
    random.shuffle([1, 10, 100], stream)
    |> pair.first()

  [[1, 10, 100], [10, 1, 100], [10, 100, 1], [100, 1, 10], [100, 10, 1]]
  |> list.any(fn(x) { x == shuffled_list })
  |> should.be_true()
}

pub fn example_bools_test() {
  let stream = generators.seed_pcg32(5, 1)
  assert Ok(out) = random.bools(n, stream)
  let bools: List(Bool) = pair.first(out)
  let count: Int =
    bools
    |> list.fold(
      0,
      fn(acc: Int, a: Bool) -> Int {
        case a {
          True -> acc + 1
          False -> acc
        }
      },
    )
  int.to_float(count) /. int.to_float(list.length(bools))
  |> stats.isclose(0.5, rtol, atol)
}
