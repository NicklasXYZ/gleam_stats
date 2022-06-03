import gleam/int
import gleam/list
import gleam/pair
import gleam_stats/random
import gleam_stats/generators
import gleeunit
import gleeunit/should
import gleam/io

pub fn main() {
  gleeunit.main()
}

pub fn example_shuffle_test() {
  assert stream = generators.seed_pcg32(5, 1)
  let shuffled_list =
    random.shuffle([1, 10, 100], stream)
    |> pair.first()

  io.debug(shuffled_list)

  [[1, 10, 100], [10, 1, 100], [10, 100, 1], [100, 1, 10], [100, 10, 1]]
  |> list.any(fn(x) {
    io.debug(x)
    x == shuffled_list
  })
  |> should.be_true()
}
