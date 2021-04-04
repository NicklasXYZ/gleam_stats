import gleam/should
import gleam/pair
import gleam/list
import stats/generators
import stats/rand
import stats/math

pub fn seed_mt19937_test() {
  // Test that the raw output from the pseudo-random generator is
  // the same as the python reference implementation
  // seed: 5. The 1000 value should be 3298208229
  // seed: 5. The 2000 value should be 4018181980
  // seed: 5. The 3000 value should be 824242727
  [tuple(1000, 3298208229), tuple(2000, 4018181980), tuple(3000, 824242727)]
  |> check_mt1993_randints(5)
  // seed: 50. The 1000 value should be 46010871
  // seed: 50. The 2000 value should be 1714137953
  // seed: 50. The 3000 value should be 2335134544
  [tuple(1000, 46010871), tuple(2000, 1714137953), tuple(3000, 2335134544)]
  |> check_mt1993_randints(50)
  // // seed: 500. The 1000 value should be 1964173339
  // // seed: 500. The 2000 value should be 3506793793
  // // seed: 500. The 3000 value should be 2009758904
  [tuple(1000, 1964173339), tuple(2000, 3506793793), tuple(3000, 2009758904)]
  |> check_mt1993_randints(500)
}

fn check_mt1993_randints(tuples: List(tuple(Int, Int)), seed: Int) {
  tuples
  |> list.map(fn(x: tuple(Int, Int)) {
    let number = pair.first(x)
    let value = pair.second(x)
    let out =
      generators.seed_mt19937(seed)
      |> rand.take_randints(number)
    pair.first(out)
    |> list.drop(number - 1)
    |> should.equal([value])
  })
}
