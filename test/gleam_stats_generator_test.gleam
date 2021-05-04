import gleam/should
import gleam/pair
import gleam/list
import gleam_stats/generators
import gleam_stats/rand
import gleam_stats/math

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
  // seed: 500. The 1000 value should be 1964173339
  // seed: 500. The 2000 value should be 3506793793
  // seed: 500. The 3000 value should be 2009758904
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

pub fn seed_pcg32_test() {
  // Test that the raw output from the pseudo-random generator is
  // the same as the python reference implementation
  // seed: 5. The 1000 value should be 1464584865
  // seed: 5. The 2000 value should be 120012735
  // seed: 5. The 3000 value should be 586352375
  [tuple(1000, 1464584865), tuple(2000, 120012735), tuple(3000, 586352375)]
  |> check_pcg_randints(5, 1)

  // seed: 50. The 1000 value should be 1209697506
  // seed: 50. The 2000 value should be 2383515560
  // seed: 50. The 3000 value should be 2186603191
  [tuple(1000, 1209697506), tuple(2000, 2383515560), tuple(3000, 2186603191)]
  |> check_pcg_randints(50, 1)

  // seed: 500. The 1000 value should be 737957794
  // seed: 500. The 2000 value should be 684589838
  // seed: 500. The 3000 value should be 2702993137
  [tuple(1000, 737957794), tuple(2000, 684589838), tuple(3000, 2702993137)]
  |> check_pcg_randints(500, 1)
}

fn check_pcg_randints(tuples: List(tuple(Int, Int)), seed: Int, seq: Int) {
  tuples
  |> list.map(fn(x: tuple(Int, Int)) {
    let number = pair.first(x)
    let value = pair.second(x)
    let out =
      generators.seed_pcg32(seed, seq)
      |> rand.take_randints(number)
    pair.first(out)
    |> list.drop(number - 1)
    |> should.equal([value])
  })
}

pub fn seed_lcg32_test() {
  // Test that the raw output from the pseudo-random generator is
  // the same as the python reference implementation
  // seed: 5. The 1000 value should be 371509416
  // seed: 5. The 2000 value should be 514604208
  // seed: 5. The 3000 value should be 2513530296
  [tuple(1000, 371509416), tuple(2000, 514604208), tuple(3000, 2513530296)]
  |> check_lcg_randints(5)

  // seed: 50. The 1000 value should be 2910717329
  // seed: 50. The 2000 value should be 2107902521
  // seed: 50. The 3000 value should be 2032832481
  [tuple(1000, 2910717329), tuple(2000, 2107902521), tuple(3000, 2032832481)]
  |> check_lcg_randints(50)

  // seed: 500. The 1000 value should be 2532992683
  // seed: 500. The 2000 value should be 861016467
  // seed: 500. The 3000 value should be 1520821627
  [tuple(1000, 2532992683), tuple(2000, 861016467), tuple(3000, 1520821627)]
  |> check_lcg_randints(500)
}

fn check_lcg_randints(tuples: List(tuple(Int, Int)), seed: Int) {
  tuples
  |> list.map(fn(x: tuple(Int, Int)) {
    let number = pair.first(x)
    let value = pair.second(x)
    let out =
      generators.seed_lcg32(seed)
      |> rand.take_randints(number)
    pair.first(out)
    |> list.drop(number - 1)
    |> should.equal([value])
  })
}
