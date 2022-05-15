// import gleam/should
import gleam/pair
import gleam/list
import gleam_stats/generators
import gleeunit
import gleeunit/should
import gleam/iterator.{Iterator}

pub fn main() {
  gleeunit.main()
}

pub fn seed_pcg32_test() {
  // Test that the raw output from the pseudo-random generator is
  // the same as the python reference implementation
  // seed: 5. The 1000 value should be 1464584865
  // seed: 5. The 2000 value should be 120012735
  // seed: 5. The 3000 value should be 586352375
  [#(1000, 1464584865), #(2000, 120012735), #(3000, 586352375)]
  |> check_pcg_randints(5, 1)
  // seed: 50. The 1000 value should be 1209697506
  // seed: 50. The 2000 value should be 2383515560
  // seed: 50. The 3000 value should be 2186603191
  [#(1000, 1209697506), #(2000, 2383515560), #(3000, 2186603191)]
  |> check_pcg_randints(50, 1)
  // seed: 500. The 1000 value should be 737957794
  // seed: 500. The 2000 value should be 684589838
  // seed: 500. The 3000 value should be 2702993137
  [#(1000, 737957794), #(2000, 684589838), #(3000, 2702993137)]
  |> check_pcg_randints(500, 1)
}

fn check_pcg_randints(tuples: List(#(Int, Int)), seed: Int, seq: Int) {
  tuples
  |> list.map(fn(x: #(Int, Int)) {
    let number = pair.first(x)
    let value = pair.second(x)
    assert Ok(out) =
      generators.seed_pcg32(seed, seq)
      |> generators.take_randints(number)
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
  [#(1000, 371509416), #(2000, 514604208), #(3000, 2513530296)]
  |> check_lcg_randints(5)

  // seed: 50. The 1000 value should be 2910717329
  // seed: 50. The 2000 value should be 2107902521
  // seed: 50. The 3000 value should be 2032832481
  [#(1000, 2910717329), #(2000, 2107902521), #(3000, 2032832481)]
  |> check_lcg_randints(50)

  // seed: 500. The 1000 value should be 2532992683
  // seed: 500. The 2000 value should be 861016467
  // seed: 500. The 3000 value should be 1520821627
  [#(1000, 2532992683), #(2000, 861016467), #(3000, 1520821627)]
  |> check_lcg_randints(500)
}

fn check_lcg_randints(tuples: List(#(Int, Int)), seed: Int) {
  tuples
  |> list.map(fn(x: #(Int, Int)) {
    let number = pair.first(x)
    let value = pair.second(x)
    assert Ok(out) =
      generators.seed_lcg32(seed)
      |> generators.take_randints(number)
    pair.first(out)
    |> list.drop(number - 1)
    |> should.equal([value])
  })
}

// Small examples ALSO used in the docs...
pub fn pcg32_generator_example_test() {
  let seed: Int = 5
  let seed_sequence: Int = 5
  assert Ok(out) =
    generators.seed_pcg32(seed, seed_sequence)
    |> generators.take_randints(5_000)
  let _randints: List(Int) = pair.first(out)
  let _stream: Iterator(Int) = pair.second(out)
}

pub fn lcg32_generator_example_test() {
  let seed: Int = 5
  assert Ok(out) =
    generators.seed_lcg32(seed)
    |> generators.take_randints(5_000)
  let _randints: List(Int) = pair.first(out)
  let _stream: Iterator(Int) = pair.second(out)
}
