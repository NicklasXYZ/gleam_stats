import gleam/should
import gleam/list
import gleam/float
import gleam/int
import stats/generators
import stats/rand
import stats/math

pub fn sum_test() {
  let n: Int = 1000
  list.range(1, n + 1)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.sum()
  // Check correspondance with the nth partial sum of natural nmubers
  // that can be calculated analytically
  |> should.equal(int.to_float(n) *. { int.to_float(n) +. 1. } /. 2.)
}

pub fn mean_test() {
  let n: Int = 1000
  list.range(1, n + 1)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.mean()
  // Check correspondance with the nth partial sum of natural nmubers
  // that can be calculated analytically
  |> should.equal({ int.to_float(n) +. 1. } /. 2.)
}

pub fn median_test() {
  // Test with an even number of elements in the list
  let n: Int = 1000
  // [1...1000]
  list.range(1, n + 1)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.median()
  |> should.equal(int.to_float(n / 2 + n / 2 + 1) /. 2.)
  // Test with an odd number of elements in the list
  let n: Int = 1001
  // [1...1001]
  list.range(1, n + 1)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.median()
  |> should.equal(int.to_float({ n + 1 } / 2))
}

pub fn var_test() {
  // No variance
  let n: Int = 1000
  list.repeat(1., n)
  |> math.var(1)
  |> should.equal(0.)
  // Non-zero variance
  list.range(1, n + 1)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.var(1)
  |> fn(x: Float) -> Bool {
    case x >. 0. {
      True -> True
      False -> False
    }
  }
  |> should.be_true()
}

pub fn isclose_test() {
  let val: Float = 99.
  let ref_val: Float = 100.
  // Set relative and absolute tolerance
  // Within 1 percent of the reference value +/- 0.1
  let rtol0: Float = 0.01
  let atol0: Float = 0.10
  math.isclose(val, ref_val, rtol0, atol0)
  |> should.be_true()
  // Set relative and absolute tolerance
  // Within 0.1 percent of the reference value +/- 0.0
  let rtol1: Float = 0.001
  let atol1: Float = 0.0
  math.isclose(val, ref_val, rtol1, atol1)
  |> should.be_false()
  // Set relative and absolute tolerance
  // Within 0.1 percent of the reference value +/- 1.0
  let rtol2: Float = 0.001
  let atol2: Float = 1.0
  math.isclose(val, ref_val, rtol2, atol2)
  |> should.be_true()
}

pub fn allclose_test() {
  let val: Float = 99.
  let ref_val: Float = 100.
  // Set relative and absolute tolerance
  // Within 1 percent of the reference value +/- 0.1
  let rtol0: Float = 0.01
  let atol0: Float = 0.10
  list.repeat(val, 42)
  |> math.allclose(ref_val, rtol0, atol0)
  |> list.all(fn(x) { x })
  |> should.be_true()
  // Set relative and absolute tolerance
  // Within 0.1 percent of the reference value +/- 0.0
  let rtol1: Float = 0.001
  let atol1: Float = 0.0
  list.repeat(val, 42)
  |> math.allclose(ref_val, rtol1, atol1)
  |> list.all(fn(x) { x })
  |> should.be_false()
  // Set relative and absolute tolerance
  // Within 0.1 percent of the reference value +/- 1.0
  let rtol2: Float = 0.001
  let atol2: Float = 1.0
  list.repeat(val, 42)
  |> math.allclose(ref_val, rtol2, atol2)
  |> list.all(fn(x) { x })
  |> should.be_true()
}
