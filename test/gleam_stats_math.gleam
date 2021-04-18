import gleam/should
import gleam/list
import gleam/float
import gleam/int
import gleam_stats/generators
import gleam_stats/rand
import gleam_stats/math
import gleam/io

pub fn sum_test() {
  []
  |> math.sum()
  |> should.equal(0.)

  let n: Int = 1000
  list.range(1, n + 1)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.sum()
  // Check correspondance with the nth partial sum of natural nmubers
  // that can be calculated analytically
  |> should.equal(int.to_float(n) *. { int.to_float(n) +. 1. } /. 2.)
}

pub fn mean_test() {
  []
  |> math.mean()
  |> should.equal(Error(Nil))

  let n: Int = 1000
  list.range(1, n + 1)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.mean()
  // Check correspondance with the nth partial sum of natural nmubers
  // that can be calculated analytically
  |> should.equal(Ok({ int.to_float(n) +. 1. } /. 2.))
}

pub fn hmean_test() {
  []
  |> math.hmean()
  |> should.equal(Error(Nil))

  [0]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.hmean()
  |> should.equal(Ok(0.))

  [1]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.hmean()
  |> should.equal(Ok(1.))

  [1, 3, 6]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.hmean()
  |> should.equal(Ok(2.))

  // List input
  // Check that wrong input is handled correctly:
  // - The harmonic mean is only defined for positive values >= 0
  [-1]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.hmean()
  |> should.equal(Error(Nil))
}

pub fn gmean_test() {
  []
  |> math.gmean()
  |> should.equal(Error(Nil))

  [0]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.gmean()
  |> should.equal(Ok(0.))

  [1]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.gmean()
  |> should.equal(Ok(1.))

  [1, 3, 9]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.gmean()
  |> should.equal(Ok(3.))

  // List input
  // Check that wrong input is handled correctly:
  // - The geometric mean is only defined for positive values >= 0 
  [-1]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.gmean()
  |> should.equal(Error(Nil))
}

pub fn var_test() {
  []
  |> math.var(1)
  |> should.equal(Error(Nil))

  // No variance
  let n: Int = 1000
  list.repeat(1., n)
  |> math.var(1)
  |> should.equal(Ok(0.))

  // Non-zero variance
  list.range(1, n + 1)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.var(1)
  |> fn(x: Result(Float, Nil)) -> Bool {
    case x {
      Ok(x) -> x >. 0.
      Error(Nil) -> False
    }
  }
  |> should.be_true()
}

pub fn moment_test() {
  []
  |> math.moment(0)
  |> should.equal(Error(Nil))

  let n: Int = 1000
  list.range(1, n + 1)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.moment(0)
  |> should.equal(Ok(1.))

  list.range(1, n + 1)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.moment(1)
  |> should.equal(Ok(0.))

  [0, 1, 2, 3, 4]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.moment(2)
  |> should.equal(Ok(2.))

  [0, -1, -2, -3, -4]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.moment(2)
  |> should.equal(Ok(2.))
}

pub fn skewness_test() {
  []
  |> math.skewness()
  |> should.equal(Error(Nil))

  // No skewness
  let n: Int = 1000
  list.range(1, n + 1)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.skewness()
  |> should.equal(Ok(0.))

  // Skew distribution
  let a: List(Float) = list.repeat(1., 10)
  let b: List(Float) = list.repeat(2., 5)
  let c: List(Float) = list.repeat(3., 2)
  let d: List(Float) = list.repeat(4., 1)
  a
  |> list.append(b)
  |> list.append(c)
  |> list.append(d)
  |> math.skewness()
  |> fn(x: Result(Float, Nil)) -> Bool {
    case x {
      Ok(x) -> x <. 1.1879 && x >. 1.1877
      Error(Nil) -> False
    }
  }
  |> should.be_true()
}

pub fn kurtosis_test() {
  []
  |> math.kurtosis()
  |> should.equal(Error(Nil))

  [1]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.kurtosis()
  |> should.equal(Ok(-3.))

  // No tail
  let n: Int = 1000
  list.range(1, n + 1)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.kurtosis()
  |> fn(x: Result(Float, Nil)) -> Bool {
    case x {
      Ok(x) -> x <. -1.2000 && x >. -1.2001
      Error(Nil) -> False
    }
  }
  |> should.be_true()

  // Distribution with a tail
  let a: List(Float) = list.repeat(1., 10)
  let b: List(Float) = list.repeat(2., 5)
  let c: List(Float) = list.repeat(3., 2)
  let d: List(Float) = list.repeat(4., 1)
  a
  |> list.append(b)
  |> list.append(c)
  |> list.append(d)
  |> math.kurtosis()
  |> fn(x: Result(Float, Nil)) -> Bool {
    case x {
      Ok(x) -> x <. 0.4898 && x >. 0.4896
      Error(Nil) -> False
    }
  }
  |> should.be_true()
}

pub fn percentile_test() {
  []
  |> math.percentile(40)
  |> should.equal(Error(Nil))

  [15, 20, 35, 40, 50]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.percentile(40)
  |> should.equal(Ok(29.))
}

pub fn iqr_test() {
  []
  |> math.iqr()
  |> should.equal(Error(Nil))

  [1., 2., 3., 4., 5., 6.]
  |> math.iqr()
  |> should.equal(Ok(3.))

  [1., 2., 3., 4., 5.]
  |> math.iqr()
  |> should.equal(Ok(3.))
}

pub fn freedman_diaconis_rule_test() {
  []
  |> math.freedman_diaconis_rule()
  |> should.equal(Error(Nil))

  list.range(0, 1000)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.freedman_diaconis_rule()
  |> should.equal(Ok(10.))
}

pub fn histogram_test() {
  []
  |> math.histogram(1.)
  |> should.equal(Error(Nil))

  list.range(0, 100)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.histogram(25.)
  |> should.equal(Ok([
    tuple(math.Range(0., 25.), 25),
    tuple(math.Range(25., 50.), 25),
    tuple(math.Range(50., 75.), 25),
    tuple(math.Range(75., 100.), 25),
  ]))
}

pub fn trim_test() {
  []
  |> math.trim(0, 0)
  |> should.equal(Error(Nil))

  [1, 2, 3, 4, 5, 6]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.trim(1, 4)
  |> should.equal(Ok([2., 3., 4., 5.]))

  [1, 2, 3, 4, 5, 6]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.trim(5, 5)
  |> should.equal(Ok([6.]))

  // - Negative min index 
  [1, 2, 3, 4, 5, 6]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.trim(-1, 5)
  |> should.equal(Error(Nil))

  // - Too large max index 
  [1, 2, 3, 4, 5, 6]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.trim(0, 6)
  |> should.equal(Error(Nil))
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
  let xarr: List(Float) = list.repeat(val, 42)
  let yarr: List(Float) = list.repeat(ref_val, 42)
  math.allclose(xarr, yarr, rtol0, atol0)
  |> fn(zarr: Result(List(Bool), Nil)) -> Result(Bool, Nil) {
    case zarr {
      Ok(arr) ->
        arr
        |> list.all(fn(a: Bool) -> Bool { a })
        |> Ok()
      _ -> Error(Nil)
    }
  }
  |> should.equal(Ok(True))

  // Check that length mismatched lists are handled correctly
  let xarr: List(Float) = list.repeat(val, 41)
  let yarr: List(Float) = list.repeat(ref_val, 42)
  math.allclose(xarr, yarr, rtol0, atol0)
  |> should.equal(Error(Nil))

  // Set relative and absolute tolerance
  // Within 0.1 percent of the reference value +/- 0.0
  let rtol1: Float = 0.001
  let atol1: Float = 0.0
  let xarr: List(Float) = list.repeat(val, 42)
  let yarr: List(Float) = list.repeat(ref_val, 42)
  math.allclose(xarr, yarr, rtol1, atol1)
  |> fn(zarr: Result(List(Bool), Nil)) -> Result(Bool, Nil) {
    case zarr {
      Ok(arr) ->
        arr
        |> list.all(fn(a: Bool) -> Bool { a })
        |> Ok()
      _ -> Error(Nil)
    }
  }
  |> should.equal(Ok(False))

  // Set relative and absolute tolerance
  // Within 0.1 percent of the reference value +/- 1.0
  let rtol2: Float = 0.001
  let atol2: Float = 1.0
  let xarr: List(Float) = list.repeat(val, 42)
  let yarr: List(Float) = list.repeat(ref_val, 42)
  math.allclose(xarr, yarr, rtol2, atol2)
  |> fn(zarr: Result(List(Bool), Nil)) -> Result(Bool, Nil) {
    case zarr {
      Ok(arr) ->
        arr
        |> list.all(fn(a: Bool) -> Bool { a })
        |> Ok()
      _ -> Error(Nil)
    }
  }
  |> should.equal(Ok(True))
}

pub fn amax_test() {
  []
  |> math.amax()
  |> should.equal(Error(Nil))

  let min: Int = -100
  let max: Int = 100
  list.range(min, max + 1)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.amax()
  |> should.equal(Ok(int.to_float(max)))
}

pub fn amin_test() {
  []
  |> math.amax()
  |> should.equal(Error(Nil))

  let min: Int = -100
  let max: Int = 100
  list.range(min, max + 1)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.amin()
  |> should.equal(Ok(int.to_float(min)))
}

pub fn argmax_test() {
  []
  |> math.argmax()
  |> should.equal(Error(Nil))

  let min: Int = -100
  let max: Int = 100
  let mid: Int = { max - min } / 2
  list.range(min, max + 1)
  |> list.map(fn(x: Int) -> Float { -1. *. float.power(int.to_float(x), 2.) })
  |> math.argmax()
  |> should.equal(Ok([mid]))
}

pub fn argmin_test() {
  []
  |> math.argmin()
  |> should.equal(Error(Nil))

  let min: Int = -100
  let max: Int = 100
  let mid: Int = { max - min } / 2
  list.range(min, max + 1)
  |> list.map(fn(x: Int) -> Float { float.power(int.to_float(x), 2.) })
  |> math.argmin()
  |> should.equal(Ok([mid]))
}
