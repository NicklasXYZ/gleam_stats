// import gleam/should
import gleam/list
import gleam/float
import gleam/int
import gleam/io
import gleam/pair
import gleam_stats/generators
import gleam_stats/rand
import gleam_stats/stats
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn sum_test() {
  []
  |> stats.sum()
  |> should.equal(0.)

  let n: Int = 1000
  list.range(1, n + 1)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.sum()
  // Check correspondance with the nth partial sum of natural nmubers
  // that can be calculated analytically
  |> should.equal(int.to_float(n) *. { int.to_float(n) +. 1. } /. 2.)
}

pub fn mean_test() {
  []
  |> stats.mean()
  |> should.equal(Error(Nil))

  let n: Int = 1000
  list.range(1, n + 1)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.mean()
  // Check correspondance with the nth partial sum of natural nmubers
  // that can be calculated analytically
  |> should.equal(Ok({ int.to_float(n) +. 1. } /. 2.))
}

pub fn hmean_test() {
  []
  |> stats.hmean()
  |> should.equal(Error(Nil))

  [0]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.hmean()
  |> should.equal(Ok(0.))

  [1]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.hmean()
  |> should.equal(Ok(1.))

  [1, 3, 6]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.hmean()
  |> should.equal(Ok(2.))

  // List input
  // Check that wrong input is handled correctly:
  // - The harmonic mean is only defined for positive values >= 0
  [-1]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.hmean()
  |> should.equal(Error(Nil))
}

pub fn gmean_test() {
  []
  |> stats.gmean()
  |> should.equal(Error(Nil))

  [0]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.gmean()
  |> should.equal(Ok(0.))

  [1]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.gmean()
  |> should.equal(Ok(1.))

  [1, 3, 9]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.gmean()
  |> should.equal(Ok(3.))

  // List input
  // Check that wrong input is handled correctly:
  // - The geometric mean is only defined for positive values >= 0 
  [-1]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.gmean()
  |> should.equal(Error(Nil))
}

pub fn var_test() {
  []
  |> stats.var(1)
  |> should.equal(Error(Nil))

  // No variance
  let n: Int = 1000
  list.repeat(1., n)
  |> stats.var(1)
  |> should.equal(Ok(0.))

  // Non-zero variance
  list.range(1, n + 1)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.var(1)
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
  |> stats.moment(0)
  |> should.equal(Error(Nil))

  let n: Int = 1000
  list.range(1, n + 1)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.moment(0)
  |> should.equal(Ok(1.))

  list.range(1, n + 1)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.moment(1)
  |> should.equal(Ok(0.))

  [0, 1, 2, 3, 4]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.moment(2)
  |> should.equal(Ok(2.))

  [0, -1, -2, -3, -4]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.moment(2)
  |> should.equal(Ok(2.))
}

pub fn skewness_test() {
  []
  |> stats.skewness()
  |> should.equal(Error(Nil))

  // No skewness
  let n: Int = 1000
  list.range(1, n + 1)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.skewness()
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
  |> stats.skewness()
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
  |> stats.kurtosis()
  |> should.equal(Error(Nil))

  [1]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.kurtosis()
  |> should.equal(Ok(-3.))

  // No tail
  let n: Int = 1000
  list.range(1, n + 1)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.kurtosis()
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
  |> stats.kurtosis()
  |> fn(x: Result(Float, Nil)) -> Bool {
    case x {
      Ok(x) -> x <. 0.4898 && x >. 0.4896
      Error(Nil) -> False
    }
  }
  |> should.be_true()
}

pub fn zscore_test() {
  []
  // Use degrees of freedom = 1
  |> stats.zscore(1)
  |> should.equal(Error(Nil))

  let n: Int = 10
  let xarr: List(Float) = [
    -1.48630108, -1.15601195, -0.82572282, -0.49543369, -0.16514456, 0.16514456,
    0.49543369, 0.82572282, 1.15601195, 1.48630108,
  ]

  // Set relative and absolute tolerance
  // Within 1 percent of the reference value +/- 0.1
  let rtol0: Float = 0.01
  let atol0: Float = 0.10

  list.range(1, n + 1)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  // Use degrees of freedom = 1
  |> stats.zscore(1)
  |> fn(yarr: Result(List(Float), Nil)) -> Result(Bool, Nil) {
    case yarr {
      Ok(yarr) ->
        yarr
        |> stats.allclose(xarr, rtol0, atol0)
        |> fn(zarr: Result(List(Bool), Nil)) -> Result(Bool, Nil) {
          case zarr {
            Ok(zarr) ->
              zarr
              |> list.all(fn(a: Bool) -> Bool { a })
              |> Ok()
            _ -> Error(Nil)
          }
        }
      _ -> Error(Nil)
    }
  }
  |> should.equal(Ok(True))
}

pub fn percentile_test() {
  []
  |> stats.percentile(40)
  |> should.equal(Error(Nil))

  [15, 20, 35, 40, 50]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.percentile(40)
  |> should.equal(Ok(29.))
}

pub fn iqr_test() {
  []
  |> stats.iqr()
  |> should.equal(Error(Nil))

  [1., 2., 3., 4., 5., 6.]
  |> stats.iqr()
  |> should.equal(Ok(3.))

  [1., 2., 3., 4., 5.]
  |> stats.iqr()
  |> should.equal(Ok(3.))
}

pub fn freedman_diaconis_rule_test() {
  []
  |> stats.freedman_diaconis_rule()
  |> should.equal(Error(Nil))

  list.range(0, 1000)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.freedman_diaconis_rule()
  |> should.equal(Ok(10.))
}

pub fn histogram_test() {
  []
  |> stats.histogram(1.)
  |> should.equal(Error(Nil))

  list.range(0, 100)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.histogram(25.)
  |> should.equal(Ok([
    #(stats.Range(0., 25.), 25),
    #(stats.Range(25., 50.), 25),
    #(stats.Range(50., 75.), 25),
    #(stats.Range(75., 100.), 25),
  ]))
}

pub fn trim_test() {
  []
  |> stats.trim(0, 0)
  |> should.equal(Error(Nil))

  [1, 2, 3, 4, 5, 6]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.trim(1, 4)
  |> should.equal(Ok([2., 3., 4., 5.]))

  [1, 2, 3, 4, 5, 6]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.trim(5, 5)
  |> should.equal(Ok([6.]))

  // - Negative min index 
  [1, 2, 3, 4, 5, 6]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.trim(-1, 5)
  |> should.equal(Error(Nil))

  // - Too large max index 
  [1, 2, 3, 4, 5, 6]
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.trim(0, 6)
  |> should.equal(Error(Nil))
}

pub fn isclose_test() {
  let val: Float = 99.
  let ref_val: Float = 100.

  // Set relative and absolute tolerance
  // Within 1 percent of the reference value +/- 0.1
  let rtol0: Float = 0.01
  let atol0: Float = 0.10
  stats.isclose(val, ref_val, rtol0, atol0)
  |> should.be_true()

  // Set relative and absolute tolerance
  // Within 0.1 percent of the reference value +/- 0.0
  let rtol1: Float = 0.001
  let atol1: Float = 0.0
  stats.isclose(val, ref_val, rtol1, atol1)
  |> should.be_false()

  // Set relative and absolute tolerance
  // Within 0.1 percent of the reference value +/- 1.0
  let rtol2: Float = 0.001
  let atol2: Float = 1.0
  stats.isclose(val, ref_val, rtol2, atol2)
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
  stats.allclose(xarr, yarr, rtol0, atol0)
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
  stats.allclose(xarr, yarr, rtol0, atol0)
  |> should.equal(Error(Nil))

  // Set relative and absolute tolerance
  // Within 0.1 percent of the reference value +/- 0.0
  let rtol1: Float = 0.001
  let atol1: Float = 0.0
  let xarr: List(Float) = list.repeat(val, 42)
  let yarr: List(Float) = list.repeat(ref_val, 42)
  stats.allclose(xarr, yarr, rtol1, atol1)
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
  stats.allclose(xarr, yarr, rtol2, atol2)
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
  |> stats.amax()
  |> should.equal(Error(Nil))

  let min: Int = -100
  let max: Int = 100
  list.range(min, max + 1)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.amax()
  |> should.equal(Ok(int.to_float(max)))
}

pub fn amin_test() {
  []
  |> stats.amax()
  |> should.equal(Error(Nil))

  let min: Int = -100
  let max: Int = 100
  list.range(min, max + 1)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.amin()
  |> should.equal(Ok(int.to_float(min)))
}

pub fn argmax_test() {
  []
  |> stats.argmax()
  |> should.equal(Error(Nil))

  let min: Int = -100
  let max: Int = 100
  let mid: Int = { max - min } / 2
  list.range(min, max + 1)
  |> list.map(fn(x: Int) -> Float { -1. *. float.power(int.to_float(x), 2.) })
  |> stats.argmax()
  |> should.equal(Ok([mid]))
}

pub fn argmin_test() {
  []
  |> stats.argmin()
  |> should.equal(Error(Nil))

  let min: Int = -100
  let max: Int = 100
  let mid: Int = { max - min } / 2
  list.range(min, max + 1)
  |> list.map(fn(x: Int) -> Float { float.power(int.to_float(x), 2.) })
  |> stats.argmin()
  |> should.equal(Ok([mid]))
}
