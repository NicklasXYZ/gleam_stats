//// Small examples used in the docs...

import gleam/should
import gleam/int
import gleam/list
import gleam/pair
import stats/math

pub fn example_sum_test() {
  []
  |> math.sum()
  |> should.equal(0.)

  [1., 2., 3.]
  |> math.sum()
  |> should.equal(6.)
}

pub fn example_mean_test() {
  []
  |> math.mean()
  |> should.equal(Error(Nil))

  [1., 2., 3.]
  |> math.mean()
  |> should.equal(Ok(2.))
}

pub fn example_median_test() {
  []
  |> math.median()
  |> should.equal(Error(Nil))

  [1., 2., 3.]
  |> math.median()
  |> should.equal(Ok(2.))

  [1., 2., 3., 4.]
  |> math.median()
  |> should.equal(Ok(2.5))
}

pub fn example_hmean_test() {
  []
  |> math.hmean()
  |> should.equal(Error(Nil))

  [1., 3., 6.]
  |> math.hmean()
  |> should.equal(Ok(2.))
}

pub fn example_gmean_test() {
  []
  |> math.gmean()
  |> should.equal(Error(Nil))

  [1., 3., 9.]
  |> math.gmean()
  |> should.equal(Ok(3.))
}

pub fn example_var_test() {
  // Degrees of freedom
  let ddof: Int = 1

  []
  |> math.var(ddof)
  |> should.equal(Error(Nil))

  [1., 2., 3.]
  |> math.var(ddof)
  |> should.equal(Ok(1.))
}

pub fn example_std_test() {
  // Degrees of freedom
  let ddof: Int = 1

  []
  |> math.std(ddof)
  |> should.equal(Error(Nil))

  [1., 2., 3.]
  |> math.std(ddof)
  |> should.equal(Ok(1.))
}

pub fn example_moment_test() {
  []
  |> math.moment(0)
  |> should.equal(Error(Nil))

  // 0th moment about the mean is 1. per definition
  [0., 1., 2., 3., 4.]
  |> math.moment(0)
  |> should.equal(Ok(1.))

  // 1st moment about the mean is 0. per definition
  [0., 1., 2., 3., 4.]
  |> math.moment(1)
  |> should.equal(Ok(0.))

  [0., 1., 2., 3., 4.]
  |> math.moment(2)
  |> should.equal(Ok(2.))
}

pub fn example_skewness_test() {
  []
  |> math.skewness()
  |> should.equal(Error(Nil))

  // No skewness 
  // -> Zero skewness
  [1., 2., 3., 4.]
  |> math.skewness()
  |> should.equal(Ok(0.))

  // Right-skewed distribution 
  // -> Positive skewness
  [1., 1., 1., 2.]
  |> math.skewness()
  |> fn(x: Result(Float, Nil)) -> Bool {
    case x {
      Ok(x) -> x >. 0.
    }
  }
  |> should.be_true()
}

pub fn example_kurtosis_test() {
  []
  |> math.skewness()
  |> should.equal(Error(Nil))

  // No tail 
  // -> Fisher's definition gives kurtosis -3 
  [1., 1., 1., 1.]
  |> math.kurtosis()
  |> should.equal(Ok(-3.))

  // Distribution with a tail 
  // -> Higher kurtosis 
  [1., 1., 1., 2.]
  |> math.kurtosis()
  |> fn(x: Result(Float, Nil)) -> Bool {
    case x {
      Ok(x) -> x >. -3.
    }
  }
  |> should.be_true()
}

pub fn example_percentile_test() {
  []
  |> math.percentile(40)
  |> should.equal(Error(Nil))

  // Calculate 40th percentile 
  [15., 20., 35., 40., 50.]
  |> math.percentile(40)
  |> should.equal(Ok(29.))
}

pub fn example_iqr_test() {
  []
  |> math.iqr()
  |> should.equal(Error(Nil))

  [1., 2., 3., 4., 5.]
  |> math.iqr()
  |> should.equal(Ok(3.))
}

pub fn example_freedman_diaconis_rule_test() {
  []
  |> math.freedman_diaconis_rule()
  |> should.equal(Error(Nil))

  // Calculate histogram bin widths
  list.range(0, 1000)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.freedman_diaconis_rule()
  |> should.equal(Ok(10.))
}

pub fn example_range_test() {
  // Create a range
  let range = math.Range(0., 1.)
  // Retrieve min and max values
  let math.Range(min, max) = range
  min
  |> should.equal(0.)
  max
  |> should.equal(1.)
}

pub fn example_bin_test() {
  // Create a bin
  let bin: math.Bin = tuple(math.Range(0., 1.), 999)
  // Retrieve min and max values
  let math.Range(min, max) = pair.first(bin)
  min
  |> should.equal(0.)
  max
  |> should.equal(1.)
  // Retrieve count
  let count = pair.second(bin)
  count
  |> should.equal(999)
}

pub fn example_histogram_test() {
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

pub fn example_correlation_test() {
  math.correlation([], [])
  |> should.equal(Error(Nil))

  // Perfect positive correlation
  let xarr0: List(Float) =
    list.range(0, 100)
    |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  let yarr0: List(Float) =
    list.range(0, 100)
    |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  math.correlation(xarr0, yarr0)
  |> should.equal(Ok(1.))

  // Perfect negative correlation
  let xarr0: List(Float) =
    list.range(0, 100)
    |> list.map(fn(x: Int) -> Float { -1. *. int.to_float(x) })
  let yarr0: List(Float) =
    list.range(0, 100)
    |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  math.correlation(xarr0, yarr0)
  |> should.equal(Ok(-1.))
}

pub fn example_trim_test() {
  []
  |> math.trim(0, 0)
  |> should.equal(Error(Nil))

  // Trim list to only middle part of list
  [1., 2., 3., 4., 5., 6.]
  |> math.trim(1, 4)
  |> should.equal(Ok([2., 3., 4., 5.]))
}

pub fn example_isclose_test() {
  let val: Float = 99.
  let ref_val: Float = 100.
  // We set 'atol' and 'rtol' such that the values are equivalent
  // if 'val' is within 1 percent of 'ref_val' +/- 0.1
  let rtol: Float = 0.01
  let atol: Float = 0.10
  math.isclose(val, ref_val, rtol, atol)
  |> should.be_true()
}

pub fn example_allclose_test() {
  let val: Float = 99.
  let ref_val: Float = 100.
  let xarr: List(Float) = list.repeat(val, 42)
  let yarr: List(Float) = list.repeat(ref_val, 42)
  // We set 'atol' and 'rtol' such that the values are equivalent
  // if 'val' is within 1 percent of 'ref_val' +/- 0.1
  let rtol: Float = 0.01
  let atol: Float = 0.10
  math.allclose(xarr, yarr, rtol, atol)
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

pub fn example_amax_test() {
  []
  |> math.amax()
  |> should.equal(Error(Nil))

  [4., 4., 3., 2., 1.]
  |> math.amax()
  |> should.equal(Ok(4.))
}

pub fn example_amin_test() {
  []
  |> math.amin()
  |> should.equal(Error(Nil))

  [4., 4., 3., 2., 1.]
  |> math.amin()
  |> should.equal(Ok(1.))
}

pub fn example_argmax_test() {
  []
  |> math.argmax()
  |> should.equal(Error(Nil))

  [4., 4., 3., 2., 1.]
  |> math.argmax()
  |> should.equal(Ok([0, 1]))
}

pub fn example_argmin_test() {
  []
  |> math.argmin()
  |> should.equal(Error(Nil))

  [4., 4., 3., 2., 1.]
  |> math.argmin()
  |> should.equal(Ok([4]))
}
