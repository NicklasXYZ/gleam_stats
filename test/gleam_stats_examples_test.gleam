//// Small examples used in the docs...

// import gleam/should
import gleam/int
import gleam/list
import gleam/pair
import gleam_stats/stats
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn example_sum_test() {
  []
  |> stats.sum()
  |> should.equal(0.)

  [1., 2., 3.]
  |> stats.sum()
  |> should.equal(6.)
}

pub fn example_mean_test() {
  []
  |> stats.mean()
  |> should.equal(Error(Nil))

  [1., 2., 3.]
  |> stats.mean()
  |> should.equal(Ok(2.))
}

pub fn example_median_test() {
  []
  |> stats.median()
  |> should.equal(Error(Nil))

  [1., 2., 3.]
  |> stats.median()
  |> should.equal(Ok(2.))

  [1., 2., 3., 4.]
  |> stats.median()
  |> should.equal(Ok(2.5))
}

pub fn example_hmean_test() {
  []
  |> stats.hmean()
  |> should.equal(Error(Nil))

  [1., 3., 6.]
  |> stats.hmean()
  |> should.equal(Ok(2.))
}

pub fn example_gmean_test() {
  []
  |> stats.gmean()
  |> should.equal(Error(Nil))

  [1., 3., 9.]
  |> stats.gmean()
  |> should.equal(Ok(3.))
}

pub fn example_var_test() {
  // Degrees of freedom
  let ddof: Int = 1

  []
  |> stats.var(ddof)
  |> should.equal(Error(Nil))

  [1., 2., 3.]
  |> stats.var(ddof)
  |> should.equal(Ok(1.))
}

pub fn example_std_test() {
  // Degrees of freedom
  let ddof: Int = 1

  []
  |> stats.std(ddof)
  |> should.equal(Error(Nil))

  [1., 2., 3.]
  |> stats.std(ddof)
  |> should.equal(Ok(1.))
}

pub fn example_moment_test() {
  []
  |> stats.moment(0)
  |> should.equal(Error(Nil))

  // 0th moment about the mean is 1. per definition
  [0., 1., 2., 3., 4.]
  |> stats.moment(0)
  |> should.equal(Ok(1.))

  // 1st moment about the mean is 0. per definition
  [0., 1., 2., 3., 4.]
  |> stats.moment(1)
  |> should.equal(Ok(0.))

  [0., 1., 2., 3., 4.]
  |> stats.moment(2)
  |> should.equal(Ok(2.))
}

pub fn example_skewness_test() {
  []
  |> stats.skewness()
  |> should.equal(Error(Nil))

  // No skewness 
  // -> Zero skewness
  [1., 2., 3., 4.]
  |> stats.skewness()
  |> should.equal(Ok(0.))

  // Right-skewed distribution 
  // -> Positive skewness
  [1., 1., 1., 2.]
  |> stats.skewness()
  |> fn(x: Result(Float, Nil)) -> Bool {
    case x {
      Ok(x) -> x >. 0.
      _ -> False
    }
  }
  |> should.be_true()
}

pub fn example_kurtosis_test() {
  []
  |> stats.skewness()
  |> should.equal(Error(Nil))

  // No tail 
  // -> Fisher's definition gives kurtosis -3 
  [1., 1., 1., 1.]
  |> stats.kurtosis()
  |> should.equal(Ok(-3.))

  // Distribution with a tail 
  // -> Higher kurtosis 
  [1., 1., 1., 2.]
  |> stats.kurtosis()
  |> fn(x: Result(Float, Nil)) -> Bool {
    case x {
      Ok(x) -> x >. -3.
      _ -> False
    }
  }
  |> should.be_true()
}

pub fn example_zscore_test() {
  []
  // Use degrees of freedom = 1
  |> stats.zscore(1)
  |> should.equal(Error(Nil))

  [1., 2., 3.]
  // Use degrees of freedom = 1
  |> stats.zscore(1)
  |> should.equal(Ok([-1., 0., 1.]))
}

pub fn example_percentile_test() {
  []
  |> stats.percentile(40)
  |> should.equal(Error(Nil))

  // Calculate 40th percentile 
  [15., 20., 35., 40., 50.]
  |> stats.percentile(40)
  |> should.equal(Ok(29.))
}

pub fn example_iqr_test() {
  []
  |> stats.iqr()
  |> should.equal(Error(Nil))

  [1., 2., 3., 4., 5.]
  |> stats.iqr()
  |> should.equal(Ok(3.))
}

pub fn example_freedman_diaconis_rule_test() {
  []
  |> stats.freedman_diaconis_rule()
  |> should.equal(Error(Nil))

  // Calculate histogram bin widths
  list.range(0, 1000)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> stats.freedman_diaconis_rule()
  |> should.equal(Ok(10.))
}

pub fn example_range_test() {
  // Create a range
  let range = stats.Range(0., 1.)
  // Retrieve min and max values
  let stats.Range(min, max) = range
  min
  |> should.equal(0.)
  max
  |> should.equal(1.)
}

pub fn example_bin_test() {
  // Create a bin
  let bin: stats.Bin = #(stats.Range(0., 1.), 999)
  // Retrieve min and max values
  let stats.Range(min, max) = pair.first(bin)
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

pub fn example_correlation_test() {
  stats.correlation([], [])
  |> should.equal(Error(Nil))

  // Perfect positive correlation
  let xarr0: List(Float) =
    list.range(0, 100)
    |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  let yarr0: List(Float) =
    list.range(0, 100)
    |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  stats.correlation(xarr0, yarr0)
  |> should.equal(Ok(1.))

  // Perfect negative correlation
  let xarr0: List(Float) =
    list.range(0, 100)
    |> list.map(fn(x: Int) -> Float { -1. *. int.to_float(x) })
  let yarr0: List(Float) =
    list.range(0, 100)
    |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  stats.correlation(xarr0, yarr0)
  |> should.equal(Ok(-1.))
}

pub fn example_trim_test() {
  []
  |> stats.trim(0, 0)
  |> should.equal(Error(Nil))

  // Trim list to only middle part of list
  [1., 2., 3., 4., 5., 6.]
  |> stats.trim(1, 4)
  |> should.equal(Ok([2., 3., 4., 5.]))
}

pub fn example_isclose_test() {
  let val: Float = 99.
  let ref_val: Float = 100.
  // We set 'atol' and 'rtol' such that the values are equivalent
  // if 'val' is within 1 percent of 'ref_val' +/- 0.1
  let rtol: Float = 0.01
  let atol: Float = 0.10
  stats.isclose(val, ref_val, rtol, atol)
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
  stats.allclose(xarr, yarr, rtol, atol)
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
  |> stats.amax()
  |> should.equal(Error(Nil))

  [4., 4., 3., 2., 1.]
  |> stats.amax()
  |> should.equal(Ok(4.))
}

pub fn example_amin_test() {
  []
  |> stats.amin()
  |> should.equal(Error(Nil))

  [4., 4., 3., 2., 1.]
  |> stats.amin()
  |> should.equal(Ok(1.))
}

pub fn example_argmax_test() {
  []
  |> stats.argmax()
  |> should.equal(Error(Nil))

  [4., 4., 3., 2., 1.]
  |> stats.argmax()
  |> should.equal(Ok([0, 1]))
}

pub fn example_argmin_test() {
  []
  |> stats.argmin()
  |> should.equal(Error(Nil))

  [4., 4., 3., 2., 1.]
  |> stats.argmin()
  |> should.equal(Ok([4]))
}
