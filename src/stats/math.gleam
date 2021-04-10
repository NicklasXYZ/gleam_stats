//// A module containing several helpful mathematical functions.
////
//// ---
////
//// * **Statistics**
////   * [`mean`](#mean)
////   * [`median`](#median)
////   * [`hmean`](#hmean)
////   * [`gmean`](#gmean)
////   * [`var`](#var)
////   * [`std`](#std)
////   * [`moment`](#moment)
////   * [`skewness`](#skewness)
////   * [`kurtosis`](#kurtosis)
////   * [`percentile`](#percentile)
////   * [`iqr`](#iqr)
////   * [`freedman_diaconis_rule`](#freedman_diaconis_rule)
////   * [`histogram`](#histogram)
//// * **Types**
////   * [`Range`](#Range)
////   * [`Bin`](#Bin)
//// * **Miscellaneous functions**
////   * [`sum`](#sum)
////   * [`trim`](#trim)
////   * [`isclose`](#isclose)
////   * [`allclose`](#allclose)
////   * [`amax`](#amax)
////   * [`amin`](#amin)
////   * [`argmax`](#argmax)
////   * [`argmin`](#argmin)

import gleam/list
import gleam/int
import gleam/float
import gleam/io
import gleam/pair

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculcate the sum of the elements in a list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/should
///     import stats/math
///
///     pub fn example () {
///       []
///       |> math.sum()
///       |> should.equal(0.)
///     
///       [1., 2., 3.]
///       |> math.sum()
///       |> should.equal(6.)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn sum(arr: List(Float)) -> Float {
  case arr {
    [] -> 0.
    _ ->
      arr
      |> list.fold(0., fn(a: Float, acc: Float) -> Float { a +. acc })
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculcate the arithmetic mean of the elements in a list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/should
///     import stats/math
///
///     pub fn example () {
///       []
///       |> math.mean()
///       |> should.equal(Error(Nil))
///     
///       [1., 2., 3.]
///       |> math.mean()
///       |> should.equal(Ok(2.))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn mean(arr: List(Float)) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ ->
      arr
      |> sum()
      |> fn(a: Float) -> Float { a /. int.to_float(list.length(arr)) }
      |> Ok()
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculcate the median of the elements in a list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/should
///     import stats/math
///
///     pub fn example () {
///       []
///       |> math.median()
///       |> should.equal(Error(Nil))
///     
///       [1., 2., 3.]
///       |> math.median()
///       |> should.equal(Ok(2.))
///     
///       [1., 2., 3., 4.]
///       |> math.median()
///       |> should.equal(Ok(2.5))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn median(arr: List(Float)) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      let count: Int = list.length(arr)
      let mid: Int = list.length(arr) / 2
      let sorted: List(Float) = list.sort(arr, float.compare)
      case int.is_odd(count) {
        // If there is an odd number of elements in the list, then the median
        // is just the middle value
        True ->
          case list.at(sorted, mid) {
            Ok(val0) ->
              val0
              |> Ok()
          }
        // If there is an even number of elements in the list, then the median
        // is the mean of the two middle values
        False ->
          case list.at(sorted, mid - 1), list.at(sorted, mid) {
            Ok(val0), Ok(val1) ->
              [val0, val1]
              |> mean()
          }
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculcate the harmonic mean of the elements in a list.
/// Harmonic mean is only defined for positive numbers.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/should
///     import stats/math
///
///     pub fn example () {
///       []
///       |> math.hmean()
///       |> should.equal(Error(Nil))
///     
///       [1., 3., 6.]
///       |> math.hmean()
///       |> should.equal(Ok(2.))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn hmean(arr: List(Float)) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      let xarr: Result(List(Float), Nil) =
        arr
        |> list.try_map(fn(a: Float) -> Result(Float, Nil) {
          case a >=. 0. {
            True -> Ok(1. /. a)
            False -> Error(Nil)
          }
        })
      case xarr {
        Error(Nil) -> Error(Nil)
        Ok(xarr) ->
          xarr
          |> sum()
          |> fn(x: Float) { int.to_float(list.length(xarr)) /. x }
          |> Ok()
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculcate the geometric mean of the elements in a list.
/// The geometric mean is only defined for positive numbers.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/should
///     import stats/math
///
///     pub fn example () {
///       []
///       |> math.gmean()
///       |> should.equal(Error(Nil))
///     
///       [1., 3., 9.]
///       |> math.gmean()
///       |> should.equal(Ok(3.))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn gmean(arr: List(Float)) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      let xval: Result(Float, Nil) =
        arr
        |> list.try_fold(
          1.,
          fn(a: Float, acc: Float) -> Result(Float, Nil) {
            case a >=. 0. {
              True -> Ok(acc *. a)
              False -> Error(Nil)
            }
          },
        )
      case xval {
        Error(Nil) -> Error(Nil)
        Ok(xval) ->
          xval
          |> fn(x: Float) {
            float.power(x, 1. /. int.to_float(list.length(arr)))
          }
          |> Ok()
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculcate the sample variance of the elements in a list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/should
///     import stats/math
///
///     pub fn example () {
///       // Degrees of freedom
///       let ddof: Int = 1
///     
///       []
///       |> math.var(ddof)
///       |> should.equal(Error(Nil))
///     
///       [1., 2., 3.]
///       |> math.var(ddof)
///       |> should.equal(Ok(1.))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn var(arr: List(Float), ddof: Int) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      let mean: Result(Float, Nil) = mean(arr)
      case mean {
        Ok(mean) ->
          arr
          |> list.map(fn(a: Float) -> Float { float.power(a -. mean, 2.) })
          |> sum()
          |> fn(a: Float) -> Float {
            a /. { int.to_float(list.length(arr)) -. int.to_float(ddof) }
          }
          |> Ok()
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculcate the sample standard deviation of the elements in a list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/should
///     import stats/math
///
///     pub fn example () {
///       // Degrees of freedom
///       let ddof: Int = 1
///     
///       []
///       |> math.std(ddof)
///       |> should.equal(Error(Nil))
///     
///       [1., 2., 3.]
///       |> math.std(ddof)
///       |> should.equal(Ok(1.))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn std(arr: List(Float), ddof: Int) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      let var: Result(Float, Nil) = var(arr, ddof)
      case var {
        Ok(var) ->
          var
          |> float.square_root()
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculcate the n'th moment about the mean of a list of elements.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/should
///     import stats/math
///
///     pub fn example () {
///       []
///       |> math.moment(0)
///       |> should.equal(Error(Nil))
///     
///       // 0th moment about the mean is 1. per definition
///       [0., 1., 2., 3., 4.]
///       |> math.moment(0)
///       |> should.equal(Ok(1.))
///     
///       // 1st moment about the mean is 0. per definition
///       [0., 1., 2., 3., 4.]
///       |> math.moment(1)
///       |> should.equal(Ok(0.))
///     
///       [0., 1., 2., 3., 4.]
///       |> math.moment(2)
///       |> should.equal(Ok(2.))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn moment(arr: List(Float), n: Int) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ ->
      case n >= 0 {
        True ->
          case n {
            // 0th moment about the mean is 1 by definition
            0 -> Ok(1.)
            // 1st moment about the mean is 0 by definition
            1 -> Ok(0.)
            // nth moment about the mean
            _ -> {
              let m1 =
                arr
                |> mean()
              case m1 {
                Ok(m1) ->
                  arr
                  |> list.map(fn(a: Float) {
                    float.power(a -. m1, int.to_float(n))
                  })
                  |> mean()
              }
            }
          }
        False -> Error(Nil)
      }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculcate the sample skewness of a list of elements using the 
/// Fisher-Pearson coefficient of skewness. 
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/should
///     import stats/math
///
///     pub fn example () {
///       []
///       |> math.skewness()
///       |> should.equal(Error(Nil))
///     
///       // No skewness 
///       // -> Zero skewness
///       [1., 2., 3., 4.]
///       |> math.skewness()
///       |> should.equal(Ok(0.))
///     
///       // Right-skewed distribution 
///       // -> Positive skewness
///       [1., 1., 1., 2.]
///       |> math.skewness()
///       |> fn(x: Result(Float, Nil)) -> Bool {
///         case x {
///           Ok(x) -> x >. 0.
///         }
///       }
///       |> should.be_true()
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn skewness(arr: List(Float)) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      let m2: Result(Float, Nil) = moment(arr, 2)
      let m3: Result(Float, Nil) = moment(arr, 3)
      case m2, m3 {
        Ok(m2), Ok(m3) ->
          m3 /. float.power(m2, 1.5)
          |> Ok()
        _, _ -> Error(Nil)
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculcate the sample kurtosis of a list of elements using the 
/// definition of Fisher. 
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/should
///     import stats/math
///
///     pub fn example () {
///       []
///       |> math.skewness()
///       |> should.equal(Error(Nil))
///     
///       // No tail 
///       // -> Fisher's definition gives kurtosis -3 
///       [1., 1., 1., 1.]
///       |> math.kurtosis()
///       |> should.equal(Ok(-3.))
///     
///       // Distribution with a tail 
///       // -> Higher kurtosis 
///       [1., 1., 1., 2.]
///       |> math.kurtosis()
///       |> fn(x: Result(Float, Nil)) -> Bool {
///         case x {
///           Ok(x) -> x >. -3.
///         }
///       }
///       |> should.be_true()
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn kurtosis(arr: List(Float)) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      let m2: Result(Float, Nil) = moment(arr, 2)
      let m4: Result(Float, Nil) = moment(arr, 4)
      case m2, m4 {
        Ok(m2), Ok(m4) ->
          m4 /. float.power(m2, 2.0) -. 3.
          |> Ok()
        _, _ -> Error(Nil)
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the n'th percentile of the elements in a list using 
/// linear interpolation between closest ranks.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/should
///     import stats/math
///
///     pub fn example () {
///       []
///       |> math.percentile(40)
///       |> should.equal(Error(Nil))
///     
///       // Calculate 40th percentile 
///       [15., 20., 35., 40., 50.]
///       |> math.percentile(40)
///       |> should.equal(Ok(29.))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn percentile(arr: List(Float), n: Int) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      let s: List(Float) = list.sort(arr, float.compare)
      // Calculate the rank of the n'th percentile
      let r: Float =
        int.to_float(n) /. 100.0 *. int.to_float(list.length(arr) - 1)
      let f: Int = float.truncate(r)
      case list.at(s, f), list.at(s, f + 1) {
        Ok(lower), Ok(upper) ->
          lower +. { upper -. lower } *. { r -. int.to_float(f) }
          |> Ok()
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the interquartile range (IQR) of the elements in a list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/should
///     import stats/math
///
///     pub fn example () {
///       []
///       |> math.iqr()
///       |> should.equal(Error(Nil))
///     
///       [1., 2., 3., 4., 5.]
///       |> math.iqr()
///       |> should.equal(Ok(3.))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn iqr(arr: List(Float)) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      let length: Int = list.length(arr)
      case int.is_even(length) {
        True -> {
          // x contains the n smallest values
          // y contains the n largest values
          let tuple(x, y) =
            arr
            |> list.split(length / 2)
          case median(y), median(x) {
            Ok(val0), Ok(val1) -> Ok(val0 -. val1)
          }
        }
        False -> {
          // x contains the n smallest values
          let tuple(x, _z) =
            arr
            |> list.split({ length - 1 } / 2)
          // y contains the n largest values
          let tuple(_z, y) =
            arr
            |> list.split({ length + 1 } / 2)
          case median(y), median(x) {
            Ok(val0), Ok(val1) -> Ok(val0 -. val1)
          }
        }
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Use Freedman-Diaconis’s Rule to determine the bin widths of a
/// histogram.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/should
///     import stats/math
///
///     pub fn example () {
///       []
///       |> math.freedman_diaconis_rule()
///       |> should.equal(Error(Nil))
///     
///       // Calculate histogram bin widths
///       list.range(0, 1000)
///       |> list.map(fn(x: Int) -> Float { int.to_float(x) })
///       |> math.freedman_diaconis_rule()
///       |> should.equal(Ok(10.))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn freedman_diaconis_rule(arr: List(Float)) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      let length: Float = int.to_float(list.length(arr))
      let iqr: Result(Float, Nil) =
        arr
        |> iqr()
      let lower: Result(Float, Nil) =
        arr
        |> amin()
      let upper: Result(Float, Nil) =
        arr
        |> amax()
      case lower, upper, iqr {
        Ok(lower), Ok(upper), Ok(iqr) -> {
          let width: Float = 2. *. iqr /. float.power(length, 1. /. 3.)
          case width <. { upper -. lower } /. length {
            True -> Error(Nil)
            False ->
              float.ceiling({ upper -. lower } /. width)
              |> Ok
          }
        }
        _, _, _ -> Error(Nil)
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// A type used to represent a min/max interval. The `Range` type is among
/// others used to represent the bin boundaries in a histogram.
///
/// <details>
///     <summary>Example:</summary>
///
///     import stats/math
///     import gleam/should
///
///     pub fn example () {
///       // Create a range
///       let range = math.Range(0., 1.)
///       // Retrieve min and max values
///       let math.Range(min, max) = range
///       min
///       |> should.equal(0.)
///       max
///       |> should.equal(1.)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub type Range {
  Range(min: Float, max: Float)
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// A type used to represent the bins in a histogram. The type is an alias 
/// of a tuple containing a min/max range and a count of the values in 
/// that range.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/should
///     import gleam/pair
///     import stats/math
///
///     pub fn example () {
///       // Create a bin
///       let bin: math.Bin = tuple(math.Range(0., 1.), 999)
///       // Retrieve min and max values
///       let math.Range(min, max) = pair.first(bin)
///       min
///       |> should.equal(0.)
///       max
///       |> should.equal(1.)
///       // Retrieve count
///       let count = pair.second(bin)
///       count
///       |> should.equal(999)
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub type Bin =
  tuple(Range, Int)

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Create a histogram of the elements in a list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import stats/math
///     import gleam/should
///
///     pub fn example () {
///       list.range(0, 100)
///       |> list.map(fn(x: Int) -> Float { int.to_float(x) })
///       // Below 25. is the bin width
///       // The Freedman-Diaconis’s Rule can be used to determine a decent value
///       |> math.histogram(25.)
///       |> should.equal(Ok([
///         tuple(math.Range(0., 25.), 25),
///         tuple(math.Range(25., 50.), 25),
///         tuple(math.Range(50., 75.), 25),
///         tuple(math.Range(75., 100.), 25),
///       ]))
///
///       []
///       |> math.histogram(1.)
///       |> should.equal(Error(Nil))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn histogram(arr: List(Float), width: Float) -> Result(List(Bin), Nil) {
  case arr {
    [] -> Error(Nil)
    _ ->
      create_bins(arr, width)
      |> bin_elements(arr)
      |> Ok()
  }
}

fn create_bins(arr: List(Float), width: Float) -> List(Bin) {
  let min: Result(Float, Nil) =
    arr
    |> amin()
  let max: Result(Float, Nil) =
    arr
    |> amax()
  case min, max {
    Ok(min), Ok(max) -> {
      let inc: Float =
        { 1.001 *. max -. 0.999 *. min } /. width
        |> float.ceiling()
      list.range(0, float.round(inc))
      |> list.map(fn(x) -> Bin {
        tuple(Range(width *. int.to_float(x), width *. int.to_float(x + 1)), 0)
      })
    }
  }
}

fn find_bin(bins: List(Bin), key: Float) -> Result(Bin, Nil) {
  bins
  |> list.find(fn(b: Bin) -> Bool {
    let Range(cmin, cmax) = pair.first(b)
    key >=. cmin && key <. cmax
  })
}

fn bin_elements(bins: List(Bin), arr: List(Float)) -> List(Bin) {
  arr
  |> list.fold(
    bins,
    fn(key: Float, acc: List(Bin)) {
      let bin: Result(Bin, Nil) =
        acc
        |> find_bin(key)
      case bin {
        Ok(bin) ->
          case list.key_pop(acc, pair.first(bin)) {
            Ok(kv) ->
              list.key_set(pair.second(kv), pair.first(bin), pair.first(kv) + 1)
          }
      }
    },
  )
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the Pearson correlation coefficient to determine the linear 
/// relationship between the elements in two lists of equal length. 
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/should
///     import stats/math
///
///     pub fn example () {
///       math.correlation([], [])
///       |> should.equal(Error(Nil))
///     
///       // Perfect positive correlation
///       let xarr0: List(Float) =
///         list.range(0, 100)
///         |> list.map(fn(x: Int) -> Float { int.to_float(x) })
///       let yarr0: List(Float) =
///         list.range(0, 100)
///         |> list.map(fn(x: Int) -> Float { int.to_float(x) })
///       math.correlation(xarr0, yarr0)
///       |> should.equal(Ok(1.))
///     
///       // Perfect negative correlation
///       let xarr0: List(Float) =
///         list.range(0, 100)
///         |> list.map(fn(x: Int) -> Float { -1. *. int.to_float(x) })
///       let yarr0: List(Float) =
///         list.range(0, 100)
///         |> list.map(fn(x: Int) -> Float { int.to_float(x) })
///       math.correlation(xarr0, yarr0)
///       |> should.equal(Ok(-1.))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn correlation(xarr: List(Float), yarr: List(Float)) -> Result(Float, Nil) {
  let xlen: Int = list.length(xarr)
  let ylen: Int = list.length(yarr)
  case xlen == ylen && xlen > 2 && { xlen >= 2 && ylen >= 2 } {
    True -> {
      let xmean: Result(Float, Nil) =
        xarr
        |> mean()
      let ymean: Result(Float, Nil) =
        yarr
        |> mean()
      case xmean, ymean {
        Ok(xmean), Ok(ymean) -> {
          let a: Float =
            list.zip(xarr, yarr)
            |> list.map(fn(z: tuple(Float, Float)) -> Float {
              { pair.first(z) -. xmean } *. { pair.second(z) -. ymean }
            })
            |> sum()
          let b: Float =
            xarr
            |> list.map(fn(x: Float) { { x -. xmean } *. { x -. xmean } })
            |> sum()
          let c: Float =
            yarr
            |> list.map(fn(y: Float) { { y -. ymean } *. { y -. ymean } })
            |> sum()
          case float.square_root(b *. c) {
            Ok(val0) -> Ok(a /. val0)
          }
        }
      }
    }
    False -> Error(Nil)
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Trim a list to a certain size given a max and min index. Min and max index 
/// are inclusive. 
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/should
///     import stats/math
///
///     pub fn example () {
///       []
///       |> math.trim(0, 0)
///       |> should.equal(Error(Nil))
///     
///       // Trim list to only middle part of list
///       [1., 2., 3., 4., 5., 6.]
///       |> math.trim(1, 4)
///       |> should.equal(Ok([2., 3., 4., 5.]))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn trim(arr: List(Float), min: Int, max: Int) -> Result(List(Float), Nil) {
  case arr {
    [] -> Error(Nil)
    _ ->
      case min >= 0 && max < list.length(arr) {
        True ->
          arr
          |> list.drop(min)
          |> list.take(max - min + 1)
          |> Ok()
        False -> Error(Nil)
      }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Determine if a given value is close to or equivalent to a reference value 
/// based on supplied relative and absolute tolerance values. The equivalance
/// of the two given values are determined based on the equation:
/// absolute(a - b) <= (atol + rtol * absolute(b))
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/should
///     import stats/math
///
///     pub fn example () {
///       let val: Float = 99.
///       let ref_val: Float = 100.
///       // We set 'atol' and 'rtol' such that the values are equivalent
///       // if 'val' is within 1 percent of 'ref_val' +/- 0.1
///       let rtol: Float = 0.01
///       let atol: Float = 0.10
///       math.isclose(val, ref_val, rtol, atol)
///       |> should.be_true()
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn isclose(a: Float, b: Float, rtol: Float, atol: Float) -> Bool {
  let x: Float = float.absolute_value(a -. b)
  let y: Float = atol +. rtol *. float.absolute_value(b)
  case x <=. y {
    True -> True
    False -> False
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Determine if a list of given values are close to or equivalent to a 
/// another list of reference values.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/should
///     import stats/math
///
///     pub fn example () {
///       let val: Float = 99.
///       let ref_val: Float = 100.
///       let xarr: List(Float) = list.repeat(val, 42)
///       let yarr: List(Float) = list.repeat(ref_val, 42)
///       // We set 'atol' and 'rtol' such that the values are equivalent
///       // if 'val' is within 1 percent of 'ref_val' +/- 0.1
///       let rtol: Float = 0.01
///       let atol: Float = 0.10
///       math.allclose(xarr, yarr, rtol, atol)
///       |> fn(zarr: Result(List(Bool), Nil)) -> Result(Bool, Nil) {
///         case zarr {
///           Ok(arr) ->
///             arr
///             |> list.all(fn(a: Bool) -> Bool { a })
///             |> Ok()
///           _ -> Error(Nil)
///         }
///       }
///       |> should.equal(Ok(True))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn allclose(
  xarr: List(Float),
  yarr: List(Float),
  rtol: Float,
  atol: Float,
) -> Result(List(Bool), Nil) {
  let xlen: Int = list.length(xarr)
  let ylen: Int = list.length(yarr)
  case xlen == ylen {
    True ->
      list.zip(xarr, yarr)
      |> list.map(fn(z: tuple(Float, Float)) -> Bool {
        isclose(pair.first(z), pair.second(z), rtol, atol)
      })
      |> Ok()
    _ -> Error(Nil)
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Returns the maximum value of a list. 
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/should
///     import stats/math
///
///     pub fn example () {
///       []
///       |> math.amax()
///       |> should.equal(Error(Nil))
///     
///       [4., 4., 3., 2., 1.]
///       |> math.amax()
///       |> should.equal(Ok(4.))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn amax(arr: List(Float)) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ ->
      case list.at(arr, 0) {
        Ok(val0) ->
          arr
          |> list.fold(
            val0,
            fn(a: Float, acc: Float) {
              case a >. acc {
                True -> a
                False -> acc
              }
            },
          )
          |> Ok()
      }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Returns the minimum value of a list. 
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/should
///     import stats/math
///
///     pub fn example () {
///       []
///       |> math.amin()
///       |> should.equal(Error(Nil))
///     
///       [4., 4., 3., 2., 1.]
///       |> math.amin()
///       |> should.equal(Ok(1.))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn amin(arr: List(Float)) -> Result(Float, Nil) {
  case arr {
    [] -> Error(Nil)
    _ ->
      case list.at(arr, 0) {
        Ok(val0) ->
          arr
          |> list.fold(
            val0,
            fn(a: Float, acc: Float) {
              case a <. acc {
                True -> a
                False -> acc
              }
            },
          )
          |> Ok()
      }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Returns the indices of the maximum values in a list. 
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/should
///     import stats/math
///
///     pub fn example () {
///       []
///       |> math.argmax()
///       |> should.equal(Error(Nil))
///     
///       [4., 4., 3., 2., 1.]
///       |> math.argmax()
///       |> should.equal(Ok([0, 1]))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn argmax(arr: List(Float)) -> Result(List(Int), Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      let max: Result(Float, Nil) =
        arr
        |> amax()
      case max {
        Ok(max) ->
          arr
          |> list.index_map(fn(index: Int, a: Float) -> Int {
            case a -. max {
              0. -> index
              _ -> -1
            }
          })
          |> list.filter(fn(index: Int) -> Bool {
            case index {
              -1 -> False
              _ -> True
            }
          })
          |> Ok()
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Returns the indices of the minimum values in a list. 
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/should
///     import stats/math
///
///     pub fn example () {
///       []
///       |> math.argmin()
///       |> should.equal(Error(Nil))
///     
///       [4., 4., 3., 2., 1.]
///       |> math.argmin()
///       |> should.equal(Ok([4]))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn argmin(arr: List(Float)) -> Result(List(Int), Nil) {
  case arr {
    [] -> Error(Nil)
    _ -> {
      let min: Result(Float, Nil) =
        arr
        |> amin()
        |> io.debug()
      case min {
        Ok(min) ->
          arr
          |> list.index_map(fn(index: Int, a: Float) -> Int {
            case a -. min {
              0. -> index
              _ -> -1
            }
          })
          |> io.debug()
          |> list.filter(fn(index: Int) -> Bool {
            case index {
              -1 -> False
              _ -> True
            }
          })
          |> io.debug()
          |> Ok()
      }
    }
  }
}
/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam/should
///     import stats/math
///
///     pub fn example () {
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
// pub fn describe(arr: List(Float)) -> Result(List(tuple(String, Float)), Nil) {
//   todo
// }
