//// A module containing several helpful functions for computing and working with statistics.
////
//// ---
////
//// * **Types**
////   * [`Bin`](#Bin)
////   * [`Range`](#Range)
//// * **Statistical functions**
////   * [`freedman_diaconis_rule`](#freedman_diaconis_rule)
////   * [`correlation`](#correlation)
////   * [`gmean`](#gmean)
////   * [`histogram`](#histogram)
////   * [`hmean`](#hmean)
////   * [`iqr`](#iqr)
////   * [`kurtosis`](#kurtosis)
////   * [`mean`](#mean)
////   * [`median`](#median)
////   * [`moment`](#moment)
////   * [`percentile`](#percentile)
////   * [`skewness`](#skewness)
////   * [`std`](#std)
////   * [`var`](#var)
////   * [`zscore`](#zscore)
//// * **Miscellaneous functions**
////   * [`allclose`](#allclose)
////   * [`amax`](#amax)
////   * [`amin`](#amin)
////   * [`argmax`](#argmax)
////   * [`argmin`](#argmin)
////   * [`isclose`](#isclose)
////   * [`sum`](#sum)
////   * [`trim`](#trim)

import gleam/list
import gleam/int
import gleam/float
import gleam/pair

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
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
///     import gleam_stats/stats
///     import gleeunit/should
///
///     pub fn example () {
///       // Create a range
///       let range = stats.Range(0., 1.)
///       // Retrieve min and max values
///       let stats.Range(min, max) = range
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
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
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
///     import gleeunit/should
///     import gleam/pair
///     import gleam_stats/stats
///
///     pub fn example () {
///       // Create a bin
///       let bin: stats.Bin = #(stats.Range(0., 1.), 999)
///       // Retrieve min and max values
///       let stats.Range(min, max) = pair.first(bin)
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
  #(Range, Int)

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
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
///     import gleeunit/should
///     import gleam_stats/stats
///
///     pub fn example () {
///       // An empty list returns an error
///       []
///       |> stats.freedman_diaconis_rule()
///       |> should.be_error()
///     
///       // Calculate histogram bin widths
///       list.range(0, 1000)
///       |> list.map(fn(x: Int) -> Float { int.to_float(x) })
///       |> stats.freedman_diaconis_rule()
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
pub fn freedman_diaconis_rule(arr: List(Float)) -> Result(Float, String) {
  case arr {
    [] ->
      "Invalid input argument: The list is empty."
      |> Error
    _ -> {
      let length: Float = int.to_float(list.length(arr))
      assert Ok(iqr) =
        arr
        |> iqr()
      assert Ok(lower) =
        arr
        |> amin()
      assert Ok(upper) =
        arr
        |> amax()
      let width: Float = 2. *. iqr /. float.power(length, 1. /. 3.)
      case width <. { upper -. lower } /. length {
        // If the bin size/width is too small then return an error.
        // The bin size/width should be set manually or in some other
        // way.
        True ->
          "The determined bin width is too small. Determine the width manually or in another way."
          |> Error
        False ->
          float.ceiling({ upper -. lower } /. width)
          |> Ok
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate Pearson's correlation coefficient to determine the linear 
/// relationship between the elements in two lists of equal length. 
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_stats/stats
///
///     pub fn example () {
///       // An empty lists returns an error
///       stats.correlation([], [])
///       |> should.be_error()
///     
///       // Lists with fewer than 2 elements return an error
///       stats.correlation([1.0], [1.0])
///       |> should.be_error()
///
///       // Lists of uneqal length return an error
///       stats.correlation([1.0, 2.0, 3.0], [1.0, 2.0])
///       |> should.be_error()
///
///       // Perfect positive correlation
///       let xarr0: List(Float) =
///         list.range(0, 100)
///         |> list.map(fn(x: Int) -> Float { int.to_float(x) })
///       let yarr0: List(Float) =
///         list.range(0, 100)
///         |> list.map(fn(x: Int) -> Float { int.to_float(x) })
///       stats.correlation(xarr0, yarr0)
///       |> should.equal(Ok(1.))
///     
///       // Perfect negative correlation
///       let xarr0: List(Float) =
///         list.range(0, 100)
///         |> list.map(fn(x: Int) -> Float { -1. *. int.to_float(x) })
///       let yarr0: List(Float) =
///         list.range(0, 100)
///         |> list.map(fn(x: Int) -> Float { int.to_float(x) })
///       stats.correlation(xarr0, yarr0)
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
pub fn correlation(
  xarr: List(Float),
  yarr: List(Float),
) -> Result(Float, String) {
  let xlen: Int = list.length(xarr)
  let ylen: Int = list.length(yarr)
  case xlen <= 0, ylen <= 0 {
    True, True ->
      "Invlaid input argument: length(xarr) == 0 or length(yarr) == 0. Valid input is length(xarr) > 0 and length(yarr) > 0."
      |> Error
    _, _ ->
      case xlen == ylen {
        False ->
          "Invalid input argument: length(xarr) != length(yarr). Valid input is when length(xarr) == length(yarr)."
          |> Error
        True ->
          case xlen >= 2 && ylen >= 2 {
            False ->
              "Invalid input argument: length(xarr) < 2 or length(yarr) < 2. Valid input is when length(xarr) >= 2 and length(yarr) >= 2."
              |> Error
            True -> {
              assert Ok(xmean) =
                xarr
                |> mean()
              assert Ok(ymean) =
                yarr
                |> mean()
              let a: Float =
                list.zip(xarr, yarr)
                |> list.map(fn(z: #(Float, Float)) -> Float {
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
              // The argument is the product of two sums of squared differences
              // it will never be negative. So extract it directly:
              assert Ok(val0) = float.square_root(b *. c)
              a /. val0
              |> Ok
            }
          }
      }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculcate the geometric mean of the elements in a list.
/// Note: The geometric mean is only defined for positive numbers.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_stats/stats
///
///     pub fn example () {
///       // An empty list returns an error
///       []
///       |> stats.gmean()
///       |> should.be_error()
///
///       // List with negative numbers returns an error
///       [-1., -3., -6.]
///       |> stats.gmean()
///       |> should.be_error()
///
///       // Valid input returns a result
///       [1., 3., 9.]
///       |> stats.gmean()
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
pub fn gmean(arr: List(Float)) -> Result(Float, String) {
  case arr {
    [] ->
      "Invalid input argument: The list is empty."
      |> Error
    _ -> {
      let xval: Result(Float, String) =
        arr
        |> list.try_fold(
          1.,
          fn(acc: Float, a: Float) -> Result(Float, String) {
            case a >=. 0. {
              True ->
                acc *. a
                |> Ok
              False ->
                "The geometric mean is only defined for positive numbers."
                |> Error
            }
          },
        )
      case xval {
        Error(string) ->
          string
          |> Error
        Ok(xval) ->
          xval
          |> fn(x: Float) {
            float.power(x, 1. /. int.to_float(list.length(arr)))
          }
          |> Ok
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Create a histogram of the elements in a list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleam_stats/stats
///     import gleeunit/should
///
///     pub fn example () {
///       // An empty lists returns an error
///       []
///       |> stats.histogram(1.)
///       |> should.be_error()
///
///       // Create the bins of a histogram given a list of values
///       list.range(0, 100)
///       |> list.map(fn(x: Int) -> Float { int.to_float(x) })
///       // Below 25. is the bin width
///       // The Freedman-Diaconis’s Rule can be used to determine a decent value
///       |> stats.histogram(25.)
///       |> should.equal(Ok([
///         #(stats.Range(0., 25.), 25),
///         #(stats.Range(25., 50.), 25),
///         #(stats.Range(50., 75.), 25),
///         #(stats.Range(75., 100.), 25),
///       ]))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn histogram(arr: List(Float), width: Float) -> Result(List(Bin), String) {
  case arr {
    [] ->
      "Invalid input argument: The list is empty."
      |> Error
    _ ->
      case create_bins(arr, width) {
        Error(string) ->
          string
          |> Error
        Ok(bins) ->
          bins
          |> bin_elements(arr)
          |> Ok
      }
  }
}

fn create_bins(arr: List(Float), width: Float) -> Result(List(Bin), String) {
  case arr {
    [] ->
      "Invalid input argument: The list is empty."
      |> Error
    _ ->
      case width <. 0.0 {
        True ->
          "Invalid input argument: width < 0. Valid input is width > 0."
          |> Error
        False -> {
          assert Ok(min) =
            arr
            |> amin()
          assert Ok(max) =
            arr
            |> amax()
          let inc: Float =
            { 1.001 *. max -. 0.999 *. min } /. width
            |> float.ceiling()
          list.range(0, float.round(inc))
          |> list.map(fn(x: Int) -> Bin {
            #(Range(width *. int.to_float(x), width *. int.to_float(x + 1)), 0)
          })
          |> Ok
        }
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
    fn(acc: List(Bin), key: Float) -> List(Bin) {
      // If the bins were constructed correctly then there should be a bin that fits
      // every value in the input array
      assert Ok(bin) =
        acc
        |> find_bin(key)
      // Retrieve key-value pair
      assert Ok(kv) = list.key_pop(acc, pair.first(bin))
      // Update and set key-value pair
      list.key_set(pair.second(kv), pair.first(bin), pair.first(kv) + 1)
    },
  )
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculcate the harmonic mean of the elements in a list.
/// Note: The harmonic mean is only defined for positive numbers.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_stats/stats
///
///     pub fn example () {
///       // An empty list returns an error
///       []
///       |> stats.hmean()
///       |> should.be_error()
///
///       // List with negative numbers returns an error
///       [-1., -3., -6.]
///       |> stats.hmean()
///       |> should.be_error()
///     
///       // Valid input returns a result
///       [1., 3., 6.]
///       |> stats.hmean()
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
pub fn hmean(arr: List(Float)) -> Result(Float, String) {
  case arr {
    [] ->
      "Invalid input argument: The list is empty."
      |> Error
    _ -> {
      let xarr: Result(List(Float), String) =
        arr
        |> list.try_map(fn(a: Float) -> Result(Float, String) {
          case a >=. 0. {
            True ->
              1. /. a
              |> Ok
            False ->
              "The harmonic mean is only defined for positive numbers."
              |> Error
          }
        })
      case xarr {
        Error(string) ->
          string
          |> Error
        Ok(xarr) ->
          xarr
          |> sum()
          |> fn(x: Float) { int.to_float(list.length(xarr)) /. x }
          |> Ok
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the interquartile range (IQR) of the elements in a list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_stats/stats
///
///     pub fn example () {
///       // An empty list returns an error
///       []
///       |> stats.iqr()
///       |> should.be_error()
///     
///       // Valid input returns a result
///       [1., 2., 3., 4., 5.]
///       |> stats.iqr()
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
pub fn iqr(arr: List(Float)) -> Result(Float, String) {
  case arr {
    [] ->
      "Invalid input argument: The list is empty."
      |> Error
    _ -> {
      let length: Int = list.length(arr)
      case int.is_even(length) {
        True -> {
          // x contains the n smallest values
          // y contains the n largest values
          let #(x, y) =
            arr
            |> list.split(length / 2)
          assert Ok(val0) = median(y)
          assert Ok(val1) = median(x)
          val0 -. val1
          |> Ok
        }
        False -> {
          // x contains the n smallest values
          let #(x, _z) =
            arr
            |> list.split({ length - 1 } / 2)
          // y contains the n largest values
          let #(_z, y) =
            arr
            |> list.split({ length + 1 } / 2)
          assert Ok(val0) = median(y)
          assert Ok(val1) = median(x)
          val0 -. val1
          |> Ok
        }
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
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
///     import gleeunit/should
///     import gleam_stats/stats
///
///     pub fn example () {
///       // An empty list returns an error
///       []
///       |> stats.skewness()
///       |> should.be_error()
///     
///       // No tail 
///       // -> Fisher's definition gives kurtosis -3 
///       [1., 1., 1., 1.]
///       |> stats.kurtosis()
///       |> should.equal(Ok(-3.))
///     
///       // Distribution with a tail 
///       // -> Higher kurtosis 
///       [1., 1., 1., 2.]
///       |> stats.kurtosis()
///       |> fn(x: Result(Float, String)) -> Bool {
///         case x {
///           Ok(x) -> x >. -3.
///           _ -> False
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
pub fn kurtosis(arr: List(Float)) -> Result(Float, String) {
  case arr {
    [] ->
      "Invalid input argument: The list is empty."
      |> Error
    _ -> {
      assert Ok(m2) = moment(arr, 2)
      assert Ok(m4) = moment(arr, 4)
      m4 /. float.power(m2, 2.0) -. 3.
      |> Ok
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculcate the arithmetic mean of the elements in a list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_stats/stats
///
///     pub fn example () {
///       // An empty list returns an error
///       []
///       |> stats.mean()
///       |> should.be_error()
///
///       // Valid input returns a result
///       [1., 2., 3.]
///       |> stats.mean()
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
pub fn mean(arr: List(Float)) -> Result(Float, String) {
  case arr {
    [] ->
      "Invalid input argument: The list is empty."
      |> Error
    _ ->
      arr
      |> sum()
      |> fn(a: Float) -> Float { a /. int.to_float(list.length(arr)) }
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculcate the median of the elements in a list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_stats/stats
///
///     pub fn example () {
///       // An empty list returns an error
///       []
///       |> stats.median()
///       |> should.be_error()
///
///       // Valid input returns a result
///       [1., 2., 3.]
///       |> stats.median()
///       |> should.equal(Ok(2.))
///     
///       [1., 2., 3., 4.]
///       |> stats.median()
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
pub fn median(arr: List(Float)) -> Result(Float, String) {
  case arr {
    [] ->
      "Invalid input argument: The list is empty."
      |> Error
    _ -> {
      let count: Int = list.length(arr)
      let mid: Int = list.length(arr) / 2
      let sorted: List(Float) = list.sort(arr, float.compare)
      case int.is_odd(count) {
        // If there is an odd number of elements in the list, then the median
        // is just the middle value
        True -> {
          assert Ok(val0) = list.at(sorted, mid)
          val0
          |> Ok
        }
        // If there is an even number of elements in the list, then the median
        // is the mean of the two middle values
        False -> {
          assert Ok(val0) = list.at(sorted, mid - 1)
          assert Ok(val1) = list.at(sorted, mid)
          [val0, val1]
          |> mean()
        }
      }
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculcate the n'th moment about the mean of a list of elements.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_stats/stats
///
///     pub fn example () {
///       // An empty list returns an error
///       []
///       |> stats.moment(0)
///       |> should.be_error()
///     
///       // 0th moment about the mean is 1. per definition
///       [0., 1., 2., 3., 4.]
///       |> stats.moment(0)
///       |> should.equal(Ok(1.))
///     
///       // 1st moment about the mean is 0. per definition
///       [0., 1., 2., 3., 4.]
///       |> stats.moment(1)
///       |> should.equal(Ok(0.))
///     
///       // 2nd moment about the mean
///       [0., 1., 2., 3., 4.]
///       |> stats.moment(2)
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
pub fn moment(arr: List(Float), n: Int) -> Result(Float, String) {
  case arr {
    [] ->
      "Invalid input argument: The list is empty."
      |> Error
    _ ->
      case n >= 0 {
        True ->
          case n {
            // 0th moment about the mean is 1.0 by definition
            0 ->
              1.0
              |> Ok
            // 1st moment about the mean is 0.0 by definition
            1 ->
              0.0
              |> Ok
            // n'th moment about the mean
            _ -> {
              assert Ok(m1) = mean(arr)
              arr
              |> list.map(fn(a: Float) { float.power(a -. m1, int.to_float(n)) })
              |> mean()
            }
          }
        False ->
          "Invalid input argument: n < 0. Valid input is n > 0."
          |> Error
      }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
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
///     import gleeunit/should
///     import gleam_stats/stats
///
///     pub fn example () {
///       // An empty list returns an error
///       []
///       |> stats.percentile(40)
///       |> should.be_error()
///     
///       // Calculate 40th percentile 
///       [15., 20., 35., 40., 50.]
///       |> stats.percentile(40)
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
pub fn percentile(arr: List(Float), n: Int) -> Result(Float, String) {
  case arr {
    [] ->
      "Invalid input argument: The list is empty."
      |> Error
    _ ->
      case n < 0 || n > 100 {
        True ->
          "Invalid input argument: n < 0 or n > 100. Valid input is 0 <= n <= 100."
          |> Error
        False -> {
          let s: List(Float) = list.sort(arr, float.compare)
          // Calculate the rank of the n'th percentile
          let r: Float =
            int.to_float(n) /. 100.0 *. int.to_float(list.length(arr) - 1)
          let f: Int = float.truncate(r)
          // Directly extract the lower and upper values. Theoretically an error
          // value will not be returned as the largest index in the array that is
          // accessed will be the length of the array - 1 (last element). 
          assert Ok(lower) = list.at(s, f)
          assert Ok(upper) = list.at(s, f + 1)
          lower +. { upper -. lower } *. { r -. int.to_float(f) }
          |> Ok
        }
      }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
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
///     import gleeunit/should
///     import gleam_stats/stats
///
///     pub fn example () {
///       // An empty list returns an error
///       []
///       |> stats.skewness()
///       |> should.be_error()
///     
///       // No skewness 
///       // -> Zero skewness
///       [1., 2., 3., 4.]
///       |> stats.skewness()
///       |> should.equal(Ok(0.))
///     
///       // Right-skewed distribution 
///       // -> Positive skewness
///       [1., 1., 1., 2.]
///       |> stats.skewness()
///       |> fn(x: Result(Float, String)) -> Bool {
///         case x {
///           Ok(x) -> x >. 0.
///           _ -> False
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
pub fn skewness(arr: List(Float)) -> Result(Float, String) {
  case arr {
    [] ->
      "Invalid input argument: The list is empty."
      |> Error
    _ -> {
      assert Ok(m2) = moment(arr, 2)
      assert Ok(m3) = moment(arr, 3)
      m3 /. float.power(m2, 1.5)
      |> Ok
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculcate the sample standard deviation of the elements in a list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_stats/stats
///
///     pub fn example () {
///       // Degrees of freedom
///       let ddof: Int = 1
///     
///       // An empty list returns an error
///       []
///       |> stats.std(ddof)
///       |> should.be_error()
///     
///       // Valid input returns a result
///       [1., 2., 3.]
///       |> stats.std(ddof)
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
pub fn std(arr: List(Float), ddof: Int) -> Result(Float, String) {
  case arr {
    [] ->
      "Invalid input argument: The list is empty."
      |> Error
    _ ->
      case ddof < 0 {
        True ->
          "Invalid input argument: ddof < 0. Valid input is ddof >= 0."
          |> Error
        False -> {
          assert Ok(variance) = var(arr, ddof)
          // The computed variance will always be positive
          // So an error should never be returned 
          assert Ok(stdev) = float.square_root(variance)
          stdev
          |> Ok
        }
      }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculcate the sample variance of the elements in a list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_stats/stats
///
///     pub fn example () {
///       // Degrees of freedom
///       let ddof: Int = 1
///     
///       // An empty list returns an error
///       []
///       |> stats.var(ddof)
///       |> should.be_error()
///     
///       // Valid input returns a result
///       [1., 2., 3.]
///       |> stats.var(ddof)
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
pub fn var(arr: List(Float), ddof: Int) -> Result(Float, String) {
  case arr {
    [] ->
      "Invalid input argument: The list is empty."
      |> Error
    _ ->
      case ddof < 0 {
        True ->
          "Invalid input argument: ddof < 0. Valid input is ddof >= 0."
          |> Error
        False -> {
          assert Ok(mean) = mean(arr)
          arr
          |> list.map(fn(a: Float) -> Float { float.power(a -. mean, 2.) })
          |> sum()
          |> fn(a: Float) -> Float {
            a /. { int.to_float(list.length(arr)) -. int.to_float(ddof) }
          }
          |> Ok
        }
      }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculate the z-score of each value in the list relative to the sample 
/// mean and standard deviation.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_stats/stats
///
///     pub fn example () {
///       // An empty list returns an error
///       []
///       // Use degrees of freedom = 1
///       |> stats.zscore(1)
///       |> should.be_error()
///     
///       [1., 2., 3.]
///       // Use degrees of freedom = 1
///       |> stats.zscore(1)
///       |> should.equal(Ok([-1., 0., 1.]))
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn zscore(arr: List(Float), ddof: Int) -> Result(List(Float), String) {
  case arr {
    [] ->
      "Invalid input argument: The list is empty."
      |> Error
    _ ->
      case ddof < 0 {
        True ->
          "Invalid input argument: ddof < 0. Valid input is ddof >= 0."
          |> Error
        False -> {
          assert Ok(mean) = mean(arr)
          assert Ok(stdev) = std(arr, ddof)
          arr
          |> list.map(fn(a: Float) -> Float { { a -. mean } /. stdev })
          |> Ok
        }
      }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Determine if a list of values are close to or equivalent to a 
/// another list of reference values.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_stats/stats
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
///       stats.allclose(xarr, yarr, rtol, atol)
///       |> fn(zarr: Result(List(Bool), String)) -> Result(Bool, Nil) {
///         case zarr {
///           Ok(arr) ->
///             arr
///             |> list.all(fn(a: Bool) -> Bool { a })
///             |> Ok
///           _ -> Nil |> Error
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
) -> Result(List(Bool), String) {
  let xlen: Int = list.length(xarr)
  let ylen: Int = list.length(yarr)
  case xlen == ylen {
    False ->
      "Invalid input argument: length(xarr) != length(yarr). Valid input is when length(xarr) == length(yarr)."
      |> Error
    True ->
      list.zip(xarr, yarr)
      |> list.map(fn(z: #(Float, Float)) -> Bool {
        isclose(pair.first(z), pair.second(z), rtol, atol)
      })
      |> Ok
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Returns the maximum value of a list. 
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_stats/stats
///
///     pub fn example () {
///       // An empty lists returns an error
///       []
///       |> stats.amax()
///       |> should.be_error()
///
///       // Valid input returns a result
///       [4., 4., 3., 2., 1.]
///       |> stats.amax()
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
pub fn amax(arr: List(Float)) -> Result(Float, String) {
  case arr {
    [] ->
      "Invalid input argument: The list is empty."
      |> Error
    _ -> {
      assert Ok(val0) = list.at(arr, 0)
      arr
      |> list.fold(
        val0,
        fn(acc: Float, a: Float) {
          case a >. acc {
            True -> a
            False -> acc
          }
        },
      )
      |> Ok
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Returns the minimum value of a list. 
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_stats/stats
///
///     pub fn example () {
///       // An empty lists returns an error
///       []
///       |> stats.amin()
///       |> should.be_error()
///     
///       // Valid input returns a result
///       [4., 4., 3., 2., 1.]
///       |> stats.amin()
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
pub fn amin(arr: List(Float)) -> Result(Float, String) {
  case arr {
    [] ->
      "Invalid input argument: The list is empty."
      |> Error
    _ -> {
      assert Ok(val0) = list.at(arr, 0)
      arr
      |> list.fold(
        val0,
        fn(acc: Float, a: Float) {
          case a <. acc {
            True -> a
            False -> acc
          }
        },
      )
      |> Ok
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Returns the indices of the maximum values in a list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_stats/stats
///
///     pub fn example () {
///       // An empty lists returns an error
///       []
///       |> stats.argmax()
///       |> should.be_error()
///     
///       // Valid input returns a result
///       [4., 4., 3., 2., 1.]
///       |> stats.argmax()
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
pub fn argmax(arr: List(Float)) -> Result(List(Int), String) {
  case arr {
    [] ->
      "Invalid input argument: The list is empty."
      |> Error
    _ -> {
      assert Ok(max) =
        arr
        |> amax()
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
      |> Ok
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Returns the indices of the minimum values in a list. 
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_stats/stats
///
///     pub fn example () {
///       // An empty lists returns an error
///       []
///       |> stats.argmin()
///       |> should.be_error()
///     
///       // Valid input returns a result
///       [4., 4., 3., 2., 1.]
///       |> stats.argmin()
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
pub fn argmin(arr: List(Float)) -> Result(List(Int), String) {
  case arr {
    [] ->
      "Invalid input argument: The list is empty."
      |> Error
    _ -> {
      assert Ok(min) =
        arr
        |> amin()
      arr
      |> list.index_map(fn(index: Int, a: Float) -> Int {
        case a -. min {
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
      |> Ok
    }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Determine if a given value is close to or equivalent to a reference value 
/// based on supplied relative and absolute tolerance values. The equivalance
/// of the two given values are then determined based on the equation:
///
/// <center>
///     absolute(a - b) <= (atol + rtol * absolute(b))
/// </center>
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_stats/stats
///
///     pub fn example () {
///       let val: Float = 99.
///       let ref_val: Float = 100.
///       // We set 'atol' and 'rtol' such that the values are equivalent
///       // if 'val' is within 1 percent of 'ref_val' +/- 0.1
///       let rtol: Float = 0.01
///       let atol: Float = 0.10
///       stats.isclose(val, ref_val, rtol, atol)
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
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculcate the sum of the elements in a list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_stats/stats
///
///     pub fn example () {
///       // An empty list returns an error
///       []
///       |> stats.sum()
///       |> should.equal(0.)
///
///       // Valid input returns a result
///       [1., 2., 3.]
///       |> stats.sum()
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
      |> list.fold(0., fn(acc: Float, a: Float) -> Float { a +. acc })
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/gleam_stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Trim a list to a certain size given min/max indices. The min/max indices 
/// are inclusive. 
///
/// <details>
///     <summary>Example:</summary>
///
///     import gleeunit/should
///     import gleam_stats/stats
///
///     pub fn example () {
///       // An empty lists returns an error
///       []
///       |> stats.trim(0, 0)
///       |> should.be_error()
///     
///       // Trim the list to only the middle part of list
///       [1., 2., 3., 4., 5., 6.]
///       |> stats.trim(1, 4)
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
pub fn trim(arr: List(Float), min: Int, max: Int) -> Result(List(Float), String) {
  case arr {
    [] ->
      "Invalid input argument: The list is empty."
      |> Error
    _ ->
      case min >= 0 && max < list.length(arr) {
        False ->
          "Invalid input argument: min < 0 or max < length(arr). Valid input is min > 0 and max < length(arr)."
          |> Error
        True ->
          arr
          |> list.drop(min)
          |> list.take(max - min + 1)
          |> Ok
      }
  }
}
