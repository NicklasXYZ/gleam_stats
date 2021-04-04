//// A module containing several helpful mathematical functions.
////
//// ---
////
//// * **Calculating summary statistics**
////   * [`mean`](#mean)
////   * [`median`](#median)
////   * [`var`](#var)
////   * [`std`](#std)
//// * **Miscellaneous functions**
////   * [`sum`](#sum)
////   * [`isclose`](#isclose)
////   * [`allclose`](#allclose)

import gleam/list
import gleam/int
import gleam/float
import gleam/io

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculcate the sum of the elements in a given list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import stats/math
///
///     pub fn example () {
///         let arr: List(Float) = [1., 2., 3.]
///         let result: Float = math.sum(arr)
///         // Result is 6.
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
  arr
  |> list.fold(0., fn(a: Float, b: Float) -> Float { a +. b })
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculcate the arithmetic mean of the elements in a given list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import stats/math
///
///     pub fn example () {
///         let arr: List(Float) = [1., 2., 3.]
///         let result: Float = math.mean(arr)
///         // Result is 2.
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn mean(arr: List(Float)) -> Float {
  arr
  |> sum()
  |> fn(total: Float) { total /. int.to_float(list.length(arr)) }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculcate the median of the elements in a given list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import stats/math
///
///     pub fn example () {
///         let arr0: List(Float) = [1., 2., 3.]
///         let result0: Float = math.median(arr0)
///         // Result is 2.
///
///         let arr1: List(Float) = [1., 2., 3., 4.]
///         let result1: Float = math.median(arr1)
///         // Result is 2.5
///
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn median(arr: List(Float)) -> Float {
  let count: Int = list.length(arr)
  let mid_point: Int = list.length(arr) / 2
  let tmp: List(Float) = list.sort(arr, float.compare)
  case int.is_odd(count) {
    // If there is an odd number of elements in the list, then take median
    // as the middle value
    True ->
      case list.at(tmp, mid_point) {
        Ok(value0) -> value0
      }
    // If there is an even number of elements in the list, then take median
    // as the mean of the two middle values
    False ->
      case list.at(tmp, mid_point - 1), list.at(tmp, mid_point) {
        Ok(value0), Ok(value1) ->
          [value0, value1]
          |> mean()
      }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculcate the sample variance of the elements in a given list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import stats/math
///
///     pub fn example () {
///         let arr: List(Float) = [1., 2., 3.]
///         let ddof: Int = 1
///         let result: Float = math.var(arr, ddof)
///         // Result is 1.
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn var(arr: List(Float), ddof: Int) -> Float {
  let mean: Float = mean(arr)
  arr
  |> list.map(fn(x: Float) -> Float { float.power(x -. mean, 2.) })
  |> sum()
  |> fn(x: Float) -> Float {
    x /. { int.to_float(list.length(arr)) -. int.to_float(ddof) }
  }
}

/// <div style="text-align: right;">
///     <a href="https://github.com/nicklasxyz/stats/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
/// </div>
///
/// Calculcate the sample standard deviation of the elements in a given list.
///
/// <details>
///     <summary>Example:</summary>
///
///     import stats/math
///
///     pub fn example () {
///         let arr: List(Float) = [1., 2., 3.]
///         let ddof: Int = 1
///         let result: Float = math.std(arr, ddof)
///         // Result is 1.
///     }
/// </details>
///
/// <div style="text-align: right;">
///     <a href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn std(arr: List(Float), ddof: Int) -> Float {
  arr
  |> var(ddof)
  |> fn(x: Float) -> Float {
    case float.square_root(x) {
      Ok(value) -> value
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
///     import stats/math
///     import gleam/should
///
///     pub fn example () {
///         let val: Float = 99.
///         let ref_val: Float = 100.
///         // We set 'atol' and 'rtol' such that the values are equivalent
///         // if 'val' is within 1 percent of 'ref_val' +/- 0.1
///         let rtol: Float = 0.01
///         let atol: Float = 0.10
///         math.isclose(val, ref_val, rtol, atol)
///         |> should.be_true()
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
  io.debug(a)
  io.debug(b)
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
/// reference value.
///
/// <details>
///     <summary>Example:</summary>
///
///     import stats/math
///     import gleam/should
///
///     pub fn example () {
///         let val: Float = 99.
///         let ref_val: Float = 100.
///         // We set 'atol' and 'rtol' such that the values are equivalent if
///         // 'val' is within 1 percent of 'ref_val' +/- 0.1
///         let rtol: Float = 0.01
///         let atol: Float = 0.10
///         math.isclose(val, ref_val, rtol, atol)
///         |> should.be_true()
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
  arr: List(Float),
  b: Float,
  rtol: Float,
  atol: Float,
) -> List(Bool) {
  arr
  |> list.map(fn(x: Float) -> Bool { isclose(x, b, rtol, atol) })
}
