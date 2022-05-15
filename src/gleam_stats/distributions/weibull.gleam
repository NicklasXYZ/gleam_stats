//// Functions related to the continuous weibull random distribution.
////
//// ---
////
//// * **Available Functions**
////   * [`weibull_mean`](#weibull_mean)
////   * [`weibull_variance`](#weibull_variance)
////   * [`weibull_pdf`](#weibull_pdf)
////   * [`weibull_cdf`](#weibull_cdf)
////   * [`weibull_random`](#weibull_random)

import gleam/list
import gleam/iterator.{Iterator}
import gleam/float
import gleam/pair
import gleam_stats/math
import gleam_stats/distributions/uniform

fn check_weibull_parameters(lambda: Float, k: Float) {
  case lambda >. 0.0 && k >. 0.0 {
    False ->
      "Invalid input argument: lambda < 0 or k < 0. Valid input is lambda > 0 and k > 0."
      |> Error
    True ->
      True
      |> Ok
  }
}

pub fn weibull_mean(lambda: Float, k: Float) -> Result(Float, String) {
  case check_weibull_parameters(lambda, k) {
    Error(string) ->
      string
      |> Error
    _ ->
      lambda *. math.gamma(1.0 +. 1.0 /. k)
      |> Ok
  }
}

pub fn weibull_variance(lambda: Float, k: Float) -> Result(Float, String) {
  case check_weibull_parameters(lambda, k) {
    Error(string) ->
      string
      |> Error
    _ ->
      float.power(lambda, 2.0) *. {
        math.gamma(1.0 +. 2.0 /. k) -. float.power(
          math.gamma(1.0 +. 1.0 /. k),
          2.0,
        )
      }
      |> Ok
  }
}

pub fn weibull_pdf(x: Float, lambda: Float, k: Float) -> Result(Float, String) {
  case check_weibull_parameters(lambda, k) {
    Error(string) ->
      string
      |> Error
    _ ->
      case x <. 0.0 {
        True ->
          0.0
          |> Ok
        False ->
          k /. lambda *. float.power(x /. lambda, k -. 1.0) *. math.exp(
            -1.0 *. float.power(x /. lambda, k),
          )
          |> Ok
      }
  }
}

pub fn weibull_cdf(x: Float, lambda: Float, k: Float) -> Result(Float, String) {
  case check_weibull_parameters(lambda, k) {
    Error(string) ->
      string
      |> Error
    _ ->
      case x <. 0.0 {
        True ->
          0.0
          |> Ok
        False ->
          1.0 -. math.exp(-1.0 *. float.power(x /. lambda, k))
          |> Ok
      }
  }
}

pub fn weibull_random(
  stream: Iterator(Int),
  lambda: Float,
  k: Float,
  m: Int,
) -> Result(#(List(Float), Iterator(Int)), String) {
  case check_weibull_parameters(lambda, k) {
    Error(string) ->
      string
      |> Error
    _ ->
      case m > 0 {
        False -> Error("Invalid input arugment: m < 0. Valid input is m > 0.")
        True -> {
          // Take out 'm' integers from the stream of pseudo-random numbers and generate 
          // uniform random numbers.
          assert Ok(out) = uniform.uniform_random(stream, 0., 1., m)
          // Transform the 'm' continuous uniform random numbers to exponential distributed
          // random numbers.
          // Use the inverse CDF of the exponential distribution - i.e. the quantile function
          // for this purpose.
          let numbers: List(Float) =
            pair.first(out)
            |> list.map(fn(x: Float) -> Float {
              lambda *. float.power(-1.0 *. math.log(1.0 -. x), 1.0 /. k)
            })
          #(numbers, pair.second(out))
          |> Ok
        }
      }
  }
}
