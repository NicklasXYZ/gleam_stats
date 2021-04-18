import gleam/should
import gleam/io
import gleam/pair
import gleam/list
import gleam/iterator.{Iterator}
import gleam/float
import gleam/int
import gleam/result
import gleam_stats/generators
import gleam_stats/rand
import gleam_stats/math

pub fn next_uniform_test() {
  // MEAN     : (min + max) / 2
  // VARIANCE : 1 / 12 * (max - min)^2
  let rtol: Float = 0.05
  let atol: Float = 0.05
  let min: Float = 0.
  let max: Float = 1.
  let mean: Float = { min +. max } /. 2.
  let variance: Float = float.power(max -. min, 2.) /. 12.
  let out: tuple(List(Float), Iterator(Int)) =
    generators.seed_mt19937(5)
    |> rand.next_uniform(min, max, 5_000)
  // Make sure the uniform random numbers are within the given
  // min/max bounds
  pair.first(out)
  |> list.all(fn(x) {
    case x <. max, x >=. min {
      True, True -> True
      _, _ -> False
    }
  })
  |> should.be_true()
  // Make sure the mean of the uniform random numbers
  // is close to the analytically calculated mean
  pair.first(out)
  |> math.mean()
  |> result.unwrap(-9999.)
  |> fn(x) { math.isclose(x, mean, rtol, atol) }
  |> should.be_true()
  // Make sure the variance of the uniform random numbers
  // is close to the analytically calculated variance
  pair.first(out)
  |> math.var(1)
  |> result.unwrap(-9999.)
  |> fn(x) { math.isclose(x, variance, rtol, atol) }
  |> should.be_true()
}

pub fn next_normal_test() {
  // MEAN     : mean
  // VARIANCE : sigma^2
  let rtol: Float = 0.05
  let atol: Float = 0.05
  let mean: Float = 0.
  let variance: Float = 1.
  let out: tuple(List(Float), Iterator(Int)) =
    generators.seed_mt19937(5)
    |> rand.next_normal(mean, variance, 5_000)
  // Make sure the mean of the uniform random numbers
  // is close to the analytically calculated mean
  pair.first(out)
  |> math.mean()
  |> result.unwrap(-9999.)
  |> fn(x) { math.isclose(x, mean, rtol, atol) }
  |> should.be_true()
  // Make sure the variance of the uniform random numbers
  // is close to the analytically calculated variance
  pair.first(out)
  |> math.var(1)
  |> result.unwrap(-9999.)
  |> fn(x) { math.isclose(x, variance, rtol, atol) }
  |> should.be_true()
}

pub fn next_randint_test() {
  // MEAN     : (min + max) / 2
  // VARIANCE : 1 / 12 * (max - min)^2
  let rtol: Float = 0.05
  let atol: Float = 0.05
  let min: Float = 0.
  let max: Float = 10.
  let mean: Float = { min +. max } /. 2.
  let variance: Float = { float.power(max -. min +. 1., 2.) -. 1. } /. 12.
  let out: tuple(List(Int), Iterator(Int)) =
    generators.seed_mt19937(5)
    |> rand.next_randint(float.round(min), float.round(max), 5_000)
  pair.first(out)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  // Make sure the discrete uniform random numbers are within the given
  // min and max bounds
  pair.first(out)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> list.all(fn(x) {
    case x <=. max, x >=. min {
      True, True -> True
      _, _ -> False
    }
  })
  |> should.be_true()
  // Make sure the mean of the discrete uniform random numbers
  // is close to the analytically calculated mean
  pair.first(out)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.mean()
  |> result.unwrap(-9999.)
  |> fn(x) { math.isclose(x, mean, rtol, atol) }
  |> should.be_true()
  // Make sure the variance of the discrete uniform random numbers
  // is close to the analytically calculated variance
  pair.first(out)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.var(1)
  |> result.unwrap(-9999.)
  |> fn(x) { math.isclose(x, variance, rtol, atol) }
  |> should.be_true()
}

pub fn next_bern_test() {
  // p: Success probability for each trial
  // MEAN     : p
  // VARIANCE : p * (1 - p) 
  let rtol: Float = 0.05
  let atol: Float = 0.05
  let min: Float = 0.
  let max: Float = 1.
  let p: Float = 0.5
  let mean: Float = p
  let variance: Float = p *. { 1. -. p }
  let out: tuple(List(Int), Iterator(Int)) =
    generators.seed_mt19937(5)
    |> rand.next_bern(p, 5_000)
  // Make sure the bernoulli random numbers are 0 or 1
  pair.first(out)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> list.all(fn(x) {
    case x <=. max, x >=. min {
      True, True -> True
      _, _ -> False
    }
  })
  |> should.be_true()
  // Make sure the mean of the bernoulli random numbers
  // is close to the analytically calculated mean
  pair.first(out)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.mean()
  |> result.unwrap(-9999.)
  |> fn(x) { math.isclose(x, mean, rtol, atol) }
  |> should.be_true()
  // Make sure the variance of the bernoulli random numbers
  // is close to the analytically calculated variance
  pair.first(out)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.var(1)
  |> result.unwrap(-9999.)
  |> fn(x) { math.isclose(x, variance, rtol, atol) }
  |> should.be_true()
}

pub fn next_binom_test() {
  // n: Number of trials
  // p: Success probability for each trial
  // MEAN     : n * p
  // VARIANCE : n * p * (1 - p)
  let rtol: Float = 0.05
  let atol: Float = 0.05
  let n: Float = 5.
  let p: Float = 0.5
  let mean: Float = n *. p
  let variance: Float = n *. p *. { 1. -. p }
  let out: tuple(List(Int), Iterator(Int)) =
    generators.seed_mt19937(5)
    |> rand.next_binom(p, float.round(n), 5_000)
  // Make sure the mean of the binomial random numbers
  // is close to the analytically calculated mean
  pair.first(out)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.mean()
  |> result.unwrap(-9999.)
  |> fn(x) { math.isclose(x, mean, rtol, atol) }
  |> should.be_true()
  // Make sure the variance of the binomial random numbers
  // is close to the analytically calculated variance
  pair.first(out)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.var(1)
  |> result.unwrap(-9999.)
  |> fn(x) { math.isclose(x, variance, rtol, atol) }
  |> should.be_true()
}

pub fn next_negbinom_test() {
  // r = n: Number of failures until the experiment is stopped
  // p: Success probability for each trial
  // MEAN     : r * p / (1 - p)
  // VARIANCE : r * p / (1 - p)^2
  let rtol: Float = 0.05
  let atol: Float = 0.05
  let r: Float = 5.
  let p: Float = 0.5
  let mean: Float = r *. p /. { 1. -. p }
  let variance = r *. p /. float.power(1. -. p, 2.)
  let out: tuple(List(Int), Iterator(Int)) =
    generators.seed_mt19937(5)
    |> rand.next_negbinom(p, float.round(r), 5_000)
  // Make sure the mean of the binomial random numbers
  // is close to the analytically calculated mean
  pair.first(out)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.mean()
  |> result.unwrap(-9999.)
  |> fn(x) { math.isclose(x, mean, rtol, atol) }
  |> should.be_true()
  // Make sure the variance of the binomial random numbers
  // is close to the analytically calculated variance
  pair.first(out)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.var(1)
  |> result.unwrap(-9999.)
  |> fn(x) { math.isclose(x, variance, rtol, atol) }
  |> should.be_true()
}

pub fn next_geom_test() {
  // p: Success probability for each trial
  // MEAN     : (1 - p) / p
  // VARIANCE : (1 - p) / p^2
  let rtol: Float = 0.05
  let atol: Float = 0.05
  let p: Float = 0.5
  let mean: Float = { 1. -. p } /. p
  let variance = { 1. -. p } /. float.power(p, 2.)
  let out: tuple(List(Int), Iterator(Int)) =
    generators.seed_mt19937(5)
    |> rand.next_geom(p, 5_000)
  // Make sure the mean of the binomial random numbers
  // is close to the analytically calculated mean
  pair.first(out)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.mean()
  |> result.unwrap(-9999.)
  |> fn(x) { math.isclose(x, mean, rtol, atol) }
  |> should.be_true()
  // Make sure the variance of the binomial random numbers
  // is close to the analytically calculated variance
  pair.first(out)
  |> list.map(fn(x: Int) -> Float { int.to_float(x) })
  |> math.var(1)
  |> result.unwrap(-9999.)
  |> fn(x) { math.isclose(x, variance, rtol, atol) }
  |> should.be_true()
}

pub fn next_exp_test() {
  // lambda: rate
  // MEAN     : 1 / lamnda
  // VARIANCE : 1 / lambda^2
  let rtol: Float = 0.05
  let atol: Float = 0.05
  let lambda: Float = 0.5
  let mean: Float = 1. /. lambda
  let variance = 1. /. float.power(lambda, 2.)
  let out: tuple(List(Float), Iterator(Int)) =
    generators.seed_mt19937(5)
    |> rand.next_exp(lambda, 5_000)
  // Make sure the mean of the exponential random numbers
  // is close to the analytically calculated mean
  pair.first(out)
  |> math.mean()
  |> result.unwrap(-9999.)
  |> fn(x) { math.isclose(x, mean, rtol, atol) }
  |> should.be_true()
  // Make sure the variance of the exponential random numbers
  // is close to the analytically calculated variance
  pair.first(out)
  |> math.var(1)
  |> result.unwrap(-9999.)
  |> fn(x) { math.isclose(x, variance, rtol, atol) }
  |> should.be_true()
}
