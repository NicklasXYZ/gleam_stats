import * as $list from "../../gleam_stdlib/dist/gleam/list.mjs";
import * as $pair from "../../gleam_stdlib/dist/gleam/pair.mjs";
import * as $bernoulli from "./bernoulli.mjs";
import * as $bignumber_math from "./bignumber_math.mjs";
import {
    Ok,
    Error,
    throwError
} from "./gleam.mjs";
import {
    Decimal
} from './decimal.mjs';

// NOTE: Input 'n' is expected to be a JavaScript 'BigInt' value.
// NOTE: Input 'p' is expected to be a JavaScript 'Number' value.
// NOTE: Output by the function is a JavaScript 'bool' value.
function check_binomial_parameters(n, p) {
    if (n >= 0n) {
        if ((0.0 <= p) && (p <= 1.0)) {
            return new Ok(true);
        } else {
            let _pipe = "Invalid input argument: p < 0 or p > 1. Valid input is 0 <= p <= 1.";
            return new Error(_pipe);
        }
    } else {
        let _pipe = "Invalid input argument: n < 0. Valid input is n >= 0.";
        return new Error(_pipe);
    }
}

// NOTE: Input 'x', 'n' and 'p' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a JavaScript 'Number' value.
export function binomial_pmf(x, n, p) {
    // Convert 'x' and 'n' to JavaScript 'BigInt' values so integer overflow does not happen
    // in the following computations 
    let _x = BigInt(x)
    let _n = BigInt(n)
    let check = check_binomial_parameters(_n, p);
    if (!check.isOk()) {
        return new Error(check[0]);
    } else {
        if ((x >= 0n) && (x <= n)) {
            // NOTE: Variable 'combination' is a JavaScript 'BigInt' value and the function
            //       expects 'BigInt' values
            let combination = $bignumber_math._bigint_combination(_n, _x);
            if (!combination.isOk()) {
                throwError("Error: The calculation of the binomial coefficient resultet in an error.", {
                    value: combination
                });
            }
            let _combination = combination[0];
            let v1 = new Decimal(p).pow(_x.toString());
            let v2 = new Decimal(1.0 - p).pow((_n - _x).toString());
            let v3 = new Decimal(_combination.toString()).times(v1).times(v2);
            // Downcast the resulting 'Decimal' value to a JavaScript 'Number' value 
            return new Ok(Number(v3));
        } else {
            return new Ok(0.0);
        }
    }
}

// NOTE: Input 'x', 'n' and 'p' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a JavaScript 'Number' value.
export function binomial_cdf(x, n, p) {
    // Convert 'x' and 'n' to JavaScript 'BigInt' values so integer overflow does not happen
    // in the following computations 
    let _x = BigInt(x)
    let _n = BigInt(n)
    let check = check_binomial_parameters(_n, p);
    if (!check.isOk()) {
        return new Error(check[0]);
    } else {
        if (_x < 0n) {
            return new Ok(0.0);
        } else {
            if ((_x >= 0) && (_x < _n)) {
                // Create list of JavaScipt 'Number' values
                let range = $list.range(0, x + 1);
                let result = $list.fold(
                    range,
                    new Decimal("0.0"),
                    (acc, i) => {
                        // 'i' is a JavaScript 'Number' value. Convert 'i' to a 'BigInt' value
                        let _i = BigInt(i);
                        // NOTE: Variable 'combination' is a JavaScript 'BigInt' value and the function
                        //       expects 'BigInt' values
                        let combination = $bignumber_math._bigint_combination(_n, _i);
                        if (!combination.isOk()) {
                            throwError("Error: The calculation of the binomial coefficient resultet in an error.", {
                                value: combination
                            });
                        }
                        let _combination = combination[0];
                        let v1 = new Decimal(p).pow(_i.toString());
                        let v2 = new Decimal(1.0 - p).pow((_n - _i).toString());
                        let v3 = new Decimal(_combination.toString()).times(v1).times(v2);
                        return acc.add(v3);
                    },
                );
                // Downcast the resulting 'Decimal' value to a JavaScript 'Number' value 
                return new Ok(Number(result));
            } else {
                return new Ok(1.0);
            }
        }
    }
}

// NOTE: Input 'n', 'p' and 'm' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a list containing JavaScript 'Number' values and 
//       a 'BigInt' value representing the current state of the random number generator.
export function binomial_random(stream, n, p, m) {
    // Convert 'x' and 'r' to JavaScript 'BigInt' values
    let _n = BigInt(n)
    let check = check_binomial_parameters(_n, p);
    if (!check.isOk()) {
        return new Error(check[0]);
    } else {
        if (!(m > 0)) {
            let _pipe = "Invalid input arugment: m < 0. Valid input is m > 0.";
            return new Error(_pipe);
        } else {
            let rands = $bernoulli.bernoulli_random(stream, p, n * m);
            if (!rands.isOk()) {
                throwError("Error: The intermediate generation of Bernoulli random variables resultet in an error.", {
                    value: rands
                });
            }
            let out = rands[0];
            let numbers = (() => {
                let first = $pair.first(out);
                let window = $list.window(first, n);
                return $list.map(
                    window,
                    (x) => {
                        return $list.fold(x, 0, (a, b) => {
                            return a + b;
                        });
                    },
                );
            })();
            let result = [numbers, $pair.second(out)];
            return new Ok(result);
        }
    }
}