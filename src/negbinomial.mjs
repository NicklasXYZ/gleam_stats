import * as $list from "../../gleam_stdlib/dist/gleam/list.mjs";
import * as $pair from "../../gleam_stdlib/dist/gleam/pair.mjs";
import * as $geometric from "./geometric.mjs";
import * as $bignumber_math from "./bignumber_math.mjs";
import {
    Ok,
    Error,
    throwError
} from "./gleam.mjs";
import {
    Decimal
} from './decimal.mjs';

// NOTE: Input 'd' is expected to be a JavaScript 'Number' value.
// NOTE: Output by the function is a JavaScript 'bool' value.
function check_negbinomial_parameters(r, p) {
    if (r > 0n) {
        if ((0.0 <= p) && (p <= 1.0)) {
            let _pipe = true;
            return new Ok(_pipe);
        } else {
            let _pipe = "Invalid input argument: p < 0 or p > 1. Valid input is 0 <= p <= 1.";
            return new Error(_pipe);
        }
    } else {
        let _pipe = "Invalid input argument: r <= 0. Valid input is r > 0.";
        return new Error(_pipe);
    }
}

// NOTE: Input 'x', 'r' and 'p' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a JavaScript 'Number' value.
export function negbinomial_pmf(x, r, p) {
    // Convert 'x' and 'r' to JavaScript 'BigInt' values so integer overflow does not happen
    // in the following computations 
    let _x = BigInt(x)
    let _r = BigInt(r)
    let check = check_negbinomial_parameters(_r, p);
    if (!check.isOk()) {
        return new Error(check[0]);
    } else {
        if ((_x >= 0n) && (((_x + _r) - 1n) > 0n)) {
            // NOTE: Variable 'combination' is a JavaScript 'BigInt' value and the function
            //       expects 'BigInt' values
            let combination = $bignumber_math._bigint_combination(_x + _r - 1n, _x);
            if (!combination.isOk()) {
                throwError("Error: The calculation of the binomial coefficient resultet in an error.", {
                    value: combination
                });
            }
            let _combination = combination[0];
            let v1 = new Decimal(1.0 - p).pow(_r.toString());
            let v2 = new Decimal(p).pow(_x.toString());
            let v3 = new Decimal(_combination.toString()).times(v1).times(v2);
            // Downcast the resulting 'Decimal' value to a JavaScript 'Number' value 
            return new Ok(Number(v3))
        } else {
            return new Ok(0.0);
        }
    }
}

// NOTE: Input 'x', 'r' and 'p' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a JavaScript 'Number' value.
export function negbinomial_cdf(x, r, p) {
    // Convert 'x' and 'r' to JavaScript 'BigInt' values so integer overflow does not happen
    // in the following computations 
    let _x = BigInt(x)
    let _r = BigInt(r)
    let check = check_negbinomial_parameters(_r, p);
    if (!check.isOk()) {
        return new Error(check[0]);
    } else {
        if (_x < 0n) {
            return new Ok(0.0);
        } else {
            if ((_x >= 0n) && (((_x + _r) - 1n) > 0n)) {
                // Create list of JavaScipt 'Number' values
                let range = $list.range(0, x + 1);
                let result = $list.fold(
                    range,
                    new Decimal("0.0"),
                    (acc, i) => {
                        // 'i' is a JavaScript 'Number' value. Convert 'i' to a 'BigInt' value
                        let _i = BigInt(i)
                        // NOTE: Variable 'combination' is a JavaScript 'BigInt' value and the function
                        //       expects 'BigInt' values
                        let combination = $bignumber_math._bigint_combination((_i + _r) - 1n, _i);
                        if (!combination.isOk()) {
                            throwError("Error: Calculation of the binomial coefficient resultet in an error.", {
                                value: combination
                            });
                        }
                        let _combination = combination[0];
                        let v1 = new Decimal(1.0 - p).pow(_r.toString());
                        let v2 = new Decimal(p).pow(_i.toString());
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

// NOTE: Input 'r', 'p' and 'm' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a list containing JavaScript 'Number' values and 
//       a 'BigInt' value representing the current state of the random number generator.

export function negbinomial_random(stream, r, p, m) {
    // Convert 'r' to a JavaScript 'BigInt' value
    let _r = BigInt(r)
    let check = check_negbinomial_parameters(_r, p);
    if (!check.isOk()) {
        return new Error(check[0]);
    } else {
        if (!(m > 0)) {
            let _pipe = "Invalid input arugment: m < 0. Valid input is m > 0.";
            return new Error(_pipe);
        } else {
            let rands = $geometric.geometric_random(stream, p, r * m);
            if (!rands.isOk()) {
                throwError("Error: The intermediate generation of geometric random variables resultet in an error.", {
                    value: rands
                });
            }
            let out = rands[0];
            let numbers = (() => {
                let first = $pair.first(out);
                let window = $list.window(first, r);
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