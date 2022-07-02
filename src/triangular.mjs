import * as $list from "../../gleam_stdlib/dist/gleam/list.mjs";
import * as $pair from "../../gleam_stdlib/dist/gleam/pair.mjs";
import * as $uniform from "./uniform.mjs";
import {
    Ok,
    Error,
    throwError
} from "./gleam.mjs";

// NOTE: Input 'a', 'b' and 'c' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a JavaScript 'bool' value.
function check_triangular_parameters(a, b, c) {
    if (a <= b) {
        if (a <= c) {
            if (c <= b) {
                return new Ok(true);
            } else {
                let _pipe = "Invalid input argument: c > b. Valid input is a <= c <= b.";
                return new Error(_pipe);
            }
        } else {
            let _pipe = "Invalid input argument: a > c. Valid input is a <= c <= b.";
            return new Error(_pipe);
        }
    } else {
        let _pipe = "Invalid input arugment: a > b. Valid input is a <= b.";
        return new Error(_pipe);
    }
}

// NOTE: Input 'x', 'a', 'b' and 'c' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a JavaScript 'Number' value.
export function triangular_pdf(x, a, b, c) {
    let check = check_triangular_parameters(a, b, c);
    if (!check.isOk()) {
        return new Error(check[0]);
    } else {
        if (x < a) {
            return new Ok(0.0);
        } else {
            if ((a <= x) && (x < c)) {
                let _pipe = 2.0 * (x - a) / ((b - a) * (c - a));
                return new Ok(_pipe);
            } else {
                if (x === c) {
                    let _pipe = 2.0 / (b - a);
                    return new Ok(_pipe);
                } else {
                    if ((c < x) && (x <= b)) {
                        let _pipe = 2.0 * (b - x) / ((b - a) * (b - c));
                        return new Ok(_pipe);
                    } else {
                        return new Ok(0.0);
                    }
                }
            }
        }
    }
}

// NOTE: Input 'x', 'a', 'b' and 'c' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a JavaScript 'Number' value.
export function triangular_cdf(x, a, b, c) {
    let check = check_triangular_parameters(a, b, c);
    if (!check.isOk()) {
        return new Error(check[0]);
    } else {
        if (x <= a) {
            return new Ok(0.0);
        } else {
            if ((a < x) && (x <= c)) {
                let _pipe = Math.pow(x - a, 2.0) / ((b - a) * (c - a));
                return new Ok(_pipe);
            } else {
                if ((c < x) && (x < b)) {
                    let _pipe = 1.0 - Math.pow(b - x, 2.0) / ((b - a) * (b - c));
                    return new Ok(_pipe);
                } else {
                    return new Ok(1.0);
                }
            }
        }
    }
}

// NOTE: Input 'a', 'b', 'c' and 'm' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a list containing JavaScript 'Number' values and 
//       a 'BigInt' value representing the current state of the random number generator.

export function triangular_random(stream, a, b, c, m) {
    let check = check_triangular_parameters(a, b, c);
    if (!check.isOk()) {
        return new Error(check[0]);
    } else {
        if (!(m > 0)) {
            return new Error("Invalid input arugment: m < 0. Valid input is m > 0.");
        } else {
            let middle = (c - a) / (b - a);
            let rands = $uniform.uniform_random(stream, 0., 1., m);
            if (!rands.isOk()) {
                throwError("Error: The intermediate generation of uniform random variables resultet in an error.", {
                    value: rands
                });
            }
            let out = rands[0];
            let numbers = (() => {
                let first = $pair.first(out);
                return $list.map(
                    first,
                    (x) => {
                        if (x < middle) {
                            return a + Math.pow((x * (b - a)) * (b - c), 0.5);
                        } else {
                            return b - Math.pow(((1. - x) * (b - a)) * (b - c), 0.5);
                        }
                    },
                );
            })();
            let result = [numbers, $pair.second(out)];
            return new Ok(result);
        }
    }
}