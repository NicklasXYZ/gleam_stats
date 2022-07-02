import * as $list from "../../gleam_stdlib/dist/gleam/list.mjs";
import * as $pair from "../../gleam_stdlib/dist/gleam/pair.mjs";
import * as $normal from "./normal.mjs";
import * as $math from "./gleam_stats/math.mjs";
import {
    Ok,
    Error,
    throwError
} from "./gleam.mjs";

// NOTE: Input 'd' is expected to be a JavaScript 'Number' value.
// NOE:  Output by the function is a JavaScript 'bool' value.
function check_chisquared_parameters(d) {
    if (d > 0) {
        return new Ok(true);
    } else {
        let _pipe = "Invalid input argument: d < 0. Valid input is d > 0.";
        return new Error(_pipe);
    }
}

// NOTE: Input 'x' and 'd' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a JavaScript 'Number' value.
export function chisquared_pdf(x, d) {
    let check = check_chisquared_parameters(d);
    if (!check.isOk()) {
        return new Error(check[0]);
    } else {
        if (x > 0.0) {
            let expr = d / 2.0;
            let denominator = Math.pow(2.0, expr) * $math.gamma(expr);
            let numerator = Math.pow(x, expr - 1.0) * Math.exp(-1.0 * x / 2.0);
            let _pipe = numerator / denominator;
            return new Ok(_pipe);
        } else {
            return new Ok(0.0);
        }
    }
}

// NOTE: Input 'x' and 'd' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a JavaScript 'Number' value.
export function chisquared_cdf(x, d) {
    let check = check_chisquared_parameters(d);
    if (!check.isOk()) {
        return new Error(check[0]);
    } else {
        if ((x > 0.0) && (d === 1)) {
            let _pipe = chisquared_cdf_helper(x, d);
            return new Ok(_pipe);
        } else {
            if ((x >= 0.0) && (d > 1)) {
                let _pipe = chisquared_cdf_helper(x, d);
                return new Ok(_pipe);
            } else {
                return new Ok(0.0);
            }
        }
    }
}

// NOTE: Input 'x' and 'd' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a JavaScript 'Number' value.
function chisquared_cdf_helper(x, d) {
    let expr = d / 2.0;
    let evaluation = $math.gammainc(expr, x / 2.0);
    if (!evaluation.isOk()) {
        throwError("Error: The calculation of the incomplete gamma function resultet in an error.", {
            value: evaluation
        });
    }
    let numerator = evaluation[0];
    let denominator = $math.gamma(expr);
    return numerator / denominator;
}

// NOTE: Input 'd' and 'm' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a list containing JavaScript 'Number' values and 
//       a 'BigInt' value representing the current state of the random number generator.
export function chisquared_random(stream, d, m) {
    let check = check_chisquared_parameters(d);
    if (!check.isOk()) {
        return new Error(check[0]);
    } else {
        if (!(m > 0)) {
            return new Error("Invalid input arugment: m < 0. Valid input is m > 0.");
        } else {
            let rands = $normal.normal_random(stream, 0.0, 1.0, d * m);
            if (!rands.isOk()) {
                throwError("Error: The intermediate generation of normal random variables resultet in an error.", {
                    value: rands
                });
            }
            let out = rands[0];
            let numbers = (() => {
                let first = $pair.first(out);
                let chunked = $list.sized_chunk(first, d);
                return $list.map(
                    chunked,
                    (x) => {
                        return $list.fold(
                            x,
                            0.0,
                            (acc, a) => {
                                return (a * a) + acc;
                            },
                        );
                    },
                );
            })();
            let result = [numbers, $pair.second(out)];
            return new Ok(result);
        }
    }
}