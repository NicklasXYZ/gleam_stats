import * as $list from "../../gleam_stdlib/dist/gleam/list.mjs";
import * as $pair from "../../gleam_stdlib/dist/gleam/pair.mjs";
import * as $uniform from "./uniform.mjs";
import {
    Ok,
    Error,
    throwError
} from "./gleam.mjs";

// NOTE: Input 'lambda' and 'k' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a JavaScript 'bool' value.
function check_geometric_parameters(p) {
    if ((0.0 < p) && (p <= 1.0)) {
        return new Ok(true);
    } else {
        let _pipe = "Invalid input argument: p <= 0 or p > 1. Valid input is 0 < p <= 1.";
        return new Error(_pipe);
    }
}

// NOTE: Input 'x' and 'p' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a JavaScript 'Number' value.
export function geometric_pmf(x, p) {
    let check = check_geometric_parameters(p);
    if (!check.isOk()) {
        return new Error(check[0]);
    } else {
        if (x >= 0) {
            let _pipe = Math.pow(1.0 - p, x) * p;
            return new Ok(_pipe);
        } else {
            return new Ok(0.0);
        }
    }
}

// NOTE: Input 'x' and 'p' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a JavaScript 'Number' value.
export function geometric_cdf(x, p) {
    let check = check_geometric_parameters(p);
    if (!check.isOk()) {
        return new Error(check);
    } else {
        if (x >= 0) {
            let _pipe = 1.0 - Math.pow(1.0 - p, x + 1.0);
            return new Ok(_pipe);
        } else {
            return new Ok(0.0);
        }
    }
}

// NOTE: Input 'p' and and 'm' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a list containing JavaScript 'Number' values and 
//       a 'BigInt' value representing the current state of the random number generator.
export function geometric_random(stream, p, m) {
    let check = check_geometric_parameters(p);
    if (!check.isOk()) {
        return new Error(check[0]);
    } else {
        if (!(m > 0)) {
            let _pipe = "Invalid input arugment: m < 0. Valid input is m > 0.";
            return new Error(_pipe);
        } else {
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
                        let x1 = Math.log(x);
                        let x2 = Math.log(1. - p);
                        return Math.round(Math.floor(x1 / x2));
                    },
                );
            })();
            let result = [numbers, $pair.second(out)];
            return new Ok(result);
        }
    }
}