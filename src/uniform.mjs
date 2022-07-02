import * as $list from "../../gleam_stdlib/dist/gleam/list.mjs";
import * as $pair from "../../gleam_stdlib/dist/gleam/pair.mjs";
import {
    Ok,
    Error,
    throwError
} from "./gleam.mjs";
import {
    mask_32,
    take_randints
} from "./gleam_stats/generators.mjs";

// NOTE: Input 'a' and 'b' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a JavaScript 'bool' value.
function check_uniform_parameters(a, b) {
    if (a <= b) {
        return new Ok(true);
    } else {
        let _pipe = "Invalid input arugment: a > b. Valid input is a <= b.";
        return new Error(_pipe);
    }
}

// NOTE: Input 'x', 'a' and 'b' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a JavaScript 'Number' value.
export function uniform_pdf(x, a, b) {
    let check = check_uniform_parameters(a, b);
    if (!check.isOk()) {
        return new Error(check[0]);
    } else {
        if ((x >= a) && (x <= b)) {
            return new Ok(1.0 / (b - a));
        } else {
            return new Ok(0.0);
        }
    }
}

// NOTE: Input 'x', 'a' and 'b' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a JavaScript 'Number' value.
export function uniform_cdf(x, a, b) {
    let check = check_uniform_parameters(a, b);
    if (!check.isOk()) {
        return new Error(check[0]);
    } else {
        if (x < a) {
            return new Ok(0.0);
        } else {
            if ((x >= a) && (x <= b)) {
                return new Ok((x - a) / (b - a));
            } else {
                return new Ok(1.0);
            }
        }
    }
}

// NOTE: Input 'a', 'b' and 'm' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a list containing JavaScript 'Number' values and 
//       a 'BigInt' value representing the current state of the random number generator.
export function uniform_random(stream, a, b, m) {
    let check = check_uniform_parameters(a, b);
    if (!check.isOk()) {
        return new Error(check[0]);
    } else {
        if (!(m > 0)) {
            return new Error("Invalid input arugment: m < 0. Valid input is m > 0.");
        } else {
            let rands = take_randints(stream, m);
            if (!rands.isOk()) {
                throwError("Error: The intermediate generation of random integer values resultet in an error.", {
                    value: rands
                });
            }
            let out = rands[0];
            let numbers = (() => {
                let first = $pair.first(out);
                return $list.map(
                    first,
                    (x) => {
                        return a + (x * (b - a) / mask_32);
                    },
                );
            })();
            let result = [numbers, $pair.second(out)];
            return new Ok(result);
        }
    }
}