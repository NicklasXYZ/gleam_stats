import * as $list from "../../gleam_stdlib/dist/gleam/list.mjs";
import * as $pair from "../../gleam_stdlib/dist/gleam/pair.mjs";
import * as $uniform from "./uniform.mjs";
import {
    Ok,
    Error,
    throwError
} from "./gleam.mjs";

// NOTE: Input 'lambda' is expected to be a JavaScript 'Number' value.
// NOTE: Output by the function is a JavaScript 'bool' value.
function check_exponential_parameters(lambda) {
    if (lambda > 0.0) {
        return new Ok(true);
    } else {
        let _pipe = "Invalid input argument: lambda <= 0. Valid input is lambda > 0.";
        return new Error(_pipe);
    }
}

// NOTE: Input 'x' and 'lambda' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a JavaScript 'Number' value.
export function exponential_pdf(x, lambda) {
    let check = check_exponential_parameters(lambda);
    if (!check.isOk()) {
        return new Error(check[0]);
    } else {
        if (x >= 0.0) {
            let _pipe = lambda * Math.exp((-1.0 * lambda) * x);
            return new Ok(_pipe);
        } else {
            return new Ok(0.0);
        }
    }
}

// NOTE: Input 'x' and 'lambda' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a JavaScript 'Number' value.
export function exponential_cdf(x, lambda) {
    let check = check_exponential_parameters(lambda);
    if (!check.isOk()) {
        return new Error(check[0]);
    } else {
        if (x >= 0.0) {
            let _pipe = 1.0 - Math.exp((-1.0 * lambda) * x);
            return new Ok(_pipe);
        } else {
            return new Ok(0.0);
        }
    }
}

// NOTE: Input 'lambda' and and 'm' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a list containing JavaScript 'Number' values and 
//       a 'BigInt' value representing the current state of the random number generator.
export function exponential_random(stream, lambda, m) {
    let check = check_exponential_parameters(lambda);
    if (!check.isOk()) {
        return new Error(check[0]);
    } else {
        if (!(m > 0)) {
            return new Error("Invalid input arugment: m < 0. Valid input is m > 0.");
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
                        return (1. / (-1. * lambda)) * x1;
                    },
                );
            })();
            let result = [numbers, $pair.second(out)];
            return new Ok(result);
        }
    }
}