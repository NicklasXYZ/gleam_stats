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
function check_weibull_parameters(lambda, k) {
    if ((lambda > 0.0) && (k > 0.0)) {
        let _pipe = true;
        return new Ok(_pipe);
    } else {
        let _pipe = "Invalid input argument: lambda < 0 or k < 0. Valid input is lambda > 0 and k > 0.";
        return new Error(_pipe);
    }
}

// NOTE: Input 'x', 'lambda' and 'k' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a JavaScript 'Number' value.
export function weibull_pdf(x, lambda, k) {
    let check = check_weibull_parameters(lambda, k);
    if (!check.isOk()) {
        return new Error(check[0]);
    } else {
        if (x < 0.0) {
            return new Ok(0.0);
        } else {
            let x1 = x / lambda;
            let _pipe = k / lambda * Math.pow(x1, k - 1.0) * Math.exp(-1.0 * Math.pow(x1, k));
            return new Ok(_pipe);
        }
    }
}

// NOTE: Input 'x', 'lambda' and 'k' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a JavaScript 'Number' value.
export function weibull_cdf(x, lambda, k) {
    let check = check_weibull_parameters(lambda, k);
    if (!check.isOk()) {
        return new Error(check[0]);
    } else {
        if (x < 0.0) {
            return new Ok(0.0);
        } else {
            let _pipe = 1.0 - Math.exp(-1.0 * Math.pow(x / lambda, k));
            return new Ok(_pipe);
        }
    }
}

// NOTE: Input 'lambda', 'k' and 'm' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a list containing JavaScript 'Number' values and 
//       a 'BigInt' value representing the current state of the random number generator.
export function weibull_random(stream, lambda, k, m) {
    let check = check_weibull_parameters(lambda, k);
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
                        let x1 = Math.log(1.0 - x);
                        return lambda * Math.pow(-1.0 * x1, 1.0 / k);
                    },
                );
            })();
            let result = [numbers, $pair.second(out)];
            return new Ok(result);
        }
    }
}