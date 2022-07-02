import * as $list from "../../gleam_stdlib/dist/gleam/list.mjs";
import * as $pair from "../../gleam_stdlib/dist/gleam/pair.mjs";
import {
    Ok,
    Error,
    toList,
    throwError
} from "./gleam.mjs";
import * as $uniform from "./uniform.mjs";
import * as $math from "./gleam_stats/math.mjs";

// NOTE: Input 'mu' and 'sigma' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a JavaScript 'bool' value.
function check_normal_parameters(mu, sigma) {
    if (sigma > 0.0) {
        return new Ok(true);
    } else {
        let _pipe = "Invalid input argument: sigma < 0. Valid input is sigma > 0.";
        return new Error(_pipe);
    }
}

// NOTE: Input 'mu' and 'sigma' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a JavaScript 'Number' value.
export function normal_pdf(x, mu, sigma) {
    let check = check_normal_parameters(mu, sigma);
    if (!check.isOk()) {
        return new Error(check[0]);
    } else {
        let numexp = Math.pow(x - mu, 2.0) / (2.0 * Math.pow(sigma, 2.0));
        let denominator = sigma * Math.pow(2.0 * Math.PI, 0.5);
        let numerator = Math.exp(numexp * -1.0);
        let _pipe = numerator / denominator;
        return new Ok(_pipe);
    }
}

// NOTE: Input 'x', 'mu' and 'sigma' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a JavaScript 'Number' value.
export function normal_cdf(x, mu, sigma) {
    let check = check_normal_parameters(mu, sigma);
    if (!check.isOk()) {
        return new Error(check[0]);
    } else {
        let denominator = sigma * Math.pow(2.0, 0.5);
        let _pipe = 0.5 * (1.0 + $math.erf((x - mu) / denominator));
        return new Ok(_pipe);
    }
}

function box_muller(u1, u2) {
    let u = Math.log(u1);
    let x = Math.pow(-2.0 * u, 0.5);
    return toList([x * Math.cos((2.0 * Math.PI) * u2), x * Math.sin((2.0 * Math.PI) * u2)]);
}

// NOTE: Input 'mu', 'sigma' and 'm' are expected to be JavaScript 'Number' values.
// NOTE: Output by the function is a list containing JavaScript 'Number' values and 
//       a 'BigInt' value representing the current state of the random number generator.
export function normal_random(stream, mu, sigma, m) {
    let check = check_normal_parameters(mu, sigma);
    if (!check.isOk()) {
        return new Error(check[0]);
    } else {
        if (!(m > 0)) {
            return new Error("Invalid input arugment: m < 0. Valid input is m > 0.");
        } else {
            let m_even = m + (m % 2);
            let rands = $uniform.uniform_random(stream, 0., 1., m_even);
            if (!rands.isOk()) {
                throwError("Error: The intermediate generation of uniform random variables resultet in an error.", {
                    value: rands
                });
            }
            let out = rands[0];
            let numbers = (() => {
                let first = $pair.first(out);
                let chunked = $list.sized_chunk(first, 2);
                let normal_rands = $list.flatten(
                    $list.map(
                        chunked,
                        (x) => {
                            if (x.hasLength(2)) {
                                let u1 = x.head;
                                let u2 = x.tail.head;
                                let ns = box_muller(u1, u2);
                                if (ns.hasLength(2)) {
                                    let z1 = ns.head;
                                    let z2 = ns.tail.head;
                                    return toList([(sigma * z1) + mu, (sigma * z2) + mu]);
                                } else {
                                    throwError("Error: The list does not have size 2.", {
                                        value: ns
                                    });
                                }
                            } else {
                                throwError("Error: The list does not have size 2.", {
                                    value: x
                                });
                            }
                        },
                    )
                );
                return ((x) => {
                    if (m === $list.length(x)) {
                        return x;
                    } else {
                        return $list.drop(x, 1);
                    }
                })(normal_rands);
            })();
            let result = [numbers, $pair.second(out)];
            return new Ok(result);
        }
    }
}