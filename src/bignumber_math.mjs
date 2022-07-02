import * as $list from "../../gleam_stdlib/dist/gleam/list.mjs";
import { Ok, Error } from "./gleam.mjs";

// NOTE: Input 'n' and 'k' are expected to be JavaScript 'BigInt' values
export function _bigint_combination(n, k) {
    if (!(typeof n === "bigint") || !(typeof k == "bigint")) {
        let _pipe = "Invalid input argument: n and/or k. Valid input is when n, k are BitInt primitives.";
        return new Error(_pipe);
    } else {
        if (n < 0n) {
            let _pipe = "Invalid input argument: n < 0. Valid input is n > 0.";
            return new Error(_pipe);
        } else {
            if ((k < 0n) || (k > n)) {
                // Return a JavaScript 'BigInt' value
                return new Ok(0n);
            } else {
                if ((k === 0n) || (k === n)) {
                    return new Ok(1n);
                } else {
                    let min = n - k;
                    if (k < n - k) {
                        min = k;
                    }
                    // Create list of JavaScipt 'Number' values 
                    let range = $list.range(1, Number(min) + 1);
                    let result = $list.fold(
                        range,
                        1n,
                        (acc, x) => {
                            // 'x' is a JavaScript 'Number' value. Convert 'x' to a 'BigInt' value
                            let _x = BigInt(x)
                            return (acc * (n + 1n - _x)) / _x;
                        },
                    );
                    // Return a JavaScript 'BigInt' value
                    return new Ok(result);
                }
            }
        }
    }
}