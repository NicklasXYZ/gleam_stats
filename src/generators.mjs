import * as $iterator from "../../gleam_stdlib/dist/gleam/iterator.mjs";
import * as $pair from "../../gleam_stdlib/dist/gleam/pair.mjs";
import {
    Ok,
    Error,
    CustomType
} from "./gleam.mjs";
import { Next } from "../../gleam_stdlib/dist/gleam/iterator.mjs";

const mask_64 = 18446744073709551615n
const mask_32 = 4294967295n

function and(x, y) {
    return x & y;
}

function not(x) {
    return ~x;
}

function or(x, y) {
    return x | y;
}

function exclusive_or(x, y) {
    return x ^ y;
}

function shift_left(x, y) {
    return x << y;
}

function shift_right(x, y) {
    return x >> y;
}

export function uint64(bigint) {
    return bigint & mask_64
}
export function uint32(bigint) {
    return bigint & mask_32
}

class PermutedCongruentialGenerator extends CustomType {
    constructor(int_1, int_18, int_27, int_59, int_31, multiplier) {
        super();
        this.int_1 = int_1;
        this.int_18 = int_18;
        this.int_27 = int_27;
        this.int_59 = int_59;
        this.int_31 = int_31;
        this.multiplier = multiplier;
    }
}

const pcg32 = new PermutedCongruentialGenerator(
    1n,
    18n,
    27n,
    59n,
    31n,
    6364136223846793005n,
);


class LinearCongruentialGenerator extends CustomType {
    constructor(a, c) {
        super();
        this.a = a;
        this.c = c;
    }
}

const lcg32 = new LinearCongruentialGenerator(1664525n, 1013904223n);

export function pcg32_next_rn(state, pcg) {
    let old_state = $pair.first(state);
    let xorshifted = uint32(
        shift_right(
            exclusive_or(
                shift_right(old_state, pcg.int_18),
                old_state,
            ),
            pcg.int_27,
        ),
    );
    let rotation = uint32(shift_right(old_state, pcg.int_59));
    return uint32(
        or(
            shift_right(xorshifted, rotation),
            shift_left(
                xorshifted,
                and(-1n * rotation, pcg.int_31),
            ),
        ),
    );
}

export function pcg32_init(seed, seq, pcg) {
    let _pipe = [
        0n,
        or(uint64(shift_left(seq, pcg32.int_1)), pcg.int_1),
    ];
    let _pipe$1 = pcg32_next_state(_pipe, pcg);
    let _pipe$2 = ((state) => {
        let s = state[0];
        let i = state[1];
        return [uint64(s + seed), i];
    })(_pipe$1);
    return pcg32_next_state(_pipe$2, pcg);
}

export function pcg32_next_state(state, pcg) {
    let s = state[0];
    let i = state[1];
    return [uint64(s * pcg.multiplier) + i, i];
}

export function seed_pcg32(seed, seq) {
    // Convert 'seed' and 'seq' to JavaScript 'BigInt' values so integer overflow does not happen
    // in the following computations 
    let _seed = BigInt(seed)
    let _seq = BigInt(seq)
    let pcg = pcg32;
    let _pipe = pcg32_init(uint64(_seed), uint64(_seq), pcg);
    return $iterator.unfold(
        _pipe,
        (state) => {
            let next_rn = pcg32_next_rn(state, pcg);
            let next_state = pcg32_next_state(state, pcg);
            // Downcast 'BigInt' value to 'Number' value. The PCG32 generator 
            // produce 32bit unsigned integer values which can be handled by the Number value.
            // Only the state needs to remain a 'BigInt' value.
            return new Next(Number(next_rn), next_state);
        },
    );
}

export function lcg32_init(seed, lcg) {
    return and((lcg.a * seed) + lcg.c, mask_32);
}

export function lcg32_next_state(state, lcg) {
    return and((lcg.a * state) + lcg.c, mask_32);
}

export function seed_lcg32(seed) {
    // Convert 'seed' to a JavaScript 'BigInt' value so integer overflow does not happen
    // in the following computations 
    let _seed = BigInt(seed)
    let lcg = lcg32;
    let _pipe = lcg32_init(_seed, lcg);
    return $iterator.unfold(
        _pipe,
        (state) => {
            let next_state = lcg32_next_state(state, lcg);
            // Downcast 'BigInt' value to 'Number' value. The LCG32 generator 
            // produce 32bit unsigned integer values which can be handled by the Number value.
            // Only the state needs to remain a 'BigInt' value.
            return new Next(Number(next_state), next_state);
        },
    );
}

export function take_randints(stream, m) {
    if (m > 0) {
        let numbers = (() => {
            let _pipe = stream;
            let _pipe$1 = $iterator.take(_pipe, m);
            return $iterator.to_list(_pipe$1);
        })();
        let _pipe = [numbers, $iterator.drop(stream, m)];
        return new Ok(_pipe);
    } else {
        return new Error("Invalid input arugment: m < 0. Valid input is m > 0.");
    }
}