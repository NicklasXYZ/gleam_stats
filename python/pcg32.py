from typing import (
    List,
)

MASK32 = 2 ** 32 - 1
MASK64 = 2 ** 64 - 1

class PermutedCongruentialGenerator:
    """PCG32 reference implementation"""

    def __init__(self, state = 0x853c49e6748fea9b, increment = 0xda3e39cb94b95bdb) -> None:
        self.state = [state & MASK64, increment & MASK64]
    
    def seed_pcg32(self, seed: int, seq: int):
        self.state[::] = [0, ((seq & MASK64) << 1) & MASK64 | 1]
        self.take_randint()
        self.state[0] = (self.state[0] + (seed & MASK64)) & MASK64
        self.take_randint()

    def take_randint(self):
        old_state, increment = self.state
        self.state[0] = (old_state * 6364136223846793005 + increment) & MASK64
        xorshifted = (((old_state >> 18) ^ old_state) >> 27) & MASK32
        rotation = (old_state >> 59) & MASK32
        return ((xorshifted >> rotation) | (xorshifted << ((-rotation) & 31))) & MASK32

    def take_randints(self, m: int) -> List[int]:
        list_ = []
        for _ in range(m):
            list_.append(
                self.take_randint()
            )
        return list_


def get_values(seed: int, seq: int, skip_values: int) -> None:
    pcg32 = PermutedCongruentialGenerator()
    pcg32.seed_pcg32(seed, seq)
    init_val = skip_values
    list_ = pcg32.take_randints(skip_values)  # Go through the first values
    print(f"seed: {seed}. The {init_val} value should be", list_[-1])
    list_ = pcg32.take_randints(skip_values)  # Go through the next values
    init_val += skip_values
    print(f"seed: {seed}. The {init_val} value should be", list_[-1])
    list_ = pcg32.take_randints(skip_values)  # Go through the next values
    init_val += skip_values
    print(f"seed: {seed}. The {init_val} value should be", list_[-1])
    print()


if __name__ == "__main__":
    # Generate random numbers...
    get_values(5, 1, 1000)
    get_values(50, 1, 1000)
    get_values(500, 1, 1000)
