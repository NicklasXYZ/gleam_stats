from typing import (
    List,
)

MASK32 = 2 ** 32 - 1

class LinearCongruentialGenerator:
    """LCG32 reference implementation"""

    def __init__(self, a, c) -> None:
        self.state = None
        self.a = a
        self.c = c
        # m: Modulo. Simply the largest 32 bit int that can be represented
    
    def seed_lcg32(self, seed: int):
        self.state = (self.a * seed + self.c) & MASK32

    def take_randint(self):
        self.state =  (self.a * self.state + self.c) & MASK32
        return  self.state

    def take_randints(self, m: int) -> List[int]:
        list_ = []
        for _ in range(m):
            list_.append(
                self.take_randint()
            )
        return list_


def get_values(seed: int, skip_values: int) -> None:
    # Use a, c, m parameter settings from:
    # W. H. Press, S. A. Teukolsky, W. T. Vetterling, et al., Numerical Recipes: The Art of
    # Scientific Computing, 3rd edn. (Cambridge University Press, Cambridge, 2007)
    lcg32 = LinearCongruentialGenerator(1664525, 1013904223)
    lcg32.seed_lcg32(seed)
    init_val = skip_values
    list_ = lcg32.take_randints(skip_values)  # Go through the first values
    print(f"seed: {seed}. The {init_val} value should be", list_[-1])
    list_ = lcg32.take_randints(skip_values)  # Go through the next values
    init_val += skip_values
    print(f"seed: {seed}. The {init_val} value should be", list_[-1])
    list_ = lcg32.take_randints(skip_values)  # Go through the next values
    init_val += skip_values
    print(f"seed: {seed}. The {init_val} value should be", list_[-1])
    print()


if __name__ == "__main__":
    # Generate random numbers...
    get_values(5, 1000)
    get_values(50, 1000)
    get_values(500, 1000)