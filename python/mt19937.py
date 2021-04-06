class MersenneTwister:
    """MT19937 reference implementation"""

    def __init__(self) -> None:
        # Coefficients for MT19937
        (self.w, self.n, self.m, self.r) = (32, 624, 397, 31)
        self.a = 0x9908B0DF
        (self.u, self.d) = (11, 0xFFFFFFFF)
        (self.s, self.b) = (7, 0x9D2C5680)
        (self.t, self.c) = (15, 0xEFC60000)
        self.l = 18
        self.f = 1812433253
        self.MT = [0 for i in range(self.n)]
        self.index = self.n + 1

    def lowest_bits(self, x: int):
        return x & (1 << self.w) - 1

    def lower_bitmask(self) -> int:
        # 0xFFFFFFFF, for current parameters
        return (1 << self.r) - 1

    def upper_bitmask(self) -> int:
        # 0x00000000, for current paramters
        return self.lowest_bits(~self.lower_bitmask())

    def seed_mt19937(self, seed: int) -> None:
        # Initialize the generator
        self.MT[0] = seed
        for i in range(1, self.n):
            temp = self.f * (self.MT[i-1] ^ (self.MT[i-1] >> (self.w - 2))) + i
            self.MT[i] = temp & 0xffffffff

    def take_randint(self) -> int:
        if self.index >= self.n:
            self.twist()
            self.index = 0
        y = self.MT[self.index]
        y = y ^ ((y >> self.u) & self.d)
        y = y ^ ((y << self.s) & self.b)
        y = y ^ ((y << self.t) & self.c)
        y = y ^ (y >> self.l)
        self.index += 1
        return y & 0xffffffff

    def take_randints(self, m: int) -> int:
        list_ = []
        for _ in range(m):
            list_.append(
                self.take_randint()
            )
        return list_

    def twist(self) -> None:
        for i in range(0, self.n):
            x = (self.MT[i] & self.upper_bitmask()) + \
                (self.MT[(i+1) % self.n] & self.lower_bitmask())
            xA = x >> 1
            if (x % 2) != 0:
                xA = xA ^ self.a
            self.MT[i] = self.MT[(i + self.m) % self.n] ^ xA


def get_values(seed: int, skip_values: int) -> None:
    mt19937 = MersenneTwister()
    mt19937.seed_mt19937(seed)
    init_val = skip_values
    list_ = mt19937.take_randints(skip_values)  # Go through the first values
    print(f"seed: {seed}. The {init_val} value should be", list_[-1])
    list_ = mt19937.take_randints(skip_values)  # Go through the next values
    init_val += skip_values
    print(f"seed: {seed}. The {init_val} value should be", list_[-1])
    list_ = mt19937.take_randints(skip_values)  # Go through the next values
    init_val += skip_values
    print(f"seed: {seed}. The {init_val} value should be", list_[-1])
    print()


if __name__ == "__main__":
    # Generate random numbers...
    get_values(5, 1000)
    get_values(50, 1000)
    get_values(500, 1000)