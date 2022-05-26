# Changelog

## 1.0.0

- Removed the implementation of the Mersenne Twister (MT19937) Pseudo Random Number Generator (PRNG) as the algorithm is generally outcompeted by newer and more recent families of PRNGs such as the family of Permuted Congruential Generators (PCGs). Using the PCGs implemented by the library should be the preferred way of generating random numbers when using the library as these generators are fast, space effecient and have good statitical properties. 
- Re-factored the code so that all functions (probability density function, cumulative distribution function, sampling methods, etc.) directly assoicated with a specific probability distribution now reside in an appropriately named file located in the `gleam_stats/distributions` module. 
- Added several new functions to the `gleam_stats/distributions` sub-modules to allow the generation of random numbers from the triangular, chi-squared and weibull distribution.
- Added clear error messages to all implemented functions
- Created gleam bindings to the Javascript Math library to make the library compatible with the Javascript compilation target.

## Unreleased

- Added missing math functions `tan` and `asinh`.
- Fixed bugs and handled error cases related to the evaluation of math functions outside of their domain. For example, the evaluation of `math.log(0.0)` should result in an error.
- Added tests for math functions.
- Organized implemented math and stats functions such they are ordered in alphabetical order.
