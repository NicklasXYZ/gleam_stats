# gleam_stats

[![Package Version](https://img.shields.io/hexpm/v/gleam_stats)](https://hex.pm/packages/gleam_stats)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gleam_stats/)

A Gleam mathematics and statistics library. The library supports both targets: Erlang and JavaScript.

The repository is available on:
- [Github](https://github.com/nicklasxyz/gleam_stats)

The documentation is hosted on:
- [HexDocs](https://hexdocs.pm/gleam_stats/)





## Installation

The library is available on [Hex](https://hex.pm/packages/gleam_stats) so it can be added to your Gleam project by simply running:

```sh
gleam add gleam_stats
```

### Disclaimer

To align the Gleam and Javascript implementation of the library, the JavaScript implementation utilizes the  [decimal.js](https://github.com/MikeMcl/decimal.js) library **internally** for computations that require arbitrary precision integers and floating-point values. However, the input and output of the different functions in the JavaScript implementation of the library are still constrained by the precision of the JS [`Number`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number) primitive.


## Development

```sh
# Run the tests
gleam test

# Build and run docs locally
gleam docs build && python -m http.server
```
