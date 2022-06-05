# gleeunit

Gleam bindings to the Erlang EUnit test framework.

A custom test runner is included for when compiled to JavaScript.

## Usage

Add this package to your Gleam project.

```sh
gleam add gleeunit --dev
```

And then call the `gleeunit.main` function from your test main function.

```rust
// In test/yourapp_test.gleam
import gleeunit

pub fn main() {
  gleeunit.main()
}
```

Now any public function with a name ending in `_test` in the `test` directory
will be found and run as a test.

```rust
pub fn the_universe_test() {
  assert 1 = 1
}
```

Run the tests by entering `gleam test` in the command line.
