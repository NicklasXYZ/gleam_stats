# gleam_stats

A Gleam project for generating and working with random numbers, distributions and statistics. The repository is available on [Github](https://github.com/nicklasxyz/gleam_stats) while the documentation is hosted on:
- [Github Pages](https://nicklasxyz.github.io/gleam_stats/) for the current development version.
- [HexDocs](https://hexdocs.pm/gleam_stats/) for the current stable version.


## Quick start

```sh
# Run the eunit tests
rebar3 eunit

# Run the Erlang REPL
rebar3 shell
```

## Installation

If [available in Hex](https://rebar3.org/docs/configuration/dependencies/#declaring-dependencies)
this package can be installed by adding one of the following options to your `rebar.config` dependencies:

```erlang
{deps, [
    %% Hex dependencies
    %% - Current stable version:
    {gleam_stdlib, "~> 0.15.0"}

    %% Git dependencies
    %% - Current development version:
    %% {gleam_stats, "", {git, "git://github.com/nicklasxyz/gleam_stats.git", {branch, "main"}}}
]}.
```
