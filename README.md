# stats

A Gleam project for generating and working with random numbers.

## Quick start

```sh
# Run the eunit tests
rebar3 eunit

# Run the Erlang REPL
rebar3 shell
```

## Installation

This package can be installed by adding `stats` to your `rebar.config` dependencies:

```erlang
{deps, [
     {stats, "", 
        {git, "git://github.com/nickasxyz/stats.git",
            {branch, "main"}
        }
    },
]}.
```
