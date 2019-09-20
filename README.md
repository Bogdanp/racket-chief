# chief

Chief is a port of [foreman] to Racket.  It runs sets of processes
together based on a `Procfile`.

## Getting Started

You can install `chief` from the package server by running:

    $ raco pkg install chief

## Usage

Create a `Procfile`:

```procfile
assets: npm start
web: raco koyo serve
redis: redis-server --port $PORT
```

Run the processes:

    $ raco chief start

Chief will automatically load environment variables from a `.env` file
located in the current directory.  Additional `.env` files can be
specified using the `-e` flag:

    $ raco chief start -e env-file-1 -e env-file-2

Different procfiles can be specified using the `-f` flag:

    $ raco chief start -f Procfile.dev

You can run arbitrarily many processes of each type using the `-m`
flag:

    $ raco chief start -m web=1 -m redis=3

## Differences from `foreman`

* The `export` and `run` commands are not supported.
* The `-e` flag can be passed multiple times.
* The `-m` flag can be passed multiple times.


[foreman]: http://ddollar.github.io/foreman/
