# Networkhs

Graph analysis tool in purely functional taste.

---

> *Development Status:* In progress

---

## Features

[x] Graph data structure
[x] Supports directed and undirected graph
[x] Calculates traversal distance

---

## Building

The project required the following dependencies.

- `GHC` 7.10.3 or newer
- `Cabal` 1.22.6 or newer

Make sure the project dependencies are met:

```
$ cabal install
```

Build the project:

```
$ cabal build
```

---

## Test

Using `hspec` to run BDD test suites:

```
$ cabal test
```

