
## Pre-Reqs

Scala CLI:

```bash
# with Coursier
cs install scala-cli
```

## Usage


### IDE Setup (Metals)

```bash
scala-cli setup-ide . -v -S 2.13
```

### REPL

```bash
scala-cli repl -S 2.13

# OR
scala-cli repl -S 2.13 --dep org.scalacheck::scalacheck:1.17.0 .
```

### Cleanup

Or, to just start over:
```bash
rm -rf .bsp .scala-build .metals

# find and kill suspect bsp/bloop/metals processes
jps -lmv
```
