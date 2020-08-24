# OBFT Checkpointing Specification

This repository contains specifications for OBFT Checkpointing Federation that will be used to checkpoint a Blockchain system. It contains a formal spec written in Latex and an executable written in Haskell.

### Formal Spec

Build:

```
cd formal-spec
nix-build
```

The resulting document is available at `result/main.pdf`


### Executable Spec

It contains a few basic unit tests for the ledger transition rules and a scripted test for a federation of mocked nodes.

Test:
```
cd executable-spec
stack test
```


