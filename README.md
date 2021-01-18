# Rummikub Solver

A rummikub solver using GLPK

prerequisites:
- stack
- GLPK

usage:
```
stack run -- RACK_TILES TABLE_TILES
```

a set of tiles is defined as follows:
```
TILE = <COLOR_CHAR><1-13> | j
COLOR_CHAR = k | r | o | b
TILES = TILE,TILE,...
```
