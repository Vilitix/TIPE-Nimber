# Travail d'initiative personnelle encadré (TIPE)

Repository of my [TIPE](https://fr.wikipedia.org/wiki/Travail_d%27initiative_personnelle_encadr%C3%A9) project in OCaml using [Dune](https://dune.readthedocs.io/en/latest/quick-start.html), focused on **Nimber Concepts and Applications** (final grade 18/20).

## Project Overview

This project explores solving multiple [impartial games](https://en.wikipedia.org/wiki/Impartial_game) using Nimber theory, with the Cram game as the ultimate challenge. The work applies Conway's Nimber theory and the [Sprague-Grundy theorem](https://en.wikipedia.org/wiki/Sprague%E2%80%93Grundy_theorem) to determine optimal play in combinatorial games.

## Technical Approach

### Core Concepts
- **Nimber Theory**: Field of characteristic 2 based on the famous game of Nim
- **Sprague-Grundy Theorem**: Enables nimber calculation for all impartial games
- **Game Decomposition**: Using nimber addition to divide complex games into simpler components

### Implementation Features
- **MinMax Algorithm**: Searches for game positions that can be decomposed into sums of simpler games
- **Union-Find Structure**: Detects game decompositions (using a [persistent](https://usr.lmf.cnrs.fr/~jcf/publis/puf-wml07.pdf) implementation for recursive usage)
- **Pruning Optimization**: Identifies losing positions to eliminate branches
- **Memoization**: Uses [Zobrist hashing](https://en.wikipedia.org/wiki/Zobrist_hashing) adapted for the Cram game

## Games Implemented

Based on the codebase structure:
- **Nim**: Classic nim game implementation
- **Marienbad**: Variant of nim with visualization
- **Kayles**: Bowling pin game
- **Cram**: Domino placement game (main focus)
- **Twopins**: Custom variant

## Building and Running

```bash
# Build the project
dune build

# Run the main executable
dune exec bin/main.exe
```

## Visualization

The project includes graph generation capabilities using Graphviz, particularly visible in the [`Marienbad_reso`](TIPE_Nimber/bin/Marienbad_reso.ml) module, which creates visual representations of game states and move sequences.

---

*Note: The original LaTeX presentation for the exam was lost, but this repository contains the complete implementation of the theoretical concepts discussed.*