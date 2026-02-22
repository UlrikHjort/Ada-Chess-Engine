# Ada Chess Engine

A complete, playable chess - but very weak - engine written in Ada 2012.  Implements all chess rules, a full interactive game interface, a forced-mate tactical solver, a negamax playing engine with transposition table and quiescence search, and a UCI interface for connecting to GUI front-ends such as Arena or Cutechess. Written for fun and for exploring how chess engines works. Engine is too weak to play seriously with. 

---

## Building

Requirements: `gnatmake` / GNAT Ada 2012 (gnat-12 or later).

```bash
# Interactive terminal engine
make

# UCI engine (for GUI front-ends)
make uci

# Both at once
make all uci

# Clean everything
make clean
```

Binaries are placed in `bin/`:

| Binary | Purpose |
|--------|---------|
| `bin/chess` | Interactive terminal interface |
| `bin/chess_uci` | UCI protocol engine for GUIs |

---

## Interactive Engine (`bin/chess`)

### Starting a game

```
$ bin/chess

Ada Chess Engine v0.1
====================

  ┌───┬───┬───┬───┬───┬───┬───┬───┐
8 │ r │ n │ b │ q │ k │ b │ n │ r │
  ...
    a   b   c   d   e   f   g   h

White to move>
```

### Making moves

Use standard algebraic notation:

```
White to move> e4          ← pawn to e4
Black to move> e5
White to move> Nf3         ← knight to f3
Black to move> Nc6
White to move> O-O         ← castle kingside
White to move> exd5        ← pawn capture
White to move> e8=Q        ← pawn promotion
```

### All commands

| Command | Description |
|---------|-------------|
| `<move>` | Make a move (e.g. `e4`, `Nf3`, `O-O`, `exd5`, `e8=Q`) |
| `moves` | List all legal moves in the current position |
| `undo` | Take back the last move |
| `redo` | Redo a taken-back move |
| `save <file>` | Save the current game to a PGN file |
| `load <file>` | Load a game or position from a PGN file |
| `analyze <N>` | Find forced mate in N moves (N = 1–4) |
| `eval` | Show a detailed position evaluation breakdown |
| `go [depth]` | Engine plays best move at fixed depth (default 4) |
| `go v [depth]` | Engine plays best move with verbose thinking output |
| `go verbose [depth]` | Same as `go v` |
| `go time <ms>` | Engine plays using iterative deepening within the time budget |
| `autoplay [depth]` | Engine plays both sides; shows board after every half-move |
| `help` | Show command list |
| `quit` | Exit |

---

## Playing Against the Engine

### Fixed-depth search

```
White to move> go 4
Thinking at depth 4...
Engine plays: e4

=== Search Statistics ===
Nodes visited:       18432
Quiescence nodes:    14218
Beta cutoffs:         3104
TT hits:              1572
```

### Time-based search (iterative deepening)

```
White to move> go time 3000
Thinking (iterative deepening, 3000 ms)...
Engine plays: d4

=== Search Statistics ===
Nodes visited:       142048
Quiescence nodes:    119334
Beta cutoffs:        28417
TT hits:             41280
Time elapsed:        2847 ms
```

### Verbose mode — see what the engine is thinking

```
White to move> go v 4
Thinking at depth 4 (verbose)...
  e4       +0.56
  d4       +0.51
  Nf3      +0.44
  ...

Best line: e4 e5 Nf3 Nc6
Score: +0.56
Engine plays: e4
```

### Engine vs engine autoplay

```
White to move> autoplay 3
Autoplay: White vs Black, depth 3 each side
----------------------------------------------
1. e4

  ┌───┬───┬───┬───┬───┬───┬───┬───┐
  ...

1... e5
  ...
```

---

## Tactical Solver (`analyze`)

The `analyze N` command uses a dedicated AND/OR tree solver (separate from the
playing engine) with its own transposition table, killer moves, and history
heuristic.

```
White to move> load problems/mate-in-2/problem_001.pgn
Loaded position ...

White to move> analyze 2
Searching for mate in 2 moves...
Solution found! Mate in 2
Moves:
 1. Qxe6+
   Rxe6 (forced)
 2. Rf8#
```


### Problem library

The `problems/` directory contains 1 170 tactical positions:

| Directory | Count | Notes |
|-----------|------:|-------|
| `problems/mate-in-1/` | varies | Fastest to solve |
| `problems/mate-in-2/` | 221 | 100% success rate |
| `problems/mate-in-3/` | 488 | ~60% success rate |
| `problems/mate-in-4/` | varies | Partial support |

**Note**: Some mate-in-3/4 problems are mislabeled or have no forced mate at
the stated depth — this is a data quality issue, not an engine bug.  See
`LIMITATIONS.md` for details.

Note the problems directory is provided as problems.tgz due to the number of files in here. Just uncompress with tar xvzf problems.tgz.

---

## Position Evaluation (`eval`)

```
Black to move> eval
Material:   White=+3960  Black=+3910
Mobility:   White=+66    Black=+60
Bshp pair:  White=+50    Black=+50
Pawn struc: White=+10    Black=+0
King sfty:  White=+30    Black=+20
Total (cp):+76  (White is better)
```

Components:

| Term | Description |
|------|-------------|
| Material | Piece values (P=100, N=320, B=330, R=500, Q=900 cp) |
| Piece-square tables | Encourage good piece placement |
| Mobility | Legal move count × 3 cp |
| Bishop pair | +50 cp for having both bishops |
| Passed pawns | Bonus scaled by advancement rank (up to 110 cp) |
| Doubled pawns | −20 cp per extra pawn on same file |
| Isolated pawns | −15 cp per pawn with no friendly neighbour file |
| King safety | Pawn shield (+10/+5 cp); open file with enemy major (−25 cp) |

---

## PGN Support

```
White to move> save mygame.pgn
Game saved to mygame.pgn

White to move> load mygame.pgn
Loaded 18 moves

White to move> load problems/mate-in-3/problem_042.pgn
Loaded position from FEN
```

PGN files may contain:
- A full game (sequence of moves from the starting position)
- A FEN header (`[FEN "..."]`) to set a custom starting position
- Both combined (FEN + subsequent moves)

---

## UCI Engine (`bin/chess_uci`)

The `bin/chess_uci` binary speaks the Universal Chess Interface protocol and
can be plugged into any UCI-compatible GUI (Arena, Cutechess, lichess-bot, etc.).

### Supported UCI commands

| Command | Description |
|---------|-------------|
| `uci` | Identify engine, list options |
| `isready` | Synchronisation ping → `readyok` |
| `ucinewgame` | Reset internal state |
| `position startpos [moves ...]` | Set position from start + move list |
| `position fen <FEN> [moves ...]` | Set position from FEN string |
| `go movetime <ms>` | Search for exactly N milliseconds |
| `go depth <N>` | Search to fixed depth |
| `go wtime <N> btime <N> [movestogo <N>]` | Time control mode |
| `go infinite` | Search until `stop` (searches to `Max_ID_Depth`) |
| `stop` | Stop searching (single-threaded; gracefully ignored) |
| `quit` | Exit |

### Connecting to Arena

1. Open Arena → Engines → Install New Engine
2. Select `bin/chess_uci`
3. Engine type: **UCI**
4. Click OK — Ada Chess will appear in the engine list

### Quick UCI session (manual)

```
$ bin/chess_uci
uci
id name Ada Chess
id author Ulrik Hoerlyk Hjort
option name Hash type spin default 64 min 1 max 512
option name Threads type spin default 1 min 1 max 1
option name Depth type spin default 4 min 1 max 20
uciok
isready
readyok
position startpos moves e2e4 e7e5
go movetime 2000
bestmove g1f3
```

---

## Architecture Overview

### Two search engines

| Engine | Package | Algorithm | Use |
|--------|---------|-----------|-----|
| Tactical solver | `Chess.Tactics` | AND/OR tree, TT, killer moves, history heuristic | `analyze N` |
| Playing engine | `Chess.Search` | Negamax alpha-beta, quiescence, TT, iterative deepening | `go`, `autoplay`, UCI |

### Playing engine internals (`Chess.Search`)

- **Negamax alpha-beta**: classic fail-hard with `-(Mate_Score - Ply)` for
  preferring shorter mates
- **Quiescence search**: up to 8 extra plies, searches captures + promotions
  only; stand-pat pruning
- **Transposition table**: 2^18 entries, Zobrist keyed; stores Exact /
  Lower-bound / Upper-bound; best move reused for ordering
- **Move ordering**: TT best move first → promotions (8000+) → captures
  MVV-LVA (6000+) → quiet moves
- **Draw detection**: 50-move rule at every node; threefold repetition via
  Zobrist hash stack seeded with game history
- **Iterative deepening**: `Find_Best_Move_Timed` runs depths 1…20 and returns
  the result of the last completed depth

### Bitboard attack detection

All check-detection calls (`Is_In_Check`, `Is_Square_Attacked`) use
precomputed bitboard tables rather than generating pseudo-legal moves.  This
gives roughly a 5× speedup for the legal-move filter step.

Attack tables initialised at startup (`Initialize_Bitboards`):
- Knight, king: 64-entry lookup
- Pawn: 2 × 64 entry lookup (per colour)
- Rook, bishop, queen: classical ray-casting on the occupancy bitboard

---

## Requirements

- **GNAT Ada compiler**: gnat-12 or later 
- No external dependencies

## References

**David Levy: Computer Chess compendium**

**David Levy/Monty Newborn: How Computers Play Chess**

**David Levy: Computer Gamesmanship - Elements of Intelligent Game Design**