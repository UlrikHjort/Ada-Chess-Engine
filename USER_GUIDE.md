# User Guide

---

### You want to play chess yourself

Just type moves:
```
White to move> e4
Black to move> Nf6
White to move> d4
```

---

### You want the engine to suggest a move

```
White to move> go           -- depth 4 (fast, ~1-2 s)
White to move> go 5         -- depth 5 (stronger, ~5-15 s)
White to move> go time 5000 -- iterative deepening, 5 second budget
```

The time-based command (`go time`) is usually the best choice: it uses all
available time and returns the result of the deepest fully-completed search.

---

### You want to see what the engine is thinking

```
White to move> go v 4
Thinking at depth 4 (verbose)...
  e4       +0.56
  d4       +0.51
  Nf3      +0.44
  c4       +0.38
  ...

Best line: e4 e5 Nf3 Nc6
Score: +0.56
Engine plays: e4
```

---

### You want to watch a full game

```
White to move> autoplay 3
```

Both sides play at depth 3.  The board is printed after every half-move.
The game ends at checkmate, stalemate, or the 50-move rule.

---

### You want to solve a tactical puzzle

```
White to move> load problems/mate-in-2/problem_042.pgn
White to move> analyze 2
```

| Depth | Reliability | Typical time |
|-------|-------------|-------------|
| 2 | 100% | < 1 s |
| 3 | ~60% | 1–10 s |
| 4 | ~40% | 10–60 s |

For depths 3–4, failures are usually mislabeled problems, not engine bugs.

---

### You want to understand the position

```
White to move> eval
```

Shows material, mobility, bishop pair, pawn structure, and king safety
broken down per side.

---

### You want to connect to a GUI (Arena, Cutechess, etc.)

Use the UCI binary:
```bash
bin/chess_uci
```

In Arena: **Engines → Install New Engine**, select `bin/chess_uci`, type = **UCI**.

---

## Performance expectations

| Command | Typical depth | Nodes searched | Time |
|---------|--------------|---------------|------|
| `go 3` | 3 + quiescence | ~5 000 | < 1 s |
| `go 4` | 4 + quiescence | ~20 000 | 1–3 s |
| `go 5` | 5 + quiescence | ~100 000 | 5–15 s |
| `go time 3000` | 5–7 (varies) | varies | ≤ 3 s |
| `autoplay 3` | 3 per move | — | ~30–90 s/game |

---

## Known limitations

- `analyze 3/4` fails on ~40% of problems (often mislabeled data)
- No parallel search (single thread)
- No endgame tablebases
- `Apply_Move` copies full state at every node — make/unmake is next

