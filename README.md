# Snake (Game Boy, RGBDS)

A classic Snake game for the Nintendo Game Boy (DMG), written in RGBDS assembly.

This project is intentionally simple and “hardware-correct”:
- **VRAM writes are VBlank-safe**
  - Small updates (head/tail/food) are written **only during VBlank**
  - Full-screen rebuilds (title / play init / game over) are done with **LCD OFF**
- Uses **BG tiles + BG tilemap** (no sprites)
- Includes a small **text blitter** for title/game-over messages

## Project layout

~~~
.
├── art/                  # Source art (tiles, palettes, etc.)
├── bin/                  # Build outputs
├── db/                   # Binary data includes (tilemaps, fonts, etc.)
├── src/
│   └── main.asm          # All game code
├── lib/
│   └── hardware.inc      # Common hardware definitions
└── tools/                # Bundled toolchain
    ├── rgbasm
    ├── rgblink
    ├── rgbfix
    ├── rgbgfx
    └── emulicious/
~~~

## Build

~~~shell
make
~~~

Outputs are written to `./bin/`:
- `bin/snake.gb`  (ROM)
- `bin/snake.sym` (symbols)
- `bin/snake.map` (link map)

Clean:
~~~shell
make clean
~~~

## Run (optional)

The Makefile includes a `run` target configured for Emulicious via Wine:

~~~shell
make run
~~~

If you use a different emulator, just open `bin/snake.gb`.

## Controls

- **D-Pad**: move
- **Start**: begin (from title) / restart (from game over)

## VRAM safety notes (how it avoids tearing / blank screens)

Game Boy VRAM is not always writable while the LCD is drawing.
This code uses two strategies:

1. **Per-frame updates are “dirty-tile” writes during VBlank**
   - Only a handful of BG map bytes are updated each tick (head, tail, food)
   - Updates occur between `WaitVBlankStart` and `WaitVBlankEnd`

2. **Large VRAM updates are done with LCD OFF**
   - Clearing the full BG map or drawing the border is too large for a single VBlank
   - Screen transitions (title/play/over) disable LCD, write VRAM freely, then re-enable LCD

## Common issues

### Blank screen / partial border / tearing
This happens if you try to write too much to VRAM in one VBlank.
Make sure:
- Full-screen rebuilds disable LCD (LCDC bit 7 cleared)
- During play, only do tiny BG updates inside VBlank

# Future improvements

## Correctness & robustness

- Centralize VRAM writes behind a tiny “write queue” (addr+value entries) and flush it during VBlank. That prevents accidental “one extra write” outside VBlank as the code grows.
- Make state transitions atomic: set wNextState + wNeedRebuild, and only change wState inside the rebuild handler. (You’re mostly doing this already—just keep it strict.)
- Add an optional debug overlay (e.g., show head/tail X/Y and GetOcc at head). It makes collision/occupancy bugs obvious in minutes.

## Performance

- Replace busy-wait VBlank loops with HALT + VBlank interrupt (set a flag in the ISR). It reduces CPU time and makes timing steadier.
- Speed up occupancy indexing: instead of computing y*20+x with shifts/adds every time, keep a small table of row base offsets (18 bytes) or row base pointers.

## UX / gameplay

- Score + high score: add digit tiles 0–9 and render via the same blitter. Store hi-score in HRAM/WRAM (or SRAM if you later add MBC).
- Difficulty ramp: decrease SPEED_FRAMES as length increases (or every N foods).
- Pause on Start during play.
- Wall wrap / no-wrap option as a compile-time flag (fun toggle).

## Presentation

- Use the Window layer for UI text (“SCORE”, “GAME OVER”, etc.) so your playfield BG map stays untouched. Then only the Window map changes for HUD.
- Add small sound effects (eat / death). Even 2–3 register writes to NR10–NR14 per event is enough.

## Build / assets

- Stop hand-writing tile bytes: use rgbgfx and INCBIN.
- Example workflow: assets/tiles.png -> bin/tiles.2bpp then INCBIN "bin/tiles.2bpp".
- Makefile can rebuild tiles only when PNG changes.

## Code quality

- Adopt a consistent “no clobber” convention per routine (document which regs are preserved).
- Add a couple of tiny macros (e.g., PUSHALL/POPALL, QUEUE_TILE_WRITE, SET_OCC/GET_OCC) to reduce copy/paste mistakes.
