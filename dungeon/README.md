# Dungeon

Public route: https://jjant.github.io/dungeon/. Static page, no dependencies,
external fonts or telemetry. `play.html` is the self-contained, stripped
ReleaseSafe Zig/Wasm game. Its memory is fixed; assertions remain active.
Private source, manuals, engineering evidence and replay archives stay private.

The launcher fetches the game only after **Start playing**, checks its response,
and offers retry, close and a separate tab. Closing unloads the iframe and returns
focus. Opening scrolls to the board; settings remain above it. The same-origin iframe is the reviewed game, not a sandbox for untrusted
mods. `site.js` controls loading only; it contains no game rules.

Playing starts in manual mode with simulated faults off. `play.html#lab` opens
the bot with fault injection. Browser runs are not saved automatically across tab
closure: export a replay to keep one. Replay files require matching game/content versions;
a replay library, seeking and taking control are future work.

The handheld illustration is a labeled concept. Both PNGs are actual gameplay:

| Asset | Scene |
| --- | --- |
| `assets/floor-3-boss.png` | Marshals Crossing, 3/8, turn 71; rammer guardian |
| `assets/floor-8-boss.png` | The Last Bell, 8/8, turn 189; coal guardian |

Preview from the repository root with any local HTTP server. Before publishing a
new game, check keyboard/touch controls, pause, replay download/import, iframe
close/reopen, initial board visibility and the separate tab at desktop and
390/320 px widths. Keep temporary
screenshots, recordings and test fixtures outside the public checkout.
[Third-party notices](notices.txt).
