# Dungeon

Public route: https://jjant.github.io/dungeon/. A browser roguelike prototype with
eight authored floors of rooms and corridors, two guardians, and time to think
between turns. Start at the title screen; victory and defeat menus offer another
expedition. The static landing page uses no external fonts or telemetry.
`play.html` contains the stripped ReleaseSafe Zig/Wasm game, with fixed memory
and assertions enabled. Private source, manuals, engineering evidence and replay
archives stay private; publish only this page and the reviewed compiled assets.

Walls hide actors; explored terrain stays grey when it leaves view. Choose an
action, aim at highlighted cells with the arrow keys, then confirm. Aiming and
canceling don’t spend a turn. Turns animate in order: player → enemy → environment.
New input skips the visuals.

The rules build on fire, water, spikes, shoves and recoverable metal throws:

* Throw a key into a locked chest to unlock it. The key is consumed; bump into
  the chest later to open it and collect its treasure once.
* Blobs take no damage or interruption from keys and coins. Glass potions still
  hurt and interrupt them.
* A thief that survives a thrown key or coin catches it if unladen, then flees.
  Defeat it to drop its metal at the death cell. Step onto the item to recover it.
* Combine those rules: throw a key, finish the thief with a shove onto spikes,
  recover the key, then use it to unlock a chest. The illustrated chain shows
  one possible sequence; a shove must actually defeat the thief to release it.

The launcher fetches the game only after **Start playing**, checks its response,
and offers retry, close and a separate tab. Closing unloads the iframe and returns
focus. Opening scrolls to the board; settings remain above it. The same-origin iframe is the reviewed game, not a sandbox for untrusted
mods. `site.js` controls loading only; it contains no game rules.

Playing starts in manual mode with simulated faults off. `play.html#lab` opens
the bot with fault injection. Browser runs are not saved automatically across tab
closure: export a replay to keep one. Replay files require matching game/content versions;
a replay library, seeking and taking control are future work.

The handheld and action drawings are illustrations, not gameplay captures.
The combination sequence uses inline SVG and readable ordered steps; it introduces
no image assets. The PNGs below predate wall visibility and ordered turn animation;
replacement captures are pending. They show the rules v12 eight-floor campaign,
reached through ordinary play by the built-in explorer with seed 42 and simulated
faults off. Both show a living guardian preparing its attack:

| Asset | Scene |
| --- | --- |
| `assets/floor-3-boss.png` | Marshals Crossing, 3/8, turn 114; rammer guardian |
| `assets/floor-8-boss.png` | The Last Bell, 8/8, turn 227; coal guardian |

The captures preserve the complete 320 × 240 game frame, scaled to 488 × 367
with pixel smoothing off. Keep both filenames and dimensions when refreshing them. Keep
launcher IDs, `data-game-ready`, `data-src`, `data-release-copy` and navigation
anchors compatible with `site.js`. The combination strip reads across on desktop
and down on smaller screens. No native download is offered by this page.

Preview from the repository root with any local HTTP server. Before publishing a
new game, check the start/win/loss menus, keyboard/touch controls, pause, replay
download/import, iframe close/reopen, initial board visibility and the separate
tab at desktop and 390/320 px widths. Keep temporary screenshots and test
fixtures outside the public checkout. Describe the current authored prototype
when updating the copy; it remains a work in progress.
[Third-party notices](notices.txt).
