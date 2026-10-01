# Dungeon

https://jjant.github.io/dungeon/ runs the game directly. `index.html` is a
self-contained, stripped ReleaseSafe Zig/Wasm build: fixed 320 KiB memory,
assertions enabled, no iframe, external font, telemetry or account.
`about.html` is the illustrated field guide. `play.html` redirects old links,
preserving `#lab`. Private source, manuals and engineering evidence stay private.

The compact view puts the board above a cross-shaped gamepad on phones, with a
backpack for aiming, objects and help. Landscape puts controls beside the board.
The Laboratory button changes the view without restarting or enabling faults;
`#lab` initially starts the bot with simulated power cuts. Fullscreen depends on
the browser. Runs live in the tab: export a replay before closing it. Replays need
matching engine/content versions.

Publish the generated `zig build web -Dstrip-web=true` artifact only after its
browser checks and visual review. Check desktop, narrow portrait and landscape,
title/end menus, rapid inputs, touch aiming/canceling, laboratory and replay
export/import. Chromium touch emulation is not physical Safari/Android acceptance.
The private PR and public PR record source version and validation for each update.

Artwork in the field guide is illustrative. `assets/floor-3-boss.png` and
`assets/floor-8-boss.png` are actual version 13 captures, seed 42, turns 114/227;
they illustrate the guardians rather than certify the current build.
Preview this directory with a local HTTP server. [Third-party notices](notices.txt).
