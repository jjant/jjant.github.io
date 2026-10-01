# Dungeon

https://jjant.github.io/dungeon/ runs the game directly. `index.html` is a
self-contained, stripped ReleaseSafe Zig/Wasm build: fixed 320 KiB memory,
assertions enabled, no iframe, external font, telemetry or account.
`about.html` is the illustrated field guide. `play.html` redirects old links,
preserving `#lab`. Private source, manuals and engineering evidence stay private.

Phones default to a 2× close view with readable health, resources and turn text,
a cross-shaped gamepad below and a backpack for aiming, objects and help.
The camera retains the full throwing reach; “Vista completa” restores the full
board without changing the run. Landscape puts controls beside the board.
Confirmed loot labels linger across quick moves; reduced motion keeps the text
still and returns to idle after it expires. The explorer walks between tiles;
hits tint the player and fragment the lost HP segment in cause order. Fresh
launches choose a random seed. New inputs always skip attack motion.
Title and end choices live inside the game; browser controls retain accessible
equivalents without a duplicate visible menu. Guardian seals visibly block
stairs until defeated, distinct from the keys used for chests.
The Laboratory button changes the view without restarting or enabling faults;
its optional two-room trials stay out of the normal title menu.
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
