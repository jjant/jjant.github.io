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
Confirmed loot labels linger at fixed locations across quick moves; reduced
motion keeps the text still and returns to idle after it expires. The explorer
walks between tiles; close attacks move the whole attacker toward its recorded
target. Shoves slide the entire displaced row together while the attacker leans
into contact; health and carried objects stay attached to each enemy. Hits tint
the player and fragment the lost HP segment in cause order. Keys and coins keep
their silhouette and size when thrown onto an unobstructed floor.
Fresh launches choose a random seed. New inputs always skip attack motion.
Title and end choices live inside the game; browser controls retain accessible
equivalents without a duplicate visible menu. Guardian seals visibly block
stairs until defeated, distinct from the keys used for chests.
Death finishes the fatal hit, plays a short fall, then enlarges the actual scene
with its cause and the result choices. It preserves fog and the exact run.
A first input or scene tap skips to the result; a separate gesture can retry.
Holding a pointer while the menu appears cannot activate a new choice on release.
Coal lights dry reeds when it moves onto them, including forced movement. The
shove preview marks those landings; water can redirect Coal or stop fire spread.
Rammers cut through dry reeds. Sentries can shove through a movable row of allies;
sidestepping can redirect that row into terrain. Current replay engine: 29;
older replays need their matching build.
The Laboratory button changes the view without restarting or enabling faults;
its optional two-room trials stay out of the normal title menu.
“Próxima partida” can select experimental seeded rooms and corridors. New Run
prepares a new map; Continue, Retry and replay seeking preserve the current one.
“Guardar mapa” downloads its exact `.dngp`: keep it beside the replay and load
the map before opening that replay. The authored campaign remains the default.
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
