# Dungeon

https://jjant.github.io/dungeon/ runs the game directly from a self-contained
ReleaseSafe Zig/Wasm build, with fixed 320 KiB memory and assertions enabled.
No iframe, external font, telemetry or account. `about.html` is the illustrated
field guide; `play.html` redirects old links and preserves `#lab`.

Arrows move/aim; Z opens the inventory or goes back, X confirms a selection. A potion aimed
at yourself heals; another target throws it. New runs generate eight floors, with
narrow passages and occasional galleries around pillars that provide room to flank.
Continue/Retry keeps the exact map. Unsaved runs live in the tab: in Laboratory, export
both the replay and its map for a portable backup. The Laboratory library retains
up to eight named recordings on this device; browser storage can be cleared.
Replays require their matching build. Beside stairs on floors 2/5, SUPPLY buys a
potion for 25 carried gold and descends; walking onto the stairs remains free.

The game owns title, inventory and result screens. Phones retain the full board
and a cross-shaped gamepad with X/Z. Reduced motion keeps useful static feedback.
Fresh input skips movement/attack/stair motion. The public gallery below the game
contains real engine clips in disclosed, authored encounters. Videos play only
on request and pause when hidden; reduced motion disables looping.

Publish the generated `zig build web -Dstrip-web=true` artifact after relevant
browser tests and visual review. Preserve `interactions/`: its MP4s, posters and
`gallery.json` are intentional public assets; raw witnesses, source and replays
stay private. The template reveals this section only on jjant.github.io.
Validate narrow portrait/landscape, keyboard/touch, gallery playback, Laboratory
and replay import/export. Browser emulation does not establish physical mobile
or hardware performance. Publication PRs record source and validation.

Field-guide artwork is illustrative. The two guardian captures in `assets/`
come from version 13, seed 42, turns 114/227; gallery source bindings live in the
private evidence manifest. [Third-party notices](notices.txt).
