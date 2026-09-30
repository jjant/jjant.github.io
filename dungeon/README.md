# Dungeon page

Standalone GitHub Pages route: `https://jjant.github.io/dungeon/`.
No build step, dependencies, external fonts, or telemetry. The copy describes
the eight-floor prototype: two guardians, spark/douse/shove, locked attack
warnings, recovery, strikes/charges that can hit other enemies, and replay files.
The expedition descends; no return to earlier floors is advertised.

The handheld and action symbols are original illustrations and labeled as such.
The two PNGs are actual browser gameplay captures, with fog and HUD intact:

| Asset | Capture |
| --- | --- |
| `assets/floor-3-boss.png` | Marshals Crossing, floor 3/8, turn 71; guardian rammer's marked lane |
| `assets/floor-8-boss.png` | The Last Bell, floor 8/8, turn 189; coal guardian beside the explorer |

Both are from the reviewed version-6 eight-floor run. Captures establish what
these scenes looked like; they are not a hardware-testing claim. No private
source, modding manual, evidence JSON or replay archive is included in this route.

The game asset is deliberately absent and `data-game-ready="false"` stays set.
Before release, main should:

1. Add only the validated, self-contained browser output as `play.html`. Keep
   private source, manuals and internal reports outside this public route.
2. Verify that the stripped build still provides the advertised controls,
   fault simulation and replay export/import. This page adds no replay library,
   forking system or second implementation of the game.
3. Set `data-game-ready="true"` on the play section in `index.html` after testing.
4. Check the final iframe, keyboard/touch controls, downloads and new-tab mode
   on desktop and mobile. A readiness fixture does not validate the real game.

The closed gate makes no request for `play.html`. The open gate loads the game
only after “Start playing,” checks for an HTML response, and provides retry and
close controls. Closing unloads the frame; there is no parent keyboard capture.
The iframe is the reviewed same-origin game, not an isolation boundary for
untrusted third-party content. `site.js` owns loading only, not game rules.

Landing checks pass at 1440, 1280, 390 and 320 px without horizontal overflow.
With an inert HTML fixture, loading, missing/wrong-type rejection, the 20-second
timeout, retry, close and focus restoration pass. The closed gate makes zero
game requests. These checks do not replace main's final real-game iframe test.

Preview from the repository root with `python3 -m http.server 8765 --bind 127.0.0.1`,
then open `http://127.0.0.1:8765/dungeon/`. Follow the shared machine preview lock
and disk `TMPDIR` policy when using a browser. Keep layout screenshots, fixture
HTML and test logs outside this public checkout; the two approved gameplay
PNGs above are the intentional exception.
