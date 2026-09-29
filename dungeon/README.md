# Dungeon page

Standalone GitHub Pages route: `https://jjant.github.io/dungeon/`.
No build step, dependencies, external fonts, or telemetry. All artwork here is
original presentation art; the handheld is explicitly labeled as a concept.

The game is deliberately absent. Before release, main should:

1. Review and add the validated, self-contained game output as `play.html`.
2. Set `data-game-ready="true"` on the play section in `index.html`.
3. Check the embedded game, its keyboard/touch controls and new-tab mode on
   desktop and mobile. Confirm its layout fits the frame and update feature
   status copy only where the validated game supports it.

The closed gate makes no request for `play.html`. The open gate loads the game
only after “Start playing,” checks for an HTML response, and provides retry and
close controls. Closing unloads the frame; there is no parent keyboard capture.
The iframe is the reviewed same-origin game, not an isolation boundary for
untrusted third-party content.

Preview from the repository root with `python3 -m http.server 8765 --bind 127.0.0.1`,
then open `http://127.0.0.1:8765/dungeon/`. Follow the shared machine build/preview
lock policy when using a browser. Keep screenshots and other review evidence
outside the public checkout.
