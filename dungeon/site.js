(() => {
  "use strict";

  const room = document.querySelector("#play");
  const frame = document.querySelector("#game-frame");
  const placeholder = document.querySelector("#player-placeholder");
  const launch = document.querySelector("#launch-game");
  const close = document.querySelector("#close-game");
  const toolbar = document.querySelector("#game-toolbar");
  const state = document.querySelector("#player-state");
  const message = document.querySelector("#player-message");
  const description = document.querySelector("#player-description");
  const help = document.querySelector("#game-help");

  // Main enables this only after reviewing the final, self-contained play.html.
  // With the gate closed, no game URL is requested, even if a file is present.
  if (room.dataset.gameReady !== "true") return;

  const gameURL = new URL(frame.dataset.src, window.location.href);
  let loading = false;
  let attempt = 0;
  let controller;
  let timeout;

  state.setAttribute("role", "status");
  state.setAttribute("aria-live", "polite");
  state.textContent = "Prototype";
  message.textContent = "Your next expedition awaits.";
  description.textContent = "Step inside and use the game’s own controls to begin.";
  launch.disabled = false;
  launch.textContent = "Start playing";
  document.querySelectorAll("[data-release-copy]").forEach((node) => {
    node.textContent = "Browser prototype · Ready to explore";
  });
  help.textContent = "Use the controls inside the game. You can also open it in its own tab.";

  function stopLoading() {
    clearTimeout(timeout);
    timeout = undefined;
    controller?.abort();
    controller = undefined;
    loading = false;
    room.removeAttribute("aria-busy");
  }

  function showError() {
    attempt++;
    stopLoading();
    frame.onload = null;
    frame.onerror = null;
    frame.hidden = true;
    frame.removeAttribute("src");
    placeholder.hidden = false;
    toolbar.hidden = true;
    state.textContent = "Couldn’t load the prototype";
    message.textContent = "The door is stuck.";
    description.textContent = "The game couldn’t be loaded. Please try again in a moment.";
    launch.disabled = false;
    launch.textContent = "Try again";
    launch.focus({ preventScroll: true });
  }

  launch.addEventListener("click", async () => {
    if (loading) return;
    loading = true;
    const thisAttempt = ++attempt;
    controller = new AbortController();
    room.setAttribute("aria-busy", "true");
    launch.disabled = true;
    launch.textContent = "Opening the dungeon…";
    state.textContent = "Loading";
    timeout = setTimeout(showError, 20000);

    try {
      // Check for a missing Pages asset before exposing an iframe full of 404 UI.
      const response = await fetch(gameURL, {
        method: "HEAD",
        cache: "no-cache",
        signal: controller.signal,
      });
      if (thisAttempt !== attempt) return;
      if (!response.ok || !response.headers.get("content-type")?.includes("text/html")) {
        throw new Error("Game asset unavailable");
      }

      frame.onload = () => {
        if (thisAttempt !== attempt) return;
        stopLoading();
        frame.onload = null;
        frame.onerror = null;
        placeholder.hidden = true;
        frame.hidden = false;
        toolbar.hidden = false;
        state.textContent = "Prototype loaded";
        frame.focus({ preventScroll: true });
      };
      frame.onerror = () => {
        if (thisAttempt === attempt) showError();
      };
      frame.src = gameURL.href;
    } catch {
      if (thisAttempt === attempt) showError();
    }
  });

  close.addEventListener("click", () => {
    attempt++;
    stopLoading();
    frame.onload = null;
    frame.onerror = null;
    frame.hidden = true;
    frame.removeAttribute("src");
    toolbar.hidden = true;
    placeholder.hidden = false;
    state.textContent = "Prototype";
    message.textContent = "Another expedition?";
    description.textContent = "Start again whenever you’re ready.";
    launch.disabled = false;
    launch.textContent = "Start playing";
    launch.focus({ preventScroll: true });
  });
})();
