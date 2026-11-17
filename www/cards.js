/* =========================================================================
   Draft Stock Exchange — card physics
   -------------------------------------------------------------------------
   Every card is a tiny spring system. Each angle (tilt X, tilt Y, flip) has a
   position, a velocity and a target. Each animation frame:

       force        = -stiffness * (position - target) - damping * velocity
       velocity    += force * dt
       position    += velocity * dt

   Low damping = the card overshoots and wobbles before it settles. That's
   the whole trick behind the "spring physics" feel.

   Cards are added and removed by Shiny at any time, so nothing is set up per
   card in advance: listeners sit on `document` and a card gets its spring
   state the first time it's touched (event delegation + lazy state).
   ========================================================================= */
(function () {
  "use strict";

  const TILT_MAX = 16;          // degrees the card leans toward the pointer
  const TILT = { k: 170, c: 16 };  // stiffness, damping for tilt
  const FLIP = { k: 120, c: 11 };  // a bit bouncier for the flip
  const REDUCED = window.matchMedia("(prefers-reduced-motion: reduce)").matches;

  const states = new WeakMap(); // card element -> spring state
  const active = new Set();     // cards currently moving
  let running = false;

  function stateFor(card) {
    let s = states.get(card);
    if (!s) {
      const start = card.classList.contains("sealed") ? 180 : 0;
      s = {
        card,
        inner: card.querySelector(".dse-card-inner"),
        rx: spring(0), ry: spring(0), flip: spring(start),
        shine: 0
      };
      states.set(card, s);
    }
    return s;
  }

  function spring(x) { return { x: x, v: 0, target: x }; }

  function step(sp, cfg, dt) {
    const force = -cfg.k * (sp.x - sp.target) - cfg.c * sp.v;
    sp.v += force * dt;
    sp.x += sp.v * dt;
    return Math.abs(sp.x - sp.target) > 0.01 || Math.abs(sp.v) > 0.01;
  }

  function wake(s) {
    active.add(s);
    if (!running) { running = true; last = performance.now(); requestAnimationFrame(loop); }
  }

  let last = 0;
  function loop(now) {
    // Cap dt so a background tab doesn't make the springs explode.
    const dt = Math.min((now - last) / 1000, 1 / 30);
    last = now;
    for (const s of active) {
      if (!document.body.contains(s.card)) { active.delete(s); continue; }
      const moving = [step(s.rx, TILT, dt), step(s.ry, TILT, dt), step(s.flip, FLIP, dt)]
        .some(Boolean);
      render(s);
      if (!moving) active.delete(s);
    }
    if (active.size) requestAnimationFrame(loop); else running = false;
  }

  function render(s) {
    s.inner.style.transform =
      `rotateX(${s.rx.x.toFixed(2)}deg) rotateY(${(s.ry.x + s.flip.x).toFixed(2)}deg)`;
    s.card.style.setProperty("--shine", s.shine.toFixed(3));
  }

  // ---- tilt + foil follow the pointer -------------------------------------
  document.addEventListener("pointermove", (e) => {
    const card = e.target.closest && e.target.closest(".dse-card");
    if (!card || REDUCED) return;
    const s = stateFor(card);
    const r = card.getBoundingClientRect();
    const px = (e.clientX - r.left) / r.width;   // 0..1
    const py = (e.clientY - r.top) / r.height;   // 0..1
    s.ry.target = (px - 0.5) * 2 * TILT_MAX;
    s.rx.target = -(py - 0.5) * 2 * TILT_MAX;
    s.shine = Math.min(1, Math.hypot(px - 0.5, py - 0.5) * 2);
    // When the card is flipped, mirror the foil so it tracks the pointer.
    const flipped = Math.round(s.flip.target / 180) % 2 !== 0;
    card.style.setProperty("--mx", `${((flipped ? 1 - px : px) * 100).toFixed(1)}%`);
    card.style.setProperty("--my", `${(py * 100).toFixed(1)}%`);
    wake(s);
  });

  document.addEventListener("pointerout", (e) => {
    const card = e.target.closest && e.target.closest(".dse-card");
    if (!card || card.contains(e.relatedTarget)) return;
    const s = stateFor(card);
    s.rx.target = 0; s.ry.target = 0; s.shine = 0;
    wake(s);
  });

  // ---- click: add, flip, or reveal ------------------------------------------
  // Safari still "hits" a 3D face that is turned away, so a click on the "+"
  // could land on the hidden back face and flip the card instead. Two guards:
  //  1. CSS gives the hidden face pointer-events: none (via .is-flipped)
  //  2. here we also check whether the click fell inside the "+" button's box
  function isFaceUp(card, s) {
    return !card.classList.contains("sealed") && Math.round(s.flip.target / 180) % 2 === 0;
  }

  function clickedAdd(card, e) {
    const btn = card.querySelector(".dse-add");
    if (!btn) return false;
    const r = btn.getBoundingClientRect(), pad = 6;
    return e.clientX >= r.left - pad && e.clientX <= r.right + pad &&
           e.clientY >= r.top - pad && e.clientY <= r.bottom + pad;
  }

  document.addEventListener("click", (e) => {
    const card = e.target.closest && e.target.closest(".dse-card");
    if (!card) return;
    const s = stateFor(card);

    if (isFaceUp(card, s) && (e.target.closest(".dse-add") || clickedAdd(card, e))) {
      addToPortfolio(card);
      const btn = card.querySelector(".dse-add");
      btn.classList.add("added");
      setTimeout(() => btn.classList.remove("added"), 900);
      return;
    }

    if (card.classList.contains("sealed")) {
      s.flip.target = 0;                         // turn face up...
      s.flip.v -= 260;                           // ...with a little extra kick
      // swap the cover for the stats once the back is out of sight
      setTimeout(() => card.classList.remove("sealed"), 260);
      if (window.Shiny) Shiny.setInputValue("pack_reveal", card.dataset.cardId, { priority: "event" });
    } else {
      s.flip.target += 180;
    }
    card.classList.toggle("is-flipped", !isFaceUp(card, s));
    if (REDUCED) { s.flip.x = s.flip.target; s.flip.v = 0; render(s); return; }
    wake(s);
  });

  // ---- drag a card into the portfolio tray --------------------------------
  function addToPortfolio(card) {
    if (!card || !window.Shiny) return;
    Shiny.setInputValue("portfolio_add", card.dataset.cardId, { priority: "event" });
  }

  document.addEventListener("dragstart", (e) => {
    const card = e.target.closest && e.target.closest(".dse-card");
    if (!card || card.classList.contains("sealed")) { if (card) e.preventDefault(); return; }
    e.dataTransfer.setData("text/plain", card.dataset.cardId);
    e.dataTransfer.effectAllowed = "copy";
    card.classList.add("dragging");
  });
  document.addEventListener("dragend", (e) => {
    const card = e.target.closest && e.target.closest(".dse-card");
    if (card) card.classList.remove("dragging");
  });
  document.addEventListener("dragover", (e) => {
    const drop = e.target.closest && e.target.closest(".drop");
    if (!drop) return;
    e.preventDefault();
    drop.classList.add("over");
  });
  document.addEventListener("dragleave", (e) => {
    const drop = e.target.closest && e.target.closest(".drop");
    if (drop && !drop.contains(e.relatedTarget)) drop.classList.remove("over");
  });
  document.addEventListener("drop", (e) => {
    const drop = e.target.closest && e.target.closest(".drop");
    if (!drop) return;
    e.preventDefault();
    drop.classList.remove("over");
    const id = e.dataTransfer.getData("text/plain");
    if (id && window.Shiny) Shiny.setInputValue("portfolio_add", id, { priority: "event" });
  });
})();