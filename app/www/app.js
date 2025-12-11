// www/app.js ---------------------------------------------------------
// Custom JS for the Water My Lawn Shiny app

//---------------------------------------------------------
// SWIPE GESTURES (Hammer.js)
//---------------------------------------------------------
Shiny.addCustomMessageHandler("enableSwipe", function (msg) {
  var el = document.getElementById("card");
  if (!el || typeof Hammer === "undefined") return;

  var mc = new Hammer(el);
  mc.on("swipeleft", function () {
    Shiny.setInputValue("swipe_next", Math.random());
  });
  mc.on("swiperight", function () {
    Shiny.setInputValue("swipe_prev", Math.random());
  });
});

//---------------------------------------------------------
// PROGRESS BAR
//---------------------------------------------------------
Shiny.addCustomMessageHandler("progress", function (pct) {
  var bar = document.getElementById("pbar");
  if (bar) {
    bar.style.width = pct + "%";
  }
});

//---------------------------------------------------------
// DRAGGABLE WATERING BARS
//---------------------------------------------------------
Shiny.addCustomMessageHandler("initDrag", function (maxMinutes) {
  var bars = document.querySelectorAll(".bar-bg");
  if (!bars || bars.length === 0) return;

  bars.forEach(function (bg) {
    var idx = bg.getAttribute("data-i");
    var fill = bg.querySelector(".bar-fill");
    var lbl = bg.parentElement.querySelector(".day-value");
    var dragging = false;

    function update(e) {
      var rect = bg.getBoundingClientRect();
      var clientY;

      if (e.touches && e.touches.length > 0) {
        clientY = e.touches[0].clientY;
      } else {
        clientY = e.clientY;
      }

      var ratio = (rect.bottom - clientY) / rect.height;
      if (ratio < 0) ratio = 0;
      if (ratio > 1) ratio = 1;

      var mins = Math.round(ratio * maxMinutes);
      if (fill) {
        fill.style.height = ratio * 100 + "%";
      }
      if (lbl) {
        lbl.textContent = mins + " min";
      }

      Shiny.setInputValue("w" + idx, mins, { priority: "event" });
    }

    function startDrag(e) {
      dragging = true;
      update(e);
      e.preventDefault();
    }

    function moveDrag(e) {
      if (!dragging) return;
      update(e);
      e.preventDefault();
    }

    function endDrag() {
      dragging = false;
    }

    bg.onmousedown = startDrag;
    bg.ontouchstart = startDrag;

    document.addEventListener("mousemove", moveDrag);
    document.addEventListener("touchmove", moveDrag, { passive: false });

    document.addEventListener("mouseup", endDrag);
    document.addEventListener("touchend", endDrag);
  });
});

//---------------------------------------------------------
// THEME TOGGLE (light/dark with localStorage)
//---------------------------------------------------------
document.addEventListener("DOMContentLoaded", function () {
  var sw = document.getElementById("themeSwitch");
  if (!sw) return;

  // Load saved theme or default to light
  var saved = localStorage.getItem("theme") || "light";
  if (saved === "dark") {
    document.body.classList.add("dark");
    sw.checked = true;
  }

  // Notify Shiny of initial theme
  setTimeout(function () {
    Shiny.setInputValue("theme", saved, { priority: "event" });
  }, 50);

  // Handle toggle changes
  sw.addEventListener("change", function () {
    var mode = sw.checked ? "dark" : "light";

    if (mode === "dark") {
      document.body.classList.add("dark");
    } else {
      document.body.classList.remove("dark");
    }

    localStorage.setItem("theme", mode);

    setTimeout(function () {
      Shiny.setInputValue("theme", mode, { priority: "event" });
    }, 25);
  });
});
