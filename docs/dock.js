// Description: Inject a personal "dock" of quick-access shortcuts into the
// navbar, next to the dark/light toggle. Desktop: hover/click pill. Mobile: bottom sheet.
(function () {
  var thisScript = document.currentScript;
  var BASE = new URL(".", thisScript.src).href;

  var ICONS = {
    jellyfin: '<svg viewBox="0 0 24 24" fill="currentColor" xmlns="http://www.w3.org/2000/svg"><path d="M12 .002C8.826.002-1.398 18.537.16 21.666c1.56 3.129 22.14 3.094 23.682 0C25.384 18.573 15.177 0 12 0zm7.76 18.949c-1.008 2.028-14.493 2.05-15.514 0C3.224 16.9 9.92 4.755 12.003 4.755c2.081 0 8.77 12.166 7.759 14.196zM12 9.198c-1.054 0-4.446 6.15-3.93 7.189.518 1.04 7.348 1.027 7.86 0 .511-1.027-2.874-7.19-3.93-7.19z"/></svg>',
    immich: '<svg viewBox="0 0 24 24" fill="currentColor" xmlns="http://www.w3.org/2000/svg"><path d="M11.9863.2695c-2.409 0-5.207 1.091-5.207 3.8946v.1523c1.3428.597 2.9347 1.6629 4.4121 2.9707 1.5713 1.3912 2.8374 2.8821 3.6524 4.2871 1.3997-2.5034 2.3358-5.4784 2.3476-7.373V4.164c0-2.8035-2.796-3.8946-5.205-3.8946m7.5117 4.4903c-.3778-.0081-.7747.0502-1.1914.1855-.0366.0118-.086.0278-.1445.0469-.1525 1.4611-.6756 3.304-1.4629 5.1133-.8373 1.9243-1.8627 3.5898-2.9472 4.7988 2.8132.558 5.9307.5273 7.7363-.0469.0126-.004.0246-.0065.0351-.0097 2.6665-.8666 2.84-3.8636 2.0957-6.1543-.6279-1.9332-2.081-3.89-4.121-3.9336m-14.996.039C2.4618 4.8424 1.0088 6.7973.3809 8.7305c-.7442 2.291-.5708 5.288 2.0957 6.1543l.1445.0468c.982-1.0926 2.4873-2.2761 4.1875-3.2773 1.8088-1.0646 3.619-1.808 5.207-2.1484-1.9483-2.1049-4.4884-3.9132-6.287-4.5098l-.0352-.0117c-.4167-.1354-.8136-.1936-1.1914-.1856m4.6718 6.7578c-2.6038 1.2025-5.1088 3.0598-6.2324 4.586l-.0215.0293c-1.6478 2.2683-.0272 4.7953 1.9219 6.211 1.9487 1.4159 4.8518 2.1765 6.5-.0919.0228-.0309.0536-.071.0898-.121-.7356-1.2717-1.396-3.0718-1.8222-4.9981-.4534-2.0492-.6023-4-.4356-5.6153m1.0723 3.338c.3387 2.8478 1.3315 5.8037 2.4355 7.3437l.0215.0293c1.6478 2.2683 4.551 1.5078 6.5.0918 1.9487-1.416 3.5697-3.943 1.9219-6.211-.0228-.0309-.0517-.073-.0879-.123-1.4367.3066-3.3522.3794-5.3164.1894-2.089-.2017-3.9895-.6623-5.4746-1.3203"/></svg>'
  };

  // Edit shortcuts here. Each group is a cluster in the desktop dock and a
  // titled section in the mobile sheet. The "Research projects" group is
  // generated at render time (see generate_dock_projects.R) from every
  // work_projects/*.qmd that has a .ico in its media/ folder.
  var workProjects = (window.CM_DOCK_WORK_PROJECTS || []).map(function (p) {
    return { label: p.label, href: p.href, img: BASE + p.icon };
  });

  var groups = [
    {
      title: "Personal",
      items: [
        { label: "Photos", href: "https://photos.cmaslard.xyz/", svg: ICONS.immich },
        { label: "Jellyfin", href: "https://jellyfin.cmaslard.xyz/", svg: ICONS.jellyfin },
        { label: "Jellyseerr", href: "https://jellyseerr.cmaslard.xyz/", img: BASE + "media/dock/jellyseerr.svg" },
        { label: "Garden Harvest", href: "https://cmaslard.xyz/garden-harvest/", img: BASE + "media/dock/garden-harvest.png" }
      ]
    },
    {
      title: "Teaching",
      items: [
        { label: "Bioinformatics Tools course", href: "https://bioinformatics-tools-course.cmaslard.xyz/", img: BASE + "media/dock/bioinformatics-tools-course.png" }
      ]
    },
    { title: "Research projects", items: workProjects }
  ].filter(function (g) { return g.items.length > 0; });

  // Collapsed navbar (Bootstrap lg breakpoint) or touch screen: use the
  // bottom sheet with labels instead of the hover dock.
  var compactMQ = window.matchMedia("(max-width: 991.98px), (hover: none)");

  function itemMarkup(item) {
    if (item.svg) return item.svg;
    if (item.img) return '<img src="' + item.img + '" alt="" loading="lazy">';
    if (item.bi) return '<i class="bi ' + item.bi + '"></i>';
    return "";
  }

  function makeLink(item, className) {
    var a = document.createElement("a");
    a.className = className;
    a.href = item.href;
    a.target = "_blank";
    a.rel = "noopener";
    a.setAttribute("aria-label", item.label);
    return a;
  }

  // Desktop: horizontal pill next to the trigger, tooltips on hover.
  function buildPanel() {
    var panel = document.createElement("div");
    panel.className = "cm-dock-panel";

    groups.forEach(function (g, i) {
      if (i > 0) {
        var sep = document.createElement("div");
        sep.className = "cm-dock-sep";
        panel.appendChild(sep);
      }

      var group = document.createElement("div");
      group.className = "cm-dock-group";

      g.items.forEach(function (item) {
        var a = makeLink(item, "cm-dock-item");
        a.setAttribute("data-label", item.label);
        a.innerHTML = itemMarkup(item);
        group.appendChild(a);
      });

      panel.appendChild(group);
    });

    return panel;
  }

  // Mobile: bottom sheet appended to <body> (the fixed header uses a
  // transform, which would trap a position:fixed child inside it).
  function buildSheet() {
    var backdrop = document.createElement("div");
    backdrop.className = "cm-sheet-backdrop";

    var sheet = document.createElement("div");
    sheet.className = "cm-sheet";
    sheet.id = "cm-dock-sheet";
    sheet.setAttribute("role", "dialog");
    sheet.setAttribute("aria-modal", "true");
    sheet.setAttribute("aria-labelledby", "cm-sheet-heading");
    sheet.setAttribute("aria-hidden", "true");

    var handle = document.createElement("div");
    handle.className = "cm-sheet-handle";
    sheet.appendChild(handle);

    var header = document.createElement("div");
    header.className = "cm-sheet-header";
    var heading = document.createElement("div");
    heading.className = "cm-sheet-heading";
    heading.id = "cm-sheet-heading";
    heading.textContent = "Quick links";
    var close = document.createElement("button");
    close.type = "button";
    close.className = "cm-sheet-close";
    close.setAttribute("aria-label", "Close");
    close.innerHTML = '<i class="bi bi-x-lg"></i>';
    header.appendChild(heading);
    header.appendChild(close);
    sheet.appendChild(header);

    var body = document.createElement("div");
    body.className = "cm-sheet-body";

    groups.forEach(function (g) {
      var section = document.createElement("section");
      section.className = "cm-sheet-section";

      var title = document.createElement("div");
      title.className = "cm-sheet-title";
      title.textContent = g.title;
      section.appendChild(title);

      var grid = document.createElement("div");
      grid.className = "cm-sheet-grid";

      g.items.forEach(function (item) {
        var a = makeLink(item, "cm-sheet-item");
        var icon = document.createElement("span");
        icon.className = "cm-sheet-icon";
        icon.innerHTML = itemMarkup(item);
        var label = document.createElement("span");
        label.className = "cm-sheet-label";
        label.textContent = item.label;
        a.appendChild(icon);
        a.appendChild(label);
        grid.appendChild(a);
      });

      section.appendChild(grid);
      body.appendChild(section);
    });

    sheet.appendChild(body);
    return { backdrop: backdrop, sheet: sheet, close: close };
  }

  function addDock() {
    var tools = document.querySelector(".quarto-navbar-tools");
    if (!tools || tools.querySelector(".cm-dock")) return;

    var toggle = tools.querySelector(".quarto-color-scheme-toggle");

    var dock = document.createElement("div");
    dock.className = "cm-dock";

    var trigger = document.createElement("button");
    trigger.type = "button";
    trigger.className = "cm-dock-trigger quarto-navigation-tool px-1";
    trigger.title = "Quick links";
    trigger.setAttribute("aria-label", "Quick links");
    trigger.setAttribute("aria-expanded", "false");
    trigger.innerHTML = '<i class="bi bi-grid-3x3-gap-fill"></i>';

    var panel = buildPanel();
    dock.appendChild(trigger);
    dock.appendChild(panel);
    tools.insertBefore(dock, toggle);

    var s = buildSheet();
    document.body.appendChild(s.backdrop);
    document.body.appendChild(s.sheet);

    var hoverOpen = false;
    var clickOpen = false;
    var closeTimer = null;

    function isCompact() {
      return compactMQ.matches;
    }

    function keepOnScreen() {
      panel.style.right = "";
      var rect = panel.getBoundingClientRect();
      var overflow = 8 - rect.left;
      if (overflow > 0) {
        panel.style.right = "calc(100% + 10px - " + overflow + "px)";
      }
    }

    function sync() {
      var open = hoverOpen || clickOpen;
      var compact = isCompact();
      var sheetOpen = open && compact;
      var wasSheetOpen = s.sheet.classList.contains("open");

      dock.classList.toggle("open", open && !compact);
      s.sheet.classList.toggle("open", sheetOpen);
      s.backdrop.classList.toggle("open", sheetOpen);
      s.sheet.setAttribute("aria-hidden", String(!sheetOpen));
      document.documentElement.classList.toggle("cm-sheet-open", sheetOpen);
      trigger.setAttribute("aria-expanded", String(open));

      if (open && !compact) keepOnScreen();
      if (sheetOpen && !wasSheetOpen) s.close.focus({ preventScroll: true });
      if (!sheetOpen && wasSheetOpen && s.sheet.contains(document.activeElement)) {
        trigger.focus({ preventScroll: true });
      }
    }

    function reset() {
      clearTimeout(closeTimer);
      hoverOpen = false;
      clickOpen = false;
      sync();
    }

    // Hover only for a real mouse: on touch screens the emulated
    // mouseenter used to keep the dock open after a second tap.
    dock.addEventListener("pointerenter", function (e) {
      if (e.pointerType !== "mouse" || isCompact()) return;
      clearTimeout(closeTimer);
      hoverOpen = true;
      sync();
    });

    dock.addEventListener("pointerleave", function (e) {
      if (e.pointerType !== "mouse") return;
      clearTimeout(closeTimer);
      closeTimer = setTimeout(function () {
        hoverOpen = false;
        sync();
      }, 200);
    });

    trigger.addEventListener("click", function (e) {
      e.stopPropagation();
      clickOpen = !clickOpen;
      if (!clickOpen) hoverOpen = false;
      sync();
    });

    s.close.addEventListener("click", reset);
    s.backdrop.addEventListener("click", reset);

    // Close the sheet once a link is followed (it opens in a new tab).
    s.sheet.addEventListener("click", function (e) {
      if (e.target.closest("a")) setTimeout(reset, 0);
    });

    document.addEventListener("click", function (e) {
      if (!dock.contains(e.target) && !s.sheet.contains(e.target)) reset();
    });

    document.addEventListener("keydown", function (e) {
      if (e.key === "Escape") reset();
    });

    if (compactMQ.addEventListener) {
      compactMQ.addEventListener("change", reset);
    } else if (compactMQ.addListener) {
      compactMQ.addListener(reset);
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", addDock);
  } else {
    addDock();
  }
})();
