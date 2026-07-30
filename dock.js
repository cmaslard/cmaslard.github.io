// Description: Inject a personal "dock" of quick-access shortcuts into the
// navbar, next to the dark/light toggle. Opens on hover or click.
(function () {
  var thisScript = document.currentScript;
  var BASE = new URL(".", thisScript.src).href;

  var ICONS = {
    jellyfin: '<svg viewBox="0 0 24 24" fill="currentColor" xmlns="http://www.w3.org/2000/svg"><path d="M12 .002C8.826.002-1.398 18.537.16 21.666c1.56 3.129 22.14 3.094 23.682 0C25.384 18.573 15.177 0 12 0zm7.76 18.949c-1.008 2.028-14.493 2.05-15.514 0C3.224 16.9 9.92 4.755 12.003 4.755c2.081 0 8.77 12.166 7.759 14.196zM12 9.198c-1.054 0-4.446 6.15-3.93 7.189.518 1.04 7.348 1.027 7.86 0 .511-1.027-2.874-7.19-3.93-7.19z"/></svg>',
    immich: '<svg viewBox="0 0 24 24" fill="currentColor" xmlns="http://www.w3.org/2000/svg"><path d="M11.9863.2695c-2.409 0-5.207 1.091-5.207 3.8946v.1523c1.3428.597 2.9347 1.6629 4.4121 2.9707 1.5713 1.3912 2.8374 2.8821 3.6524 4.2871 1.3997-2.5034 2.3358-5.4784 2.3476-7.373V4.164c0-2.8035-2.796-3.8946-5.205-3.8946m7.5117 4.4903c-.3778-.0081-.7747.0502-1.1914.1855-.0366.0118-.086.0278-.1445.0469-.1525 1.4611-.6756 3.304-1.4629 5.1133-.8373 1.9243-1.8627 3.5898-2.9472 4.7988 2.8132.558 5.9307.5273 7.7363-.0469.0126-.004.0246-.0065.0351-.0097 2.6665-.8666 2.84-3.8636 2.0957-6.1543-.6279-1.9332-2.081-3.89-4.121-3.9336m-14.996.039C2.4618 4.8424 1.0088 6.7973.3809 8.7305c-.7442 2.291-.5708 5.288 2.0957 6.1543l.1445.0468c.982-1.0926 2.4873-2.2761 4.1875-3.2773 1.8088-1.0646 3.619-1.808 5.207-2.1484-1.9483-2.1049-4.4884-3.9132-6.287-4.5098l-.0352-.0117c-.4167-.1354-.8136-.1936-1.1914-.1856m4.6718 6.7578c-2.6038 1.2025-5.1088 3.0598-6.2324 4.586l-.0215.0293c-1.6478 2.2683-.0272 4.7953 1.9219 6.211 1.9487 1.4159 4.8518 2.1765 6.5-.0919.0228-.0309.0536-.071.0898-.121-.7356-1.2717-1.396-3.0718-1.8222-4.9981-.4534-2.0492-.6023-4-.4356-5.6153m1.0723 3.338c.3387 2.8478 1.3315 5.8037 2.4355 7.3437l.0215.0293c1.6478 2.2683 4.551 1.5078 6.5.0918 1.9487-1.416 3.5697-3.943 1.9219-6.211-.0228-.0309-.0517-.073-.0879-.123-1.4367.3066-3.3522.3794-5.3164.1894-2.089-.2017-3.9895-.6623-5.4746-1.3203"/></svg>'
  };

  // Edit shortcuts here — each group renders as a cluster separated by a divider.
  var groups = [
    [
      { label: "Photos", href: "https://photos.cmaslard.xyz/", svg: ICONS.immich },
      { label: "Jellyfin", href: "https://naslard.taile58962.ts.net/", svg: ICONS.jellyfin },
      { label: "Jellyseerr", href: "https://naslard.taile58962.ts.net:8443/login", img: BASE + "media/dock/jellyseerr.svg" },
      { label: "Garden Harvest", href: "https://cmaslard.xyz/garden-harvest/", img: BASE + "media/dock/garden-harvest.png" }
    ],
    [
      { label: "PhD Soybean", href: "https://phd-soybean-2021-2024.private.cmaslard.xyz/", img: BASE + "work_projects/media/phd_soybean_2021_2024/root.ico" },
      { label: "SCBDLR", href: "https://scbdlr.private.cmaslard.xyz/", img: BASE + "work_projects/media/ScBdLR_init_2025/image_bg_rm.ico" }
    ]
  ];

  function itemMarkup(item) {
    if (item.svg) return item.svg;
    if (item.img) return '<img src="' + item.img + '" alt="" loading="lazy">';
    if (item.bi) return '<i class="bi ' + item.bi + '"></i>';
    return "";
  }

  function buildPanel() {
    var panel = document.createElement("div");
    panel.className = "cm-dock-panel";

    groups.forEach(function (items, i) {
      if (i > 0) {
        var sep = document.createElement("div");
        sep.className = "cm-dock-sep";
        panel.appendChild(sep);
      }

      var group = document.createElement("div");
      group.className = "cm-dock-group";

      items.forEach(function (item) {
        var a = document.createElement("a");
        a.className = "cm-dock-item";
        a.href = item.href;
        a.target = "_blank";
        a.rel = "noopener";
        a.setAttribute("data-label", item.label);
        a.setAttribute("aria-label", item.label);
        a.innerHTML = itemMarkup(item);
        group.appendChild(a);
      });

      panel.appendChild(group);
    });

    return panel;
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

    dock.appendChild(trigger);
    dock.appendChild(buildPanel());
    tools.insertBefore(dock, toggle);

    var hoverOpen = false;
    var clickOpen = false;
    var closeTimer = null;

    function sync() {
      var shouldOpen = hoverOpen || clickOpen;
      dock.classList.toggle("open", shouldOpen);
      trigger.setAttribute("aria-expanded", String(shouldOpen));
    }

    function reset() {
      hoverOpen = false;
      clickOpen = false;
      sync();
    }

    dock.addEventListener("mouseenter", function () {
      clearTimeout(closeTimer);
      hoverOpen = true;
      sync();
    });

    dock.addEventListener("mouseleave", function () {
      clearTimeout(closeTimer);
      closeTimer = setTimeout(function () {
        hoverOpen = false;
        sync();
      }, 200);
    });

    trigger.addEventListener("click", function (e) {
      e.stopPropagation();
      clickOpen = !clickOpen;
      sync();
    });

    document.addEventListener("click", function (e) {
      if (!dock.contains(e.target)) reset();
    });

    document.addEventListener("keydown", function (e) {
      if (e.key === "Escape") reset();
    });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", addDock);
  } else {
    addDock();
  }
})();
