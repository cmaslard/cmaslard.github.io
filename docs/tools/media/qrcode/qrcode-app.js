// QR code generator for tools/qrcode.qmd
// Everything runs in the browser: the text and logo given in the page are never sent anywhere.
// Relies on qrcode-generator (Kazuhiko Arase, MIT), vendored in qrcode.js next to this file,
// and loads jsQR (Apache 2.0) on demand to check that codes carrying a logo still decode.
(function () {
  "use strict";

  qrcode.stringToBytes = qrcode.stringToBytesFuncs["UTF-8"];

  // Max bytes a version-40 QR code can hold in byte mode, per error-correction level
  var CAPACITY = { L: 2953, M: 2331, Q: 1663, H: 1273 };
  // Bitmap logos larger than this are downscaled so SVG exports stay light
  // (a 30% logo on a 2048 px export is about 615 px wide)
  var LOGO_MAX_PX = 640;
  var DECODER_SRC = document.currentScript.src.replace(/[^/]*$/, "jsQR.js");

  var $ = function (id) { return document.getElementById(id); };
  var el = {
    text: $("qr-text"), ecc: $("qr-ecc"),
    size: $("qr-size"), sizeOut: $("qr-size-out"),
    margin: $("qr-margin"), marginOut: $("qr-margin-out"),
    fg: $("qr-fg"), bg: $("qr-bg"), transparent: $("qr-transparent"),
    logoFile: $("qr-logo"), logoRemove: $("qr-logo-remove"), logoError: $("qr-logo-error"),
    logoSize: $("qr-logo-size"), logoSizeOut: $("qr-logo-size-out"), logoClear: $("qr-logo-clear"),
    preview: $("qr-preview"), info: $("qr-info"), scan: $("qr-scan"), warn: $("qr-warn"),
    png: $("qr-png"), svg: $("qr-svg"), copy: $("qr-copy")
  };
  var current = null; // { qr, opts } of the code currently displayed
  var logo = null;    // { src, img, ratio } of the loaded logo

  function readOptions() {
    return {
      text: el.text.value,
      ecc: el.ecc.value,
      size: parseInt(el.size.value, 10),
      margin: parseInt(el.margin.value, 10),
      fg: el.fg.value,
      bg: el.bg.value,
      transparent: el.transparent.checked,
      logoSize: parseInt(el.logoSize.value, 10) / 100,
      logoClear: el.logoClear.checked
    };
  }

  function oddCeil(v) {
    var k = Math.ceil(v);
    return k % 2 ? k : k + 1;
  }

  // Logo placement in module units, centred on the code. The cleared zone snaps to whole
  // modules (an odd count, like the module count) and leaves about half a module of padding.
  function logoBox(qr, o) {
    if (!logo) return null;
    var n = qr.getModuleCount();
    var side = n * o.logoSize;
    var w = logo.ratio >= 1 ? side : side * logo.ratio;
    var h = logo.ratio >= 1 ? side / logo.ratio : side;
    var cw = oddCeil(w + 1), ch = oddCeil(h + 1);
    return {
      x: o.margin + (n - w) / 2, y: o.margin + (n - h) / 2, w: w, h: h,
      c0: (n - cw) / 2, c1: (n + cw) / 2, r0: (n - ch) / 2, r1: (n + ch) / 2
    };
  }

  // Calls cb(x, y, width) for each horizontal run of dark modules, in module units
  function eachRun(qr, o, box, cb) {
    var n = qr.getModuleCount();
    var clear = box && o.logoClear ? box : null;
    var dark = function (r, c) {
      return qr.isDark(r, c) &&
        !(clear && r >= clear.r0 && r < clear.r1 && c >= clear.c0 && c < clear.c1);
    };
    for (var r = 0; r < n; r++) {
      for (var c = 0; c < n; c++) {
        if (!dark(r, c)) continue;
        var start = c;
        while (c + 1 < n && dark(r, c + 1)) c++;
        cb(start + o.margin, r + o.margin, c - start + 1);
      }
    }
  }

  // Integer pixels per module, so exported PNG edges stay sharp
  function geometry(qr, o) {
    var total = qr.getModuleCount() + 2 * o.margin;
    var scale = Math.max(1, Math.round(o.size / total));
    return { total: total, scale: scale, px: total * scale };
  }

  function toSvg(qr, o) {
    var g = geometry(qr, o);
    var box = logoBox(qr, o);
    var d = "";
    eachRun(qr, o, box, function (x, y, w) {
      d += "M" + x + " " + y + "h" + w + "v1h-" + w + "z";
    });
    var num = function (v) { return +v.toFixed(3); };
    return '<svg xmlns="http://www.w3.org/2000/svg"' +
      (box ? ' xmlns:xlink="http://www.w3.org/1999/xlink"' : "") +
      ' viewBox="0 0 ' + g.total + " " + g.total +
      '" width="' + g.px + '" height="' + g.px + '" shape-rendering="crispEdges">' +
      (o.transparent ? "" : '<rect width="' + g.total + '" height="' + g.total + '" fill="' + o.bg + '"/>') +
      '<path fill="' + o.fg + '" d="' + d + '"/>' +
      (box ? '<image x="' + num(box.x) + '" y="' + num(box.y) + '" width="' + num(box.w) +
        '" height="' + num(box.h) + '" xlink:href="' + logo.src + '"/>' : "") +
      "</svg>";
  }

  function toCanvas(qr, o) {
    var g = geometry(qr, o);
    var box = logoBox(qr, o);
    var canvas = document.createElement("canvas");
    canvas.width = canvas.height = g.px;
    var ctx = canvas.getContext("2d");
    if (!o.transparent) {
      ctx.fillStyle = o.bg;
      ctx.fillRect(0, 0, g.px, g.px);
    }
    ctx.fillStyle = o.fg;
    eachRun(qr, o, box, function (x, y, w) {
      ctx.fillRect(x * g.scale, y * g.scale, w * g.scale, g.scale);
    });
    if (box) {
      ctx.imageSmoothingQuality = "high";
      ctx.drawImage(logo.img, box.x * g.scale, box.y * g.scale, box.w * g.scale, box.h * g.scale);
    }
    return canvas;
  }

  function toPngBlob(qr, o) {
    return new Promise(function (resolve) {
      toCanvas(qr, o).toBlob(resolve, "image/png");
    });
  }

  function luminance(hex) {
    var rgb = [1, 3, 5].map(function (i) {
      var v = parseInt(hex.substr(i, 2), 16) / 255;
      return v <= 0.03928 ? v / 12.92 : Math.pow((v + 0.055) / 1.055, 2.4);
    });
    return 0.2126 * rgb[0] + 0.7152 * rgb[1] + 0.0722 * rgb[2];
  }

  function warnings(o) {
    var out = [];
    if (logo && o.ecc !== "H") {
      out.push("With a logo, High error correction is recommended so that the hidden part of the code can be recovered.");
    }
    if (o.margin < 2) {
      out.push("A margin (quiet zone) of at least 4 modules is recommended; some scanners struggle with less.");
    }
    if (o.transparent) {
      out.push("Transparent background: place the code on a plain, light background for reliable scanning.");
    } else {
      var lf = luminance(o.fg), lb = luminance(o.bg);
      if (lf > lb) {
        out.push("Light code on a dark background: some scanners cannot read inverted QR codes.");
      } else if ((lb + 0.05) / (lf + 0.05) < 4) {
        out.push("Low contrast between the two colours: the code may be hard to scan.");
      }
    }
    return out;
  }

  // --- Scan check (only when a logo hides part of the code) ---

  var decoder = null;
  function loadDecoder() {
    if (!decoder) {
      decoder = new Promise(function (resolve, reject) {
        var s = document.createElement("script");
        s.src = DECODER_SRC;
        s.onload = function () { resolve(window.jsQR); };
        s.onerror = function () { decoder = null; reject(); };
        document.head.appendChild(s);
      });
    }
    return decoder;
  }

  // Decodes a clean rendering of the code, padded with a quiet zone, and compares the bytes
  function decodes(jsQR, state) {
    var qr = state.qr, o = state.opts;
    var total = qr.getModuleCount() + 2 * o.margin;
    var scale = Math.min(8, Math.max(3, Math.floor(640 / total)));
    var code = toCanvas(qr, Object.assign({}, o, { size: total * scale }));
    var pad = 4 * scale;
    var canvas = document.createElement("canvas");
    canvas.width = canvas.height = code.width + 2 * pad;
    var ctx = canvas.getContext("2d");
    ctx.fillStyle = o.transparent ? "#ffffff" : o.bg;
    ctx.fillRect(0, 0, canvas.width, canvas.height);
    ctx.drawImage(code, pad, pad);
    var img = ctx.getImageData(0, 0, canvas.width, canvas.height);
    var result = jsQR(img.data, img.width, img.height, { inversionAttempts: "attemptBoth" });
    var expected = qrcode.stringToBytes(o.text);
    return !!result && result.binaryData.length === expected.length &&
      result.binaryData.every(function (b, i) { return b === expected[i]; });
  }

  var scanTimer = null;
  function scheduleScanCheck() {
    clearTimeout(scanTimer);
    el.scan.innerHTML = "";
    if (!current || !logo) return;
    var state = current;
    el.scan.innerHTML = '<p class="qr-scan-status">Checking that the code still scans…</p>';
    scanTimer = setTimeout(function () {
      loadDecoder().then(function (jsQR) {
        if (state !== current) return;
        el.scan.innerHTML = decodes(jsQR, state)
          ? '<p class="qr-scan-status text-success"><i class="bi bi-check-circle"></i> ' +
            "Scan check passed: the code still decodes.</p>"
          : '<div class="alert alert-danger py-1 px-2 mb-2 small"><i class="bi bi-x-circle"></i> ' +
            "Scan check failed: this code cannot be decoded. Make the logo smaller or use High error correction.</div>";
      }, function () {
        if (state === current) el.scan.innerHTML = "";
      });
    }, 300);
  }

  // --- Rendering ---

  function setEnabled(on) {
    [el.png, el.svg, el.copy].forEach(function (b) { b.disabled = !on; });
  }

  function showMessage(msg) {
    current = null;
    el.preview.classList.remove("qr-checker");
    el.preview.innerHTML = '<p class="qr-placeholder">' + msg + "</p>";
    el.info.textContent = "";
    el.warn.innerHTML = "";
    scheduleScanCheck();
    setEnabled(false);
  }

  function render() {
    var o = readOptions();
    el.sizeOut.textContent = o.size + " px";
    el.marginOut.textContent = o.margin + (o.margin === 1 ? " module" : " modules");
    el.logoSizeOut.textContent = Math.round(o.logoSize * 100) + "%";
    el.bg.disabled = o.transparent;

    if (!o.text) {
      showMessage("Type a URL or some text to generate a QR code.");
      return;
    }

    var qr;
    try {
      qr = qrcode(0, o.ecc);
      qr.addData(o.text, "Byte");
      qr.make();
    } catch (e) {
      showMessage("Too much content for a single QR code at this error-correction level" +
        " (max " + CAPACITY[o.ecc].toLocaleString("en") + " bytes). " +
        "Shorten the text or lower the error-correction level.");
      return;
    }

    current = { qr: qr, opts: o };
    var n = qr.getModuleCount();
    var g = geometry(qr, o);
    var bytes = qrcode.stringToBytes(o.text).length;

    el.preview.classList.toggle("qr-checker", o.transparent);
    el.preview.innerHTML = toSvg(qr, o);
    el.info.textContent = "Version " + ((n - 17) / 4) + " · " + n + "×" + n + " modules · " +
      g.px + "×" + g.px + " px · " + bytes + (bytes === 1 ? " byte" : " bytes");
    el.warn.innerHTML = warnings(o).map(function (w) {
      return '<div class="alert alert-warning py-1 px-2 mb-2 small">' + w + "</div>";
    }).join("");
    scheduleScanCheck();
    setEnabled(true);
  }

  // --- Logo ---

  function loadImage(src) {
    return new Promise(function (resolve, reject) {
      var img = new Image();
      img.onload = function () { resolve(img); };
      img.onerror = reject;
      img.src = src;
    });
  }

  function readLogo(file) {
    return new Promise(function (resolve, reject) {
      var reader = new FileReader();
      reader.onload = function () { resolve(reader.result); };
      reader.onerror = reject;
      reader.readAsDataURL(file);
    }).then(function (src) {
      return loadImage(src).then(function (img) {
        var w = img.naturalWidth, h = img.naturalHeight;
        var ratio = w && h ? w / h : 1;
        if (file.type === "image/svg+xml" || Math.max(w, h) <= LOGO_MAX_PX) {
          return { src: src, img: img, ratio: ratio };
        }
        var k = LOGO_MAX_PX / Math.max(w, h);
        var c = document.createElement("canvas");
        c.width = Math.round(w * k);
        c.height = Math.round(h * k);
        c.getContext("2d").drawImage(img, 0, 0, c.width, c.height);
        var small = file.type === "image/jpeg" ? c.toDataURL("image/jpeg", 0.9) : c.toDataURL("image/png");
        return loadImage(small).then(function (s) { return { src: small, img: s, ratio: ratio }; });
      });
    });
  }

  function setLogo(value) {
    logo = value;
    el.logoRemove.hidden = !logo;
    document.querySelectorAll("[data-logo-option]").forEach(function (e) { e.hidden = !logo; });
    render();
  }

  el.logoFile.addEventListener("change", function () {
    var file = el.logoFile.files[0];
    el.logoError.textContent = "";
    if (!file) return;
    readLogo(file).then(function (value) {
      el.ecc.value = "H";
      setLogo(value);
    }, function () {
      el.logoFile.value = "";
      el.logoError.textContent = "This file could not be read as an image.";
    });
  });

  el.logoRemove.addEventListener("click", function () {
    el.logoFile.value = "";
    el.logoError.textContent = "";
    setLogo(null);
  });

  // --- Export ---

  function fileName(text, ext) {
    var slug = text.trim()
      .replace(/^[a-z][a-z0-9+.-]*:\/\//i, "").replace(/^www\./i, "")
      .normalize("NFD").replace(/[̀-ͯ]/g, "")
      .replace(/[^a-z0-9]+/gi, "-").slice(0, 40)
      .replace(/^-+|-+$/g, "").toLowerCase();
    return "qrcode" + (slug ? "-" + slug : "") + "." + ext;
  }

  function download(blob, name) {
    var a = document.createElement("a");
    a.href = URL.createObjectURL(blob);
    a.download = name;
    document.body.appendChild(a);
    a.click();
    a.remove();
    setTimeout(function () { URL.revokeObjectURL(a.href); }, 1000);
  }

  function flash(button, label) {
    var original = button.dataset.label || button.innerHTML;
    button.dataset.label = original;
    button.textContent = label;
    clearTimeout(button._flash);
    button._flash = setTimeout(function () { button.innerHTML = original; }, 1500);
  }

  el.png.addEventListener("click", function () {
    if (!current) return;
    var o = current.opts;
    toPngBlob(current.qr, o).then(function (blob) { download(blob, fileName(o.text, "png")); });
  });

  el.svg.addEventListener("click", function () {
    if (!current) return;
    var o = current.opts;
    var blob = new Blob([toSvg(current.qr, o)], { type: "image/svg+xml" });
    download(blob, fileName(o.text, "svg"));
  });

  if (navigator.clipboard && window.ClipboardItem) {
    el.copy.addEventListener("click", function () {
      if (!current) return;
      // Safari needs the ClipboardItem to be created synchronously in the click handler
      var item = new ClipboardItem({ "image/png": toPngBlob(current.qr, current.opts) });
      navigator.clipboard.write([item]).then(
        function () { flash(el.copy, "Copied!"); },
        function () { flash(el.copy, "Copy failed"); }
      );
    });
  } else {
    el.copy.hidden = true;
  }

  ["text", "ecc", "size", "margin", "fg", "bg", "transparent", "logoSize", "logoClear"].forEach(function (k) {
    el[k].addEventListener("input", render);
    el[k].addEventListener("change", render);
  });

  render();
})();
