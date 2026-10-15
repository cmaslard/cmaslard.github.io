// Description: Turn the warming stripes in the footer into a clickable link
// to the Climate & Biodiversity Dashboard page.
(function () {
  function addStripeLink() {
    var footer = document.querySelector("footer.footer");
    if (!footer || footer.querySelector(".climate-stripe-link")) return;

    var link = document.createElement("a");
    link.href = "https://cmaslard.xyz/perso_projects/climate_change.html";
    link.className = "climate-stripe-link";
    link.title = "See the Climate & Biodiversity Dashboard";
    link.setAttribute("aria-label", "See the Climate & Biodiversity Dashboard");

    footer.appendChild(link);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", addStripeLink);
  } else {
    addStripeLink();
  }
})();
