# Cache busting for the site's own CSS/JS.
#
# Browsers may keep these files for hours (cache-control: max-age=14400), so a
# visitor could see an old dock or stripe after an update. Each URL gets a
# short content hash (?v=...): when a file changes its URL changes, and the
# browser downloads the new version right away.
#
# Runs as a Quarto pre-render step, after the scripts that regenerate
# stripe.css and media/dock/work-projects-data.js.

version_of <- function(f) substr(unname(tools::md5sum(f)), 1, 8)

css <- c("stripe.css", "dock.css")
js <- c(
  "image-dark-light.js",
  "stripe-link.js",
  "media/dock/work-projects-data.js",
  "dock.js"
)

dir.create("_includes", showWarnings = FALSE)

writeLines(
  sprintf('<link rel="stylesheet" href="/%s?v=%s">', css, vapply(css, version_of, "")),
  "_includes/assets-head.html"
)
writeLines(
  sprintf(
    '<script type="application/javascript" src="/%s?v=%s"></script>',
    js, vapply(js, version_of, "")
  ),
  "_includes/assets-body.html"
)

message("asset versions updated (_includes/assets-*.html)")
