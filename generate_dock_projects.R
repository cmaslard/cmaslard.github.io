library(here)
library(rmarkdown)
library(jsonlite)

message("Generating dock shortcuts for professional projects...")

path <- "work_projects"
qmd_files <- list.files(here::here(path), pattern = "\\.qmd$", full.names = TRUE)

build_entry <- function(qmd_path) {
  name <- tools::file_path_sans_ext(basename(qmd_path))
  fm <- tryCatch(rmarkdown::yaml_front_matter(qmd_path), error = function(e) list())

  icon_dir <- here::here(path, "media", name)
  icon_files <- if (dir.exists(icon_dir)) list.files(icon_dir, pattern = "\\.ico$") else character(0)
  if (length(icon_files) < 1) return(NULL)

  href  <- if (!is.null(fm[["dock-url"]])) fm[["dock-url"]] else paste0("https://cmaslard.xyz/", name, "/")
  label <- if (!is.null(fm$title)) fm$title else name

  list(label = label, href = href, icon = paste0(path, "/media/", name, "/", icon_files[1]))
}

entries <- Filter(Negate(is.null), lapply(qmd_files, build_entry))

json <- jsonlite::toJSON(entries, auto_unbox = TRUE, pretty = FALSE)
writeLines(
  sprintf("window.CM_DOCK_WORK_PROJECTS = %s;", json),
  here::here("media/dock/work-projects-data.js")
)

message(sprintf("dock work-projects data updated — %d project(s)", length(entries)))
