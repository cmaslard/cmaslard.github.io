# One-time Strava authorization for perso_projects/strava_report.qmd
#
# Prints the long-lived refresh token the site uses to fetch new activities
# automatically. Run it again only if you revoke the app's access on Strava.
#
# 1. In ~/.Renviron, add (values from https://www.strava.com/settings/api):
#      STRAVA_CLIENT_ID=...
#      STRAVA_CLIENT_SECRET=...
#    then restart R.
# 2. From the project root: source("strava_auth.R")
#    A browser tab opens on Strava: click "Authorize".
#    (The app's "Authorization Callback Domain" must be: localhost)
# 3. The refresh token is written to ~/.Renviron (STRAVA_REFRESH_TOKEN=...)
#    automatically, without being printed. Then add the three values as
#    GitHub repository secrets (Settings > Secrets and variables > Actions).

creds <- Sys.getenv(c("STRAVA_CLIENT_ID", "STRAVA_CLIENT_SECRET"))
if (any(!nzchar(creds))) {
  stop("Set STRAVA_CLIENT_ID and STRAVA_CLIENT_SECRET in ~/.Renviron, then restart R")
}

tok <- rStrava::strava_oauth(
  app_name = "website",
  app_client_id = creds[[1]],
  app_secret = creds[[2]],
  app_scope = "activity:read_all",
  cache = FALSE
)

refresh <- tok$credentials$refresh_token
if (!nzchar(refresh %||% "")) stop("Strava did not return a refresh token")

# Save it in ~/.Renviron (replacing any previous one) and load it now
renviron <- path.expand("~/.Renviron")
lines <- if (file.exists(renviron)) readLines(renviron, warn = FALSE) else character()
lines <- c(lines[!grepl("^STRAVA_REFRESH_TOKEN=", lines)], paste0("STRAVA_REFRESH_TOKEN=", refresh))
writeLines(lines, renviron)
readRenviron(renviron)
message("STRAVA_REFRESH_TOKEN saved in ~/.Renviron")
