library(readr)
library(dplyr)

message("Generating warming stripes CSS from NASA GISTEMP...")

temp_url <- "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv"
temp <- tryCatch({
  read_csv(temp_url, skip = 1, na = "***", show_col_types = FALSE) |>
    select(Year, `J-D`) |>
    filter(!is.na(`J-D`)) |>
    rename(year = Year, anomaly = `J-D`) |>
    mutate(year = as.integer(year))
}, error = function(e) {
  message("Could not fetch GISTEMP data — keeping existing stripe.css")
  NULL
})

if (is.null(temp)) quit(save = "no")

ref_mean <- temp |> filter(year >= 1961, year <= 1990) |> pull(anomaly) |> mean(na.rm = TRUE)
temp     <- temp |> mutate(z = anomaly - ref_mean)

make_gradient <- function(neutral_hex) {
  nr <- strtoi(substr(neutral_hex, 2, 3), 16L)
  ng <- strtoi(substr(neutral_hex, 4, 5), 16L)
  nb <- strtoi(substr(neutral_hex, 6, 7), 16L)

  stops <- data.frame(
    pos = c(0.0,   0.3,   0.5,  0.7,   1.0),
    r   = c(0x1D,  0x45,  nr,   0xE6,  0x7A),
    g   = c(0x35,  0x7B,  ng,   0x39,  0x00),
    b   = c(0x57,  0x9D,  nb,   0x46,  0x00)
  )

  hex <- function(z) {
    p   <- (max(-1.5, min(1.5, z)) + 1.5) / 3
    idx <- max(1, min(findInterval(p, stops$pos), nrow(stops) - 1))
    t   <- (p - stops$pos[idx]) / (stops$pos[idx + 1] - stops$pos[idx])
    sprintf("#%02X%02X%02X",
      round(stops$r[idx] + t * (stops$r[idx+1] - stops$r[idx])),
      round(stops$g[idx] + t * (stops$g[idx+1] - stops$g[idx])),
      round(stops$b[idx] + t * (stops$b[idx+1] - stops$b[idx])))
  }

  colors  <- sapply(temp$z, hex)
  n       <- length(colors)
  pct     <- seq(0, 100, length.out = n + 1)
  parts   <- sapply(seq_along(colors), function(i)
    sprintf("%s %.2f%% %.2f%%", colors[i], pct[i], pct[i + 1]))
  sprintf("linear-gradient(to right, %s)", paste(parts, collapse = ", "))
}

light <- make_gradient("#F5F5F5")  # near-white neutral (light mode)
dark  <- make_gradient("#181818")  # body-bg colour (dark mode — neutral years vanish into background)

css <- sprintf(
'/* Warming stripes -- auto-generated from NASA GISTEMP v4 at last render (%s)
   Baseline 1961-1990 | %d years (%d--%d) */
footer.footer {
  border-top: none !important;
  background-image: %s;
  background-size: 100%% 6px;
  background-repeat: no-repeat;
  background-position: bottom;
  padding-bottom: 6px;
}

body.quarto-dark footer.footer {
  background-image: %s;
}
',
  format(Sys.Date(), "%Y-%m-%d"),
  nrow(temp), min(temp$year), max(temp$year),
  light, dark
)

writeLines(css, "stripe.css")
message(sprintf("stripe.css updated — %d years (%d to %d)", nrow(temp), min(temp$year), max(temp$year)))
