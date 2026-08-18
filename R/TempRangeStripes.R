
# Temperaturrekkevidde-graf per stasjon fra Frost API.
# For hver måned (jan øverst, des nederst) vises én horisontal strek per år
# fra laveste til høyeste observerte timeverdi.
# Inneværende årets observasjoner plottes som punkter på streken.
# Ny rekord (varmest/kaldest noensinne for måneden) markeres.

library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

API_ID     <- "6f8370e6-c157-4bb8-8f19-2935c210299b"
API_SECRET <- "b5e4375f-67c3-42d4-a3a1-6d6599eb3d45"

# --- Brukervalg ---------------------------------------------------------------
STATION    <- "SN40510"   # Blindern, bytt etter ønske
ELEMENT    <- "air_temperature"
HIST_REQ_START <- 1800    # tidlig forespurt start, faktisk start hentes fra datasettet
CUR_YEAR   <- as.integer(format(Sys.Date(), "%Y"))
# ------------------------------------------------------------------------------

MAANED_NAVN <- c("Januar","Februar","Mars","April","Mai","Juni",
                 "Juli","August","September","Oktober","November","Desember")

# --- Hjelpefunksjon: enkel Frost-GET ------------------------------------------
frost_get <- function(source, element, ref_time, timeresolution,
                      api_id = API_ID, api_secret = API_SECRET) {
  endpoint <- "https://frost.met.no/observations/v0.jsonld"
  res <- GET(
    url = endpoint,
    authenticate(api_id, api_secret),
    query = list(
      sources         = source,
      referencetime   = ref_time,
      elements        = element,
      timeresolutions = timeresolution,
      limit           = 100000
    )
  )
  if (status_code(res) == 404) return(NULL)
  if (status_code(res) != 200) {
    msg <- content(res, as = "text", encoding = "UTF-8")
    stop(sprintf("Frost API-feil (%d): %s", status_code(res), msg))
  }
  payload <- fromJSON(content(res, as = "text", encoding = "UTF-8"),
                      simplifyDataFrame = FALSE)
  if (is.null(payload$data) || length(payload$data) == 0) return(NULL)

  rows <- lapply(payload$data, function(item) {
    t   <- as.POSIXct(item$referenceTime, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    obs <- item$observations
    if (length(obs) == 0 || is.null(obs[[1]]$value)) return(NULL)
    data.frame(time = t, value = as.numeric(obs[[1]]$value),
               stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}

# Hent PT1H for ett enkelt år (brukes kun for inneværende år)
fetch_obs_hourly_year <- function(source, element, year,
                                  api_id = API_ID, api_secret = API_SECRET) {
  ref_time <- sprintf("%d-01-01T00:00:00Z/%d-12-31T23:59:59Z", year, year)
  frost_get(source, element, ref_time, "PT1H", api_id, api_secret)
}

# Enkel null-koalescing for eldre R-versjoner
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Indikator for datadekning i normalperioder
normal_status <- function(df, start_year, end_year) {
  needed_years <- end_year - start_year + 1
  period_df <- df %>%
    filter(year >= start_year, year <= end_year)

  if (nrow(period_df) == 0) {
    return(list(status = "ingen", label = "Ingen data"))
  }

  by_month <- period_df %>%
    group_by(month) %>%
    summarise(n_years = n_distinct(year), .groups = "drop")

  year_span <- period_df %>%
    summarise(ymin = min(year, na.rm = TRUE), ymax = max(year, na.rm = TRUE))

  full_coverage <- nrow(by_month) == 12 && all(by_month$n_years == needed_years)
  if (full_coverage) {
    list(status = "full", label = "Full dekning")
  } else {
    list(
      status = "delvis",
      label = sprintf("Delvis (%d-%d)", year_span$ymin, year_span$ymax)
    )
  }
}

# --- 1. Hent historiske månedlige min/max via P1M-aggregater ------------------
message("Henter historiske min/max (P1M) fra ", HIST_REQ_START, " til ", CUR_YEAR - 1, " ...")

hist_ref <- sprintf("%d-01-01T00:00:00Z/%d-12-31T23:59:59Z",
                    HIST_REQ_START, CUR_YEAR - 1)

hist_min_raw <- frost_get(STATION, sprintf("min(%s P1M)", ELEMENT), hist_ref, "P1M")
hist_max_raw <- frost_get(STATION, sprintf("max(%s P1M)", ELEMENT), hist_ref, "P1M")

if (is.null(hist_min_raw) || is.null(hist_max_raw))
  stop("Ingen historiske P1M-data returnert.")

hist_min <- hist_min_raw %>% mutate(year = year(time), month = month(time)) %>%
  rename(t_min = value) %>% select(year, month, t_min)
hist_max <- hist_max_raw %>% mutate(year = year(time), month = month(time)) %>%
  rename(t_max = value) %>% select(year, month, t_max)

hist_df <- inner_join(hist_min, hist_max, by = c("year", "month"))
HIST_START <- min(hist_df$year, na.rm = TRUE)

if (!is.finite(HIST_START)) {
  stop("Fant ikke gyldig startaar i historiske data.")
}

# Absolutt rekord per måned over alle historiske år
records <- hist_df %>%
  group_by(month) %>%
  summarise(
    rec_min = min(t_min, na.rm = TRUE),
    rec_max = max(t_max, na.rm = TRUE),
    .groups = "drop"
  )

# --- 2. Hent inneværende årets data -------------------------------------------
message("Henter inneværende år (", CUR_YEAR, ") ...")

cur_raw <- fetch_obs_hourly_year(STATION, ELEMENT, CUR_YEAR)

# Daglige min/maksmålinger for mer presis månedsrekkevidde
cur_ref <- sprintf("%d-01-01T00:00:00Z/%d-12-31T23:59:59Z", CUR_YEAR, CUR_YEAR)
cur_min_daily_raw <- frost_get(STATION, sprintf("min(%s P1D)", ELEMENT), cur_ref, "P1D")
cur_max_daily_raw <- frost_get(STATION, sprintf("max(%s P1D)", ELEMENT), cur_ref, "P1D")

if (is.null(cur_raw)) {
  warning("Ingen data for inneværende år – fortsetter uten.")
  cur_df <- data.frame(time = POSIXct(), value = numeric(),
                       year = integer(), month = integer())
} else {
  cur_df <- cur_raw %>%
    mutate(year = year(time), month = month(time))
}

# Min/max per måned for inneværende år (til strekene)
# Primært fra daglige min/maksmålinger (P1D), fallback til timeverdier ved manglende data.
if (is.null(cur_min_daily_raw) || is.null(cur_max_daily_raw)) {
  warning("Mangler P1D min/maks for inneværende år – bruker timeverdier som fallback.")
  cur_range <- cur_df %>%
    group_by(month) %>%
    summarise(
      t_min = min(value, na.rm = TRUE),
      t_max = max(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(year = CUR_YEAR)
} else {
  cur_min_daily <- cur_min_daily_raw %>%
    mutate(year = year(time), month = month(time)) %>%
    rename(t_min_day = value) %>%
    select(year, month, time, t_min_day)

  cur_max_daily <- cur_max_daily_raw %>%
    mutate(year = year(time), month = month(time)) %>%
    rename(t_max_day = value) %>%
    select(year, month, time, t_max_day)

  cur_daily <- inner_join(cur_min_daily, cur_max_daily, by = c("year", "month", "time"))

  cur_range <- cur_daily %>%
    group_by(month) %>%
    summarise(
      t_min = min(t_min_day, na.rm = TRUE),
      t_max = max(t_max_day, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(year = CUR_YEAR)
}

# Sjekk om inneværende år setter ny rekord
cur_records <- cur_range %>%
  left_join(records, by = "month") %>%
  mutate(
    ny_varmest = t_max > rec_max,
    ny_kaldest = t_min < rec_min
  )

# Normalindikatorer
norm_6190 <- normal_status(hist_df, 1961, 1990)
norm_9120 <- normal_status(hist_df, 1991, 2020)

# Normalstreker per måned (rekkevidde innen hver normalperiode)
normal_6190 <- hist_df %>%
  filter(year >= 1961, year <= 1990) %>%
  group_by(month) %>%
  summarise(
    t_min = min(t_min, na.rm = TRUE),
    t_max = max(t_max, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mnd_f = factor(month, levels = 12:1, labels = rev(MAANED_NAVN)),
    periode = "Normal 1961-1990"
  )

normal_9120 <- hist_df %>%
  filter(year >= 1991, year <= 2020) %>%
  group_by(month) %>%
  summarise(
    t_min = min(t_min, na.rm = TRUE),
    t_max = max(t_max, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mnd_f = factor(month, levels = 12:1, labels = rev(MAANED_NAVN)),
    periode = "Normal 1991-2020"
  )

normal_stripes <- bind_rows(normal_6190, normal_9120)

# --- 3. Bygg plottdata --------------------------------------------------------

# Én samlet historisk strek per måned (absolutt min/maks over alle år)
hist_stripe <- records %>%
  rename(t_min = rec_min, t_max = rec_max) %>%
  mutate(mnd_f = factor(month, levels = 12:1, labels = rev(MAANED_NAVN)))

# Inneværende årets strek
cur_stripe <- cur_range %>%
  mutate(mnd_f = factor(month, levels = 12:1, labels = rev(MAANED_NAVN)))

# Timevise punkter for inneværende år
cur_pts <- cur_df %>%
  filter(!is.na(value)) %>%
  mutate(mnd_f = factor(month, levels = 12:1, labels = rev(MAANED_NAVN)))

# Varmeste/kaldeste verdi per måned for inneværende år (samme grunnlag som cur_range)
cur_ext <- cur_range %>%
  transmute(month, t_max_val = t_max, t_min_val = t_min) %>%
  tidyr::pivot_longer(c(t_max_val, t_min_val), names_to = "type", values_to = "value") %>%
  left_join(cur_records %>% select(month, ny_varmest, ny_kaldest), by = "month") %>%
  mutate(
    rekord = (type == "t_max_val" & ny_varmest) | (type == "t_min_val" & ny_kaldest),
    mnd_f  = factor(month, levels = 12:1, labels = rev(MAANED_NAVN))
  )

# --- 4. Plot ------------------------------------------------------------------

p <- ggplot() +
  # Historisk samlet rekkevidde (grå)
  geom_segment(
    data = hist_stripe,
    aes(x = t_min, xend = t_max, y = mnd_f, yend = mnd_f),
    colour = "grey60", linewidth = 4, alpha = 0.5, lineend = "round"
  ) +
  # Normalperioder som egne streker, lett høydeforskjøvet for synlighet
  geom_segment(
    data = normal_6190,
    aes(
      x = t_min, xend = t_max, y = mnd_f, yend = mnd_f,
      colour = periode, linetype = periode
    ),
    linewidth = 1.5, alpha = 0.95, lineend = "round",
    position = position_nudge(y = 0.12)
  ) +
  geom_segment(
    data = normal_9120,
    aes(
      x = t_min, xend = t_max, y = mnd_f, yend = mnd_f,
      colour = periode, linetype = periode
    ),
    linewidth = 1.5, alpha = 0.95, lineend = "round",
    position = position_nudge(y = -0.12)
  ) +
  # Inneværende årets strek med lys halo for å være synlig over normalstrekene
  geom_segment(
    data = cur_stripe,
    aes(x = t_min, xend = t_max, y = mnd_f, yend = mnd_f),
    colour = "white", linewidth = 2.8, alpha = 0.95, lineend = "round"
  ) +
  geom_segment(
    data = cur_stripe,
    aes(x = t_min, xend = t_max, y = mnd_f, yend = mnd_f),
    colour = "firebrick", linewidth = 1.6, lineend = "round"
  ) +
  # Timevise punkter for inneværende år
  geom_jitter(
    data = cur_pts,
    aes(x = value, y = mnd_f),
    colour = "firebrick", alpha = 0.20, size = 0.7,
    height = 0.20, width = 0
  ) +
  # Varmeste og kaldeste punkt per måned
  geom_point(
    data = cur_ext,
    aes(x = value, y = mnd_f,
        shape = ifelse(rekord, "Ny rekord", "Ekstrem"),
        fill  = ifelse(type == "t_max_val", "varm", "kald")),
    size = 3.5, colour = "black", stroke = 0.8
  ) +
  scale_shape_manual(
    name   = NULL,
    values = c("Ny rekord" = 23, "Ekstrem" = 21)
  ) +
  scale_fill_manual(
    name   = NULL,
    values = c("varm" = "#d73027", "kald" = "#4575b4"),
    guide  = "none"
  ) +
  scale_colour_manual(
    name   = NULL,
    values = c("Normal 1961-1990" = "#1b9e77", "Normal 1991-2020" = "#7570b3")
  ) +
  scale_linetype_manual(
    name   = NULL,
    values = c("Normal 1961-1990" = "dashed", "Normal 1991-2020" = "dotdash")
  ) +
  labs(
    title    = sprintf("Temperaturrekkevidde – stasjon %s (%d–%d)",
                       STATION, HIST_START, CUR_YEAR),
    subtitle = sprintf(
      "Grå strek = historisk rekkevidde (%d–%d)  |  Rød strek = %d  |  Grønn stiplet = normal 1961–1990  |  Lilla strek-punkt = normal 1991–2020",
      HIST_START, CUR_YEAR - 1, CUR_YEAR
    ),
    x       = "Temperatur (°C)",
    y       = NULL,
    caption = sprintf(
      "Data: frost.met.no | Dekning normal 1961-1990: %s | Dekning normal 1991-2020: %s",
      norm_6190$label, norm_9120$label
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_line(colour = "grey92"),
    panel.grid.minor   = element_blank(),
    legend.position    = "bottom",
    plot.title         = element_text(face = "bold"),
    axis.text.y        = element_text(size = 11)
  )

print(p)

# Lagre
outfile <- sprintf("TempRangeStripes_%s_%d.png", STATION, CUR_YEAR)
ggsave(outfile, p, width = 14, height = 10, dpi = 150)
message("Lagret: ", outfile)
