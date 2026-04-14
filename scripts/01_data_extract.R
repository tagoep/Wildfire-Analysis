# ============================================================
# U.S. Wildfire Risk Analysis (1992–2015)
# Step 1: Data Extraction and Cleaning
# ============================================================

library(DBI)
library(RSQLite)
library(dplyr)
library(lubridate)

# ── Connect to database ──────────────────────────────────────
con <- dbConnect(RSQLite::SQLite(),
                 "C:/Users/............/FPA_FOD_20170508.sqlite")

# ── Extract only the columns we need ─────────────────────────
cat("=== Extracting Fires Data ===\n")

fires_raw <- dbGetQuery(con, "
  SELECT 
    FIRE_YEAR,
    DISCOVERY_DOY,
    STAT_CAUSE_DESCR,
    FIRE_SIZE,
    FIRE_SIZE_CLASS,
    STATE,
    LATITUDE,
    LONGITUDE,
    OWNER_DESCR
  FROM Fires
  WHERE FIRE_YEAR BETWEEN 1992 AND 2015
")

# ── Disconnect now that data is loaded into R ─────────────────
dbDisconnect(con)
cat("Database disconnected cleanly\n")

# ── Basic checks ─────────────────────────────────────────────
cat("\n=== Data Overview ===\n")
cat("Total records:", format(nrow(fires_raw), big.mark = ","), "\n")
cat("Years covered:", min(fires_raw$FIRE_YEAR), "to",
    max(fires_raw$FIRE_YEAR), "\n")
cat("Columns:", ncol(fires_raw), "\n")

# ── Check missing values ──────────────────────────────────────
cat("\n=== Missing Values per Column ===\n")
missing_summary <- fires_raw %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  tidyr::pivot_longer(everything(),
                      names_to = "column",
                      values_to = "missing") %>%
  filter(missing > 0)

if(nrow(missing_summary) == 0) {
  cat("No missing values found\n")
} else {
  print(missing_summary)
}

# ── Clean the data ────────────────────────────────────────────
cat("\n=== Cleaning Data ===\n")

fires_clean <- fires_raw %>%
  filter(!is.na(FIRE_SIZE),
         !is.na(STATE),
         !is.na(STAT_CAUSE_DESCR),
         FIRE_SIZE > 0) %>%
  mutate(
    # Convert day of year to approximate month
    MONTH = as.integer(ceiling(DISCOVERY_DOY / 30.44)),
    MONTH = pmin(MONTH, 12),
    MONTH_NAME = month.abb[MONTH],
    # Classify cause as human or natural
    CAUSE_TYPE = ifelse(STAT_CAUSE_DESCR == "Lightning",
                        "Natural", "Human"),
    # Create readable size class labels
    SIZE_CLASS_LABEL = case_when(
      FIRE_SIZE_CLASS == "A" ~ "A: <0.25 acres",
      FIRE_SIZE_CLASS == "B" ~ "B: 0.25-9.9 acres",
      FIRE_SIZE_CLASS == "C" ~ "C: 10-99.9 acres",
      FIRE_SIZE_CLASS == "D" ~ "D: 100-299 acres",
      FIRE_SIZE_CLASS == "E" ~ "E: 300-999 acres",
      FIRE_SIZE_CLASS == "F" ~ "F: 1000-4999 acres",
      FIRE_SIZE_CLASS == "G" ~ "G: 5000+ acres",
      TRUE ~ "Unknown"
    )
  )

cat("Records after cleaning:", format(nrow(fires_clean), big.mark = ","), "\n")
cat("Records removed:", format(nrow(fires_raw) - nrow(fires_clean),
                               big.mark = ","), "\n")

# ── Summary statistics ────────────────────────────────────────
cat("\n=== Fire Size Summary (acres) ===\n")
print(summary(fires_clean$FIRE_SIZE))

cat("\n=== Records by Size Class ===\n")
fires_clean %>%
  count(SIZE_CLASS_LABEL) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  arrange(SIZE_CLASS_LABEL) %>%
  print()

cat("\n=== Top 10 Causes ===\n")
fires_clean %>%
  count(STAT_CAUSE_DESCR, sort = TRUE) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  head(10) %>%
  print()

cat("\n=== Human vs Natural Fires ===\n")
fires_clean %>%
  count(CAUSE_TYPE) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()

# ── Save clean data as CSV for use in later scripts ───────────
# We save to CSV so subsequent scripts do not need the SQLite file
write.csv(fires_clean,
          "C:/Users/......csv",
          row.names = FALSE)

cat("\nClean data saved to data/fires_clean.csv\n")
cat("=== Step 1 Complete ===\n")

