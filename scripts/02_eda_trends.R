# ============================================================
# U.S. Wildfire Risk Analysis (1992–2015)
# Exploratory Data Analysis — Trends and Patterns
# ============================================================

library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(patchwork)

# ── Load clean data ──────────────────────────────────────────
fires <- read_csv(
  "C:/........../fires_clean.csv",
  show_col_types = FALSE)

cat("Records loaded:", format(nrow(fires), big.mark = ","), "\n")

# ── Set consistent theme ─────────────────────────────────────
theme_wildfire <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray40"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# ── Plot 1: Number of fires per year ─────────────────────────
fires_per_year <- fires %>%
  count(FIRE_YEAR) %>%
  rename(num_fires = n)

p1 <- ggplot(fires_per_year, aes(x = FIRE_YEAR, y = num_fires)) +
  geom_col(fill = "#d6604d", alpha = 0.85) +
  geom_smooth(method = "loess", se = FALSE,
              color = "black", linewidth = 0.7,
              linetype = "dashed") +
  scale_x_continuous(breaks = 1992:2015) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Number of Wildfires per Year — United States",
       subtitle = "1992–2015 | Source: FPA FOD Database",
       x = "Year", y = "Number of Fires",
       caption = "Dashed line = LOESS trend") +
  theme_wildfire

ggsave("outputs/01_fires_per_year.png",
       p1, width = 12, height = 5, dpi = 150)
cat("Saved: 01_fires_per_year.png\n")

# ── Plot 2: Total acres burned per year ──────────────────────
acres_per_year <- fires %>%
  group_by(FIRE_YEAR) %>%
  summarise(total_acres = sum(FIRE_SIZE), .groups = "drop")

p2 <- ggplot(acres_per_year, aes(x = FIRE_YEAR, y = total_acres)) +
  geom_col(fill = "#762a83", alpha = 0.85) +
  geom_smooth(method = "loess", se = FALSE,
              color = "black", linewidth = 0.7,
              linetype = "dashed") +
  scale_x_continuous(breaks = 1992:2015) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total Acres Burned per Year — United States",
       subtitle = "1992–2015 | Source: FPA FOD Database",
       x = "Year", y = "Total Acres Burned",
       caption = "Dashed line = LOESS trend") +
  theme_wildfire

ggsave("outputs/02_acres_burned_per_year.png",
       p2, width = 12, height = 5, dpi = 150)
cat("Saved: 02_acres_burned_per_year.png\n")

# ── Plot 3: Fire size distribution ───────────────────────────
p3 <- ggplot(fires %>% filter(FIRE_SIZE <= 1000),
             aes(x = FIRE_SIZE)) +
  geom_histogram(fill = "#2166ac", color = "white",
                 bins = 60, alpha = 0.85) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Distribution of Fire Size — Fires Under 1,000 Acres",
       subtitle = "Showing 99%+ of all fires | Extreme outliers excluded for visibility",
       x = "Fire Size (acres)", y = "Number of Fires") +
  theme_wildfire

ggsave("outputs/03_fire_size_distribution.png",
       p3, width = 10, height = 5, dpi = 150)
cat("Saved: 03_fire_size_distribution.png\n")

# ── Plot 4: Size class breakdown ─────────────────────────────
size_class_summary <- fires %>%
  group_by(SIZE_CLASS_LABEL) %>%
  summarise(
    num_fires = n(),
    total_acres = sum(FIRE_SIZE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_fires = round(num_fires / sum(num_fires) * 100, 1),
    pct_acres = round(total_acres / sum(total_acres) * 100, 1)
  )

p4a <- ggplot(size_class_summary,
              aes(x = SIZE_CLASS_LABEL, y = pct_fires)) +
  geom_col(fill = "#2166ac", alpha = 0.85) +
  labs(title = "% of Total Fires by Size Class",
       x = "Size Class", y = "% of Fires") +
  theme_wildfire

p4b <- ggplot(size_class_summary,
              aes(x = SIZE_CLASS_LABEL, y = pct_acres)) +
  geom_col(fill = "#d6604d", alpha = 0.85) +
  labs(title = "% of Total Acres Burned by Size Class",
       x = "Size Class", y = "% of Acres") +
  theme_wildfire

p4 <- p4a + p4b +
  plot_annotation(
    title = "Fire Frequency vs Burn Severity by Size Class",
    subtitle = "Small fires dominate frequency — large fires dominate acres burned",
    caption = "Source: FPA FOD Database 1992–2015"
  )

ggsave("outputs/04_size_class_breakdown.png",
       p4, width = 12, height = 5, dpi = 150)
cat("Saved: 04_size_class_breakdown.png\n")

# ── Plot 5: Top causes ────────────────────────────────────────
cause_summary <- fires %>%
  filter(STAT_CAUSE_DESCR != "Missing/Undefined") %>%
  count(STAT_CAUSE_DESCR, sort = TRUE) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  head(12)

p5 <- ggplot(cause_summary,
             aes(x = fct_reorder(STAT_CAUSE_DESCR, n), y = n)) +
  geom_col(fill = "#1b7837", alpha = 0.85) +
  geom_text(aes(label = paste0(pct, "%")),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, max(cause_summary$n) * 1.15)) +
  labs(title = "Top Causes of U.S. Wildfires (1992–2015)",
       subtitle = "Missing/Undefined causes excluded",
       x = "Cause", y = "Number of Fires",
       caption = "Source: FPA FOD Database") +
  theme_wildfire +
  theme(axis.text.x = element_text(angle = 0))

ggsave("outputs/05_top_causes.png",
       p5, width = 10, height = 6, dpi = 150)
cat("Saved: 05_top_causes.png\n")

# ── Plot 6: Seasonal pattern ─────────────────────────────────
month_order <- c("Jan","Feb","Mar","Apr","May","Jun",
                 "Jul","Aug","Sep","Oct","Nov","Dec")

seasonal <- fires %>%
  count(MONTH_NAME) %>%
  mutate(MONTH_NAME = factor(MONTH_NAME, levels = month_order))

p6 <- ggplot(seasonal, aes(x = MONTH_NAME, y = n)) +
  geom_col(fill = "#d6604d", alpha = 0.85) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Seasonal Pattern of U.S. Wildfires (1992–2015)",
       subtitle = "Number of fires by month of discovery",
       x = "Month", y = "Number of Fires",
       caption = "Source: FPA FOD Database") +
  theme_wildfire +
  theme(axis.text.x = element_text(angle = 0))

ggsave("outputs/06_seasonal_pattern.png",
       p6, width = 10, height = 5, dpi = 150)
cat("Saved: 06_seasonal_pattern.png\n")

# ── Plot 7: Top 15 states by number of fires ─────────────────
state_fires <- fires %>%
  count(STATE, sort = TRUE) %>%
  head(15)

p7 <- ggplot(state_fires,
             aes(x = fct_reorder(STATE, n), y = n)) +
  geom_col(fill = "#2166ac", alpha = 0.85) +
  geom_text(aes(label = scales::comma(n)),
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, max(state_fires$n) * 1.15)) +
  labs(title = "Top 15 States by Number of Wildfires (1992–2015)",
       subtitle = "Total fire count across all size classes",
       x = "State", y = "Number of Fires",
       caption = "Source: FPA FOD Database") +
  theme_wildfire +
  theme(axis.text.x = element_text(angle = 0))

ggsave("outputs/07_fires_by_state.png",
       p7, width = 10, height = 6, dpi = 150)
cat("Saved: 07_fires_by_state.png\n")

# ── Plot 8: Top 15 states by acres burned ────────────────────
state_acres <- fires %>%
  group_by(STATE) %>%
  summarise(total_acres = sum(FIRE_SIZE), .groups = "drop") %>%
  arrange(desc(total_acres)) %>%
  head(15)

p8 <- ggplot(state_acres,
             aes(x = fct_reorder(STATE, total_acres),
                 y = total_acres)) +
  geom_col(fill = "#762a83", alpha = 0.85) +
  geom_text(aes(label = scales::comma(round(total_acres))),
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, max(state_acres$total_acres) * 1.15)) +
  labs(title = "Top 15 States by Total Acres Burned (1992–2015)",
       subtitle = "Severity measure — total burn area regardless of fire count",
       x = "State", y = "Total Acres Burned",
       caption = "Source: FPA FOD Database") +
  theme_wildfire +
  theme(axis.text.x = element_text(angle = 0))

ggsave("outputs/08_acres_by_state.png",
       p8, width = 10, height = 6, dpi = 150)
cat("Saved: 08_acres_by_state.png\n")

# ── Plot 9: Human vs lightning fires over time ────────────────
cause_trend <- fires %>%
  count(FIRE_YEAR, CAUSE_TYPE)

p9 <- ggplot(cause_trend,
             aes(x = FIRE_YEAR, y = n,
                 color = CAUSE_TYPE, fill = CAUSE_TYPE)) +
  geom_area(alpha = 0.3, position = "identity") +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = seq(1992, 2015, by = 2)) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("Human" = "#d6604d",
                                "Natural" = "#2166ac")) +
  scale_fill_manual(values = c("Human" = "#d6604d",
                               "Natural" = "#2166ac")) +
  labs(title = "Human vs Natural (Lightning) Wildfires Over Time",
       subtitle = "Human-caused fires dominate throughout 1992–2015",
       x = "Year", y = "Number of Fires",
       color = "Cause Type", fill = "Cause Type",
       caption = "Source: FPA FOD Database") +
  theme_wildfire +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

ggsave("outputs/09_human_vs_lightning.png",
       p9, width = 12, height = 5, dpi = 150)
cat("Saved: 09_human_vs_lightning.png\n")