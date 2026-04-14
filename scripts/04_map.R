
# ============================================================
# U.S. Wildfire Risk Analysis (1992–2015)
# ============================================================

library(readr)
library(dplyr)
library(ggplot2)
library(maps)
library(scales)


fires <- read_csv("data/fires_clean.csv", show_col_types = FALSE)

# ── Base map data ─────────────────────────────────────────────
us_states <- map_data("state")

# ── State label centroids ─────────────────────────────────────
state_labels <- data.frame(
  region = tolower(state.name),
  abb    = state.abb,
  long   = state.center$x,
  lat    = state.center$y
) %>%
  filter(!abb %in% c("AK", "HI"))

# ── Filter fires to continental U.S. ─────────────────────────
fires_conus <- fires %>%
  filter(
    !STATE %in% c("AK", "HI", "PR", "VI", "GU"),
    LATITUDE  >= 24, LATITUDE  <= 50,
    LONGITUDE >= -125, LONGITUDE <= -65
  )

cat("Continental U.S. fires:", format(nrow(fires_conus),
                                      big.mark = ","), "\n")

# ── Plot 15: Fire locations with state labels ─────────
cat("\n=== Building fire location map with labels ===\n")

p15 <- ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "#f5f5f0", color = "white",
               linewidth = 0.4) +
  geom_point(data = fires_conus %>% sample_frac(0.15),
             aes(x = LONGITUDE, y = LATITUDE,
                 color = FIRE_SIZE_CLASS),
             size = 0.15, alpha = 0.4) +
  geom_text(data = state_labels,
            aes(x = long, y = lat, label = abb),
            size = 2.5, color = "gray25",
            fontface = "bold") +
  scale_color_manual(
    values = c(
      "A" = "#fee8c8", "B" = "#fdbb84",
      "C" = "#fc8d59", "D" = "#ef6548",
      "E" = "#d7301f", "F" = "#990000",
      "G" = "#4a0000"
    ),
    labels = c(
      "A" = "A: <0.25 acres",
      "B" = "B: 0.25–9.9 acres",
      "C" = "C: 10–99.9 acres",
      "D" = "D: 100–299 acres",
      "E" = "E: 300–999 acres",
      "F" = "F: 1,000–4,999 acres",
      "G" = "G: 5,000+ acres"
    )
  ) +
  coord_fixed(1.3) +
  labs(title = "U.S. Wildfire Locations by Size Class (1992–2015)",
       subtitle = "15% random sample of continental U.S. fires | Darker = larger fire",
       color = "Fire Size Class",
       caption = "Source: FPA FOD Database | Alaska and Hawaii excluded") +
  theme_void(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 14,
                                    margin = margin(10, 0, 4, 10)),
    plot.subtitle    = element_text(color = "gray40", size = 10,
                                    margin = margin(0, 0, 4, 10)),
    plot.caption     = element_text(color = "gray60", size = 8,
                                    margin = margin(4, 10, 8, 0),
                                    hjust = 1),
    legend.position  = "right",
    legend.title     = element_text(face = "bold", size = 9),
    legend.text      = element_text(size = 8),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("outputs/15_fire_locations_map.png",
       p15, width = 13, height = 7, dpi = 150)
cat("Saved: 15_fire_locations_map.png\n")

# ── Plot 16 Updated: Risk heatmap with state labels ───────────
cat("\n=== Building risk heatmap with labels ===\n")

state_risk <- fires_conus %>%
  group_by(STATE) %>%
  summarise(
    pure_premium = (n() / 24) * mean(FIRE_SIZE),
    .groups = "drop"
  ) %>%
  mutate(
    state_lower = tolower(state.name[match(STATE, state.abb)])
  ) %>%
  filter(!is.na(state_lower))

map_risk <- us_states %>%
  left_join(state_risk, by = c("region" = "state_lower"))

p16 <- ggplot() +
  geom_polygon(data = map_risk,
               aes(x = long, y = lat,
                   group = group,
                   fill = pure_premium),
               color = "white", linewidth = 0.4) +
  geom_text(data = state_labels,
            aes(x = long, y = lat, label = abb),
            size = 2.5, color = "gray20",
            fontface = "bold") +
  scale_fill_gradientn(
    colors = c("#fff7ec", "#fee8c8", "#fdd49e",
               "#fdbb84", "#fc8d59", "#ef6548",
               "#d7301f", "#990000"),
    na.value = "gray90",
    labels = comma,
    name = "Pure Risk\nPremium\n(acres/yr)"
  ) +
  coord_fixed(1.3) +
  labs(title = "Wildfire Pure Risk Premium by State — Continental U.S.",
       subtitle = "Pure Risk Premium = Annual Avg. Fires × Avg. Fire Size | Darker = higher risk",
       caption = "Source: FPA FOD Database 1992–2015 | Actuarial method") +
  theme_void(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 14,
                                    margin = margin(10, 0, 4, 10)),
    plot.subtitle    = element_text(color = "gray40", size = 10,
                                    margin = margin(0, 0, 4, 10)),
    plot.caption     = element_text(color = "gray60", size = 8,
                                    margin = margin(4, 10, 8, 0),
                                    hjust = 1),
    legend.position  = "right",
    legend.title     = element_text(face = "bold", size = 9),
    legend.text      = element_text(size = 8),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("outputs/16_state_risk_heatmap.png",
       p16, width = 13, height = 7, dpi = 150)
cat("Saved: 16_state_risk_heatmap.png\n")

# ── Plot 17 Updated: Cause map with state labels ──────────────
cat("\n=== Building cause map with labels ===\n")

p17 <- ggplot() +
  geom_polygon(data = us_states,
               aes(x = long, y = lat, group = group),
               fill = "#f5f5f0", color = "white",
               linewidth = 0.4) +
  geom_point(data = fires_conus %>%
               filter(FIRE_SIZE_CLASS %in% c("E","F","G")) %>%
               arrange(CAUSE_TYPE),
             aes(x = LONGITUDE, y = LATITUDE,
                 color = CAUSE_TYPE),
             size = 0.8, alpha = 0.6) +
  geom_text(data = state_labels,
            aes(x = long, y = lat, label = abb),
            size = 2.5, color = "gray25",
            fontface = "bold") +
  scale_color_manual(
    values = c("Human"   = "#d6604d",
               "Natural" = "#2166ac"),
    labels = c("Human"   = "Human-caused",
               "Natural" = "Lightning-caused")
  ) +
  coord_fixed(1.3) +
  labs(title = "Large Wildfire Locations by Cause — Continental U.S. (1992–2015)",
       subtitle = "Size Class E, F and G fires only (300+ acres) | Red = human | Blue = lightning",
       color = "Cause Type",
       caption = "Source: FPA FOD Database") +
  theme_void(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 14,
                                    margin = margin(10, 0, 4, 10)),
    plot.subtitle    = element_text(color = "gray40", size = 10,
                                    margin = margin(0, 0, 4, 10)),
    plot.caption     = element_text(color = "gray60", size = 8,
                                    margin = margin(4, 10, 8, 0),
                                    hjust = 1),
    legend.position  = "right",
    legend.title     = element_text(face = "bold", size = 9),
    legend.text      = element_text(size = 8),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("outputs/17_large_fires_cause_map.png",
       p17, width = 13, height = 7, dpi = 150)