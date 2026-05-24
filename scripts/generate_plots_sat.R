library(tidyverse)
library(scales)
library(ggplot2)
library(showtext)

base_dir <- "."`nif (basename(getwd()) == "scripts") { base_dir <- ".." }`ndir.create(file.path(base_dir, "plots"), showWarnings = FALSE)

# 閮剖?銝剜?摮?
font_add("TC", "msjh.ttc") 
showtext_auto()

# ?勗??◢?潸蟡?reporter_orange <- "#DE4429"
reporter_dark <- "#1F3145"
reporter_blue <- "#385E78"
reporter_yellow <- "#E8A343"
reporter_grey <- "#8998A1"

theme_news <- function() {
  theme_minimal(base_family = "TC") +
    theme(
      text = element_text(family = "TC"),
      plot.title = element_text(size = 28, face = "bold", margin = margin(b=15)),
      plot.subtitle = element_text(size = 18, margin = margin(b=15)),
      plot.caption = element_text(size = 14, color = "grey50", hjust = 1, margin = margin(t=15)),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 18, face="bold", margin=margin(r=10)),
      axis.text.x = element_text(size = 16, face = "bold"),
      axis.text.y = element_text(size = 16),
      legend.title = element_text(size = 16, face="bold"),
      legend.position = "bottom",
      legend.text = element_text(size = 16),
      panel.grid.minor = element_blank(),
      plot.margin = margin(30, 30, 30, 30)
    )
}

# 霈????data113 <- read.csv("113撟渡宏撌?摰嗅滬??data113.csv")

# ??皛踵?摨西???df_sat <- data113 %>%
  mutate(
    ?? = factor(q8d, levels = 1:4, labels = c("?啣側", "?脣?鞈?, "瘜啣?", "頞?")),
    甈? = as.numeric(w)
  ) %>%
  filter(!is.na(??))

# ?Ｗ?璅惜
sat_labels <- c("q20_1"="撌乩??漲", "q20_2"="霅瑞??銵?, "q20_3"="?航?皞?,
                "q20_4"="?暑蝧", "q20_5"="??", "q20_6"="??摨?)

# === ???? (??鈭? ===
sat_long <- df_sat %>%
  select(q20_1:q20_6, 甈?) %>%
  pivot_longer(cols = q20_1:q20_6, names_to = "?Ｗ?", values_to = "?") %>%
  mutate(? = as.numeric(?)) %>%
  filter(!is.na(?))

sat_summary <- sat_long %>%
  group_by(?Ｗ?) %>%
  summarise(撟喳?? = weighted.mean(?, 甈?, na.rm = TRUE)) %>%
  mutate(
    ?Ｗ?璅惜 = sat_labels[?Ｗ?],
    ?Ｗ?璅惜 = fct_reorder(?Ｗ?璅惜, 撟喳??, .desc = TRUE) # ?頞?頞?皛踵?嚗??其???  )

p6 <- ggplot(sat_summary, aes(x = 撟喳??, y = ?Ｗ?璅惜)) +
  geom_segment(aes(x = 1, xend = 撟喳??, y = ?Ｗ?璅惜, yend = ?Ｗ?璅惜), color = reporter_grey, size = 1.5) +
  geom_point(color = reporter_orange, size = 8) +
  geom_text(aes(label = round(撟喳??, 2)), hjust = -0.6, fontface = "bold", size = 7) +
  scale_x_continuous(limits = c(1, 3.5), breaks = 1:5) +
  labs(title = "?蜓?銝遛?宏撌亦??隤????風??銵?,
       subtitle = "??像?遛?漲 (1=敺遛??5=敺?皛踵?嚗??貉?擃誨銵刻?銝遛)",
       y = NULL,
       caption = "鞈?靘?嚗??113撟渡宏撌亦恣???隤踵") +
  theme_news() +
  theme(panel.grid.major.y = element_blank())

ggsave(file.path(base_dir, "plots/6_satisfaction_painpoints.png", plot = p6, width = 10, height = 6, dpi = 150)

# === ???餅?啗情 (??銝? ===
sat_nat <- df_sat %>%
  select(??, q20_1:q20_6, 甈?) %>%
  pivot_longer(cols = q20_1:q20_6, names_to = "?Ｗ?", values_to = "?") %>%
  mutate(? = as.numeric(?)) %>%
  filter(!is.na(?)) %>%
  group_by(??, ?Ｗ?) %>%
  # ?箔??渲死嚗????嚗? = 敺遛?? 1 = 敺?皛踵?
  summarise(撟喳?皛踵?摨?= weighted.mean(6 - ?, 甈?, na.rm = TRUE), .groups="drop") %>%
  mutate(
    ?Ｗ?璅惜 = factor(sat_labels[?Ｗ?], levels = rev(levels(sat_summary$?Ｗ?璅惜)))
  )

p7 <- ggplot(sat_nat, aes(x = ??, y = ?Ｗ?璅惜, fill = 撟喳?皛踵?摨?) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = round(撟喳?皛踵?摨? 2)), color = "white", fontface = "bold", size = 7) +
  scale_fill_gradient(low = reporter_dark, high = reporter_orange, name = "皛踵?摨?) +
  labs(title = "頞?蝐擃遛?漲頛?嚗敺?蝐???雿?,
       subtitle = "?? ? ??遛?漲?勗???(蝬??????頞?隞?”頞遛??",
       x = "蝘餃極??", y = NULL,
       caption = "鞈?靘?嚗??113撟渡宏撌亦恣???隤踵") +
  theme_news() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

ggsave(file.path(base_dir, "plots/7_satisfaction_nationality.png", plot = p7, width = 10, height = 6, dpi = 150)
