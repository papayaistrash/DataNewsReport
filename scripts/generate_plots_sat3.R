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

data113 <- read.csv("113撟渡宏撌?摰嗅滬??data113.csv")

# === ?芾??遛?漲?? (???? ===
sat_salary <- data113 %>%
  mutate(
    ? = as.numeric(nq10a),
    ?鞎?= as.numeric(nq10ab),
    ?鞎?= ifelse(is.na(?鞎?, 0, ?鞎?,
    蝮質鞈?= ? + ?鞎?
    皞遛?漲 = as.numeric(q20_3),
    甈? = as.numeric(w)
  ) %>%
  filter(!is.na(蝮質鞈?, 蝮質鞈?>= 17000, 蝮質鞈?<= 40000, !is.na(皞遛?漲)) %>%
  mutate(
    銝遛?漲? = 皞遛?漲, # ???: 1=敺遛?? 5=敺?皛踵?嚗?擃?銝遛
    ?芾????= cut(蝮質鞈? 
                   breaks = c(0, 20000, 22000, 24000, 26000, Inf), 
                   labels = c("2?砌誑銝?, "2.0-2.2??, "2.2-2.4??, "2.4-2.6??, "2.6?砌誑銝?))
  )

total_w <- sum(sat_salary$甈?)

sat_salary_summary <- sat_salary %>%
  group_by(?芾???? %>%
  summarise(
    撟喳?銝遛?漲 = weighted.mean(銝遛?漲?, 甈?, na.rm = TRUE),
    雿? = sum(甈?) / total_w,
    .groups = "drop"
  )

scale_factor <- 0.8 / 5

p8 <- ggplot(sat_salary_summary, aes(x = ?芾????) +
  geom_col(aes(y = 雿?), fill = reporter_grey, alpha = 0.6, width = 0.5) +
  geom_line(aes(y = 撟喳?銝遛?漲 * scale_factor, group = 1), color = reporter_orange, linewidth = 2) +
  geom_point(aes(y = 撟喳?銝遛?漲 * scale_factor), color = reporter_orange, size = 5) +
  geom_text(aes(y = 雿?, label = percent(雿?, 0.1)), vjust = -1, size = 7, color = reporter_dark) +
  geom_text(aes(y = 撟喳?銝遛?漲 * scale_factor, label = round(撟喳?銝遛?漲, 2)), vjust = 2, size = 7, color = reporter_orange, fontface = "bold") +
  scale_y_continuous(
    name = paste(strsplit("鈭箸雿?", "")[[1]], collapse = "\n"),
    labels = percent_format(),
    limits = c(0, 0.8),
    sec.axis = sec_axis(~ . / scale_factor, name = paste(strsplit("皞?皛?, "")[[1]], collapse = "\n"))
  ) +
  labs(title = "?芾?蝯??隤???皛踵?摨衣?????,
       subtitle = "?瑟??鈭箸雿? (撌西遘)嚗?蝺??箇宏撌交????撟喳?銝遛?漲 (?唾遘)",
       x = "蝮質鞈???(?怠??剛祥)",
       caption = "鞈?靘?嚗??113撟渡宏撌亦恣???隤踵") +
  theme_news() +
  theme(
    axis.title.y.left = element_text(color = reporter_dark, margin = margin(r=15), angle = 0, vjust = 0.5),
    axis.text.y.left = element_text(color = reporter_dark),
    axis.title.y.right = element_text(color = reporter_orange, margin = margin(l=15), angle = 0, vjust = 0.5),
    axis.text.y.right = element_text(color = reporter_orange)
  )

ggsave(file.path(base_dir, "plots/9_satisfaction_communication.png", plot = p8, width = 10, height = 6, dpi = 150)
