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
      plot.title = element_text(size = 28, face = "bold", margin = margin(b = 15)),
      plot.subtitle = element_text(size = 18, margin = margin(b = 15)),
      plot.caption = element_text(size = 14, color = "grey50", hjust = 1, margin = margin(t = 15)),
      axis.title.x = element_text(size = 18, face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(size = 18, face = "bold", margin = margin(r = 10)),
      axis.text.x = element_text(size = 16, face = "bold"),
      axis.text.y = element_text(size = 16),
      legend.title = element_text(size = 16, face = "bold"),
      legend.position = "bottom",
      legend.text = element_text(size = 16),
      panel.grid.minor = element_blank(),
      plot.margin = margin(30, 30, 30, 30)
    )
}

# 霈????data113 <- read.csv("113撟渡宏撌?摰嗅滬??data113.csv")

# 鞈?皜?嚗?蝞蜇?芾????遛?漲
sat_data <- data113 %>%
  mutate(
    ? = as.numeric(nq10a),
    ?鞎?= as.numeric(nq10ab),
    ?鞎?= ifelse(is.na(?鞎?, 0, ?鞎?,
    蝮質鞈?= ? + ?鞎?
    ??摨虫?皛踵? = as.numeric(q20_6),
    皞?皛踵? = as.numeric(q20_3),
    甈? = as.numeric(w)
  ) %>%
  filter(!is.na(蝮質鞈?, 蝮質鞈?>= 17000, 蝮質鞈?<= 40000)

# ==============================================================================
# ?” 10: 蝮質鞈????漲??皛踵?頞典蝺?# ==============================================================================
plot_coop <- sat_data %>% filter(!is.na(??摨虫?皛踵?))

p10 <- ggplot(plot_coop, aes(x = 蝮質鞈? y = ??摨虫?皛踵?)) +
  geom_jitter(color = reporter_grey, alpha = 0.1, size = 2, width = 500, height = 0.2) +
  geom_smooth(aes(weight = 甈?), method = "lm", color = reporter_orange, fill = reporter_orange, alpha = 0.2, linewidth = 2) +
  scale_y_continuous(breaks = 1:5, labels = c("1?n(敺遛??", "2??, "3?n(?桅?", "4??, "5?n(敺?皛踵?)")) +
  scale_x_continuous(labels = label_comma(prefix = "$", suffix = " ??), breaks = seq(18000, 40000, by = 4000)) +
  labs(
    title = "?芾?頞?嚗?銝餃?蝘餃極???漲??皛踵???",
    subtitle = "璈怨遘?箏祕鞈芰蜇?芾?嚗?鞎鳴?嚗?蝺??敺?蝺扯隅?ｇ?暺??????瑟見??,
    x = "瘥?蝮質鞈?,
    y = "銝遛?漲 (1~5??",
    caption = "鞈?靘?嚗??113撟渡宏撌亦恣???隤踵"
  ) +
  theme_news()

ggsave(file.path(base_dir, "plots/10_satisfaction_salary_continuous.png", plot = p10, width = 10, height = 6, dpi = 150)

# ==============================================================================
# ?” 11: 蝮質鞈??隤???皛踵?頞典蝺?# ==============================================================================
plot_comm <- sat_data %>% filter(!is.na(皞?皛踵?))

p11 <- ggplot(plot_comm, aes(x = 蝮質鞈? y = 皞?皛踵?)) +
  geom_jitter(color = reporter_grey, alpha = 0.1, size = 2, width = 500, height = 0.2) +
  geom_smooth(aes(weight = 甈?), method = "lm", color = reporter_blue, fill = reporter_blue, alpha = 0.2, linewidth = 2) +
  scale_y_continuous(breaks = 1:5, labels = c("1?n(敺遛??", "2??, "3?n(?桅?", "4??, "5?n(敺?皛踵?)")) +
  scale_x_continuous(labels = label_comma(prefix = "$", suffix = " ??), breaks = seq(18000, 40000, by = 4000)) +
  labs(
    title = "?芾?蝯??隤???????,
    subtitle = "璈怨遘?箏祕鞈芰蜇?芾?嚗?鞎鳴?嚗?蝺??敺?蝺扯隅?ｇ?暺??????瑟見??,
    x = "瘥?蝮質鞈?,
    y = "銝遛?漲 (1~5??",
    caption = "鞈?靘?嚗??113撟渡宏撌亦恣???隤踵"
  ) +
  theme_news()

ggsave(file.path(base_dir, "plots/11_satisfaction_comm_continuous.png", plot = p11, width = 10, height = 6, dpi = 150)

# ==============================================================================
# ?” 12: ?遛?漲蝑??像?鞈?(?瑟???璉?蝟?)
# ?桃?嚗??整遛?漲擃?嚗?銝颱??箇?撟喳??芾??嗅祕撌桐?憭?# ==============================================================================
avg_salary <- sat_data %>%
  filter(!is.na(??摨虫?皛踵?)) %>%
  group_by(??摨虫?皛踵?) %>%
  summarise(
    撟喳??芾? = weighted.mean(蝮質鞈? 甈?, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    皛踵?摨行?蝐?= factor(??摨虫?皛踵?, levels = 1:5, labels = c("敺遛?n(1??", "皛踵?\n(2??", "?桅n(3??", "銝遛?n(4??", "敺?皛踵?\n(5??"))
  )

p12 <- ggplot(avg_salary, aes(x = 皛踵?摨行?蝐? y = 撟喳??芾?)) +
  geom_col(fill = reporter_grey, alpha = 0.7, width = 0.5) +
  geom_text(aes(label = paste0("$", formatC(round(撟喳??芾?), format = "f", big.mark = ",", digits = 0))),
    vjust = -0.5, size = 6, color = reporter_dark, fontface = "bold"
  ) +
  scale_y_continuous(labels = label_comma(prefix = "$"), limits = c(0, max(avg_salary$撟喳??芾?) * 1.15)) +
  labs(
    title = "蝘餃極??瘜???芣偌銝?,
    subtitle = "銝??蜓撠宏撌亦????漲?遛???佗??嗅祕鞈芸像?鞈??賢?抵銝??椰??,
    x = "?蜓撠宏撌乓??漲??皛踵?摨?,
    y = "蝮質鞈?(?怠??剛祥)",
    caption = "鞈?靘?嚗??113撟渡宏撌亦恣???隤踵"
  ) +
  theme_news() +
  theme(
    axis.text.x = element_text(size = 16, face = "bold", color = reporter_dark),
    panel.grid.major.x = element_blank()
  )

ggsave(file.path(base_dir, "plots/12_satisfaction_salary_bar.png", plot = p12, width = 10, height = 6, dpi = 150)
