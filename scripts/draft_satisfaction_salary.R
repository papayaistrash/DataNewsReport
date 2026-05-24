
# === 薪資與滿意度疊加 (提案四) ===
sat_salary <- data113 %>%
  mutate(
    月薪 = as.numeric(nq10a),
    加班費 = as.numeric(nq10ab),
    加班費 = ifelse(is.na(加班費), 0, 加班費),
    總薪資 = 月薪 + 加班費,
    整體滿意度 = as.numeric(q20_7),
    權重 = as.numeric(w)
  ) %>%
  filter(!is.na(總薪資), 總薪資 >= 17000, 總薪資 <= 40000, !is.na(整體滿意度)) %>%
  mutate(
    滿意度分數 = 6 - 整體滿意度, # 反轉分數
    薪資區間 = cut(總薪資, 
                   breaks = c(0, 20000, 22000, 24000, 26000, Inf), 
                   labels = c("2萬以下", "2.0-2.2萬", "2.2-2.4萬", "2.4-2.6萬", "2.6萬以上"))
  )

total_w <- sum(sat_salary$權重)

sat_salary_summary <- sat_salary %>%
  group_by(薪資區間) %>%
  summarise(
    平均滿意度 = weighted.mean(滿意度分數, 權重, na.rm = TRUE),
    佔比 = sum(權重) / total_w,
    .groups = "drop"
  )

# 雙軸圖：左軸是人數佔比(Bar)，右軸是滿意度(Line)
# 為了放在同一個圖，我們需要做數值縮放 (scale_factor)
# 滿意度大約在 3.5 ~ 4.5 之間，佔比大約在 0 ~ 0.5 之間
# 讓滿意度的 5 對應到佔比的 0.8
scale_factor <- 0.8 / 5

p8 <- ggplot(sat_salary_summary, aes(x = 薪資區間)) +
  geom_col(aes(y = 佔比), fill = reporter_grey, alpha = 0.6, width = 0.5) +
  geom_line(aes(y = 平均滿意度 * scale_factor, group = 1), color = reporter_orange, linewidth = 2) +
  geom_point(aes(y = 平均滿意度 * scale_factor), color = reporter_orange, size = 5) +
  geom_text(aes(y = 佔比, label = percent(佔比, 0.1)), vjust = -1, size = 6, color = reporter_dark) +
  geom_text(aes(y = 平均滿意度 * scale_factor, label = round(平均滿意度, 2)), vjust = 2, size = 6, color = reporter_orange, fontface = "bold") +
  scale_y_continuous(
    name = "該薪資區間人數佔比",
    labels = percent_format(),
    limits = c(0, 0.8),
    sec.axis = sec_axis(~ . / scale_factor, name = "平均整體滿意度 (滿分5分)")
  ) +
  labs(title = "薪資結構與雇主整體滿意度疊加分析",
       subtitle = "長條圖為人數佔比 (左軸)；折線圖為平均滿意度 (右軸)",
       x = "總薪資區間 (含加班費)",
       caption = "資料來源：勞動部113年移工管理及運用調查") +
  theme_news() +
  theme(
    axis.title.y.left = element_text(color = reporter_dark, margin = margin(r=10)),
    axis.text.y.left = element_text(color = reporter_dark),
    axis.title.y.right = element_text(color = reporter_orange, margin = margin(l=10)),
    axis.text.y.right = element_text(color = reporter_orange)
  )

ggsave(file.path(base_dir, "plots/8_satisfaction_salary.png", plot = p8, width = 10, height = 6, dpi = 150)

data\generate_plots_sat.R
