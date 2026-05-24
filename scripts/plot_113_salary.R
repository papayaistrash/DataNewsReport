# ============================================================================
# 📊 113年家庭看護移工「縣市別」平均薪資比較
# ============================================================================
library(tidyverse)
library(haven)
library(scales)
library(showtext)

font_add_google("Noto Sans TC", "NotoSans")
showtext_auto()

base_dir <- "."
if (basename(getwd()) == "scripts") { base_dir <- ".." }
dir.create(file.path(base_dir, "plots"), showWarnings = FALSE)

city_map <- c(
  "1"  = "新北市", "2"  = "臺北市", "3"  = "桃園市",
  "4"  = "臺中市", "5"  = "臺南市", "6"  = "高雄市",
  "7"  = "宜蘭縣", "8"  = "新竹縣", "9"  = "苗栗縣",
  "10" = "彰化縣", "11" = "南投縣", "12" = "雲林縣",
  "13" = "嘉義縣", "14" = "屏東縣", "15" = "臺東縣",
  "16" = "花蓮縣", "17" = "澎湖縣", "18" = "基隆市",
  "19" = "新竹市", "20" = "嘉義市", "21" = "金門縣及連江縣"
)

# 挑選最具代表性的六都進行比較
six_cities <- c("新北市", "臺北市", "桃園市", "臺中市", "臺南市", "高雄市")

# 讀取 113 年資料
d113 <- read_dta(file.path(base_dir, "data/113年移工/家庭面/data113.dta"))

# 計算加權平均薪資
salary_data <- d113 %>%
  mutate(
    city_code = as.character(j1),
    city = recode(city_code, !!!city_map),
    salary = as.numeric(nq10a),
    w = as.numeric(w)
  ) %>%
  # 排除薪資不合理的異常值 (小於1萬或大於5萬) 及遺漏值
  filter(!is.na(salary), salary >= 10000, salary <= 50000, !is.na(w)) %>%
  filter(city %in% six_cities) %>%
  group_by(city) %>%
  summarise(
    mean_salary = weighted.mean(salary, w, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

cat("=== 113年六都家庭看護移工加權平均薪資 ===\n")
print(salary_data %>% arrange(desc(mean_salary)))

# 繪製圖表
p <- ggplot(salary_data %>% mutate(city = fct_reorder(city, mean_salary)),
            aes(x = city, y = mean_salary, fill = city)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = comma(round(mean_salary, 0))), 
            hjust = -0.2, size = 6, family = "NotoSans", color = "grey20") +
  # 設定北中南不同的色系漸層
  scale_fill_manual(values = c(
    "臺北市" = "#3566A5", "新北市" = "#5C9EEA", "桃園市" = "#88C5F4", 
    "臺中市" = "#DE4429", "臺南市" = "#E87A65", "高雄市" = "#F2A796"
  )) +
  scale_y_continuous(limits = c(0, max(salary_data$mean_salary) * 1.15), 
                     labels = comma) +
  coord_flip() +
  labs(
    title = "六都家庭看護移工加權平均薪資比較 (113年)",
    subtitle = "資料範圍：113年家庭面問卷，薪資包含本薪、加班費及其他",
    x = NULL,
    y = "加權平均月薪 (元)",
    caption = "資料來源：勞動部 113 年移工管理及運用調查（家庭面）"
  ) +
  theme_classic(base_family = "NotoSans", base_size = 20) +
  theme(
    plot.title    = element_text(face = "bold", size = 25),
    plot.subtitle = element_text(size = 14, color = "#555555", margin = margin(b = 12)),
    plot.caption  = element_text(color = "#aaaaaa"),
    axis.line     = element_blank(),
    axis.ticks    = element_blank(),
    axis.title.y  = element_text(angle = 0, vjust = 0.5, hjust = 1)
  )

output_path <- file.path(base_dir, "plots/P_113_city_salary.png")
ggsave(output_path, plot = p, width = 12, height = 8, dpi = 150)
cat(sprintf("✅ 已輸出圖表至：%s\n", output_path))
