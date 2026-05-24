#install.packages("survey")
library(haven)
library(dplyr)
library(survey)
#install.packages("remotes")
library(ggradar)


data109 <- read.csv("109撟渡宏撌?摰嗅滬??data109.csv")
data110 <- read.csv("110撟渡宏撌?摰嗅滬??data110.csv")
data111 <- read.csv("111撟渡宏撌?摰嗅滬??data111.csv")
data112 <- read.csv("112撟渡宏撌?摰嗅滬??data112.csv")
data113 <- read.csv("113撟渡宏撌?摰嗅滬??data113.csv")




###摰嗅滬??## ???隤?憿?
table(data109$q4)
table(data110$q4)
table(data111$q4)
table(data112$q2)
table(data113$q2)

## ?嚙賜憭蕭?摰嗅滬?嚙質風撌伐蕭?嚗蜓閬憿扳嚙?109??13 撟湛蕭?嚙?---------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)
library(scales)

# 1. 銝哨蕭?摮蕭?
font_add("TC", "msjh.ttc")
showtext_auto()

# 2. ?嚙賭蔥鞈蕭?
care_long <- bind_rows(
  data109 %>% transmute(year = "109", option = q4),
  data110 %>% transmute(year = "110", option = q4),
  data111 %>% transmute(year = "111", option = q4),
  data112 %>% transmute(year = "112", option = q2),
  data113 %>% transmute(year = "113", option = q2)
) %>%
  mutate(
    option = as.numeric(option),
    year = factor(year, levels = c("109", "110", "111", "112", "113")),
    option_label = case_when(
      option == 1 ~ "?嚙賢振鈭箇嚙?,
      option == 2 ~ "?嚙賣?嚙踝蕭?嚙?,
      option == 3 ~ "?嚙賡?嚙踝蕭?嚙?,
      option == 4 ~ "隢扛?嚙賜嚙?,
      option == 5 ~ "?嚙賣?嚙賢鼠??,
      option == 6 ~ "?嚙踝蕭?",
      TRUE ~ NA_character_
    ),
    option_label = factor(
      option_label,
      levels = c("?嚙賢振鈭箇嚙?, "?嚙賣?嚙踝蕭?嚙?, "?嚙賡?嚙踝蕭?嚙?,
                 "隢扛?嚙賜嚙?, "?嚙賣?嚙賢鼠??, "?嚙踝蕭?")
    )
  ) %>%
  filter(!is.na(option_label))

# 3. 閮蕭??嚙賢僑摨佗蕭??嚙踝蕭?甈⊥?嚙賜?嚙踝蕭?
care_summary <- care_long %>%
  count(year, option_label, name = "n") %>%
  group_by(year) %>%
  mutate(
    percent = n / sum(n),
    percent_label = paste0(round(percent * 100, 1), "%")
  ) %>%
  ungroup()

# 4. 瑼Ｘ嚙?care_summary %>%
  select(year, option_label, n, percent_label)

# 5. 銝哨蕭? theme
theme_tc <- function() {
  theme_minimal(base_family = "TC") +
    theme(
      text = element_text(family = "TC"),
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 10, face = "bold", angle = 25, hjust = 1),
      axis.text.y = element_text(size = 11),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 11),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20)
    )
}

# 6. ?嚙踝蕭?嚗活?嚙踝蕭?嚙?p_care_n <- ggplot(care_summary, aes(x = option_label, y = n, fill = year)) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65
  ) +
  geom_text(
    aes(label = n),
    position = position_dodge(width = 0.75),
    vjust = -0.3,
    size = 3,
    family = "TC"
  ) +
  scale_fill_manual(
    values = c("#1f3e79", "#5c7cfa", "#52b788", "#f59f00", "#d9480f")
  ) +
  labs(
    title = "109??13 撟游?嚙踝蕭?蝐振摨哨蕭?霅瑕極?嚙賭蜓閬憿扳撘蕭?嚙?,
    subtitle = "憿嚗◤?嚙質風?嚙賢?嚙踝蕭??嚙賢?嚙踝蕭?蝐振摨哨蕭?霅瑕極?嚙踝蕭??嚙賭蜓嚙?銋憿扳撘雿蕭?",
    y = "?嚙踝蕭?甈⊥"
  ) +
  theme_tc()

p_care_n

# 7. ?嚙踝蕭?嚗?嚙踝蕭?瘥蕭?嚗蕭??嚙踝蕭?頝典僑摨佗蕭?嚙?p_care_pct <- ggplot(care_summary, aes(x = option_label, y = percent, fill = year)) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65
  ) +
  geom_text(
    aes(label = percent_label),
    position = position_dodge(width = 0.75),
    vjust = -0.3,
    size = 3,
    family = "TC"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, max(care_summary$percent, na.rm = TRUE) + 0.08)
  ) +
  scale_fill_manual(
    values = c("#1f3e79", "#5c7cfa", "#52b788", "#f59f00", "#d9480f")
  ) +
  labs(
    title = "109??13 撟游?嚙踝蕭?蝐振摨哨蕭?霅瑕極?嚙賭蜓閬憿扳撘蕭?靘蕭?嚙?,
    subtitle = "?嚙賢僑摨血?嚙踝蕭?瘥蕭?憿?嚙賢?嚙踝蕭?",
    y = "瘥蕭?"
  ) +
  theme_tc()

p_care_pct

# 8. 摮蕭?
ggsave(
  filename = "care_before_hiring_count_109_113.png",
  plot = p_care_n,
  width = 12,
  height = 8,
  dpi = 400,
  bg = "white"
)

ggsave(
  filename = "care_before_hiring_percent_109_113.png",
  plot = p_care_pct,
  width = 12,
  height = 8,
  dpi = 400,
  bg = "white"
)

## 銝鳴蕭??嚙賭誨?嚙踝蕭?
table(data109$q5)
table(data110$q5)
table(data111$q5)
table(data112$q3)
table(data113$q3)

## 銝鳴蕭??嚙賭誨?嚙踝蕭?嚙?09??13 撟湛蕭?嚙?-------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)
library(scales)

# 1. 銝哨蕭?摮蕭?
font_add("TC", "msjh.ttc")
showtext_auto()

# 2. ?嚙賭蔥鞈蕭?
alt_long <- bind_rows(
  data109 %>% transmute(year = "109", option = q5),
  data110 %>% transmute(year = "110", option = q5),
  data111 %>% transmute(year = "111", option = q5),
  data112 %>% transmute(year = "112", option = q3),
  data113 %>% transmute(year = "113", option = q3)
) %>%
  mutate(
    option = as.numeric(option),
    year = factor(year, levels = c("109", "110", "111", "112", "113")),
    option_label = case_when(
      option == 1 ~ "?嚙賢振鈭箇嚙?,
      option == 2 ~ "隢扛?嚙賜嚙?,
      option == 3 ~ "?嚙賣?嚙踝蕭?嚙?,
      option == 4 ~ "?嚙賡?嚙踝蕭?嚙?,
      option == 5 ~ "?嚙踝蕭?撟怠",
      option == 6 ~ "?嚙踝蕭?",
      TRUE ~ NA_character_
    ),
    option_label = factor(
      option_label,
      levels = c("?嚙賢振鈭箇嚙?, "隢扛?嚙賜嚙?, "?嚙賣?嚙踝蕭?嚙?,
                 "?嚙賡?嚙踝蕭?嚙?, "?嚙踝蕭?撟怠", "?嚙踝蕭?")
    )
  ) %>%
  filter(!is.na(option_label))

# 3. 閮蕭??嚙賢僑摨佗蕭??嚙踝蕭?甈⊥?嚙賜?嚙踝蕭?
alt_summary <- alt_long %>%
  count(year, option_label, name = "n") %>%
  group_by(year) %>%
  mutate(
    percent = n / sum(n),
    percent_label = paste0(round(percent * 100, 1), "%")
  ) %>%
  ungroup()

# 4. 瑼Ｘ嚙?alt_summary %>%
  select(year, option_label, n, percent_label)

# 5. 銝哨蕭? theme
theme_tc <- function() {
  theme_minimal(base_family = "TC") +
    theme(
      text = element_text(family = "TC"),
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 10, face = "bold", angle = 25, hjust = 1),
      axis.text.y = element_text(size = 11),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 11),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20)
    )
}

# 6. ?嚙踝蕭?嚗活?嚙踝蕭?嚙?p_alt_n <- ggplot(alt_summary, aes(x = option_label, y = n, fill = year)) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65
  ) +
  geom_text(
    aes(label = n),
    position = position_dodge(width = 0.75),
    vjust = -0.3,
    size = 3,
    family = "TC"
  ) +
  scale_fill_manual(
    values = c("#1f3e79", "#5c7cfa", "#52b788", "#f59f00", "#d9480f")
  ) +
  labs(
    title = "109??13 撟港蜓閬嚙?嚙踝蕭獢?嚙賣活?嚙踝蕭?嚙?,
    subtitle = "憿嚗?嚙踝蕭??嚙賢?嚙踝蕭?蝐振摨哨蕭?霅瑕極?嚙踝蕭??嚙賭蜓嚙??嚙賭誨?嚙踝蕭??嚙踝蕭?嚙?,
    y = "?嚙踝蕭?甈⊥"
  ) +
  theme_tc()

p_alt_n

# 7. ?嚙踝蕭?嚗?嚙踝蕭?瘥蕭?嚗蕭??嚙踝蕭?頝典僑摨佗蕭?嚙?p_alt_pct <- ggplot(alt_summary, aes(x = option_label, y = percent, fill = year)) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65
  ) +
  geom_text(
    aes(label = percent_label),
    position = position_dodge(width = 0.75),
    vjust = -0.3,
    size = 3,
    family = "TC"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, max(alt_summary$percent, na.rm = TRUE) + 0.08)
  ) +
  scale_fill_manual(
    values = c("#1f3e79", "#5c7cfa", "#52b788", "#f59f00", "#d9480f")
  ) +
  labs(
    title = "109??13 撟港蜓閬嚙?嚙踝蕭獢蕭?靘蕭?嚙?,
    subtitle = "憿嚗?嚙踝蕭??嚙賢?嚙踝蕭?蝐振摨哨蕭?霅瑕極?嚙踝蕭??嚙賭蜓嚙??嚙賭誨?嚙踝蕭??嚙踝蕭?嚙?,
    y = "瘥蕭?"
  ) +
  theme_tc()

p_alt_pct

# 8. 摮蕭?
ggsave(
  filename = "alternative_option_count_109_113.png",
  plot = p_alt_n,
  width = 12,
  height = 8,
  dpi = 400,
  bg = "white"
)

ggsave(
  filename = "alternative_option_percent_109_113.png",
  plot = p_alt_pct,
  width = 12,
  height = 8,
  dpi = 400,
  bg = "white"
)


## 隢蕭??嚙賢?嚙踝蕭?蝐振摨哨蕭?霅瑕極?嚙賡鈭祕鞈迎蕭??嚙賢鼠?嚙踝蕭?嚗銴嚙?table(data109$q3_1)
table(data109$q3_2)
table(data109$q3_3)
table(data109$q3_4)
table(data109$q3_5)

table(data110$q3_1)
table(data110$q3_2)
table(data110$q3_3)
table(data110$q3_4)
table(data110$q3_5)

table(data111$q3_1)
table(data111$q3_2)
table(data111$q3_3)
table(data111$q3_4)
table(data111$q3_5)

table(data112$q1_1)
table(data112$q1_2)
table(data112$q1_3)
table(data112$q1_4)
table(data112$q1_5)

table(data113$q1_1)
table(data113$q1_2)
table(data113$q1_3)
table(data113$q1_4)
table(data113$q1_5)

##皛選蕭?摨阡?嚙踝蕭?

table(data113$q20_1)
table(data113$q20_2)
table(data113$q20_3)
table(data113$q20_4)
table(data113$q20_5)
table(data113$q20_6)
table(data113$q20_7)

table(data112$q19a)
table(data112$q19b)
table(data112$q19c)
table(data112$q19d)
table(data112$q19e)
table(data112$q19f)
table(data112$q19g)

table(data111$q19a)
table(data111$q19b)
table(data111$q19c)
table(data111$q19d)
table(data111$q19e)
table(data111$q19f)
table(data111$q19g)

table(data110$q19a)
table(data110$q19b)
table(data110$q19c)
table(data110$q19d)
table(data110$q19e)
table(data110$q19f)
table(data110$q19g)

table(data109$q20a)
table(data109$q20b)
table(data109$q20c)
table(data109$q20d)
table(data109$q20e)
table(data109$q20f)
table(data109$q20g)

## picture1 ------------------------------------------------------------
## picture1嚙?09??13 撟湔遛?嚙賢漲?嚙踝蕭??嚙踝蕭?銝?嚙踝蕭?銵函嚙?----------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)

# 1. 蝜蕭?銝哨蕭?摮蕭?閮哨蕭?嚗ac嚙?font_add("TC", "msjh.ttc")
showtext_auto()

# 2. ?嚙賭蔥 109??13 撟湛蕭??嚙踝蕭?嚙?reverse嚙? - n
sat_long <- bind_rows(
  data109 %>%
    transmute(year = "109",
              q1 = q20a, q2 = q20b, q3 = q20c,
              q4 = q20d, q5 = q20e, q6 = q20f),
  
  data110 %>%
    transmute(year = "110",
              q1 = q19a, q2 = q19b, q3 = q19c,
              q4 = q19d, q5 = q19e, q6 = q19f),
  
  data111 %>%
    transmute(year = "111",
              q1 = q19a, q2 = q19b, q3 = q19c,
              q4 = q19d, q5 = q19e, q6 = q19f),
  
  data112 %>%
    transmute(year = "112",
              q1 = q19a, q2 = q19b, q3 = q19c,
              q4 = q19d, q5 = q19e, q6 = q19f),
  
  data113 %>%
    transmute(year = "113",
              q1 = q20_1, q2 = q20_2, q3 = q20_3,
              q4 = q20_4, q5 = q20_5, q6 = q20_6)
) %>%
  pivot_longer(
    cols = q1:q6,
    names_to = "item",
    values_to = "score_raw"
  ) %>%
  mutate(
    score_raw = as.numeric(score_raw),
    score_rev = ifelse(score_raw %in% 1:5, 6 - score_raw, NA),
    year = factor(year, levels = c("109", "110", "111", "112", "113")),
    item = recode(
      item,
      q1 = "?嚙質風?嚙踝蕭?,
      q2 = "撌伐蕭??嚙賢漲",
      q3 = "撌伐蕭??嚙踝蕭?",
      q4 = "撌伐蕭??嚙踝蕭?",
      q5 = "?嚙賢?嚙踝蕭?",
      q6 = "銵蕭?蝧"
    )
  )

# 3. 閮蕭??嚙賢僑?嚙踝蕭?撟喉蕭???sat_bar <- sat_long %>%
  group_by(year, item) %>%
  summarise(mean_score = mean(score_rev, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    item = factor(item,
                  levels = c("?嚙質風?嚙踝蕭?, "撌伐蕭??嚙賢漲", "撌伐蕭??嚙踝蕭?",
                             "撌伐蕭??嚙踝蕭?", "?嚙賢?嚙踝蕭?", "銵蕭?蝧"))
  )

# 4. 銝哨蕭? theme
theme_tc <- function() {
  theme_minimal(base_family = "TC") +
    theme(
      text = element_text(family = "TC"),
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 11, face = "bold"),
      axis.text.y = element_text(size = 11),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 11),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20)
    )
}

# 5. ?嚙踝蕭?
p <- ggplot(sat_bar, aes(x = item, y = mean_score, fill = year)) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65
  ) +
  geom_text(
    aes(label = round(mean_score, 2)),
    position = position_dodge(width = 0.75),
    vjust = -0.3,
    size = 3.2,
    family = "TC"
  ) +
  scale_fill_manual(values = c("#1f3e79", "#5c7cfa", "#52b788", "#f59f00", "#d9480f")) +
  scale_y_continuous(
    limits = c(0, 5.3),
    breaks = 0:5
  ) +
  labs(
    title = "109??13 撟游振摨剝蝘餃極皛選蕭?摨佗蕭?嚙?,
    subtitle = "銝?嚙踝蕭?銵函嚗蕭??嚙踝蕭??嚙踝蕭?嚗蕭??嚙踝蕭?擃誨銵剁蕭?皛選蕭?",
    y = "撟喉蕭??嚙賣"
  ) +
  theme_tc()

p

# 6. 摮蕭?
ggsave(
  filename = "picture1_bar_no_overall.png",
  plot = p,
  width = 12,
  height = 8,
  dpi = 400,
  bg = "white"
)

##皜蕭?鞎蕭?嚗蕭??嚙賣?嚙賜憭蕭?摰嗅滬?嚙質風撌伐蕭????撖西釭銝蕭?撟怠嚗蕭??嚙踝蕭??嚙踝蕭?
table(data109$q3_1)
table(data109$q3_2)
table(data109$q3_3)
table(data109$q3_4)
table(data109$q3_5)
table(data109$q3_6)

table(data110$q3_1)
table(data110$q3_2)
table(data110$q3_3)
table(data110$q3_4)
table(data110$q3_5)
table(data110$q3_6)

table(data111$q3_1)
table(data111$q3_2)
table(data111$q3_3)
table(data111$q3_4)
table(data111$q3_5)
table(data111$q3_6)

table(data112$q1_1)
table(data112$q1_2)
table(data112$q1_3)
table(data112$q1_4)
table(data112$q1_5)
table(data112$q1_6)

table(data113$q1_1)
table(data113$q1_2)
table(data113$q1_3)
table(data113$q1_4)
table(data113$q1_5)
table(data113$q1_6)


## 皜蕭?鞎蕭?嚙?09??13 撟渡?嚙踝蕭? --------------------------------------------
## 憿嚗蕭??嚙賣?嚙賜憭蕭?摰嗅滬?嚙質風撌伐蕭????撖西釭銝蕭?撟怠嚗蕭??嚙踝蕭??嚙踝蕭?
## 隞伐蕭??嚙質身嚙? = ?嚙賡嚗隞蕭?= ?嚙賡 / ?嚙賢??

# 2. ?嚙賭蔥鞈蕭?
benefit_long <- bind_rows(
  data109 %>%
    transmute(year = "109",
              q1 = q3_1, q2 = q3_2, q3 = q3_3,
              q4 = q3_4, q5 = q3_5, q6 = q3_6),
  
  data110 %>%
    transmute(year = "110",
              q1 = q3_1, q2 = q3_2, q3 = q3_3,
              q4 = q3_4, q5 = q3_5, q6 = q3_6),
  
  data111 %>%
    transmute(year = "111",
              q1 = q3_1, q2 = q3_2, q3 = q3_3,
              q4 = q3_4, q5 = q3_5, q6 = q3_6),
  
  data112 %>%
    transmute(year = "112",
              q1 = q1_1, q2 = q1_2, q3 = q1_3,
              q4 = q1_4, q5 = q1_5, q6 = q1_6),
  
  data113 %>%
    transmute(year = "113",
              q1 = q1_1, q2 = q1_2, q3 = q1_3,
              q4 = q1_4, q5 = q1_5, q6 = q1_6)
) %>%
  pivot_longer(
    cols = q1:q6,
    names_to = "item",
    values_to = "selected"
  ) %>%
  mutate(
    selected = as.numeric(selected),
    year = factor(year, levels = c("109", "110", "111", "112", "113")),
    item = recode(
      item,
      q1 = "鋡恬蕭?霅瑁?嚙踝蕭?憒伐蕭??嚙賡“",
      q2 = "皜蕭?蝎橘蕭?銝蕭?憯蕭?",
      q3 = "摰嗡犖?嚙踝蕭??嚙賢極嚙?,
      q4 = "皜蕭?摰塚蕭??嚙踝蕭???,
      q5 = "皜蕭?蝬蕭?銝蕭?鞎蕭?",
      q6 = "?嚙踝蕭?"
    ),
    item = factor(
      item,
      levels = c("鋡恬蕭?霅瑁?嚙踝蕭?憒伐蕭??嚙賡“",
                 "皜蕭?蝎橘蕭?銝蕭?憯蕭?",
                 "摰嗡犖?嚙踝蕭??嚙賢極嚙?,
                 "皜蕭?摰塚蕭??嚙踝蕭???,
                 "皜蕭?蝬蕭?銝蕭?鞎蕭?",
                 "?嚙踝蕭?")
    )
  )

# 3. 閮蕭??嚙賢僑?嚙踝蕭?鋡恍甈⊥?嚙踝蕭?嚙?benefit_summary <- benefit_long %>%
  group_by(year, item) %>%
  summarise(
    n_selected = sum(selected == 1, na.rm = TRUE),
    n_total = sum(!is.na(selected)),
    percent = n_selected / n_total,
    percent_label = paste0(round(percent * 100, 1), "%"),
    .groups = "drop"
  )

# 4. 瑼Ｘ嚙?benefit_summary %>%
  select(year, item, n_selected, n_total, percent_label)

# 5. ?嚙踝蕭???p_benefit_heat <- ggplot(
  benefit_summary,
  aes(x = year, y = item, fill = percent)
) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(
    aes(label = percent_label),
    family = "TC",
    size = 4
  ) +
  scale_fill_gradient(
    low = "#f1f3f5",
    high = "#1f3e79",
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "109??13 撟游?嚙踝蕭?蝐振摨哨蕭?霅瑕極撖西釭撟怠銋蕭?嚙?,
    subtitle = "瘥?嚙質府撟游漲?嚙賡?嚙質府?嚙賜?嚙踝蕭?靘蕭??嚙踝蕭??嚙踝蕭?",
    x = "撟港遢",
    y = NULL,
    fill = "瘥蕭?"
  ) +
  theme_minimal(base_family = "TC") +
  theme(
    text = element_text(family = "TC"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(size = 9, face = "bold"),
    axis.text.y = element_text(size = 11),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    panel.grid = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )


p_benefit_heat

# 6. 摮蕭?
ggsave(
  filename = "benefit_heatmap_109_113.png",
  plot = p_benefit_heat,
  width = 10,
  height = 6.5,
  dpi = 400,
  bg = "white"
)
####蝘餃極鈭箏嚙?table(data113$q8a)
table(data113$q8b)
table(data113$q8c)
table(data113$q8d)
table(data113$q8e)
table(data113$q18f)

## 109??13 撟湛蕭??嚙賢 ? 撟湧翩撅歹蕭??嚙賡璇蕭? ----------------------------
# 2. ?嚙賭蔥 109??13 撟湛蕭???gender_age_long <- bind_rows(
  data109 %>% transmute(year = "109", gender = q8a, age = q8b),
  data110 %>% transmute(year = "110", gender = q8a, age = q8b),
  data111 %>% transmute(year = "111", gender = q8a, age = q8b),
  data112 %>% transmute(year = "112", gender = q8a, age = q8b),
  data113 %>% transmute(year = "113", gender = q8a, age = q8b)
) %>%
  mutate(
    year = factor(year, levels = c("109", "110", "111", "112", "113")),
    gender = as.numeric(gender),
    age = as.numeric(age),
    
    gender_label = case_when(
      gender == 1 ~ "??,
      gender == 2 ~ "嚙?,
      TRUE ~ NA_character_
    ),
    
    age_label = case_when(
      age == 1 ~ "?嚙賣遛25嚙?,
      age == 2 ~ "25??4嚙?,
      age == 3 ~ "35??4嚙?,
      age == 4 ~ "45甇莎蕭?隞伐蕭?",
      TRUE ~ NA_character_
    ),
    
    gender_label = factor(gender_label, levels = c("??, "嚙?)),
    age_label = factor(
      age_label,
      levels = c("?嚙賣遛25嚙?, "25??4嚙?, "35??4嚙?, "45甇莎蕭?隞伐蕭?")
    )
  ) %>%
  filter(!is.na(gender_label), !is.na(age_label))

# 3. 閮蕭??嚙賢僑摨艾批?嚙踝蕭?撟湧翩瘥蕭?
gender_age_summary <- gender_age_long %>%
  count(year, gender_label, age_label, name = "n") %>%
  group_by(year, gender_label) %>%
  mutate(
    percent = n / sum(n),
    percent_label = ifelse(percent >= 0.03, paste0(round(percent * 100, 1), "%"), "")
  ) %>%
  ungroup()

# 4. 銝哨蕭? theme
theme_tc <- function() {
  theme_minimal(base_family = "TC") +
    theme(
      text = element_text(family = "TC"),
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 11, face = "bold"),
      axis.text.y = element_text(size = 11),
      strip.text = element_text(size = 13, face = "bold"),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 11),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20)
    )
}

# 5. 100% ?嚙踝蕭??嚙踝蕭??嚙踝蕭??嚙賢戊?嚙踝蕭??嚙踝蕭?頝典僑摨佗蕭?嚙?p_gender_age <- ggplot(
  gender_age_summary,
  aes(x = year, y = percent, fill = age_label)
) +
  geom_col(width = 0.65, color = "white") +
  geom_text(
    aes(
      label = percent_label,
      color = age_label == "?嚙賣遛25嚙?
    ),
    position = position_stack(vjust = 0.5),
    size = 3.2,
    family = "TC",
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "TRUE" = "#1f3e79",
      "FALSE" = "white"
    )
  ) +
  facet_wrap(~ gender_label, nrow = 1) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  scale_fill_manual(
    values = c(
      "?嚙賣遛25嚙? = "#d0ebff",
      "25??4嚙? = "#74c0fc",
      "35??4嚙? = "#228be6",
      "45甇莎蕭?隞伐蕭?" = "#1c3d5a"
    )
  ) +
  labs(
    title = "109??13 撟湛蕭?蝐振摨哨蕭?霅瑕極撟湧翩蝯蕭?瘥蕭?",
    subtitle = "靘批?嚙踝蕭?嚗蕭??嚙賡璇閰脣僑摨西府?嚙賢?嚙踝蕭?撟湧翩瘥蕭?",
    y = "瘥蕭?"
  ) +
  theme_tc()

p_gender_age

# 6. 摮蕭?
ggsave(
  filename = "gender_age_stacked_109_113.png",
  plot = p_gender_age,
  width = 11,
  height = 7,
  dpi = 400,
  bg = "white"
)
###?嚙踝蕭??嚙賢撟湧翩
## 109??13 撟湛蕭?靘蕭?蝐蕭?蝯蕭??嚙賢 ? 撟湧翩撅歹蕭??嚙賡璇蕭? ------------------------



# 2. ?嚙賭蔥 109??13 撟湛蕭??嚙踝蕭??嚙賢?嚙賢僑朣～蕭?嚙?gender_age_country_long <- bind_rows(
  data109 %>% transmute(year = "109", gender = q8a, age = q8b, country = q8d),
  data110 %>% transmute(year = "110", gender = q8a, age = q8b, country = q8d),
  data111 %>% transmute(year = "111", gender = q8a, age = q8b, country = q8d),
  data112 %>% transmute(year = "112", gender = q8a, age = q8b, country = q8d),
  data113 %>% transmute(year = "113", gender = q8a, age = q8b, country = q8d)
) %>%
  mutate(
    year = factor(year, levels = c("109", "110", "111", "112", "113")),
    gender = as.numeric(gender),
    age = as.numeric(age),
    country = as.numeric(country),
    
    gender_label = case_when(
      gender == 1 ~ "??,
      gender == 2 ~ "嚙?,
      TRUE ~ NA_character_
    ),
    
    age_label = case_when(
      age == 1 ~ "?嚙賣遛25嚙?,
      age == 2 ~ "25??4嚙?,
      age == 3 ~ "35??4嚙?,
      age == 4 ~ "45甇莎蕭?隞伐蕭?",
      TRUE ~ NA_character_
    ),
    
    country_label = case_when(
      country == 1 ~ "?嚙賢側",
      country == 2 ~ "?嚙踝蕭?嚙?,
      country == 3 ~ "瘜堆蕭?",
      country == 4 ~ "頞蕭?",
      TRUE ~ NA_character_
    ),
    
    gender_label = factor(gender_label, levels = c("??, "嚙?)),
    age_label = factor(
      age_label,
      levels = c("?嚙賣遛25嚙?, "25??4嚙?, "35??4嚙?, "45甇莎蕭?隞伐蕭?")
    ),
    country_label = factor(
      country_label,
      levels = c("?嚙賢側", "?嚙踝蕭?嚙?, "瘜堆蕭?", "頞蕭?")
    )
  ) %>%
  filter(!is.na(gender_label), !is.na(age_label), !is.na(country_label))

# 3. 閮蕭?嚗蕭?撟游漲 ? ?嚙踝蕭? ? ?嚙賢 ?嚙踝蕭?撟湧翩瘥蕭?
gender_age_country_summary <- gender_age_country_long %>%
  count(year, country_label, gender_label, age_label, name = "n") %>%
  group_by(year, country_label, gender_label) %>%
  mutate(
    percent = n / sum(n),
    percent_label = ifelse(percent >= 0.03, paste0(round(percent * 100, 1), "%"), "")
  ) %>%
  ungroup()

# 4. 銝哨蕭? theme
theme_tc <- function() {
  theme_minimal(base_family = "TC") +
    theme(
      text = element_text(family = "TC"),
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 11, face = "bold"),
      axis.text.y = element_text(size = 11),
      strip.text = element_text(size = 13, face = "bold"),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 11),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20)
    )
}

# 5. ?嚙踝蕭?蝐僑朣∪惜?嚙賜頂嚗蕭??嚙踝蕭??嚙賜瘛箏瘛梧蕭?頝喉蕭??嚙質嚙?country_age_cols <- list(
  "?嚙賢側" = c(
    "?嚙賣遛25嚙? = "#d3f9d8",
    "25??4嚙? = "#8ce99a",
    "35??4嚙? = "#40c057",
    "45甇莎蕭?隞伐蕭?" = "#2b8a3e"
  ),
  "?嚙踝蕭?嚙? = c(
    "?嚙賣遛25嚙? = "#fff3bf",
    "25??4嚙? = "#ffd43b",
    "35??4嚙? = "#f59f00",
    "45甇莎蕭?隞伐蕭?" = "#e67700"
  ),
  "瘜堆蕭?" = c(
    "?嚙賣遛25嚙? = "#ffe3e3",
    "25??4嚙? = "#ffa8a8",
    "35??4嚙? = "#fa5252",
    "45甇莎蕭?隞伐蕭?" = "#c92a2a"
  ),
  "頞蕭?" = c(
    "?嚙賣遛25嚙? = "#f3d9fa",
    "25??4嚙? = "#da77f2",
    "35??4嚙? = "#ae3ec9",
    "45甇莎蕭?隞伐蕭?" = "#862e9c"
  )
)
# 6. 靘蕭?蝐蕭??嚙賜?嚙賢撐??# 6. 靘蕭?蝐蕭??嚙賜?嚙賢撐?嚙踝蕭?銝血?嚙賢戊?嚙踝蕭?憿舐內嚙?嚙踝蕭??plot_country_list <- list()

for (cty in levels(gender_age_country_summary$country_label)) {
  
  # 閰莎蕭?摰塚蕭?瘥蕭?鞈蕭?
  df_cty <- gender_age_country_summary %>%
    filter(country_label == cty)
  
  # 閰莎蕭?摰塚蕭??嚙賢戊蝮賣見?嚙賣嚗楊 109??13 撟湛蕭?閮蕭?
  gender_n_cty <- gender_age_country_long %>%
    filter(country_label == cty) %>%
    count(gender_label, name = "gender_n") %>%
    mutate(
      gender_label_n = paste0(gender_label, "嚗 = ", gender_n, "嚙?)
    )
  
  # ?嚙賣見?嚙賣璅惜?嚙賭蔥?嚙踝蕭??嚙踝蕭???  df_cty <- df_cty %>%
    left_join(
      gender_n_cty %>% select(gender_label, gender_label_n),
      by = "gender_label"
    )
  
  p <- ggplot(
    df_cty,
    aes(x = year, y = percent, fill = age_label)
  ) +
    geom_col(width = 0.65, color = "white") +
    geom_text(
      aes(
        label = percent_label,
        color = age_label == "?嚙賣遛25嚙?
      ),
      position = position_stack(vjust = 0.5),
      size = 3.2,
      family = "TC",
      show.legend = FALSE
    ) +
    scale_color_manual(
      values = c(
        "TRUE" = "#1f3e79",
        "FALSE" = "white"
      )
    ) +
    facet_wrap(~ gender_label_n, nrow = 1) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, 1)
    ) +
    scale_fill_manual(
      values = country_age_cols[[as.character(cty)]]
    ) +
    labs(
      title = paste0("109??13 嚙?, cty, "蝐蕭?蝐振摨哨蕭?霅瑕極撟湧翩蝯蕭?瘥蕭?"),
      subtitle = "靘批?嚙踝蕭?嚗蕭??嚙賡璇閰脣僑摨西府?嚙賢?嚙踝蕭?撟湧翩瘥蕭?",
      y = "瘥蕭?"
    ) +
    theme_tc()
  
  print(p)
  plot_country_list[[cty]] <- p
  
  ggsave(
    filename = paste0("gender_age_stacked_", cty, "_109_113.png"),
    plot = p,
    width = 11,
    height = 7,
    dpi = 400,
    bg = "white"
  )
}
###?嚙賣偌
data109$q9_0_1
data110$q11
data111$q11
data112$q11
data113$nq10a

## 109??13 撟湛蕭?銝蕭??嚙踝蕭??嚙賣偌蝞梧蕭??嚙踝蕭?嚙?---------------------------------

library(dplyr)
library(ggplot2)
library(showtext)
library(scales)

# 1. 銝哨蕭?摮蕭?
font_add("TC", "msjh.ttc")
showtext_auto()

# 2. ?嚙賭蔥?嚙賣偌?嚙踝蕭?蝐蕭???salary_long <- bind_rows(
  data109 %>% transmute(year = "109", country = q8d, salary = q9_0_1),
  data110 %>% transmute(year = "110", country = q8d, salary = q11),
  data111 %>% transmute(year = "111", country = q8d, salary = q11),
  data112 %>% transmute(year = "112", country = q8d, salary = q11),
  data113 %>% transmute(year = "113", country = q8d, salary = nq10a)
) %>%
  mutate(
    year = factor(year, levels = c("109", "110", "111", "112", "113")),
    country = as.numeric(country),
    country_label = case_when(
      country == 1 ~ "?嚙賢側",
      country == 2 ~ "?嚙踝蕭?嚙?,
      country == 3 ~ "瘜堆蕭?",
      country == 4 ~ "頞蕭?",
      TRUE ~ NA_character_
    ),
    country_label = factor(
      country_label,
      levels = c("?嚙賢側", "?嚙踝蕭?嚙?, "瘜堆蕭?", "頞蕭?")
    ),
    salary = as.character(salary),
    salary = gsub(",", "", salary),
    salary = as.numeric(salary)
  ) %>%
  filter(
    !is.na(country_label),
    !is.na(salary),
    salary > 0
  )
# 3. 瑼Ｘ?嚙賢僑?嚙踝蕭?嚙?嚙踝蕭??salary_long %>%
  count(year, country_label, name = "n")

# 4. 銝哨蕭? theme
theme_tc <- function() {
  theme_minimal(base_family = "TC") +
    theme(
      text = element_text(family = "TC"),
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 11, face = "bold"),
      axis.text.y = element_text(size = 11),
      strip.text = element_text(size = 13, face = "bold"),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20)
    )
}

## 109??13 撟湛蕭?銝蕭??嚙踝蕭??嚙賣偌蝞梧蕭??嚙踝蕭?頛蕭?x 頠賂蕭???n -------------------------

library(dplyr)
library(ggplot2)
library(showtext)
library(scales)

# 1. 銝哨蕭?摮蕭?
font_add("TC", "msjh.ttc")
showtext_auto()

# 2. ?嚙賭蔥?嚙賣偌?嚙踝蕭?蝐蕭???salary_long <- bind_rows(
  data109 %>% transmute(year = "109", country = q8d, salary = q9_0_1),
  data110 %>% transmute(year = "110", country = q8d, salary = q11),
  data111 %>% transmute(year = "111", country = q8d, salary = q11),
  data112 %>% transmute(year = "112", country = q8d, salary = q11),
  data113 %>% transmute(year = "113", country = q8d, salary = nq10a)
) %>%
  mutate(
    year = factor(year, levels = c("109", "110", "111", "112", "113")),
    country = as.numeric(country),
    country_label = case_when(
      country == 1 ~ "?嚙賢側",
      country == 2 ~ "?嚙踝蕭?嚙?,
      country == 3 ~ "瘜堆蕭?",
      country == 4 ~ "頞蕭?",
      TRUE ~ NA_character_
    ),
    country_label = factor(
      country_label,
      levels = c("?嚙賢側", "?嚙踝蕭?嚙?, "瘜堆蕭?", "頞蕭?")
    ),
    salary = as.character(salary),
    salary = gsub(",", "", salary),
    salary = as.numeric(salary)
  ) %>%
  filter(
    !is.na(country_label),
    !is.na(salary),
    salary > 0
  )

# 3. 閮蕭??嚙賢僑嚙?? ?嚙踝蕭?嚙?嚙踝蕭?嚙踝蕭?銝行??x 頠賂蕭?嚙?salary_n <- salary_long %>%
  count(year, country_label, name = "n")

salary_long_n <- salary_long %>%
  left_join(salary_n, by = c("year", "country_label")) %>%
  mutate(
    country_label_n = paste0(country_label, "\n", "n = ", n),
    country_label_n = factor(
      country_label_n,
      levels = unique(country_label_n[order(year, country_label)])
    )
  )

# 4. 銝哨蕭? theme
theme_tc <- function() {
  theme_minimal(base_family = "TC") +
    theme(
      text = element_text(family = "TC"),
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 10, face = "bold", lineheight = 0.9),
      axis.text.y = element_text(size = 11),
      strip.text = element_text(size = 13, face = "bold"),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20)
    )
}

# 5. 蝞梧蕭???p_salary_box <- ggplot(
  salary_long_n,
  aes(x = country_label_n, y = salary, fill = country_label)
) +
  geom_boxplot(
    width = 0.65,
    outlier.alpha = 0.25
  ) +
  facet_wrap(~ year, nrow = 1, scales = "free_x") +
  scale_fill_manual(
    values = c(
      "?嚙賢側" = "#8DAA91",
      "?嚙踝蕭?嚙? = "#D8B26E",
      "瘜堆蕭?" = "#C97C7C",
      "頞蕭?" = "#9B8BB4"
    )
  ) +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "109??13 撟湛蕭??嚙踝蕭?蝐蕭?蝐振摨哨蕭?霅瑕極?嚙賣偌瘥蕭?",
    subtitle = "蝞梧蕭??嚙賡＊蝷綽蕭?撟游漲?嚙踝蕭??嚙踝蕭?蝐蕭??嚙賣偌?嚙踝蕭?",
    y = "?嚙賣偌"
  ) +
  theme_tc()

p_salary_box

# 6. 摮蕭?
ggsave(
  filename = "salary_boxplot_country_by_year_109_113.png",
  plot = p_salary_box,
  width = 15,
  height = 6,
  dpi = 400,
  bg = "white"
)

## 109??13 撟湛蕭??嚙賢?嚙踝蕭?霅瘀蕭?閮毀 q8e ?嚙踝蕭? ----------------------------

## 109??13 撟湛蕭??嚙賢?嚙踝蕭?霅瘀蕭?閮毀 q8e ?嚙踝蕭? ----------------------------

# 1. 銝哨蕭?摮蕭?
font_add("TC", "msjh.ttc")
showtext_auto()

# 2. ?嚙賭蔥鞈蕭?
training_long <- bind_rows(
  data109 %>% transmute(year = "109", gender = q8a, age = q8b, country = q8d, training = q8e),
  data110 %>% transmute(year = "110", gender = q8a, age = q8b, country = q8d, training = q8e),
  data111 %>% transmute(year = "111", gender = q8a, age = q8b, country = q8d, training = q8e),
  data112 %>% transmute(year = "112", gender = q8a, age = q8b, country = q8d, training = q8e),
  data113 %>% transmute(year = "113", gender = q8a, age = q8b, country = q8d, training = q8e)
) %>%
  mutate(
    year = factor(year, levels = c("109", "110", "111", "112", "113")),
    gender = as.numeric(gender),
    age = as.numeric(age),
    country = as.numeric(country),
    training = as.numeric(training),
    
    gender_label = case_when(
      gender == 1 ~ "??,
      gender == 2 ~ "嚙?,
      TRUE ~ NA_character_
    ),
    
    age_label = case_when(
      age == 1 ~ "?嚙賣遛25嚙?,
      age == 2 ~ "25??4嚙?,
      age == 3 ~ "35??4嚙?,
      age == 4 ~ "45甇莎蕭?隞伐蕭?",
      TRUE ~ NA_character_
    ),
    
    country_label = case_when(
      country == 1 ~ "?嚙賢側",
      country == 2 ~ "?嚙踝蕭?嚙?,
      country == 3 ~ "瘜堆蕭?",
      country == 4 ~ "頞蕭?",
      TRUE ~ NA_character_
    ),
    
    training_label = case_when(
      training == 1 ~ "?嚙賣?嚙質風?嚙踝蕭?嚙?,
      training == 2 ~ "瘝蕭??嚙踝蕭?霅瘀蕭?閮毀",
      TRUE ~ NA_character_
    ),
    
    gender_label = factor(gender_label, levels = c("??, "嚙?)),
    age_label = factor(age_label, levels = c("?嚙賣遛25嚙?, "25??4嚙?, "35??4嚙?, "45甇莎蕭?隞伐蕭?")),
    country_label = factor(country_label, levels = c("?嚙賢側", "?嚙踝蕭?嚙?, "瘜堆蕭?", "頞蕭?")),
    training_label = factor(training_label, levels = c("?嚙賣?嚙質風?嚙踝蕭?嚙?, "瘝蕭??嚙踝蕭?霅瘀蕭?閮毀"))
  ) %>%
  filter(!is.na(training_label))

# 3. 銝哨蕭? theme
theme_tc <- function() {
  theme_minimal(base_family = "TC") +
    theme(
      text = element_text(family = "TC"),
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 11, face = "bold"),
      axis.text.y = element_text(size = 11),
      strip.text = element_text(size = 13, face = "bold"),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 11),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20)
    )
}

# 4. ?嚙賢僑摨佗蕭??嚙賢?嚙踝蕭?霅瘀蕭?閮毀瘥蕭?
training_year_summary <- training_long %>%
  count(year, training_label, name = "n") %>%
  group_by(year) %>%
  mutate(
    percent = n / sum(n),
    percent_label = paste0(round(percent * 100, 1), "%")
  ) %>%
  ungroup()

p_training_year <- ggplot(
  training_year_summary,
  aes(x = year, y = percent, fill = training_label)
) +
  geom_col(width = 0.65, color = "white") +
  geom_text(
    aes(label = percent_label),
    position = position_stack(vjust = 0.5),
    size = 3.5,
    family = "TC",
    color = "white"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  scale_fill_manual(
    values = c(
      "?嚙賣?嚙質風?嚙踝蕭?嚙? = "#6B8F71",
      "瘝蕭??嚙踝蕭?霅瘀蕭?閮毀" = "#C97C7C"
    )
  ) +
  labs(
    title = "109??13 撟湛蕭?蝐振摨哨蕭?霅瑕極?嚙賢?嚙踝蕭?霅瘀蕭?閮毀",
    subtitle = "?嚙賢僑摨血瘥蕭?",
    y = "瘥蕭?"
  ) +
  theme_tc()

p_training_year

# 5. ?嚙賢僑嚙?? ?嚙踝蕭?嚗?嚙賣?嚙質風?嚙踝蕭?蝺湛蕭?嚙?training_country_summary <- training_long %>%
  filter(!is.na(country_label)) %>%
  count(year, country_label, training_label, name = "n") %>%
  group_by(year, country_label) %>%
  mutate(
    percent = n / sum(n),
    percent_label = paste0(round(percent * 100, 1), "%")
  ) %>%
  ungroup()

p_training_country <- ggplot(
  training_country_summary,
  aes(x = year, y = percent, fill = training_label)
) +
  geom_col(width = 0.75, color = "white") +
  geom_text(
    aes(label = percent_label),
    position = position_stack(vjust = 0.5),
    size = 2,
    family = "TC",
    color = "white"
  ) +
  facet_wrap(~ country_label, nrow = 1) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  scale_fill_manual(
    values = c(
      "?嚙賣?嚙質風?嚙踝蕭?嚙? = "#543346",
      "瘝蕭??嚙踝蕭?霅瘀蕭?閮毀" = "#C97C7C"
    )
  ) +
  labs(
    title = "109??13 撟湛蕭??嚙踝蕭?蝐?嚙賣?嚙質風?嚙踝蕭?嚙?,
    subtitle = "?嚙賢僑摨艾蕭??嚙踝蕭??嚙踝蕭?嚙?,
    y = "瘥蕭?"
  ) +
  theme_tc()

p_training_country

# 6. 摮蕭?
ggsave(
  filename = "training_by_year_109_113.png",
  plot = p_training_year,
  width = 10,
  height = 6,
  dpi = 400,
  bg = "white"
)

ggsave(
  filename = "training_by_country_109_113.png",
  plot = p_training_country,
  width = 14,
  height = 6,
  dpi = 400,
  bg = "white"
)
