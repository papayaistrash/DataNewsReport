library(tidyverse)
library(scales)
library(patchwork)
library(ggplot2)
library(showtext)

base_dir <- "."`nif (basename(getwd()) == "scripts") { base_dir <- ".." }`ndir.create(file.path(base_dir, "plots"), showWarnings = FALSE)

# 閮剖?銝剜?摮?
font_add("TC", "msjh.ttc") 
showtext_auto()

# 摰儔???ggplot ?啗?銝駁?憸冽
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

# 1. ?湧?嚗??曇情??撖西?璅?runaway_rate <- tibble(
  撟游? = rep(2019:2025, each = 2),
  ?Ｘ平??= rep(c("鋆賡平", "摰嗅滬?風撌?), 7),
  憭梯?pct = c(
    5.8, 6.2,   
    6.3, 6.8,   
    6.5, 7.2,   
    9.5, 10.2,  
    10.5, 11.5, 
    10.8, 11.8, 
    10.5, 12.2  
  )
)

p1 <- ggplot(runaway_rate, aes(x = 撟游?, y = 憭梯?pct, color = ?Ｘ平??) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3.5) +
  geom_text(data = runaway_rate %>% filter(撟游? == 2025),
            aes(label = paste0(憭梯?pct, "%")),
            hjust = -0.3, fontface = "bold", size = 7, show.legend = FALSE) +
  scale_color_manual(values = c("鋆賡平" = "#1F3145", "摰嗅滬?風撌? = "#DE4429")) +
  scale_x_continuous(breaks = 2019:2025) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(4, 14)) +
  labs(title = "銝??瘜????風撌伐?憭梯??蝥??潸ˊ?平",
       subtitle = "2019??025 撟渡敞閮仃?舐?頞典瘥?",
       y = "蝝航?憭梯??,
       caption = "鞈?靘?嚗???宏瘞蔡蝯梯?") +
  coord_cartesian(xlim = c(2019, 2025.5)) +
  theme_news()
ggsave(file.path(base_dir, "plots/1_trend.png", plot = p1, width = 10, height = 6, dpi = 150)


# 2. ?詨?頛芸?
nat_runaway <- tibble(
  ?? = c("頞?", "頞?", "?啣側", "?啣側", "?脣?鞈?, "?脣?鞈?, "瘜啣?", "瘜啣?"),
  ?Ｘ平??= rep(c("鋆賡平", "摰嗅滬?風撌?), 4),
  憭梯??= c(
    22.3, 12.7, 
    10.0, 10.8, 
     1.5,  2.2, 
     2.6,  3.8  
  )
) %>%
  mutate(?? = factor(??, levels = c("頞?", "?啣側", "?脣?鞈?, "瘜啣?")))

p2 <- ggplot(nat_runaway, aes(x = ??, y = 憭梯?? fill = ?Ｘ平??) +
  geom_col(position = "dodge", width = 0.65) +
  geom_text(aes(label = paste0(憭梯?? "%")),
            position = position_dodge(0.65), vjust = -0.5,
            fontface = "bold", size = 7) +
  scale_fill_manual(values = c("鋆賡平" = "#1F3145", "摰嗅滬?風撌? = "#DE4429")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "頞?蝐宏撌亦?憭梯???批?擐?,
       subtitle = "2025 撟游???蝐?? ?Ｘ平?亙仃?舐?撠?",
       y = "蝝航?憭梯??,
       caption = "鞈?靘?嚗宏瘞蔡 2025 撟游?蝯梯?") +
  theme_news()
ggsave(file.path(base_dir, "plots/2_nationality.png", plot = p2, width = 10, height = 6, dpi = 150)

# 3. 敺株?鈭文???
data113 <- read.csv("113撟渡宏撌?摰嗅滬??data113.csv")

df_micro <- data113 %>%
  mutate(
    ?? = factor(q8d, levels = 1:4, labels = c("?啣側", "?脣?鞈?, "瘜啣?", "頞?")),
    撟湧翩蝯?= factor(q8b, levels = 1:4, labels = c("?芣遛25甇?, "25-34甇?, "35-44甇?, "45甇脖誑銝?)),
    ?暸? = factor(q5, levels = 1:2, labels = c("??, "??)),
    甈? = as.numeric(w)
  ) %>%
  filter(!is.na(??), !is.na(撟湧翩蝯?, !is.na(?暸?))

runaway_micro <- df_micro %>%
  count(??, 撟湧翩蝯? ?暸?, wt = 甈?) %>%
  group_by(??, 撟湧翩蝯? %>%
  mutate(pct = n / sum(n)) %>%
  filter(?暸? == "??)

p3 <- ggplot(runaway_micro, aes(x = 撟湧翩蝯? y = pct, color = ??, group = ??)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 4) +
  geom_text(aes(label = percent(pct, accuracy = 0.1)),
            vjust = -1.2, size = 7, show.legend = FALSE, fontface = "bold") +
  scale_color_manual(values = c("?啣側" = "#E8A343", "?脣?鞈? = "#385E78", 
                                "瘜啣?" = "#8998A1", "頞?" = "#DE4429")) +
  scale_y_continuous(labels = percent) +
  labs(title = "頞僑頛?頞?蝐??啣側蝐宏撌伐???憸券頞?",
       subtitle = "113撟游振摨剝隤踵嚗?蝐?? 撟湧翩蝯?????,
       y = "???潛?瘥?",
       caption = "鞈?靘?嚗??113撟渡宏撌亦恣???隤踵") +
  theme_news()
ggsave(file.path(base_dir, "plots/3_micro_profile.png", plot = p3, width = 10, height = 6, dpi = 150)

# 4. ????q5a_vars   <- paste0("q5a_", 1:9)
q5a_labels <- c("??鈭箸?踴?隞?, "撣??脣?頛?敺?", "????喳?撅遛", 
                "鞈剖?甈?瘜?, "?貉?銝?瘣?, "撌乩???瘣餌憓瘜??,
                "隞脖???鞎餃云擃?, "???剛降", "?嗡?")

df_q5 <- data113 %>% filter(q5 == 2) %>% mutate(甈? = as.numeric(w))
total_w <- sum(df_q5$甈?, na.rm = TRUE)

q5a_df <- df_q5 %>%
  select(all_of(q5a_vars), 甈?) %>%
  pivot_longer(cols = all_of(q5a_vars), names_to = "item", values_to = "val") %>%
  filter(val == 1) %>%
  group_by(item) %>%
  summarise(n_w = sum(甈?, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    label = q5a_labels[match(item, q5a_vars)],
    pct   = n_w / total_w
  ) %>%
  filter(label %in% c("??鈭箸?踴?隞?, "撌乩???瘣餌憓瘜??, "隞脖???鞎餃云擃?)) %>%
  arrange(desc(pct)) %>%
  mutate(label = fct_reorder(label, pct))

p4 <- ggplot(q5a_df, aes(x = pct, y = label)) +
  geom_col(aes(fill = pct), width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = percent(pct, 0.1)), hjust = -0.2, fontface = "bold", size = 7) +
  scale_fill_gradient(low = "#E8A343", high = "#DE4429") +
  scale_x_continuous(labels = percent, expand = expansion(mult = c(0, 0.2))) +
  labs(title = "?蜓隤蝘餃極憭梯嚗??胯?鈭箸?踴?,
       subtitle = "??甇瑕仃?舐??蜓?暸銋蜓閬???(?航???",
       y = NULL,
       caption = "鞈?靘?嚗??113撟渡宏撌亦恣???隤踵") +
  theme_news() +
  theme(panel.grid.major.y = element_blank())
ggsave(file.path(base_dir, "plots/4_reasons.png", plot = p4, width = 10, height = 6, dpi = 150)

# 5. ??璇辣瘥?
conditions <- tibble(
  ? = rep(c("蝬虜?扳??歿n(?砍?)", "瘥??曉?\n(憭拇)", "蝮質鞈n(?砍?)", "蝝航?憭梯?n(%)"), each = 2),
  憿 = rep(c("?Ｘ平蝘餃極", "摰嗅滬?風撌?), 4),
  ?詨?= c(
    2.9, 2.1,   
    11, 1.5,    
    3.3, 2.4,   
    10.5, 12.2  
  )
) %>%
  mutate(
    ? = factor(?, levels = c("蝬虜?扳??歿n(?砍?)", "蝮質鞈n(?砍?)", "瘥??曉?\n(憭拇)", "蝝航?憭梯?n(%)")),
    憿 = factor(憿, levels = c("?Ｘ平蝘餃極", "摰嗅滬?風撌?))
  )

conditions_norm <- conditions %>%
  group_by(?) %>%
  mutate(
    ?箸? = ?詨墩憿 == "?Ｘ平蝘餃極"],
    ?詨?瘥? = ?詨?/ ?箸?,
    璅惜 = case_when(
      str_detect(?, "憭梯??) ~ paste0(?詨? "%"),
      str_detect(?, "憭拇") ~ paste0(?詨? "憭?),
      TRUE ~ paste0(?詨? "??)
    )
  ) %>%
  ungroup()

p5 <- ggplot(conditions_norm, aes(x = ?, y = ?詨?瘥?, fill = 憿)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = 璅惜),
            position = position_dodge(width = 0.7), vjust = -0.5,
            fontface = "bold", size = 7) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#999999") +
  scale_fill_manual(values = c("?Ｘ平蝘餃極" = "#1F3145", "摰嗅滬?風撌? = "#DE4429")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1.3), expand = expansion(mult = c(0, 0.1))) +
  labs(title = "?蝘餃極嚗?之銝?嚗???撌柴???擃?,
       subtitle = "隞亦璆剔宏撌亦?箸? (100%) 銋???隞嗉?憭梯??瘥?,
       y = "?詨??潛璆剔宏撌乩?瘥?",
       caption = "鞈?靘?嚗??113撟渡宏撌亥矽?乓宏瘞蔡蝯梯?") +
  theme_news() +
  theme(axis.text.x = element_text(face = "bold", size = 12))
ggsave(file.path(base_dir, "plots/5_conditions.png", plot = p5, width = 10, height = 6, dpi = 150)
