library(tidyverse)
library(ggplot2)
library(showtext)

# 註冊 Google 中文字型 Noto Sans TC
font_add_google("Noto Sans TC", "NotoSans")
showtext_auto()

# ==========================================
# 📊 移工失聯率熱力圖繪製範本 (使用 NotoSans 與經典格式要求)
# ==========================================

# 先計算各職業的平均失聯率，用來排序
occupation_order <- df_missing_rate %>%
    filter(year == 2026, month == 3) %>%
    select(nationality, ends_with("_rate")) %>%
    pivot_longer(
        cols = ends_with("_rate"),
        names_to = "occupation",
        values_to = "rate"
    ) %>%
    mutate(occupation = str_remove(occupation, "_rate")) %>%
    group_by(occupation) %>%
    summarise(mean_rate = mean(rate, na.rm = TRUE)) %>%
    arrange(mean_rate) %>%  # 由低到高，這樣最高的會在 y 軸最上面
    pull(occupation)


df_missing_rate %>%
    
    filter(year == 2026, month == 3) %>%
    
    select(nationality, ends_with("_rate")) %>%
    
    pivot_longer(
        cols = ends_with("_rate"),
        names_to = "occupation",
        values_to = "rate"
    ) %>%
    
    mutate(
        occupation  = str_remove(occupation, "_rate"),
        nationality = nationality_names[nationality],
        occupation  = factor(occupation_names[occupation],
                             levels = occupation_names[occupation_order])
    ) %>%
    
    ggplot(aes(x = nationality, y = occupation, fill = rate)) +
    
    geom_tile() +
    
    geom_text(aes(label = paste0(round(rate, 1), "%")), 
              color = "white", size = 5) +
    
    scale_fill_gradient(low = "#fee0d2", high = "#a50f15") +
    
    theme_classic(base_family = "NotoSans", base_size = 20) +
    
    labs(
        title = "2026 年 3 月 移工失聯率",
        x = NULL, y = NULL,
        fill = "失聯率",
        caption = "資料來源：勞動部、內政部移民署"
    ) +
    theme(
        plot.title = element_text(face = "bold", size = 25),
        plot.caption = element_text(color = "#aaaaaa"),  # caption 變淺
        legend.position = "right",
        axis.line = element_blank(),   # 移除 x y 軸線
        axis.ticks = element_blank()   # 移除刻度線
    )
