# set.seed(42)
#
#
# # Pull out afghan provinces
# library(sf)
# library(tidyverse)
#
#
#
#
#
#
# # 17 random provinces
# # 17 Provinces
# provinces <- c(
#   "Badakhshan", "Badghis", "Baghlan", "Balkh", "Bamyan", "Daykundi", "Farah",
#   "Faryab", "Ghazni", "Ghor", "Helmand", "Herat", "Jawzjan", "Kabul",
#   "Kandahar", "Kapisa", "Khost"
# )
#
# sim_data <- expand_grid(
#   province = provinces,
#   p4p_indicators,
#   replicate = 1
# ) %>%
#   mutate(value = round(runif(n(), 0.5, 1.0), 3)) %>%
#   select(province, indicator, indicator_short, value)
#
#
# library(ggplot2)
# library(ggbump)
# library(dplyr)
# library(scales)
#
# # Summarize average value per province and indicator
# bump_data <- sim_data %>%
#   group_by(province, indicator_short) %>%
#   summarize(mean_value = mean(value), .groups = "drop")
#
# # Rank each province within each indicator
# bump_data_ranked <- bump_data %>%
#   group_by(indicator_short) %>%
#   mutate(rank = rank(-mean_value)) %>%  # higher value = better rank
#   ungroup()
#
# # Plot: Y = province, X = indicator, value determines vertical order (via rank)
# ggplot(bump_data_ranked, aes(
#   x = indicator_short,
#   y = rank,
#   group = province,
# )) +
#   geom_bump(size = 0.75, smooth = 10, color = glitr::grey20k) +
#   geom_point(size = 3, color = glitr::grey30k ) +
#     geom_point(size = 3, color = glitr::grey30k ) +
#   geom_bump(data = bump_data_ranked %>% filter(province == "Ghazni"), size = 0.5, smooth = 5, color = glitr::old_rose) +
#   geom_point(data = bump_data_ranked %>% filter(province == "Ghazni"), size = 3, color = glitr::old_rose) +
#   geom_text(aes(label = percent(mean_value, 1.0)),
#             nudge_y = -0.5, size = 8/.pt) +
#   labs(
#     title = "Province Performance Across Health Indicators",
#     x = "Indicator",
#     y = "Province (Line Tracks Relative Rank)",
#   ) +
#   glitr::si_style_nolines() +
#   geom_text(data = bump_data_ranked %>% filter(indicator_short == "ANC"),
#             aes(label = province),
#             color = glitr::grey70k,
#             nudge_x = -1,
#             hjust = 0,
#             size = 3,
#             fontface = 2) +
#   geom_text(data = bump_data_ranked %>% filter(indicator_short == "MS"),
#             aes(label = province),
#             color = glitr::grey70k,
#             nudge_x = 0.2,
#             hjust = 0,
#             size = 3,
#             fontface = 2) +
#   scale_x_discrete(position = "top", labels = label_wrap(width = 15)) +
#   theme(
#     legend.position = "none"
#   )
