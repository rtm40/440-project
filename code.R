# libraries and data
library(tidyverse)
library(scales)
library(ggridges)
library(patchwork)
library(showtext)
library(ggtext)
bbdata <- read_csv("data/bbdata.csv")
font_add_google("STIX Two Text", "Times New Roman")
showtext_auto()

# data prep
col4 <- c("#E06060", "#FFBFBF", "#E7F9FF", "#95D2EC")
col5 <- c("#E06060", "#FFBFBF", "#E7F9FF", "#95D2EC", "#47ABD8")

bb <- bbdata |>
  mutate(
    placement_bucket = case_when(
      placement_rank_percent < .09 ~ "Winner",
      placement_rank_percent <= .25 ~ "Late Game",
      placement_rank_percent <= .5 ~ "Upper Middle",
      placement_rank_percent <= .75 ~ "Lower Middle",
      placement_rank_percent <= 1 ~ "Early Out"
    ),
    wins_bucket = case_when(
      wins_rank_percent <= .25 ~ "Top Quartile",
      wins_rank_percent <= .5 ~ "High Quartile",
      wins_rank_percent <= .75 ~ "Low Quartile",
      wins_rank_percent <= 1 ~ "Bottom Quartile"
    ),
    age_bucket = case_when(
      age <= 29 ~ "19-29",
      age <= 39 ~ "30-39",
      age <= 80 ~ "40+"
    ))

bb$placement_bucket <- ordered(bb$placement_bucket,
                               levels = c("Early Out",
                                          "Lower Middle",
                                          "Upper Middle",
                                          "Late Game",
                                          "Winner"))

bb$wins_bucket <- ordered(bb$wins_bucket,
                          levels = c("Bottom Quartile",
                                     "Low Quartile",
                                     "High Quartile",
                                     "Top Quartile"))

bb$age_bucket <- ordered(bb$age_bucket,
                         levels = c("19-29",
                                    "30-39",
                                    "40+"))

# stacked bar chart dataframes
bb_bipoc_place <- bb |>
  group_by(BIPOC, placement_bucket) |>
  summarize(n = n()) |>
  mutate(freq = n / sum(n)) |>
  mutate(per = label_percent(accuracy = 1)(freq)) |>
  ungroup()

bb_lgbt_place <- bb |>
  group_by(LGBT, placement_bucket) |>
  summarize(n = n()) |>
  mutate(freq = n / sum(n)) |>
  mutate(per = label_percent(accuracy = 1)(freq)) |>
  ungroup()

bb_gender_place <- bb |>
  group_by(gender, placement_bucket) |>
  summarize(n = n()) |>
  mutate(freq = n / sum(n)) |>
  mutate(per = label_percent(accuracy = 1)(freq)) |>
  ungroup()

bb_age_place <- bb |>
  group_by(age_bucket, placement_bucket) |>
  summarize(n = n()) |>
  mutate(freq = n / sum(n)) |>
  mutate(per = label_percent(accuracy = 1)(freq)) |>
  ungroup()

bb_gender_wins <- bb |>
  group_by(gender, wins_bucket) |>
  summarize(n = n()) |>
  mutate(freq = n / sum(n)) |>
  mutate(per = label_percent(accuracy = 1)(freq)) |>
  ungroup()

bb_age_wins <- bb |>
  group_by(age_bucket, wins_bucket) |>
  summarize(n = n()) |>
  mutate(freq = n / sum(n)) |>
  mutate(per = label_percent(accuracy = 1)(freq)) |>
  ungroup()

bipoc_place <-
  ggplot(bb_bipoc_place, aes(y = BIPOC, x = freq, label = per, fill = placement_bucket)) +
  geom_col(position = "fill") +
  geom_text(size = 3, fontface = "bold", family = "Times New Roman",
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = col5) +
  labs(fill = "") +
  theme_void() +
  theme(axis.title.y = element_text(face = "bold"),
        axis.text.y = element_text(),
        text = element_text(family = "Times New Roman", size = 14),
        legend.text = element_text(size = 9))

lgbt_place <-
  ggplot(bb_lgbt_place, aes(y = LGBT, x = freq, label = per, fill = placement_bucket)) +
  geom_col(position = "fill") +
  geom_text(size = 3, fontface = "bold", family = "Times New Roman",
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = col5) +
  labs(fill = "") +
  theme_void() +
  theme(axis.title.y = element_text(face = "bold"),
        axis.text.y = element_text(),
        text = element_text(family = "Times New Roman", size = 14),
        legend.text = element_text(size = 9))

gender_place <-
  ggplot(bb_gender_place, aes(y = gender, x = freq, label = per, fill = placement_bucket)) +
  geom_col(position = "fill") +
  geom_text(size = 3, fontface = "bold", family = "Times New Roman",
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = col5) +
  labs(y = "Gender", fill = "") +
  theme_void() +
  theme(axis.title.y = element_text(face = "bold"),
        axis.text.y = element_text(),
        text = element_text(family = "Times New Roman", size = 14),
        legend.text = element_text(size = 9))

age_place <-
  ggplot(bb_age_place, aes(y = age_bucket, x = freq, label = per, fill = placement_bucket)) +
  geom_col(position = "fill") +
  geom_text(size = 3, fontface = "bold", family = "Times New Roman",
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = col5) +
  labs(y = "Age", fill = "") +
  theme_void() +
  theme(axis.title.y = element_text(face = "bold"),
        axis.text.y = element_text(),
        text = element_text(family = "Times New Roman", size = 14),
        legend.text = element_text(size = 9))

gender_wins <-
  ggplot(bb_gender_wins, aes(y = gender, x = freq, label = per, fill = wins_bucket)) +
  geom_col(position = "fill") +
  geom_text(size = 3, fontface = "bold", family = "Times New Roman",
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = col4) +
  labs(y = "Gender", fill = "") +
  theme_void() +
  theme(axis.title.y = element_text(face = "bold"),
        axis.text.y = element_text(),
        text = element_text(family = "Times New Roman", size = 14))

age_wins <-
  ggplot(bb_age_wins, aes(y = age_bucket, x = freq, label = per, fill = wins_bucket)) +
  geom_col(position = "fill") +
  geom_text(size = 3, fontface = "bold", family = "Times New Roman",
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = col4) +
  labs(y = "Age", fill = "") +
  theme_void() +
  theme(axis.title.y = element_text(face = "bold"),
        axis.text.y = element_text(),
        text = element_text(family = "Times New Roman", size = 14))

# patchwork
fig1 <- (bipoc_place / lgbt_place / gender_place / age_place) +
  plot_layout(guides = "collect", heights = c(1, 1, 1, 1.33)) &
  theme(legend.position = "top")

fig2 <- (gender_wins / age_wins) +
  plot_layout(guides = "collect", heights = c(1, 1.33)) &
  theme(legend.position = "top")