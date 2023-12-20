# libraries and data
library(tidyverse)
library(scales)
library(ggridges)
library(patchwork)
library(MASS)
library(brglm2)
library(brant)
library(VGAM)
library(car)
library(ggtext)
library(showtext)
library(kableExtra)
library(generalhoslem)
library(effects)
library(gofcat)
library(gtsummary)
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

bb$season_code <- as.numeric(substr(bb$season_code, 5, length(bb$season_code)))

bb <- bb |> mutate(initiative = if_else(season_code > 22, 1, 0))

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

bb_bipoc_wins <- bb |>
  group_by(BIPOC, wins_bucket) |>
  summarize(n = n()) |>
  mutate(freq = n / sum(n)) |>
  mutate(per = label_percent(accuracy = 1)(freq)) |>
  ungroup()

# stacked bar chart components
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
        text = element_text(family = "Times New Roman", size = 14),
        legend.text = element_text(size = 9))

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
        text = element_text(family = "Times New Roman", size = 14),
        legend.text = element_text(size = 9))

bipoc_wins <-
  ggplot(bb_bipoc_wins, aes(y = BIPOC, x = freq, label = per, fill = wins_bucket)) +
  geom_col(position = "fill") +
  geom_text(size = 3, fontface = "bold", family = "Times New Roman",
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = col4) +
  labs(y = "BIPOC", fill = "") +
  theme_void() +
  theme(axis.title.y = element_text(face = "bold"),
        axis.text.y = element_text(),
        text = element_text(family = "Times New Roman", size = 14),
        legend.text = element_text(size = 9))

# patchwork
fig1 <- (bipoc_place / lgbt_place / gender_place / age_place) +
  plot_layout(guides = "collect", heights = c(1, 1, 1, 1.33)) &
  theme(legend.position = "top")

fig3 <- (gender_wins / age_wins) +
  plot_layout(guides = "collect", heights = c(1, 1.33)) &
  theme(legend.position = "top")

# line chart -- bipoc place over time
bipoc_place_time <- bb |>
  filter(BIPOC == "Y") |>
  dplyr::select(first, season_code, placement_bucket) |>
  arrange(season_code) |>
  mutate(n = 1, cumul = cumsum(n),
         lategame = if_else(placement_bucket %in% c("Late Game", "Winner"), "Y", "N")) |>
  group_by(season_code) |>
  mutate(cumul = max(cumul), cnt = sum(lategame == "Y")) |>
  slice(1) |>
  ungroup() |>
  mutate(cumul_lategame = cumsum(cnt), cumul_perc = cumul_lategame / cumul)

bipoc_place_time_annot <-
  "CBS Introduces 50%
BIPOC Initiative"

fig2 <- ggplot(bipoc_place_time, aes(x = season_code, y = cumul_lategame)) +
  geom_line(color = col5[5], linewidth = 1.2) +
  annotate("text", x = 16, y = 20, label = bipoc_place_time_annot,
           family = "Times New Roman", fontface = "bold") + 
  geom_vline(xintercept = 22.5, linetype = "dashed", linewidth = 1.5) +
  labs(y = "Cum. # of BIPOC in Late-Game",
       x = "Season") +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman", size = 10),
        panel.grid = element_blank())


# line/scatterplot -- comp wins by women over time
gen_wins_time <- bb |>
  dplyr::select(first, season_code, gender, total_wins) |>
  group_by(season_code) |>
  mutate(numcomps = sum(total_wins)) |>
  group_by(season_code, gender) |>
  mutate(numwins_gen = sum(total_wins)) |>
  filter(gender == "F") |>
  slice(1) |>
  ungroup() |>
  mutate(perc_women_wins = numwins_gen / numcomps * 100)

fig4 <- ggplot(gen_wins_time, aes(x = season_code, y = perc_women_wins)) +
  geom_hline(yintercept = 50, linewidth = 1.2) +
  annotate("text", x = 23, y = 52, label = "Equitable Scenario",
           family = "Times New Roman", fontface = "bold") + 
  geom_line(color = col5[4], linewidth = 0.7, linetype = "dashed") +
  geom_point(color = col5[5], size = 3) +
  labs(y = "% of Comps Won by Women",
       x = "Season") +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman", size = 14),
        panel.grid = element_blank())


# placement model
bb_for_tables <- bb |>
  dplyr::rename(Gender = gender, Age = age, Initiative = initiative) |>
  mutate(Gender = if_else(Gender == "M", 1, 0),
         BIPOC = if_else(BIPOC == "Y", 1, 0),
         LGBT = if_else(LGBT == "Y", 1, 0))
placement_model <- polr(placement_bucket ~ BIPOC + LGBT + Gender + Age +
                        Initiative + Initiative * BIPOC,
                        data = bb_for_tables)
placement_coeffs <- summary(placement_model)$coefficients
placement_ints <- as.data.frame(placement_coeffs) |> slice(7:10)
rownames(placement_ints) <- c("> Early Out", "> Lower Middle", "> Upper Middle", "> Late Game")
placement_pval <- (1 - pnorm(abs(placement_coeffs[ ,"t value"]), 0, 1))*2
placement_or <- exp(placement_coeffs[ ,"Value"])
placement_ci <- exp(cbind(OR = coef(placement_model), confint(placement_model, level = 0.9)))
placement_table <- as.data.frame(placement_coeffs |> head(6) |> cbind(placement_ci)) |>
  mutate(`OR (90% CI)` = paste0(round(OR, 2), " (", round(`5 %`, 2), ", ", round(`95 %`, 2), ")")) |>
  select(Value, `Std. Error`, `OR (90% CI)`)

placement_brant <- brant::brant(placement_model)
placement_vif <- as.data.frame(vif(placement_model, type = "predictor"))
placement_lipsitz <- lipsitz(placement_model)
placement_lipsitz_tbl <- cbind(placement_lipsitz$LRT, placement_lipsitz$df, placement_lipsitz$p.value)
plot(allEffects(placement_model))

# placement model effects
placement_effects <- bb_for_tables
placement_effects$BIPOC <- as.factor(placement_effects$BIPOC)
placement_effects$LGBT <- as.factor(placement_effects$LGBT)
placement_effects$Gender <- as.factor(placement_effects$Gender)
placement_effects$Initiative <- as.factor(placement_effects$Initiative)
mod <- polr(placement_bucket ~ BIPOC + LGBT + Gender + Age +
              Initiative + Initiative * BIPOC,
            data = placement_effects)
placement_pulkrob <- pulkroben(mod)
placement_pulkrob_tbl <- cbind(placement_pulkrob$stat, placement_pulkrob$df, placement_pulkrob$p.value)
rownames(placement_pulkrob_tbl) <- NULL
placement_hosmerlem <- hosmerlem(mod)
placement_hosmerlem_tbl <- cbind(placement_hosmerlem$chi.sq, placement_hosmerlem$df, placement_hosmerlem$p.value)
placement_probs <- allEffects(mod)[4]
placement_probs_df <- as.data.frame(placement_probs[[1]])[, 1:7] |>
  dplyr::rename(`P(Early Out)` = prob.Early.Out,
                `P(Lower Middle)` = prob.Lower.Middle,
                `P(Upper Middle)` = prob.Upper.Middle,
                `P(Late Game)` = prob.Late.Game,
                `P(Winner)` = prob.Winner) |>
  arrange(BIPOC)

placement_probs_pivot <-
  pivot_longer(placement_probs_df, cols = starts_with("P("), names_to = "Placement", values_to = "Value") |>
  pivot_wider(names_from = c("BIPOC", "Initiative"), values_from = "Value")

# competition model
comp_wipmodel <- polr(wins_bucket ~ Gender + Age, data = bb_for_tables)
comp_wipbrant <- brant::brant(comp_wipmodel)

comp_model <- vglm(wins_bucket ~ Age + Gender, family = cumulative(parallel = FALSE ~ Gender, reverse = FALSE),
                   data = bb_for_tables)
comp_vif <- as.data.frame(vif(comp_wipmodel, type = "predictor"))
comp_stats <- as.data.frame(summary(comp_model)@coef3) |>
  mutate(`Odds Ratio` = exp(Estimate)) |>
  dplyr::select(-(`z value`)) |>
  dplyr::rename(p = `Pr(>|z|)`)
rownames(comp_stats)[1:3] <- c("Bottom Quartile|Low Quartile", "Low Quartile|High Quartile", "High Quartile|Top Quartile")
comp_lipsitz <- lipsitz(comp_model)
comp_lipsitz_tbl <- cbind(comp_lipsitz$LRT, comp_lipsitz$df, comp_lipsitz$p.value)
comp_pulkrob_mod <- vglm(wins_bucket ~ Age + Gender, family = cumulative(parallel = FALSE ~ Gender, reverse = FALSE),
                         data = placement_effects)
comp_pulkrob <- pulkroben(comp_pulkrob_mod)
comp_hosmerlem <- hosmerlem(comp_pulkrob_mod)
comp_pulkrob_tbl <- cbind(comp_pulkrob$stat, comp_pulkrob$df, comp_pulkrob$p.value)
comp_hosmerlem_tbl <- cbind(comp_hosmerlem$chi.sq, comp_hosmerlem$df, comp_hosmerlem$p.value)

comp_ints <- as.data.frame(comp_stats) |> slice(1:3) |> select(Estimate, `Std. Error`)
rownames(comp_ints) <- c("< Low Quartile", "< High Quartile", "< Top Quartile")
comp_ci <- as.data.frame(exp(confint(comp_model, level = 0.9))) |> slice(4:7)
comp_table <- as.data.frame(comp_stats |> slice(4:7) |> cbind(comp_ci)) |>
  mutate(`OR (90% CI)` = paste0(round(`Odds Ratio`, 2), " (", round(`5 %`, 2), ", ", round(`95 %`, 2), ")")) |>
  select(Estimate, `Std. Error`, `OR (90% CI)`)
rownames(comp_table) <- c("Age", "Gender, < Low", "Gender, < High", "Gender, < Top")