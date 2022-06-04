library(dplyr)

df_experts = read.csv("https://osf.io/mshfq/download") |>
  filter(Finished == 1) |>
  select(ratio, expert)


########################
# REPORTED IN THE POST #
########################

# Log-normal
log_ratio = log(df_experts$ratio[df_experts$ratio <= 10000])
exp(mean(log_ratio) + sd(log_ratio))


hist(log_ratio, breaks = 5, freq = FALSE, main = "log(responses) is normally distributed")
curve(dnorm(x, mean = mean(log_ratio), sd = sd(log_ratio)), add = TRUE, lwd = 2)


# Skeptical version
log_ratio_conservative = log(df_experts$ratio[df_experts$expert >= 4 & df_experts$ratio <= 1000])
exp(mean(log_ratio_conservative) + sd(log_ratio_conservative))



####################
# ALSO OF INTEREST #
####################

# Per expert level summaries on raw data
df_experts |>
  group_by(expert) %>%
  filter(ratio >= 5, ratio <= 10000) |>
  summarise(
    N = n(),
    median_ratio = median(ratio),
    mean_ratio = mean(ratio),
  )

# Lump top-experts
df_experts |>
  filter(ratio >= 4, ratio <= 10000) %>%
  group_by(is_top_expert = expert %in% c(4, 5)) %>%
  summarise(
    N_participants = n(),
    mean_ratio = mean(ratio),
    mean_all = mean(.$ratio)
  )



##############
# LAY PEOPLE #
##############

df_mturk = read.csv("https://osf.io/6tdmf/download", sep = ";") |>
  #select(ratio = ExplicitComparison) |>
  mutate(
    ratio = Tipping_point / 1000,
    sample = "mturk"
  ) |>
  select(ratio, sample)
  #mutate(sample = "mturk")

df_oxford = read.csv("https://osf.io/vmrus/download") |>
  filter(is.na(EG) == FALSE) |>
  mutate(
    ratio = EG / 1000,
    sample = "oxford students"
  ) |>
  select(ratio, sample)

df_both = bind_rows(df_mturk, df_oxford)


library(ggplot2)
df_both |>
  mutate(ratio_group = cut(ratio, c(0, 1, 2, 5, Inf))) |>
  group_by(ratio_group, sample) |>
  summarise(
    N = n()
  ) |>
  group_by(sample) |>
  mutate(
    N = cumsum(N) / sum(N),
    ratio_group = forcats::fct_inorder(factor(ratio_group)),
    ratio_group = paste0("<=", sub(".*,(.*)].*", "\\1", ratio_group))
  ) |>
  ggplot(aes(x = ratio_group, y = N)) +
  geom_col() +
  facet_wrap(~sample) +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, 0.2)) +
  labs(x = "Estimated multiplier", y = "Percentage of respondents", title = "Public perception of effectiveness") +
  theme_gray(15)




# Quantify
log_lay = log(df_both$ratio[df_both$ratio <= 5000 & df_both$ratio >= 1])  # remove misunderstandings (<1) and one extreme response
exp(mean(log_lay) + sd(log_lay))






################
# INFOGRAPHICS #
################

library(personograph)
personograph(
  data = list(average = 0.01, effective = 0.99),
  dimensions = c(22, 22),
  n.icons = 22*22,
  icon.style = 9,
  colors = list(average = "#5555FF", effective = "#55AA55")
)
