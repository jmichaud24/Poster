library(tidyverse)
library(haven)
library(sjlabelled)
library(ggplot2)
library(stargazer)
library(patchwork)
library(vtable)
library(posterdown)


CCSD <- X2022_Chicago_Council_Survey_Data_Set
rm(X2022_Chicago_Council_Survey_Data_Set)

#data transformation

CCSD1 <- 
  CCSD |>
  select(Q1010, 
         Q45_2,
         Q1015,
         Q1020,
         Q1025,
         Q1005,
         DOV_REL1,
         Q200,
         Q2020VOTE) |>
  mutate(Germany = as.numeric(Q45_2),
         PartyID = as.factor(Q1010),
         Ideology = as.factor(Q1005),
         Christian = as.factor(DOV_REL1),
         NATO = as.factor(Q200),
         Trump = as.factor(Q2020VOTE)
  ) |>
  mutate(Christian = recode(Christian,
                            "1" = "1",
                            "2" = "1",
                            "3" = "0",
                            "4" = "0",
                            "5" = "0",
                            "6" = "0",
                            "7" = "0",
                            "8" = "0",
                            "9" = "0",
                            "10" = "0",
                            "11" = "0",
                            "12" = "0",
                            "13" = "0"),
         NATO = recode(NATO,
                       "1" = "0",
                       "2" = "0",
                       "3" = "1",
                       "4" = "1"),
         Trump = recode(Trump, 
                        "1" = "0",
                        "2" = "1",
                        "3" = "0",
                        "4" = "0",
                        "5" = "0")
  ) |>
  filter(Ideology %in% c(1:7),
         NATO %in% c(0:1),
         PartyID %in% c(1:4),
         Trump %in% c(0:1),
         Christian %in% c(0:1),
         Germany %in% c(0:100))

CCSD2 <-
  CCSD1 |>
  mutate(Ideology = as.numeric(Ideology)) 

CCSD3 <- 
  CCSD2 |>
  mutate(Ideology = recode(Ideology,
                           "2" = "1",
                           "3" = "2",
                           "4" = "3",
                           "5" = "4",
                           "6" = "5",
                           "7" = "6",
                           "8" = "7")
  ) |>
  mutate(Ideology = as.numeric(Ideology))

#regression model
model_final3 = lm(Germany ~ Ideology + PartyID + Trump + Christian + NATO, data = CCSD3)

summary(model_final3)

#regression table
modelsummary(model_final3, fmt = fmt_significant(2),
             gof_map = 'nobs', statistic = NULL,
             stars = c('***' = .001, '**' = .01, '*' = 0.05), title = NULL,
             coef_map = c(
               'Ideology' = 'Ideology',
               'PartyID2' = 'Democrat',
               'PartyID3' = 'Independent',
               'PartyID4' = 'Other Party',
               'Trump1' = 'Trump Voter 2020',
               'Christian0' = 'Not Christian',
               'NATO1' = 'Decrease/Withdraw US NATO Support',
               '(Intercept)' = '(Intercept)'
             ))

# visualizations 
## text set large for poster output

#ideology means
Means_ideology = CCSD3 |>
  group_by(Ideology) |>
  summarize(avg = mean(Germany, na.rm = T)
  ) |>
  na.omit()

#ideology means viz
P1 =
  Means_ideology |>
  ggplot(aes(x = Ideology,
             y = avg,
             color = Ideology)) +
  geom_hline(yintercept = 0) +
  geom_point(size = 3) +
  geom_segment(aes(
    x = Ideology, xend = Ideology,
    y = 45, yend = avg)) +
  theme_bw() + 
  scale_y_continuous(
    limits = c(45, 75),
    name = "Mean Feeling",
    expand = c(0,0)
  ) +
  scale_x_continuous(
    name = "",
    breaks = c(1:7),
    labels = c("Extremely\nLiberal",
               "Liberal",
               "Slightly\nLiberal",
               "Moderate",
               "Slightly\nConservative",
               "Conservative",
               "Extremely\nConservative"
    )) +
  theme(
    legend.position = "none",
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(
      size = 16,
      color = "black"),
    axis.text.y = element_text(
      size = 20,
      color = "black"),
    axis.title = element_text(
      size = 20,
      color = "black")
  ) +
  ggtitle("Ideology") +
  theme(plot.title = element_text(
    hjust = .5,
    size = 20))

#NATO means
Means_NATO = CCSD3 |>
  group_by(NATO) |>
  summarize(avg = mean(Germany, na.rm = T)
  ) |>
  na.omit()

#NATO means viz
P2 =
  Means_NATO |>
  ggplot(aes(x = NATO,
             y = avg,
             color = NATO)) +
  geom_point(size = 3) +
  geom_segment(aes(
    x = NATO, xend = NATO,
    y = 45, yend = avg
  )) +
  theme_bw() + 
  scale_y_continuous(
    limits = c(45, 75),
    name = "Mean Feeling",
    expand = c(0,0)
  ) +
  scale_x_discrete(
    name = "",
    labels = c("Maintain/increase\nUS NATO commitment",
               "Decrease/withdraw\nUS NATO commitment")
  ) +
  theme(
    legend.position = "none",
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(
      size = 20,
      color = "black"),
    axis.text.y = element_text(
      size = 20,
      color = "black"),
    axis.title = element_text(
      size = 20,
      color = "black")
  ) +
  ggtitle("NATO Support") +
  theme(plot.title = element_text(
    hjust = .5,
    size = 20))


#Party means
Means_Party = CCSD3 |>
  group_by(PartyID) |>
  summarize(avg = mean(Germany, na.rm = T)
  ) |>
  na.omit()

#Party ID viz
P3 =
  Means_Party |>
  ggplot(aes(x = PartyID,
             y = avg,
             color = PartyID)) +
  geom_point(size = 3) +
  geom_segment(aes(
    x = PartyID, xend = PartyID,
    y = 45, yend = avg
  )) +
  theme_bw() + 
  scale_y_continuous(
    limits = c(45, 75),
    name = "Mean Feeling",
    expand = c(0,0)
  ) +
  scale_x_discrete(
    name = "",
    labels = c("Republican",
               "Democrat",
               "Independent",
               "Other")
  ) +
  theme(
    legend.position = "none",
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(
      size = 20,
      color = "black"),
    axis.text.y = element_text(
      size = 20,
      color = "black"),
    axis.title = element_text(
      size = 20,
      color = "black")
  ) +
  ggtitle("Party Identification") +
  theme(plot.title = element_text(
    hjust = .5,
    size = 20))


