set.seed(100)

library(tidyverse)
library(emmeans)
library(sandwich)
library(lmtest)
library(naniar)
library(Hmisc)
library(informationeffects)
library(multcomp)

df <- read_csv("data/anes_pilot_2022_csv_20221214.csv")

df <- df %>% 
  dplyr::select(birthyr_dropdown, gender, educ, faminc_new, # demographics
         pid1d, pid1r, # party
         turnout22, pipevote22a, house22t, house22p, senate22t, senate22p, # turnout and voting 2022
         turnout20, pipevote20, vote20, # turnout and voting 2020
         vote24dt, # turnout 2024
         impstem_voter_fraud, miselwin, miselstolen, # beliefs about election integrity
         weight) %>% 
  mutate(age = 2022 - birthyr_dropdown,
         gender = case_when(gender == 1 ~ "male",
                            gender == 2 ~ "female"),
         educ = case_when(educ == 1 ~ "no_hs",
                          educ == 2 ~ "hs",
                          educ == 3 ~ "some_coll",
                          educ == 4 ~ "two_year_coll",
                          educ == 5 ~ "four_year_coll",
                          educ == 6 ~ "post_grad"),
         # https://dqydj.com/household-income-percentile-calculator/
         faminc_new = case_when(faminc_new == 1 ~ "Q1",
                                faminc_new == 2 ~ "Q1",
                                faminc_new == 3 ~ "Q1",
                                faminc_new == 4 ~ "Q1",
                                faminc_new == 5 ~ "Q2",
                                faminc_new == 6 ~ "Q2",
                                faminc_new == 7 ~ "Q2",
                                faminc_new == 8 ~ "Q3",
                                faminc_new == 9 ~ "Q3",
                                faminc_new == 10 ~ "Q3",
                                faminc_new == 11 ~ "Q3",
                                faminc_new == 12 ~ "Q4"),
         party = case_when(pid1d == 1 | pid1r == 1 ~ "democrat",
                           pid1d == 2 | pid1r == 2 ~ "republican",
                           pid1d == 3 | pid1r == 3 ~ "independent",
                           pid1d == 4 | pid1r == 4 ~ "other"),
         turnout_22 = case_when(turnout22 == 1 | turnout22 == 2 | turnout22 == 3 | pipevote22a == 2 ~ 1,
                                turnout22 == 4 | pipevote22a == 1 ~ 0,
                                TRUE ~ NA_real_),
         turnout_20 = case_when(turnout20 == 1 | pipevote20 == 2 ~ 1,
                                turnout20 == 2 | pipevote20 == 1 ~ 0,
                                TRUE ~ NA_real_),
         house_22_vote = case_when(house22p == 1 ~ "democrat",
                                   house22p == 2 ~ "republican",
                                   house22p == 3 ~ "other",
                                   house22t == 2 ~ "no_vote",
                                   TRUE ~ NA_character_),
         senate_22_vote = case_when(senate22p == 1 ~ "democrat",
                                    senate22p == 2 ~ "republican",
                                    senate22p == 3 ~ "other",
                                    senate22t == 2 ~ "no_vote",
                                   TRUE ~ NA_character_),
         miselwin = case_when(miselwin == 1 ~ 0,
                              miselwin == 2 ~ 1,
                              miselwin == -7 ~ NA_real_),
         miselstolen = case_when(miselstolen == 2 ~ 0,
                                 miselstolen == -7 ~ NA_real_,
                                 TRUE ~ miselstolen),
         vote20 = case_when(vote20 == -7 ~ NA_character_,
                            vote20 == -1 ~ "no_vote",
                            vote20 == 1 ~ "trump",
                            vote20 == 2 ~ "biden",
                            vote20 == 3 ~ "other"),
         fraud_binary = case_when(impstem_voter_fraud < 3 ~ 1, # very to extremely important issue
                                  TRUE ~ 0),
         vote24dt = case_when(vote24dt == -7 ~ NA_character_,
                              vote24dt == 1 ~ "trump",
                              vote24dt == 2 ~ "biden",
                              vote24dt == 3 ~ "other",
                              vote24dt == 4 ~ "wont_vote")) %>% 
  dplyr::select(age, gender, educ, faminc_new, # demographics
         party, # party
         turnout_22, house_22_vote, senate_22_vote, # turnout and voting 2022
         turnout_20, vote20, # turnout and voting 2020
         vote24dt, # turnout 2024
         impstem_voter_fraud, fraud_binary, miselwin, miselstolen, # beliefs about election integrity
         weight)

# remove observations with missing weights
df <- df %>% 
  drop_na(weight)

# impute missing
vis_miss(df)
impute_arg <- aregImpute(~ age +
                           gender +
                           educ +
                           faminc_new +
                           party +
                           turnout_22 +
                           house_22_vote +
                           senate_22_vote +
                           turnout_20 +
                           vote20 +
                           vote24dt +
                           impstem_voter_fraud +
                           fraud_binary +
                           miselwin +
                           miselstolen,
                         data = df, n.impute = 10, nk = 0, tlinear = FALSE)
impute_arg
imp_data <- as.data.frame(impute.transcan(impute_arg, imputation=1, data=df, list.out=TRUE, pr=FALSE, check=FALSE)) 
head(imp_data, 10)
df <- cbind(imp_data, df$weight) %>% 
  rename(weight = `df$weight`)

# visualisations
df %>% 
  group_by(turnout_22) %>% 
  drop_na(weight, turnout_22) %>% 
  summarise(fraud = weighted.mean(impstem_voter_fraud, weight, na.rm = T), # 1 = extremely important; 5 = not at all
            stolen = weighted.mean(miselstolen, weight, na.rm = T), # Trump's statement is true
            trump_won = weighted.mean(miselwin, weight, na.rm = T)) # Trump won
# fraud slightly less important for voters 
# voters slightly more likely to think stolen
# voters less likely to think trump won

# by party
png(file="plots/issue_by_party.png", width = 7, height = 6, units = 'in', res = 300)
df %>% 
  group_by(party) %>% 
  summarise("Election fraud an important issue" = weighted.mean(fraud_binary, weight, na.rm = T), # Important issue
            "The election was stolen" = weighted.mean(miselstolen, weight, na.rm = T), # Trump's statement is true
            "Trump won in 2020" = weighted.mean(miselwin, weight, na.rm = T)) %>% # Trump won
  filter(party == "republican" | party == "democrat") %>% 
  mutate(party = str_to_title(party)) %>% 
  pivot_longer(cols = -party,
               names_to = "variable",
               values_to = "value")  %>% 
  ggplot() +
  aes(x = party, y = value, group = 1, fill = party) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("#2A64C5", "#D84A41")) +
  geom_text(aes(x=party, y=value+0.1, label=paste(round(value*100,0),"%", sep = ""), colour = NULL), show.legend = FALSE) +
  ylim(c(0,1)) +
  theme(legend.position="none") +
  labs(title = "Proportion of agreement by partisan affiliation",
       subtitle = "Proportions weighted to approximate representativenss",
       caption = "Data: ANES 2022 Pilot Study",
       x = "",
       y = "Proportion agreeing") +
  theme(plot.title = element_text(face="bold")) +
  facet_wrap(~variable)
dev.off()

# by turnout
png(file="plots/turnout_by_party.png", width = 8, height = 6, units = 'in', res = 300)
df %>% 
  group_by(turnout_22, party) %>% 
  filter(party == "republican" | party == "democrat") %>% 
  mutate(turnout_22 = factor(turnout_22),
         party = str_to_title(party),
         turnout_22 = case_when(turnout_22 == 1 ~ "Yes",
                                turnout_22 == 0 ~ "No")) %>%
  rename(Party = party) %>% 
  summarise("Election fraud an important issue" = weighted.mean(fraud_binary, weight, na.rm = T), # Important issue
            "The election was stolen" = weighted.mean(miselstolen, weight, na.rm = T), # Trump's statement is true
            "Trump won in 2020" = weighted.mean(miselwin, weight, na.rm = T)) %>% # Trump won
  pivot_longer(cols = - c(turnout_22, Party),
               names_to = "variable",
               values_to = "value")  %>% 
  ggplot() +
  aes(x = turnout_22, y = value, color = Party, group = Party) +
  geom_text(aes(x=turnout_22, y=value+0.1, label=paste(round(value*100,0),"%", sep = ""), colour = NULL), show.legend = FALSE) +
  geom_point() +
  geom_line() +
  ylim(c(0,1)) +
  labs(title = "Proportion of agreement by party and turnout",
       subtitle = "Proportions weighted to approximate representativenss",
       caption = "Data: ANES 2022 Pilot Study",
       x = "Voted in 2022 midterm",
       y = "Proportion agreeing") +
  theme(plot.title = element_text(face="bold")) +
  facet_wrap(~variable) +
  scale_color_manual(values=c("#2A64C5", "#D84A41"))
dev.off()

# modelled effect
m_stolen_int <- glm(turnout_22 ~
                      miselstolen +
                      age +
                      gender +
                      educ +
                      party +
                      party:miselstolen +
                      faminc_new,
                    family = binomial(link = "logit"),
                    data = df)
summary(m_stolen_int)

m_fraud_int <- glm(turnout_22 ~
                     fraud_binary +
                     age +
                     gender +
                     educ +
                     party +
                     party:fraud_binary +
                     faminc_new,
                   family = binomial(link = "logit"),
                   data = df)
summary(m_fraud_int)

m_trump_won_int <- glm(turnout_22 ~
                         miselwin +
                         age +
                         gender +
                         educ +
                         party +
                         party:miselwin +
                         faminc_new,
                       family = binomial(link = "logit"),
                       data = df)
summary(m_trump_won_int)

m_stolen_int_vcov = vcovHC(m_stolen_int)
m_fraud_int_vcov = vcovHC(m_fraud_int)
m_trump_won_int_vcov = vcovHC(m_trump_won_int)

df_plot_int <- tibble(
  var = rep(c("trump_won","stolen","fraud"),2),
  party = c(rep("democrat",3),rep("republican",3)),
  est = c(confint(glht(m_trump_won_int, linfct = c("miselwin = 0"), vcov = m_trump_won_int_vcov))$confint[1],
          confint(glht(m_stolen_int, linfct = c("miselstolen = 0"), vcov = m_stolen_int_vcov))$confint[1],
          confint(glht(m_fraud_int, linfct = c("fraud_binary = 0"), vcov = m_fraud_int_vcov))$confint[1],
          confint(glht(m_trump_won_int, linfct = c("miselwin + miselwin:partyrepublican = 0"), vcov = m_trump_won_int_vcov))$confint[1],
          confint(glht(m_stolen_int, linfct = c("miselstolen + miselstolen:partyrepublican = 0"), vcov = m_stolen_int_vcov))$confint[1],
          confint(glht(m_fraud_int, linfct = c("fraud_binary + fraud_binary:partyrepublican = 0"), vcov = m_fraud_int_vcov))$confint[1]),
  lwr = c(confint(glht(m_trump_won_int, linfct = c("miselwin = 0"), vcov = m_trump_won_int_vcov))$confint[2],
          confint(glht(m_stolen_int, linfct = c("miselstolen = 0"), vcov = m_stolen_int_vcov))$confint[2],
          confint(glht(m_fraud_int, linfct = c("fraud_binary = 0"), vcov = m_fraud_int_vcov))$confint[2],
          confint(glht(m_trump_won_int, linfct = c("miselwin + miselwin:partyrepublican = 0"), vcov = m_trump_won_int_vcov))$confint[2],
          confint(glht(m_stolen_int, linfct = c("miselstolen + miselstolen:partyrepublican = 0"), vcov = m_stolen_int_vcov))$confint[2],
          confint(glht(m_fraud_int, linfct = c("fraud_binary + fraud_binary:partyrepublican = 0"), vcov = m_fraud_int_vcov))$confint[2]),
  upr = c(confint(glht(m_trump_won_int, linfct = c("miselwin = 0"), vcov = m_trump_won_int_vcov))$confint[3],
          confint(glht(m_stolen_int, linfct = c("miselstolen = 0"), vcov = m_stolen_int_vcov))$confint[3],
          confint(glht(m_fraud_int, linfct = c("fraud_binary = 0"), vcov = m_fraud_int_vcov))$confint[3],
          confint(glht(m_trump_won_int, linfct = c("miselwin + miselwin:partyrepublican = 0"), vcov = m_trump_won_int_vcov))$confint[3],
          confint(glht(m_stolen_int, linfct = c("miselstolen + miselstolen:partyrepublican = 0"), vcov = m_stolen_int_vcov))$confint[3],
          confint(glht(m_fraud_int, linfct = c("fraud_binary + fraud_binary:partyrepublican = 0"), vcov = m_fraud_int_vcov))$confint[3])
)

png(file="plots/effect_turnout.png", width = 9, height = 6, units = 'in', res = 300)
df_plot_int %>% 
  mutate(var = case_when(var == "trump_won" ~ "Trump won in 2020",
                         var == "stolen" ~ "The 2020 election was stolen",
                         var == "fraud" ~ "Electoral fraud an important issue"),
         party = str_to_title(party)) %>%
  rename(Party = party) %>% 
  ggplot() +
  aes(x = var, y = est, color = Party) +
  geom_pointrange(aes(ymin = lwr, ymax = upr), position = position_dodge(0.3)) +
  geom_hline(yintercept=0, linetype="11", color = "black") +
  labs(title = "Effect of agreeing with corresponding claim on 2022 turnout",
       subtitle = "95% confidence intervals using robust standard errors",
       caption = "Data: ANES 2022 Pilot Study",
       x = "",
       y = "Effect (logged odds)") +
  theme(plot.title = element_text(face="bold")) +
  scale_color_manual(values=c("#2A64C5", "#D84A41")) +
  coord_flip()
dev.off()

# simulate effect on turnout
# calculate propensity scores
df$prop_score <- info_prop_scores(knowledge_var = "fraud_binary", 
                                  covariates = c("age","gender","educ","faminc_new"), 
                                  data = df)

df %>% 
  ggplot() +
  aes(x = prop_score) +
  geom_histogram(binwidth=0.1, color = "black", fill = "salmon")

# check balance
info_bal_plots(knowledge_var = "fraud_binary", 
               covariates = c("age","gender","educ","faminc_new"), 
               prop_score ="prop_score", 
               data = df)

# calculate effect on turnout
m_fraud_causal <- glm(turnout_22 ~
                     fraud_binary +
                     age +
                     gender +
                     educ +
                     party +
                     party:fraud_binary +
                     faminc_new,
                   family = binomial(link = "logit"),
                   data = df,
                   weights = prop_score)
summary(m_fraud_causal)

df_fraud <- df %>% 
  mutate(fraud_binary = 1)

df_fraud$fraud_turnout <- predict(m_fraud_causal, newdata = df_fraud, type = "response")

png(file="plots/simulated_turnout.png", width = 7, height = 6, units = 'in', res = 300)
df_fraud %>% 
  filter(party != "other") %>% 
  group_by(party) %>% 
  rename(Party = party) %>% 
  mutate(Party = str_to_title(Party)) %>% 
  summarise("Actual turnout" = weighted.mean(turnout_22, weight),
            "Simulated turnout" = weighted.mean(fraud_turnout, weight),
            "Difference" = `Simulated turnout` - `Actual turnout`) %>% 
  pivot_longer(cols = -Party,
               names_to = "type",
               values_to = "value") %>% 
  mutate(value = round(value * 100,2)) %>%
  filter(type == "Difference") %>% 
  ggplot() +
  aes(x = Party, y = value, color = Party) +
  geom_segment(aes(x=Party, xend=Party, y=0, yend=value), color="black") +
  geom_point(size=5) +
  geom_text(aes(x=Party, y=ifelse(Party=="Democrat", -2.2, value + 0.3), label=value, colour = NULL), show.legend = FALSE) +
  geom_hline(yintercept=0, color = "grey") +
  theme(legend.position="none") +
  labs(title = "Simulated difference in turnout if everyone agreed that\nelectoral fraud is an important issue",
       subtitle = "Differences weighted to approximate representativeness",
       caption = "Data: ANES 2022 Pilot Study",
       x = "",
       y = "Difference in turnout (percentage points)") +
  theme(plot.title = element_text(face="bold")) +
  scale_color_manual(values=c("#2A64C5", "#999999", "#D84A41"))
dev.off()
