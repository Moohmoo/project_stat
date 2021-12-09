library(tidyverse)
library(magrittr)
library(neuralnet)
library(caret)
# MAPK neural_net
# Général, toutes les données
df_act <- read_csv("./dude_erk2_mk01_Descriptors.csv")
form_erk <- colnames(df_act)[4:9] %>%
  paste(., collapse = " + ") %>%
  paste("is_active ~", .) %>%
  as.formula()
df_act %<>% rowid_to_column()
df_act %<>% mutate(is_active = factor(is_active))
df_act_train <- df_act %>% 
  group_by(is_active) %>%
  slice_sample(prop = .7)
df_act_test <- anti_join(df_act, df_act_train, by = "rowid")

res_erk <- neuralnet(formula = form_erk,
                     data = df_act,
                     hidden = c(3, 2))

# Input équilibré
df_act_equi <- df_act %>%
  group_by(is_active) %>%
  slice_head(n = 79)

df_act_equi_train <- df_act_equi %>%
  slice_sample(prop = .7) 
df_act_equi_test <- anti_join(df_act_equi, df_act_equi_train, by = "rowid")

res_equi <- neuralnet(formula = form_erk,
                      data = df_act_equi_train,
                      hidden = c(3, 2))

confusionMatrix(ifelse(predict(res_equi, newdata = df_act_equi_test) > 0.2, 1, 0) %>% as.vector(),
                df_act_equi_test$is_active)
