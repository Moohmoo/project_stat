library(tidyverse)
library(magrittr)
library(neuralnet)
library(caret)

# Load des données modifiées scalées, fichier à modif
df_act_scaled <- read_csv("./scaled_dude_erk2_mk01_Descriptors.csv")

# 1 : 50/50
# 2 : 75/25
# 3 : toutes les données
list_cases <- vector(mode = "list", length = 3)

# 3
list_cases[[3]] <- df_act_scaled

# 1
df_act_equi <- df_act_scaled %>%
  group_by(is_active) %>%
  slice_head(n = 79)

list_cases[[1]] <- df_act_equi

# 2
df_act_25 <- df_act_scaled %>%
  filter(is_active == 1)
df_act_75 <- df_act_scaled %>%
  filter(is_active == 0) %>%
  slice_sample(n = 3 * nrow(df_act_25))
df_act_25_75 <- rbind(df_act_25, df_act_75)

list_cases[[2]] <- df_act_25_75
  
