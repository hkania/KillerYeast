library(tidyverse)
library(ggsci)
library(readxl)
library(networkD3)

setwd("/Users/msiddiq/Dropbox (University of Michigan)/PostDoc/Wittkopp Lab/Manuscripts/KillerYeast/KillerYeast_data/KillerYeast/PrelimPlots/")

killeryeast_data <- read_csv('../Data/WGJ_DFx_MS.csv') 
str(killeryeast_data)
##Replicate 2, day 8, 3324A, well D2 observation was not recorded; removed this one row from Hannah's manually
## Can do with code 

# killeryeast_data %>%
#   group_by(Culture, Well_Num, Day, Replicate) %>%
#   mutate(normalized_aliveratio = AliveRatio_Hgui_Scer/)

df_subset <- subset(killeryeast_data, Culture %in% c("3824", "3821","3822", "3324", "3852", "3973", "3911", "3872", "3324m"))
miniset <-  subset(killeryeast_data, Culture %in% c("b", "3850", "3324a", "3324m"))



print(df_subset, n = 40)

normalizing_value_ratio <- df_subset %>% 
  filter(Day == 5) %>% 
  pull(AliveRatio_Hgui_Scer)

normalizing_value_HguiAlive <- df_subset %>% 
  filter(Day == 5) %>% 
  pull(Hgui_Alive_Total_Alive)

# gg <- df_subset %>% 
#   group_by(Culture, Well_Num, Day, Replicate) %>%
#   mutate(AliveRatio_Normalized = AliveRatio_Hgui_Scer/AliveRatio_Hgui_Scer[Day == 8])

# Prelim plots

ggplot(df_subset, aes(x = Day, y = as.numeric((Hgui_Alive_Num + Hgui_Dead_Num)/Total_Cells), col = Culture)) +
  geom_jitter(alpha = 0.2, width =0.1) +
  stat_summary(fun.data = "mean_se", linewidth = 0.5, size = 0.5, position=position_dodge(width = 0.2))

ggplot(df_subset, aes(x = Day, y = as.numeric(Scer_Alive_Num )/Total_Cells, col = Culture)) +
  geom_jitter(alpha = 0.2, width =0.1) +
  stat_summary(fun.data = "mean_se", linewidth = 0.5, size = 0.5, position=position_dodge(width = 0.1))

ggplot(df_subset, aes(x = Day, y = as.numeric(Scer_Dead_Num )/Total_Cells, col = Culture)) +
  geom_jitter(alpha = 0.2, width =0.1) +
  stat_summary(fun.data = "mean_se", linewidth = 0.5, size = 0.5, position=position_dodge(width = 0.1))

ggplot(killeryeast_data, aes(x = Day, y = as.numeric(Hgui_Alive_Num )/Total_Cells, col = Culture)) +
  geom_jitter(alpha = 0.2, width =0.1) +
  stat_summary(fun.data = "mean_se", linewidth = 0.5, size = 0.5, position=position_dodge(width = 0.2))


ggplot(killeryeast_data, aes(x = Day, y = as.numeric((Hgui_Alive_Num + Hgui_Dead_Num)/Total_Cells), col = Culture)) +
  geom_jitter(alpha = 0.2, width =0.1) +# ylim(0, 0.5) +
  stat_summary(fun.data = "mean_se", linewidth = 0.5, size = 0.5, position=position_dodge(width = 0.2))


ggplot(miniset, aes(x = Day, y = as.numeric((Hgui_Alive_Num + Hgui_Dead_Num)/Total_Cells), col = Culture)) +
  geom_jitter(alpha = 0.5, width =0.1) + ylab("Hgui alive + Hgui dead") +
  stat_summary(fun.data = "mean_se", linewidth = 0.5, size = 0.5, position = position_dodge(width = 0.2)) 

ggplot(miniset, aes(x = Day, y = as.numeric((Hgui_Alive_Num)/Total_Cells), col = Culture)) +
  geom_jitter(alpha = 0.5, width =0.1) + ylab("Hgui alive") +
  stat_summary(fun.data = "mean_se", linewidth = 0.5, size = 0.5, position = position_dodge(width = 0.2)) 

killeryeast_data %>%
  filter(Day == 8 & Culture == "b")

dim(df_subset$Hgui_Alive_Num)

