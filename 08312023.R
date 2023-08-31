library("explore")
library(tidyverse)
library(dplyr)
library(gridExtra)
library(writexl)
library("stringr")    

WGJ_DFx <- read_excel("~/Desktop/KillerYeast/WGJ_DFx.xlsx", 
                      +     col_types = c("numeric", "numeric", "text", 
                                          +         "text", "numeric", "numeric", "numeric", 
                                          +         "numeric", "numeric", "numeric", 
                                          +         "numeric", "numeric", "numeric", 
                                          +         "numeric", "numeric", "numeric", 
                                          +         "numeric", "numeric", "numeric", 
                                          +         "numeric"))

##pull out interesting columns as of August 31, 2023 and make (1) a new column with the day 5 
##values to normalize data to and (2) a new column with the normalized values = change in Hgui
##alive in trial dataset replicate 1
deletion1a <- deletions1 %>%
  select (Day, Culture, Well_Num, Per_Hgui_Alive_Total_Alive) %>%
  group_by(Well_Num) %>%
  mutate(Per_Hgui_Alive_D5 = Per_Hgui_Alive_Total_Alive[Day == 5]) %>%
  ungroup() %>%
  mutate(Norm_Hgui_Change = (Per_Hgui_Alive_Total_Alive/Per_Hgui_Alive_D5))

##test new normalized data on deletion strains from replicate 1
del1norm <- ggplot() +
  geom_point(data = deletion1a, aes(x=Day, y=Norm_Hgui_Change, color=Culture, group=Well_Num)) +
  labs(x = "Days Since Grape Juice Inoculation", y = "Percent Change in Culture (H. gui)") + ggtitle("DeletionsRep1") +
  geom_line(data = deletion1a, aes(x=Day, y=Norm_Hgui_Change, color=Culture, group=Well_Num)) + ylim(0,1)
del1norm

##other deletion datasets 
deletion2a <- deletions2 %>%
  select (Day, Culture, Well_Num, Per_Hgui_Alive_Total_Alive) %>%
  group_by(Well_Num) %>%
  mutate(Per_Hgui_Alive_D5 = Per_Hgui_Alive_Total_Alive[Day == 5]) %>%
  ungroup() %>%
  mutate(Norm_Hgui_Change = (Per_Hgui_Alive_Total_Alive/Per_Hgui_Alive_D5))

deletion3a <- deletions3 %>%
  select (Day, Culture, Well_Num, Per_Hgui_Alive_Total_Alive) %>%
  group_by(Well_Num) %>%
  mutate(Per_Hgui_Alive_D5 = Per_Hgui_Alive_Total_Alive[Day == 5]) %>%
  ungroup() %>%
  mutate(Norm_Hgui_Change = (Per_Hgui_Alive_Total_Alive/Per_Hgui_Alive_D5))

del2norm <- ggplot() +
  geom_point(data = deletion2a, aes(x=Day, y=Norm_Hgui_Change, color=Culture, group=Well_Num)) +
  labs(x = "Days Since Grape Juice Inoculation", y = "Percent Change in Culture (H. gui)") + ggtitle("DeletionsRep1") +
  geom_line(data = deletion2a, aes(x=Day, y=Norm_Hgui_Change, color=Culture, group=Well_Num)) + ylim(0,1.3)
del2norm

del3norm <- ggplot() +
  geom_point(data = deletion3a, aes(x=Day, y=Norm_Hgui_Change, color=Culture, group=Well_Num)) +
  labs(x = "Days Since Grape Juice Inoculation", y = "Percent Change in Culture (H. gui)") + ggtitle("DeletionsRep1") +
  geom_line(data = deletion3a, aes(x=Day, y=Norm_Hgui_Change, color=Culture, group=Well_Num)) + ylim(0,2.3)
del3norm

## moving forward: -->  need to complete the mean from all 6 data points across each day, need to plot these means, need to try faceting by day
## need to watch some linear modeling videos (idea is calculate means with genotype as the main factor and replicate as a cofactor or random effect)
## ? for Mo is when calculating means, am I using the normalized values based off of the day 5 measurements of each individual well or start from beginning?