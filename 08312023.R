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
  labs(x = "Days Since Grape Juice Inoculation", y = "Percent of Culture (H. gui)") + ggtitle("DeletionsRep1") +
  geom_line(data = deletion1a, aes(x=Day, y=Norm_Hgui_Change, color=Culture, group=Well_Num)) + ylim(0,1)
del1norm
