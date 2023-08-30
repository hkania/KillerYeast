library("explore")
library(tidyverse)
library(dplyr)
library(gridExtra)
library(writexl)
library("stringr")  

## dataframe is ready to be broken into groups! ##

# genotypes to compare w/wildtype#
deletions <- WGJ_DFx %>% filter(Culture %in% c("3821", "3822", "3824", "3324m"))
deletions1 <- filter(deletions, Replicate == "1")
deletions2 <- filter(deletions, Replicate == "2")
deletions3 <- filter(deletions, Replicate == "3")

amps <- WGJ_DFx %>% filter(Culture %in% c("3905", "3911", "3973", "3324m"))
amps1 <- filter(amps, Replicate == "1")
amps2 <- filter(amps, Replicate == "2")
amps3 <- filter(amps, Replicate == "3")

TDH1exp <- WGJ_DFx %>% filter(Culture %in% c("3842", "3324m"))
TDH1exp1 <- filter(TDH1exp, Replicate == "1")
TDH1exp2 <- filter(TDH1exp, Replicate == "2")
TDH1exp3 <- filter(TDH1exp, Replicate == "3")

fluoro <- WGJ_DFx %>% filter(Culture %in% c("3852", "3920", "3984", "3324m"))
TDH1exp1 <- filter(TDH1exp, Replicate == "1")

solo <- WGJ_DFx %>% filter(Culture %in% c("3850", "3324a", "3324m"))
solo1 <- filter(solo, Replicate == "1")
solo2 <- filter(solo, Replicate == "2")
solo3 <- filter(solo, Replicate == "3")

blank <- WGJ_DFx %>% filter(Culture %in% c("b"))

#singledeletionshguialive <- deletionshguialive %>% filter(ScerGenotype %in% c("∆TDH1", "∆TDH2", "∆TDH3", "WT"))
#doubledeletions <- deletionshguialive %>% filter(ScerGenotype %in% c("∆TDH1∆TDH2", "∆TDH2∆TDH3", "∆TDH1∆TDH3", "WT"))
#hguiTDHindeletions <- deletionshguialive %>% filter(ScerGenotype %in% c("∆TDH1∆TDH2pTDH3::HguiTDH", "∆TDH2pTDH3::HguiTDH", "pTDH3::HguiTDH", "WT"))

#basic line graphs for deletions#
#WGJp1 <- ggplot() +
 # geom_point(data = deletionshguialive, aes(x=Day, y=Percent, color=ScerGenotype, group=Sample)) +
  #labs(x = "Days Since Grape Juice Inoculation", y = "Percent of Culture (H. gui)") + ggtitle("Deletions") +
  #geom_line(data = deletionshguialive, aes(x=Day, y=Percent, color=ScerGenotype, group=Sample)) + ylim(0,100)
#WGJp1

WGJp1 <- ggplot() +
  geom_point(data = deletions1, aes(x=Day, y=Per_Hgui_Alive_Total_Alive, color=Culture)) +
  labs(x = "Days Since Grape Juice Inoculation", y = "Percent of Culture (H. gui)") + ggtitle("DeletionsRep1") +
  geom_line(data = deletions1, aes(x=Day, y=Per_Hgui_Alive_Total_Alive, color=Culture, group=Well_Num)) + ylim(0,100)
WGJp1
WGJp2 <- ggplot() +
  geom_point(data = deletions2, aes(x=Day, y=Per_Hgui_Alive_Total_Alive, color=Culture, group=Well_Num)) +
  labs(x = "Days Since Grape Juice Inoculation", y = "Percent of Culture (H. gui)") + ggtitle("Deletions2") +
  geom_line(data = deletions2, aes(x=Day, y=Per_Hgui_Alive_Total_Alive, color=Culture, group=Well_Num)) + ylim(0,100)
WGJp2
WGJp3 <- ggplot() +
  geom_point(data = deletions3, aes(x=Day, y=Per_Hgui_Alive_Total_Alive, color=Culture, group=Well_Num)) +
  labs(x = "Days Since Grape Juice Inoculation", y = "Percent of Culture (H. gui)") + ggtitle("Deletions3") +
  geom_line(data = deletions3, aes(x=Day, y=Per_Hgui_Alive_Total_Alive, color=Culture, group=Well_Num)) + ylim(0,100)
WGJp3
WGJp4 <- ggplot() +
  geom_point(data = amps1, aes(x=Day, y=Per_Hgui_Alive_Total_Alive, color=Culture, group=Well_Num)) +
  labs(x = "Days Since Grape Juice Inoculation", y = "Percent of Culture (H. gui)") + ggtitle("amps1") +
  geom_line(data = amps1, aes(x=Day, y=Per_Hgui_Alive_Total_Alive, color=Culture, group=Well_Num)) + ylim(0,100)
WGJp4
WGJp5 <- ggplot() +
  geom_point(data = amps2, aes(x=Day, y=Per_Hgui_Alive_Total_Alive, color=Culture, group=Well_Num)) +
  labs(x = "Days Since Grape Juice Inoculation", y = "Percent of Culture (H. gui)") + ggtitle("amps2") +
  geom_line(data = amps2, aes(x=Day, y=Per_Hgui_Alive_Total_Alive, color=Culture, group=Well_Num)) + ylim(0,100)
WGJp5
WGJp6 <- ggplot() +
  geom_point(data = amps3, aes(x=Day, y=Per_Hgui_Alive_Total_Alive, color=Culture, group=Well_Num)) +
  labs(x = "Days Since Grape Juice Inoculation", y = "Percent of Culture (H. gui)") + ggtitle("amps3") +
  geom_line(data = amps3, aes(x=Day, y=Per_Hgui_Alive_Total_Alive, color=Culture, group=Well_Num)) + ylim(0,100)
WGJp6
WGJp7 <- ggplot() +
  geom_point(data = solo1, aes(x=Day, y=Per_Hgui_Alive_Total_Alive, color=Culture, group=Well_Num)) +
  labs(x = "Days Since Grape Juice Inoculation", y = "Percent of Culture (H. gui)") + ggtitle("solo1") +
  geom_line(data = solo1, aes(x=Day, y=Per_Hgui_Alive_Total_Alive, color=Culture, group=Well_Num)) + ylim(0,100)
WGJp7
WGJp8 <- ggplot() +
  geom_point(data = solo2, aes(x=Day, y=Per_Hgui_Alive_Total_Alive, color=Culture, group=Well_Num)) +
  labs(x = "Days Since Grape Juice Inoculation", y = "Percent of Culture (H. gui)") + ggtitle("solo2") +
  geom_line(data = solo2, aes(x=Day, y=Per_Hgui_Alive_Total_Alive, color=Culture, group=Well_Num)) + ylim(0,100)
WGJp8
WGJp9 <- ggplot() +
  geom_point(data = solo3, aes(x=Day, y=Per_Hgui_Alive_Total_Alive, color=Culture, group=Well_Num)) +
  labs(x = "Days Since Grape Juice Inoculation", y = "Percent of Culture (H. gui)") + ggtitle("solo3") +
  geom_line(data = solo3, aes(x=Day, y=Per_Hgui_Alive_Total_Alive, color=Culture, group=Well_Num)) + ylim(0,100)
WGJp9
WGJp10 <- ggplot() +
  geom_point(data = TDH1exp1, aes(x=Day, y=Per_Hgui_Alive_Total_Alive, color=Culture, group=Well_Num)) +
  labs(x = "Days Since Grape Juice Inoculation", y = "Percent of Culture (H. gui)") + ggtitle("TDH1exp1") +
  geom_line(data = TDH1exp1, aes(x=Day, y=Per_Hgui_Alive_Total_Alive, color=Culture, group=Well_Num)) + ylim(0,100)
WGJp10
WGJp11 <- ggplot() +
  geom_point(data = TDH1exp2, aes(x=Day, y=Per_Hgui_Alive_Total_Alive, color=Culture, group=Well_Num)) +
  labs(x = "Days Since Grape Juice Inoculation", y = "Percent of Culture (H. gui)") + ggtitle("TDH1exp2") +
  geom_line(data = TDH1exp2, aes(x=Day, y=Per_Hgui_Alive_Total_Alive, color=Culture, group=Well_Num)) + ylim(0,100)
WGJp11
WGJp12 <- ggplot() +
  geom_point(data = TDH1exp3, aes(x=Day, y=Per_Hgui_Alive_Total_Alive, color=Culture, group=Well_Num)) +
  labs(x = "Days Since Grape Juice Inoculation", y = "Percent of Culture (H. gui)") + ggtitle("TDH1exp3") +
  geom_line(data = TDH1exp3, aes(x=Day, y=Per_Hgui_Alive_Total_Alive, color=Culture, group=Well_Num)) + ylim(0,100)
WGJp12
