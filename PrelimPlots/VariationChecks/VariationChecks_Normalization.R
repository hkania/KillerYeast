##Killer Yeast--normalization trial

## To source file 
setwd("/Users/msiddiq/Dropbox (University of Michigan)/PostDoc/Wittkopp Lab/Manuscripts/KillerYeast/KillerYeast_data/KillerYeast/Data/")

killeryeast_data <- read_csv('./WGJ_DFx_MS.csv') 
str(killeryeast_data)
##Replicate 2, day 8, 3324A, well D2 observation was not recorded; removed this one row from Hannah's manually

## Carryover/debris events artificially inflate the number of H. guillermondii cells in samples.
## This is an attempt to normalize that inflation away by using the number of events detected in S.cer alone (minimum)
## and in H. gui alone (maximum)
# Normalization function

## Make initial subset of the data with 3324a (cerevisiae alone), 3324m (cer + hgui mixed), 3850 (gui alone), and b (blank) 

miniset <-  subset(killeryeast_data, Culture %in% c("b", "3850", "3324a", "3324m"))[, 1:9]

## Data without normalization; note that the Scer alone consistently shows some guillermondii cells. However, this ratio does not change at all throughout the experiment,
## consistent with this being carryover and/or misidentification of debris. 
miniset <- miniset %>% 
  mutate(Hgui_totalprop = (Hgui_Alive_Num + Hgui_Dead_Num)/Total_Cells,
      Scer_totalprop = (Scer_Alive_Num + Scer_Dead_Num)/Total_Cells)

ggplot(miniset, aes(x = Day, y = Hgui_totalprop, col = Culture, label = Well_Num)) +
  geom_point() + geom_label(hjust = 0, nudge_x = 0.15) + 
  facet_wrap(~Replicate)

## Normalize this away--the maximum of our detection limit of guillermondii should be guillermondii alone; the minimum should be S. cerevisiae. Also removing blanks from this;
## the behavior of the blanks is largely noise and not comparable -- the one contimanted blank was removed and all other blanks were chekced everyday for contamination. 
miniset_norm <- miniset %>%  
  group_by(Day, Replicate) %>%
  filter(Culture != "b") %>%
  mutate(norm_Hgui_totalprop = (Hgui_totalprop - min(Hgui_totalprop[Culture == "3324a"]))/(max(Hgui_totalprop[Culture == "3850"])- min(Hgui_totalprop[Culture == "3324a"])))

ggplot(miniset_norm, aes(x = Day, y =norm_Hgui_totalprop, col = Culture, label = Well_Num)) +
  geom_jitter(width = 0.2) + geom_text(hjust = 0.2, nudge_x = 0.15, check_overlap = TRUE) + 
  facet_wrap(~Replicate) + theme_bw()

## Use this framework to normalize whole dataset
killeryeast_norm <- killeryeast_data[, 1:9] %>%
  mutate(Hgui_totalprop = (Hgui_Alive_Num + Hgui_Dead_Num)/Total_Cells,
         Scer_totalprop = (Scer_Alive_Num + Scer_Dead_Num)/Total_Cells)%>%  
  group_by(Day, Replicate) %>%
  filter(Culture != "b") %>%
  mutate(norm_Hgui_totalprop = (Hgui_totalprop - min(Hgui_totalprop[Culture == "3324a"]))/(max(Hgui_totalprop[Culture == "3850"])- min(Hgui_totalprop[Culture == "3324a"])))
  
## The whole dataset is a lot, so first check genotypes in batches. 
single_del <- c("3821", "3822", "3824")
scar_lines <- c('3852', '3920' ,'3984')
rec_gen <- c('3911', '3905', '3842', '3973')

ggplot(subset(killeryeast_norm, Culture %in% single_del), 
       aes(x = Day, y =norm_Hgui_totalprop, col = Culture, label = Well_Num)) +
  geom_jitter(width = 0.2) + geom_text(hjust = 0.2, nudge_x = 0.15, check_overlap = TRUE) + 
  facet_wrap(~Replicate) + theme_bw()


ggplot(subset(killeryeast_norm, Culture %in% scar_lines), 
       aes(x = Day, y =norm_Hgui_totalprop, col = Culture, label = Well_Num)) +
  geom_jitter(width = 0.2) + geom_text(hjust = 0.2, nudge_x = 0.15, check_overlap = TRUE) + 
  facet_wrap(~Replicate) + theme_bw()


ggplot(subset(killeryeast_norm, Culture %in% rec_gen), 
       aes(x = Day, y =norm_Hgui_totalprop, col = Culture, label = Well_Num)) +
  geom_jitter(width = 0.2) + geom_text(hjust = 0.2, nudge_x = 0.15, check_overlap = TRUE) + 
  facet_wrap(~Replicate) + theme_bw()


## Same done with single_dels, recombinants


## Generic normalization function
# normalit<-function(m){
#   (m - min(m))/(max(m)-min(m))
# }