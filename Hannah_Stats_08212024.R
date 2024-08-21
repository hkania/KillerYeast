library(ggsci)
library(ggthemes)
library(readxl)
library(tidyverse)
library(AICcmodavg)
library(mgcv)
library(nlme)
library(minpack.lm)

## ? for Mo & Nick remove scer category?

#Load data
yeast_data <- read_csv("Downloads/final_dataset_hk_11162023.csv")

#Remove bad row (not sure why this is in there...)
yeast_data2 <- yeast_data %>% filter(!row_number() %in% c(1))

#Define categories
single_del <- c("3821", "3822", "3824")
scar_lines <- c('3852', '3920' ,'3984')
rec_gen <- c('3911', '3905', '3842', '3973')
wildtype_scer_mix <- '3324m'
wildtype_scer_alone <- '3324a'
wildtype_hgui_alone <- '3850'
blank <- 'b'

#Make factors
yeast_data2$Day <- factor(yeast_data2$Day, ordered = T)
yeast_data2$Replicate <- factor(yeast_data2$Replicate)

#Misc. Dont need to run
single_del_data <- subset(yeast_data2, Culture %in% single_del)
scar_line_data <- subset(yeast_data2, Culture %in% scar_lines)
rec_gen_data <- subset(yeast_data2, Culture %in% rec_gen)

ggplot(subset(yeast_data2, Culture %in% single_del), 
       aes(x = Day, y =norm_Hgui_totalprop, col = Culture, label = Well_Num)) +
  geom_jitter(width = 0.2) + geom_text(hjust = 0.2, nudge_x = 0.15, check_overlap = TRUE) + 
  facet_wrap(~Replicate) + theme_bw()

ggplot(subset(yeast_data2, Culture %in% scar_lines), 
       aes(x = Day, y =norm_Hgui_totalprop, col = Culture, label = Well_Num)) +
  geom_jitter(width = 0.2) + geom_text(hjust = 0.2, nudge_x = 0.15, check_overlap = TRUE) + 
  facet_wrap(~Replicate) + theme_bw()

ggplot(subset(yeast_data2, Culture %in% rec_gen), 
       aes(x = Day, y =norm_Hgui_totalprop, col = Culture, label = Well_Num)) +
  geom_jitter(width = 0.2) + geom_text(hjust = 0.2, nudge_x = 0.15, check_overlap = TRUE) + 
  facet_wrap(~Replicate) + theme_bw()

#Define grouped data (important for statistical analyses on groups rather than ind. genos)
grouped_data <- yeast_data2 %>% 
  mutate(category = case_when(
    Culture %in% single_del ~ "single_del",
    Culture %in% rec_gen ~ "rec_geno",
    Culture %in% scar_lines ~ "scarlet",
    Culture %in% wildtype_scer_alone ~ "scer",
    Culture %in% wildtype_scer_mix ~ "wildtype_mix",
    Culture %in% wildtype_hgui_alone ~ "hgui"
  ))

grouped_data$category <- factor(grouped_data$category)
grouped_data$category <- relevel(grouped_data$category, ref = "wildtype_mix")

day_5_data <- subset(grouped_data, Day == 5)

#t-tests (can only be performed between two groups)
tapply(X = day_5_data$norm_Hgui_totalprop, INDEX = day_5_data$Replicate, FUN = mean)
tapply(X = day_5_data$norm_Hgui_totalprop, INDEX = day_5_data$Culture, FUN = mean)

mean.hprop <- tapply(X = day_5_data$norm_Hgui_totalprop, INDEX = day_5_data$category, FUN = mean)
var.hprop <- tapply(X = day_5_data$norm_Hgui_totalprop, INDEX = day_5_data$category, FUN = var)
n.hprop <- tapply(X = day_5_data$norm_Hgui_totalprop, INDEX = day_5_data$category, FUN = length)

print(mean.hprop)

diff_mean <- mean.hprop[2] - mean.hprop[6]
print(diff_mean)

se.hprop <- sqrt((var.hprop[2]/n.hprop[2]) + (var.hprop[6]/n.hprop[6]))
print(se.hprop)

t.hprop <- diff_mean/se.hprop
print(t.hprop)

#anova, between more than 2 groups, note the data is not normal for day 5
day5_aov <- aov(norm_Hgui_totalprop ~ category, 
             data = day_5_data)
par(mfrow = c(1,2))
hist(day5_aov$residuals)
library(car)

qqPlot(day5_aov$residuals, id = F)

shapiro.test(day5_aov$residuals)

#2 way anova (day and category)
# http://www.sthda.com/english/wiki/two-way-anova-test-in-r#:~:text=Two%2Dway%20ANOVA%20test%20is,levels%20can%20vary%20between%20factors.
table(grouped_data$category, grouped_data$norm_Hgui_totalprop)

library("ggpubr")
ggboxplot(grouped_data, x = "Day", y = "norm_Hgui_totalprop", color = "category")
ggline(grouped_data, x = "Day", y = "norm_Hgui_totalprop", color = "category", 
       add = c("mean_se", "dotplot"))

res.aov <- aov(norm_Hgui_totalprop ~ category + Day, data = grouped_data)
summary(res.aov)

#good anova to start with
res.aov3 <- aov(norm_Hgui_totalprop ~ category * Day, data = grouped_data)
summary(res.aov3)

par(mfrow = c(1,2))
hist(res.aov3$residuals)
qqPlot(res.aov3$residuals, id = F)
shapiro.test(res.aov3$residuals)

group_by(grouped_data, Day, category) %>% 
  summarise(
    count = n(),
    mean = mean(norm_Hgui_totalprop, na.rm = T),
    sd = sd(norm_Hgui_totalprop, na.rm = T)
  )

model.tables(res.aov3, type = "means", se = T)

#Tukey pairwise comparisons
?TukeyHSD
TukeyHSD(res.aov3, which = "category")

library(multcomp)
summary(glht(res.aov3, linfct = mcp(category = "Tukey")))

#Dunnett's test (https://statsandr.com/blog/anova-in-r/)
post_test <- glht(res.aov3,
                  linfct = mcp(category = "Dunnett"))
summary(post_test)
par(mfrow = c(1,1))
plot(post_test)

#test homogeneity
plot(res.aov3, 1) #shows 2 outliers, 223 & 100
leveneTest(norm_Hgui_totalprop ~ Day*category, data = grouped_data) #cannot assume variance across groups is homogeneous

#test normality
plot(res.aov3, 2)
shapiro.test(res.aov3$residuals)
