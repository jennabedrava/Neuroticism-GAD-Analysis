#########################################################################################################################
## Project: Neuroticism and GAD-7 Mobile Treatment Efficacy
## Objectives: Quantify impact of neuroticism score on PROMPT mobile health treatment efficacy
## Date: Last updated 04/14/2022
## Author: Jenna Bedrava
#########################################################################################################################

library(table1)
library(nlme)
library(geepack)
library(ggpubr)
library(lme4)
library(sjPlot)

#reading in previously cleaned data (cleaned during first project)
data_long <- read.csv("S:/DataDirect/HUM00210981 - BIOSTAT629 Case Studies In Health Bi/bedrava/data_long.csv")
data_wide <- read.csv("S:/DataDirect/HUM00210981 - BIOSTAT629 Case Studies In Health Bi/bedrava/data_wide.csv")
subset <-  read.csv("S:/DataDirect/HUM00210981 - BIOSTAT629 Case Studies In Health Bi/bedrava/subset.csv")

#### ADDITIONAL DATA CLEANING ####
#adding weeks variable to data_long
data_long <- mutate(data_long, Weeks = ifelse(Time == "Baseline", 0,
                                              ifelse(Time == "Week 6", 6,
                                                     ifelse(Time == "Week 18", 18, 52))))
#fixing sex variable in data_wide
data_wide$Sex <- ifelse(data_wide$Sex == "n/a", NA, data_wide$Sex)
data_wide$Sex <- factor(data_wide$Sex, levels = c("male", "female"), labels=c("Male", "Female"))

#fixing sex variable in data_long
data_long$Sex <- ifelse(data_long$Sex == "n/a", NA, data_long$Sex)
data_long$Sex <- factor(data_long$Sex, levels = c("male", "female"), labels=c("Male", "Female"))

#### VISUALIZATIONS ####
#visualizing outcome of interest - GAD over time (side by side boxplot)
gad_boxplot <- ggplot(data = filter(data_long, !is.na(GAD))) + aes(x = Time, y = GAD) + 
  theme_light()  +
  geom_boxplot(fill = "#719FB0")  + labs(y = "GAD-7", title = "GAD-7 Score Over Time") + 
  theme(axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12))
gad_boxplot

#visualizing outcome of interest - GAD over time (spaghetti plot)
ggplot(data=data_long) + aes(x = Weeks, y = GAD) + 
  geom_smooth(aes(color = Treatment), method = "loess", se = F, size = 2) + theme_light() + 
  scale_color_manual(values = c("#544179", "#6166B3", "#719FB0", "#A0C1B8", "#FAB93C")) +
  labs(y = "GAD-7", title = "GAD-7 Score Over Time by Treatment Group") + 
  theme(axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  scale_x_continuous(breaks = seq(0, 52, 4))

#visualizing predictor of interest - neuroticism (histogram)
neo_hit <- ggplot(data = data_wide) + aes(x = NEO_tot) + 
  geom_histogram(bins = 20,  fill = "#719FB0", color = "black") + theme_light() + 
  labs(title = "Distribution of NEO Neuroticism Score", y = "Count", 
       x = "Neuroticism Score") +
  theme(axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12))

ggarrange(gad_boxplot, neo_hit)

#making variable for quartiles of neuroticism
summary(data_wide$NEO_tot)
data_wide <- mutate(data_wide, NEO_quartile  = ifelse(NEO_tot <  30, 1,
                                               ifelse(NEO_tot < 36 & NEO_tot >= 30, 2,
                                               ifelse(NEO_tot < 40 & NEO_tot >=36, 3, 4))))

#making a table 1
label(data_wide$Baseline_GAD) <- "Baseline GAD-7 Score"
label(data_wide$Week6_GAD) <- "6 Week GAD-7 Score"
label(data_wide$Week18_GAD) <- "18 Week GAD-7 Score"
label(data_wide$Month12_GAD) <- "12 Month GAD-7 Score"
data_wide$race2 <- factor(data_wide$race2, levels = c("White", "Black", "Other"))
label(data_wide$race2) <- "Race"
data_wide$Sex <- factor(data_wide$Sex, levels = c("Male", "Female"))
data_wide$edu <- as.factor(data_wide$edu)
data_wide$NEO_quartile <- as.factor(data_wide$NEO_quartile)
levels(data_wide$NEO_quartile) <- list ("Quartile 1" = 1, 
                                        "Quartile 2" = 2,
                                        "Quartile 3" = 3, 
                                        "Quartile 4" = 4)
levels(data_wide$edu) <- list("Less than High School" = 1, "High School" =2, "Above High School" = 3)
label(data_wide$edu) <- "Education"
table1(~ Baseline_GAD  + Week6_GAD + Week18_GAD + Month12_GAD +
         Sex + race2 + edu | NEO_quartile, topclass = "Rtable1-zebra",
       render.missing = NULL, data = data_wide)

#making baseline GAD score category variable
data_wide <- mutate(data_wide, GAD_cat = ifelse(Baseline_GAD <= 4, "Minimal", 
                                                ifelse(Baseline_GAD >= 5 & Baseline_GAD < 10, "Mild",
                                                       ifelse(Baseline_GAD >= 10 & Baseline_GAD <15, "Moderate", "Severe"))))

#making side by side boxplot of neuroticism for GAD category
ggplot(data = data_wide) + aes(x = GAD_cat, y = NEO_tot) + 
  theme_light()  +
  geom_boxplot(fill = "#A0C1B8")  + labs(y = "Neuroticism Score", 
                                         title = "Neuroticism Score by GAD Category",
                                         x = "GAD-7 Category") + 
  theme(axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12))

data_wide$GAD_cat <- factor(data_wide$GAD_cat, levels = c("Minimal", "Mild", "Moderate", "Severe"))

#performing anova to determine to neuroticism score is significantly different in GAD categories
summary(aov(NEO_tot ~ GAD_cat, data_wide)) #means are significantly different

#recoding education variable
data_long <- mutate(data_long, edu = ifelse(Education == "Ed2", 1, 
                                            ifelse(Education == "Ed3", 2, 3)))
data_wide <- mutate(data_wide, edu = ifelse(Education == "Ed2", 1, 
                                            ifelse(Education == "Ed3", 2, 3)))

#recoding relationship status variable
data_wide <-  mutate(data_wide, rel_stat = ifelse(relationship_status == "Rel2" | 
                                                    relationship_status == "Rel3", "Relationship", 
                                                    ifelse(relationship_status == "Rel6", NA, "No Relationship")))

#recoding race variable 
data_long <- mutate(data_long, race2 = ifelse(race == "race1", "White",
                                              ifelse(race == "race2", "Black", "Other")))
data_wide <- mutate(data_wide, race2 = ifelse(race == "race1", "White",
                                              ifelse(race == "race2", "Black", "Other")))

data_long <- data_long[, -c(1)]
data_wide <- select(data_wide, -race, -Gender)
data_wide <- select(data_wide, -relationship_status)
write.csv(data_long, "S:/DataDirect/HUM00210981 - BIOSTAT629 Case Studies In Health Bi/bedrava/data_long.csv")
write.csv(data_wide, "S:/DataDirect/HUM00210981 - BIOSTAT629 Case Studies In Health Bi/bedrava/data_wide.csv")

#running linear mixed effects models
#random intercept only model
model1 <- lme(GAD ~ Weeks + NEO_tot*Weeks + Treatment*Weeks + age + Sex + race2 + edu + rel_stat, random = ~ 1 | DeID_PatientID,
              data = data_long, na.action = na.exclude, method = "ML") #feedback only is the reference group for treatment
summary(model1)

#random intercept and slope model
model2 <- lme(GAD ~ Weeks + NEO_tot*Weeks + Treatment*Weeks + age + Sex + race2 + edu + rel_stat, random = ~ Weeks | DeID_PatientID,
              data = data_long, na.action = na.exclude, method = "ML") #feedback only is the reference group for treatment
summary(model2)

anova(model1, model2) #random intercept and slope is better
#model2 is our final model 

#### MODEL DIAGNOSTICS ####
#residuals vs fitted values plot
ggplot(data = data.frame(fitted_values = fitted(model2),
                         residuals = residuals(model2, type = c('pearson')),
       group = data_long$Treatment, id = data_long$DeID_PatientID)) +
  geom_point(aes(x = fitted_values, y = residuals, color = group))
#residuals are weird

subset<-filter(data_long, !is.na(GAD), !is.na(NEO_tot), !is.na(Treatment), 
               !is.na(age), !is.na(Sex), !is.na(race2), !is.na(edu), !is.na(rel_stat))
write.csv(subset, "S:/DataDirect/HUM00210981 - BIOSTAT629 Case Studies In Health Bi/bedrava/subset.csv")

#running glmer
data_long <- mutate(data_long, age_c = (age - mean(age))/sd(age))
data_long <- mutate(data_long, time2 = ifelse(Weeks == 0, 0,
                                              ifelse(Weeks == 6, 1,
                                                     ifelse(Weeks == 18, 2, 3))))
data_long <- mutate(data_long, NEO_tot_std = (NEO_tot - mean(NEO_tot))/sd(NEO_tot))
data_long$race2 <- as.factor(data_long$race2)
data_long$race2 <- relevel(data_long$race2, ref = "White")
data_long$edu <- as.factor(data_long$edu)
data_long$Sex <- as.factor(data_long$Sex)
data_long$Sex <- relevel(data_long$Sex, ref = "Male")

glmer0<- glmer(GAD ~ time2 + NEO_tot_std*time2 + Treatment + age_c + Sex + race2 + edu + 
                 (1 | DeID_PatientID), data = data_long, 
               family = poisson(link = "log"),
               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

glmer1<- glmer(GAD ~ time2 + NEO_tot_std*time2 + Treatment + age_c + Sex + race2 + edu + 
                 (1 + time2 | DeID_PatientID), data = data_long, 
               family = poisson(link = "log"),
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmer1)

#LRT of random intercept only and random intercept and random slope models
anova(glmer0, glmer1) #random intercept and slope fits data better

glmer2<- glmer(GAD ~ time2 + NEO_tot_std*time2 + Treatment + age_c + Sex + race2 + edu + 
                 NEO_tot_std*Treatment + (1 + time2 | DeID_PatientID), data = data_long, 
               family = poisson(link = "log"),
               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmer2)

label(data_long$time2) <- "Time"
label(data_long$NEO_tot_std) <- "Standardized Neuroticism Score"

#LRT of model without interaction between treatment and neuroticism and model with interaction
anova(glmer1, glmer2) #bigger model is NOT a better fit

sjPlot::tab_model(glmer1, show.intercept = F, show.re.var = T,
                  pred.labels = c("Time", "Neuroticism", "Headspace",
                                  "Headspace plus Feedback", "Silvercloud",
                                  "Silvercloud plus Feedback", "Age", "Female",
                                  "Black", "Other Race", "High School", "Above High School",
                                  "Time * Neuroticism"), dv.labels = "GAD-7 Score")
sjPlot::tab_model(glmer2, show.intercept = F, show.re.var = T,
                 pred.labels = c("Time", "Neuroticism", "Headspace",
                                 "Headspace plus Feedback", "Silvercloud",
                                 "Silvercloud plus Feedback", "Age", "Female",
                                 "Black", "Other Race", "High School", "Above High School",
                                 "Time * Neuroticism", "Headspace * Neuroticism",
                                 "Headspace plus Feedback * Neuroticism",
                                 "Silvercloud * Neuroticism",
                                 "Silvercloud plus Feedback * Neuroticism"), 
                 dv.labels = "GAD-7 Score")

#residuals vs fitted values plot
ggplot(data = data.frame(fitted_values = fitted(glmer1),
                         residuals = residuals(glmer1, type = c('deviance')))) +
  geom_point(aes(x = (fitted_values), y = residuals)) + theme_light() +
  labs(title = "Deviance Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12))
#residuals are weird

#checking normality of random intercepts
ranef1<-as.data.frame(ranef(glmer1))
intercepts_qq <- ggplot(dplyr::filter(ranef1, term == "(Intercept)")) + 
  stat_qq(aes(sample = condval), position ="identity", size = 2) + 
  stat_qq_line(aes(sample = condval)) + 
  theme_bw() + labs(title = "Q-Q Plot of Random Intercept") + ylab("Sample Quantiles") +
  xlab("Theoretical Quantiles") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12))

#checking normality of random slopes
slopes_qq <- ggplot(dplyr::filter(ranef1, term == "time2")) + 
  stat_qq(aes(sample = condval), position ="identity", size = 2) + 
  stat_qq_line(aes(sample = condval)) + 
  theme_bw() + labs(title = "Q-Q Plot of Random Slope for Time") + ylab("Sample Quantiles") +
  xlab("Theoretical Quantiles") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12))

ggarrange(intercepts_qq, slopes_qq)













