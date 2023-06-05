library(tidyr)
library(tidyverse)
library(stringr)
library(reshape2)
library(lubridate)
library(eeptools)

#loading in treatment data
treatments <- read.csv("S:/DataDirect/HUM00210981 - BIOSTAT629 Case Studies In Health Bi/bedrava/treatment_data.csv")

#loading in survey data
data <- read.csv("S:/DataDirect/HUM00210981 - BIOSTAT629 Case Studies In Health Bi/bedrava/bedrava_data.csv")

#combining and cleaning data
data <- dplyr::full_join(data, treatments, by = "DeID_PatientID")
data <- dplyr::arrange(data, DeID_PatientID)
data <- dplyr::filter(data, ResultIdentifier == "Sex" |
                      ResultIdentifier == "Gender" |
                      ResultIdentifier == "Ethnicity" |
                      ResultIdentifier == "Sexual Orientation" |
                      ResultIdentifier == "Relationship Status" |
                      ResultIdentifier == "Education" |
                      ResultIdentifier == "Credit hours" |
                      ResultIdentifier == "Academic Year" |
                      ResultIdentifier == "COVID-19 Question 1" |
                      ResultIdentifier == "COVID-19 Question 2" |
                      ResultIdentifier == "COVID-19 Question 1b" |
                      ResultIdentifier == "1Employment" |
                      ResultIdentifier == "3Employment" |
                      str_detect(ResultIdentifier, "GAD") |
                      str_detect(ResultIdentifier, "NEO"))


data$DeID_PatientID <- as.factor(data$DeID_PatientID)

GAD_data <- data %>% select(DeID_PatientID, SurveyName, ResultIdentifier, SurveyAnswer) %>%
  filter(str_detect(ResultIdentifier, "GAD") & ResultIdentifier != "GAD8") %>% 
         group_by(DeID_PatientID, SurveyName, ResultIdentifier)

#changing names
GAD_data$SurveyName <- ifelse(GAD_data$SurveyName == "6-Week Survey Combined", "Week6",
                          ifelse(GAD_data$SurveyName == "Baseline Survey Combined", "Baseline",
                                 ifelse(GAD_data$SurveyName == "18-Week Survey Combined", "Week18",
                                        ifelse(GAD_data$SurveyName == "12-Month Survey Combined", "Month12", NA))))
GAD_data$time_GAD = paste(GAD_data$SurveyName, GAD_data$ResultIdentifier, sep="_")

GAD_data <- GAD_data %>% ungroup() %>% select(-SurveyName, -ResultIdentifier)

#finding duplicated values
GAD_data %>%
  dplyr::group_by(DeID_PatientID, time_GAD) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) 
#There are 35 people who have duplicated values

#removing duplicates of unique combinations of time_GAD and DeID_PatientID
GAD_data <- GAD_data[!duplicated(GAD_data[, c("time_GAD", "DeID_PatientID")]), ]

#converting to wide format
GAD_data_wide <- pivot_wider(GAD_data, names_from = time_GAD, values_from = SurveyAnswer)

#reordering columns
GAD_data_wide <- GAD_data_wide[, c("DeID_PatientID", "Baseline_GAD1", "Baseline_GAD2", "Baseline_GAD3", "Baseline_GAD4",
                                   "Baseline_GAD5", "Baseline_GAD6","Baseline_GAD7",
                                   "Week6_GAD1", "Week6_GAD2", "Week6_GAD3", "Week6_GAD4",
                                   "Week6_GAD5", "Week6_GAD6","Week6_GAD7",
                                   "Week18_GAD1", "Week18_GAD2", "Week18_GAD3", "Week18_GAD4",
                                   "Week18_GAD5", "Week18_GAD6", "Week18_GAD7",
                                   "Month12_GAD1", "Month12_GAD2", "Month12_GAD3", "Month12_GAD4",
                                   "Month12_GAD5", "Month12_GAD6", "Month12_GAD7")]

#finding GAD score for each person at baseline, 6 weeks, 18 weeks, 12 months
GAD_data_wide <- GAD_data_wide %>% mutate_if(is.character, as.numeric)
GAD_data_wide <- mutate(GAD_data_wide,
                        Baseline_GAD = (Baseline_GAD1 + Baseline_GAD2 + Baseline_GAD3 +
                                           Baseline_GAD4 + Baseline_GAD5 + Baseline_GAD6 +
                                           Baseline_GAD7),
                        Week6_GAD = (Week6_GAD1 + Week6_GAD2 + Week6_GAD3 + Week6_GAD4 +
                                        Week6_GAD5 + Week6_GAD6 + Week6_GAD7),
                        Week18_GAD = (Week18_GAD1 + Week18_GAD2 + Week18_GAD3 + Week18_GAD4 +
                                         Week18_GAD5 + Week18_GAD6 + Week18_GAD7),
                        Month12_GAD = (Month12_GAD1 + Month12_GAD2 + Month12_GAD3 + Month12_GAD4 +
                                          Month12_GAD5 + Month12_GAD6 + Month12_GAD7))

#making data set of all time-invariant variables
data2 <- filter(data, SurveyName == "Baseline Survey Combined")
data2 <- filter(data, ResultIdentifier != "GAD1",
                    ResultIdentifier != "GAD2",
                    ResultIdentifier != "GAD3",
                    ResultIdentifier != "GAD4",
                    ResultIdentifier != "GAD5",
                    ResultIdentifier != "GAD6",
                    ResultIdentifier != "GAD7",
                    ResultIdentifier != "GAD8")
data2 <- select(data2, DeID_PatientID, ResultIdentifier, SurveyAnswer)

#removing duplicate values
data2 <- data2[!duplicated(data2[, c("DeID_PatientID", "ResultIdentifier")]), ]

#converting to wide format
data_wide <- pivot_wider(data2, names_from = ResultIdentifier, values_from = SurveyAnswer)

#combining GAD_data_wide and data_wide, but only keeping total GAD values from GAD_data_wide
data_wide <- inner_join(data_wide, GAD_data_wide, by = "DeID_PatientID")
data_wide <- select(data_wide, -Baseline_GAD1, -Baseline_GAD2, -Baseline_GAD3,
                      -Baseline_GAD4, -Baseline_GAD5, -Baseline_GAD6,
                      -Baseline_GAD7, -Week6_GAD1, -Week6_GAD2, -Week6_GAD3, -Week6_GAD4,
                      -Week6_GAD5, -Week6_GAD6, -Week6_GAD7, -Week18_GAD1, -Week18_GAD2, -Week18_GAD3,
                      -Week18_GAD4, -Week18_GAD5, -Week18_GAD6, -Week18_GAD7, 
                    -Month12_GAD1, -Month12_GAD2, -Month12_GAD3, -Month12_GAD4,
                      -Month12_GAD5, -Month12_GAD6, -Month12_GAD7)

#finding question wording for all questions
questions <- data[!duplicated(data[, c("ResultIdentifier")]), ]
questions <- select(questions, ResultIdentifier, SurveyQuestion, PossibleAnswers)
questions <- questions %>% arrange(ResultIdentifier)
write.csv(questions, "S:/DataDirect/HUM00210981 - BIOSTAT629 Case Studies In Health Bi/bedrava/questions.csv")

#calculating age for each person
bdays <- read.csv("S:/DataDirect/HUM00210981 - BIOSTAT629 Case Studies In Health Bi/bedrava/age.csv")
#removing duplicates in bdays
colnames(bdays)
bdays<- rename(bdays, DeID_PatientID = "ï..DeID_PatientID")
bdays <- bdays[!duplicated(bdays[, c("DeID_PatientID", "DeID_DOB")]), ]
#my DeID_SurveyStartDate isn't a date, so pulling data column from another csv
andrea <- read.csv("S:/DataDirect/HUM00210981 - BIOSTAT629 Case Studies In Health Bi/pangoria/surveydata.csv")
andrea<- rename(andrea, DeID_PatientID = "ï..DeID_PatientID")
andrea <- andrea[!duplicated(andrea[, c("DeID_PatientID", "SurveyName")]), ]
andrea <- filter(andrea, SurveyName == "Baseline Survey Combined")
andrea <- select(andrea, DeID_PatientID, DeID_SurveyStartDate)

data_wide <- inner_join(andrea, data_wide, by = "DeID_PatientID")
data_wide <- inner_join(bdays, data_wide, by = "DeID_PatientID")
data_wide <- mutate(data_wide, age = age_calc(as.Date(DeID_DOB), as.Date(DeID_SurveyStartDate), units = "years"))
data_wide <- select(data_wide, -DeID_SurveyStartDate, -DeID_DOB)

#calculating NEO score for each person
#flipping the scales for NEO1, NEO3, NEO6, NEO8, AND NEO14
data_wide <- mutate(data_wide,
                    NEO1_flip = ifelse(NEO1 == 0, 4,
                                ifelse(NEO1 == 1, 3,
                                ifelse(NEO1 == 2, 2,
                                ifelse(NEO1 == 3, 1,
                                ifelse(NEO1 == 4, 0, NA))))),
                    NEO3_flip = ifelse(NEO3 == 0, 4,
                                ifelse(NEO3 == 1, 3,
                                ifelse(NEO3 == 2, 2,
                                ifelse(NEO3 == 3, 1,
                                ifelse(NEO3 == 4, 0, NA))))),
                    NEO6_flip = ifelse(NEO6 == 0, 4,
                                ifelse(NEO6 == 1, 3,
                                ifelse(NEO6 == 2, 2,
                                ifelse(NEO6 == 3, 1,
                                ifelse(NEO6 == 4, 0, NA))))),
                    NEO8_flip = ifelse(NEO8 == 0, 4,
                                ifelse(NEO8 == 1, 3,
                                ifelse(NEO8 == 2, 2,
                                ifelse(NEO8 == 3, 1,
                                ifelse(NEO8 == 4, 0, NA))))),
                    NEO14_flip = ifelse(NEO14 == 0, 4,
                                ifelse(NEO14 == 1, 3,
                                ifelse(NEO14 == 2, 2,
                                ifelse(NEO14 == 3, 1,
                                ifelse(NEO14 == 4, 0, NA))))))
#summing NEO scores for a total score
data_wide$NEO1_flip <- as.numeric(data_wide$NEO1_flip)
data_wide$NEO3_flip <- as.numeric(data_wide$NEO3_flip)
data_wide$NEO6_flip <- as.numeric(data_wide$NEO6_flip)
data_wide$NEO8_flip <- as.numeric(data_wide$NEO8_flip)
data_wide$NEO14_flip <- as.numeric(data_wide$NEO14_flip)
data_wide$NEO4 <- as.numeric(data_wide$NEO4)
data_wide$NEO5 <- as.numeric(data_wide$NEO5)
data_wide$NEO7 <- as.numeric(data_wide$NEO7)
data_wide$NEO9 <- as.numeric(data_wide$NEO9)
data_wide$NEO10 <- as.numeric(data_wide$NEO10)
data_wide$NEO11 <- as.numeric(data_wide$NEO11)
data_wide$NEO12 <- as.numeric(data_wide$NEO12)
data_wide$NEO13 <- as.numeric(data_wide$NEO13)
data_wide <- mutate(data_wide, NEO_tot = NEO1_flip + NEO3_flip + NEO4 + NEO5 +
                      NEO6_flip + NEO7 + NEO8_flip + NEO9 + NEO10 + NEO11 + NEO12 + 
                      NEO13 + NEO14_flip)

#removing unnecessary columns
data_wide <- select(data_wide, -NEO1, -NEO2, -NEO3, -NEO4, -NEO5, -NEO6, -NEO7, -NEO8, -NEO9, -NEO10,
                    -NEO11, -NEO12, -NEO13, -NEO14, -NEO1_flip, -NEO3_flip, -NEO6_flip,
                    -NEO8_flip, -NEO14_flip)

#rearranging column order
data_wide <- select(data_wide, DeID_PatientID, NEO_tot, Baseline_GAD, Week6_GAD, Week18_GAD, 
                    Month12_GAD, age, Gender, Sex, everything())

#adding variables I'm missing
andrea2 <- read.csv("S:/DataDirect/HUM00210981 - BIOSTAT629 Case Studies In Health Bi/pangoria/surveydata.csv")
unique(andrea2$ResultIdentifier) #I'm missing Race variable
andrea2<- rename(andrea2, DeID_PatientID = "ï..DeID_PatientID")
andrea2 <- filter(andrea2, ResultIdentifier == "Race")
andrea2 <- select(andrea2, DeID_PatientID, SurveyAnswer)
data_wide <- inner_join(data_wide, andrea2, by = "DeID_PatientID")
data_wide <- rename(data_wide, race = SurveyAnswer)

#rearranging column order
data_wide <- select(data_wide, DeID_PatientID, NEO_tot, Baseline_GAD, Week6_GAD, Week18_GAD, 
                    Month12_GAD, age, Gender, Sex, race, Ethnicity, everything())

#renaming columns with spaces in the names
data_wide <- rename(data_wide, COVID_Q2 = "COVID-19 Question 2")
data_wide <- rename(data_wide, relationship_status = "Relationship Status")
data_wide <- rename(data_wide, COVID_Q1 = "COVID-19 Question 1")
data_wide <- rename(data_wide, sexual_orientation = "Sexual Orientation")
data_wide <- rename(data_wide, academic_year = "Academic Year")
data_wide <- rename(data_wide, credits = "Credit hours")
data_wide <- rename(data_wide, COVID_Q1b = "COVID-19 Question 1b")

#merging data_wide with treatments
data_wide <- inner_join(data_wide, treatments, by = "DeID_PatientID")
data_wide <- rename(data_wide, Treatment = "StudyGroupName")

#writing out final wide format data to folder
write.csv(data_wide, "S:/DataDirect/HUM00210981 - BIOSTAT629 Case Studies In Health Bi/bedrava/data_wide.csv")

#converting data_wide to long format for GAD scores
data_long <- pivot_longer(data_wide, cols = c(Baseline_GAD, Week6_GAD, Week18_GAD, Month12_GAD),
                          names_to = 'Time', values_to = "GAD")

#renaming Time variable categories
data_long$Time <- ifelse(data_long$Time == "Baseline_GAD", "Baseline",
                         ifelse(data_long$Time == "Month12_GAD", "Month 12",
                                ifelse(data_long$Time == "Week18_GAD", "Week 18", "Week 6")))

#reordering levels of Time variable so they're in chronological order 
data_long$Time <- factor(data_long$Time, levels = c("Baseline", "Week 6", "Week 18", "Month 12"))

#writing out final long format data to folder
write.csv(data_long, "S:/DataDirect/HUM00210981 - BIOSTAT629 Case Studies In Health Bi/bedrava/data_long.csv")






