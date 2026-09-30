#my basic info

#my main question: How do Algebra I and Biology performance outcomes differ between York and Dauphin County schools?
install.packages(c("readxl", "dplyr", "ggplot2"))

library(readxl)
library(dplyr)
library(ggplot2)

keystone <- read_excel(
  "PA/2025-keystone-exams-school-level-data.xlsx",
  sheet = "Keystone",
  skip = 3)

# york & dauphin county in math and science the 2 counties I have worked on
county_data <- filter(
  keystone,
  County %in% c("YORK", "DAUPHIN"),
  Group == "All Students",
  Subject %in% c("Algebra I", "Biology"))

head(county_data)

# Organized the data to compare York and Dauphin County to find 
#proficiency rate for each
summary_data <- aggregate(
  cbind(
    proficient_points = county_data$`Percent Proficient and above` *
      county_data$`Number Scored`,
    students_tested = county_data$`Number Scored`),
  by = list(
    County = county_data$County,
    Subject = county_data$Subject),
  FUN = sum,
  na.rm = TRUE)

summary_data$weighted_proficient_rate <-
  summary_data$proficient_points / summary_data$students_tested

summary_data

#In simple words:York County had higher overall p-r than Dauphin County in both Algebra I and Biology.
#Biology also had a slightly higher proficiency rate than Algebra I in both counties.

#want to visualize the findings so I am creating a bar graph 

ggplot(
  summary_data,
  aes(
    x = County,
    y = weighted_proficient_rate,
    fill = Subject)) +
  geom_col(position = "dodge") +
  scale_y_continuous(
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")) +
  labs(
    title = "Keystone Proficiency Rates: York vs. Dauphin County",
    subtitle = "Algebra I and Biology, All Students",
    x = "County",
    y = "Students Proficient or Above",
    fill = "Subject") +
  theme_minimal()