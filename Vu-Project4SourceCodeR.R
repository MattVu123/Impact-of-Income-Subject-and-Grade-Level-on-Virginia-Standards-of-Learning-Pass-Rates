# Name: Matthew Vu
# Due Date: 12/03/2023

# import libraries
library(tidyverse)
library(moments)

# set working directory
setwd("~/Backup/COLLEGE/FALL 2023 SEMESTER/AIT 580/Project")

# read in the dataset
data = read.csv("Datasets/Clean Data/final_cleaned.csv")

# nominal or ordinal data to factor types
data = data %>%
  mutate(
    school_year = as.factor(school_year),
    division_name = as.factor(division_name),
    subject = as.factor(subject),
    grade_level = as.factor(grade_level)
  )

# view the dataset
View(data)

# view structure
str(data)

# compute summary statistics
summary(data)

# A.	What are the differences in pass rates between SOL exams of subjects English:Reading
# and Mathematics and grade levels 3 to 8 between the 2014-2015 to 2018-2019 school years?

# Figure 12:
# Distributions of Pass Rates (%) Separated based only on SOL Exam Subject (English:Reading and Mathematics)
# across all available Virginia School Divisions and grade levels (3-8) from the 2014-2015 to 
# 2018-2019 school years
data %>%
  ggplot() + 
  geom_boxplot(mapping = aes(y = pass_rate)) +
  facet_grid(. ~ subject) +
  labs(
    title = "Distributions of Pass Rates (%) Separated based only on SOL Exam Subject 
(English:Reading and Mathematics) from the 2014-2015 to 2018-2019 School Years",
    y = "Pass Rate (%)"
  ) + 
  theme_classic() +
  theme(
    # change font size
    plot.title = element_text(size = 30),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(),
    strip.text = element_text(size = 25)
  )

# compute the medians, iqr's, and skewness coefficients of pass rates between 
# English:Reading and Mathematics SOL exams
data %>%
  group_by(subject) %>%
  summarize(
    median = median(pass_rate, na.rm = TRUE),
    iqr = IQR(pass_rate, na.rm = TRUE),
    skewness = skewness(pass_rate, na.rm = TRUE)
  )

# Figure 13:
# Distributions of Pass Rates (%) Separated based only on SOl Exam Grade Level (3-8) for all Available
# Virginia School Divisions and Subjects (English:Reading and Mathematics) 
# from the 2014-2015 to 2018-2019 school years
data %>%
  ggplot() + 
  geom_boxplot(mapping = aes(y = pass_rate)) + 
  facet_grid(. ~ grade_level) +
  labs(
    title = "Distributions of Pass Rates (%) Separated based only on SOL Exam Grade Level (3 to 8) 
from the 2014-2015 to 2018-2019 School Years",
    y = "Pass Rate (%)"
  ) + 
  theme_classic() +
  theme(
    # change font size
    plot.title = element_text(size = 30),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(),
    strip.text = element_text(size = 25)
  )

# compute the median, iqr's, and skewness coefficients of pass rates by 
# grade level (3-8) SOL exams
data %>%
  group_by(grade_level) %>%
  summarize(
    median = median(pass_rate, na.rm = TRUE),
    iqr = IQR(pass_rate, na.rm = TRUE),
    skewness = skewness(pass_rate, na.rm = TRUE)
  )


# Figure 14:
# Distributions of Pass Rates (%) for SOL Exams of Subjects English:Reading and
# Mathematics and Grade Levels 3 to 8 across Virginia School Divisions from the 
# 2014-2015 to 2018-2019 School Years
data %>%
  ggplot() + 
  geom_boxplot(mapping = aes(y = pass_rate)) + 
  facet_grid(. ~ subject + grade_level) +
  labs(
    title = "Distributions of Pass Rates (%) Separated based on both SOL Exam Subject 
(English:Reading and Mathematics) and Grade Level (3 to 8) from the 2014-2015 to 2018-2019 
School Years",
    y = "Pass Rate (%)"
  ) + 
  theme_classic() +
  theme(
    # change font size
    plot.title = element_text(size = 30),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 20),
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(),
    strip.text = element_text(size = 15)
  )

# compute median, iqr's, and skewness coefficients pass rates by subject and grade level
data %>%
  group_by(subject, grade_level) %>%
  summarize(
    median = median(pass_rate, na.rm = TRUE),
    iqr = IQR(pass_rate, na.rm = TRUE),
    skewness = skewness(pass_rate, na.rm = TRUE)
  )

# Figure 15:
# Median English:Reading and Mathematics SOL exam pass rates across Virginia 
# school divisions and grade levels 3 to 8 over the 2014-2015 to 2018-2019 school years.  
# The median pass rates are labeled with their values.
data %>%
  group_by(school_year, subject) %>%
  summarize(
    med_pass_rate = median(pass_rate, na.rm = TRUE)
  ) %>%
  ggplot() +
  geom_point(mapping = aes(x = school_year, y = med_pass_rate, color = subject), size = 5) +
  geom_line(mapping = aes(x = school_year, y = med_pass_rate, color = subject, group = subject), size = 2) +
  geom_text(mapping = aes(x = school_year, y = med_pass_rate, label = med_pass_rate, vjust = -1.5), 
            size = 7) +
  facet_grid(subject ~ .) +
  labs(
    title = "Median English:Reading and Mathematics SOL Exam Pass Rates 
over the 2014-2015 to 2018-2019 School Years",
    y = "Median Pass Rate (%)",
    x = "School Year"
  ) +
  ylim(c(74, 85)) +
  theme_bw() +
  theme(
    # change font size
    plot.title = element_text(size = 30),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    strip.text = element_text(size = 20),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20)
  )

# Figure 16:
# Median Pass Rates for Grade Levels 3 to 8 SOL Exams across Virginia school divisions
# and subjects English:Reading and Mathematics 
# over the 2014-2015 to 2018-2019 school years
data %>%
  group_by(school_year, grade_level) %>%
  summarize(
    med_pass_rate = median(pass_rate, na.rm = TRUE)
  ) %>%
  ggplot() +
  geom_point(mapping = aes(x = school_year, y = med_pass_rate, color = grade_level), size = 5) +
  geom_line(mapping = aes(x = school_year, y = med_pass_rate, color = grade_level, group = grade_level), size = 2) +
  geom_text(mapping = aes(x = school_year, y = med_pass_rate, label = med_pass_rate, vjust = -1.2,), 
            size = 5) +
  facet_grid(grade_level ~ .) +
  labs(
    x = "School Year",
    y = "Median Pass Rate (%)",
    title = "Median Pass Rates for Grade Levels 3 to 8 SOL Exams over the 
2014-2015 to 2018-2019 School Years"
  ) +
  ylim(c(70, 85)) +
  theme_bw() +
  theme(
    # change font size
    plot.title = element_text(size = 30),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    strip.text = element_text(size = 20),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20)
  )

# Figure 17:
# Median pass rates (%) for subjects English:Reading and Mathematics and grade levels 3 to 8 SOL Exams across Virginia school divisions over the 2014-2015 to 2018-2019 school years.  The median pass rates are labeled with their values.
data %>%
  group_by(school_year, subject, grade_level) %>%
  summarize(
    med_pass_rate = median(pass_rate, na.rm = TRUE)
  ) %>%
  ggplot() + 
  geom_point(mapping = aes(x = school_year, y = med_pass_rate, color = grade_level), size = 4) +
  geom_line(mapping = aes(x = school_year, y = med_pass_rate, color = grade_level, group = grade_level), size = 2) +
  geom_text(mapping = aes(x = school_year, y = med_pass_rate, label = med_pass_rate), size = 5, vjust = -1.5, hjust = 0.75) +
  facet_wrap(~subject + grade_level) +
  labs(
    title = "Median Pass Rates (%) for Subjects English:Reading and Mathematics and Grade Levels
3 to 8 SOL Exams Across Virginia School Divisions over the 2014-2015 to 2018-2019 School Years",
    x = "School Year",
    y = "Median Pass Rate (%)"
  )+
  theme_bw() +
  ylim(c(60, 90)) +
  theme(
    # change font size
    plot.title = element_text(size = 30),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 25),
    axis.text.x = element_text(size = 10),
    strip.text = element_text(size = 20),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20)
  )

# C.	How are median household incomes trending across Virginia school divisions from 2014 to 2018?

# Figure 18:
# Median Household Income (2022 dollar equivalent) across Virginia School Divisions 
# from 2014 to 2018
data %>%
  group_by(med_income_year, division_name) %>%
  summarize(
    med_income = mean(med_income)
  ) %>%
  ggplot() + 
  geom_boxplot(mapping = aes(x = med_income_year, y = med_income, group = med_income_year)) +
  labs(
    title = "Distributions of Median Household Income (2022 dollar equivalent) across Virginia School Divisions 
from 2014 to 2018",
    x = "Year",
    y = "Median Household Income (2022 dollar equivalent)"
  ) +
  theme_classic() +
  theme(
    # change font size
    plot.title = element_text(size = 30),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.title.x = element_text(size = 25),
    axis.text.x = element_text(size = 25)
  )

# compute median and IQR for median household income for each year
data %>%
  group_by(school_year, med_income_year) %>%
  summarize(
    median_med_income = median(med_income),
    IQR = IQR(med_income),
    skewness = skewness(med_income, na.rm = TRUE)
  )

# D. Are  SOL exam pass rates significantly influenced by median household incomes, 
# the exam’s subject (English:Reading and Mathematics), or the exam’s grade level (3 to 8) 
# between the 2014-2015 and 2018-2019 school years?

# Figure 19:
# The Relationships Between SOL Pass Rates and Median Household Income for
# each Subject (English:Reading and Mathematics) and Grade Level (3-8)
data %>%
  ggplot() + 
  geom_point(mapping = aes(x = med_income, y = pass_rate, group = grade_level, color = grade_level)) +
  facet_wrap(~subject + grade_level) +
  labs(
    title = "The Relationships Between SOL Exam Pass Rates and Median Household Income for
each Subject (English:Reading and Mathematics) and Grade Level (3 to 8)",
    x = "Median Household Income (2022 Dollar Equivalent)",
    y = "Pass Rate (%)"
  ) +
  theme_bw() +
  theme(
    # change font size
    plot.title = element_text(size = 30),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 25),
    axis.text.x = element_text(size = 15),
    strip.text = element_text(size = 20),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20)
  )

# The Relationships Between SOL Pass Rates and Median Household Income
data %>%
  ggplot() +
  geom_point(mapping = aes(x = med_income, y = pass_rate)) +
  labs(
    title = "The Relationship Between SOL Exam Pass Rates (%) and Median Household Income",
    x = "Median Household Income (2022 dollar equivalent)",
    y = "Pass Rate (%)"
  ) +
  theme_bw() + 
  theme(
    # change font size
    plot.title = element_text(size = 35),
    axis.title.y = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    axis.text.x = element_text(size = 30)
  )

# Pearson correlation between pass rate and median household income
round(cor(data$pass_rate, data$med_income, use = "complete.obs"), 2)

# Figure 20:
# The statistics of the multiple regression fit with the following predictors: 
# median household income, subject, grade level, and an interaction term between subject and grade level.
# remove NA rows
data_no_na = na.omit(data)
model = lm(pass_rate ~ med_income + subject + grade_level + subject:grade_level, data = data_no_na)
summary(model)


# E.	During the 2018-2019 school year, the latest complete data available, 
# what Virginia school divisions have the highest and lowest median (typical) 
# pass rates on the Mathematics and English:Reading SOL exams across all grade levels 
# as well as lowest and highest median (typical) household income?

# Figure 22:
# The 15 school divisions with the highest and lowest median pass rates on the English:Reading SOL exam
reading2018 = data %>%
  filter(
    subject == "english:reading" & school_year == "2018-2019"
  ) %>%
  group_by(division_name) %>%
  summarize(
    med_pass_rate = median(pass_rate, na.rm = TRUE)
  ) %>%
  arrange(desc(med_pass_rate))

head(reading2018, 15)
tail(reading2018, 15)

# Figure 23:
# The 15 school divisions with the highest and lowest median pass rates on the Mathematics SOL exam
math2018 = data %>%
  filter(
    subject == "mathematics" & school_year == "2018-2019"
  ) %>%
  group_by(division_name) %>%
  summarize(
    med_pass_rate = median(pass_rate, na.rm = TRUE)
  ) %>%
  arrange(desc(med_pass_rate))

head(math2018, 15)
tail(math2018, 15)

# Figure 24:
# The 15 school divisions with the highest and lowest median household incomes are shown
income2018 = data %>%
  filter(
    school_year == "2018-2019"
  ) %>%
  group_by(division_name) %>%
  summarize(
    med_income = median(med_income, na.rm = TRUE)
  ) %>%
  arrange(desc(med_income))

head(income2018, 15)
tail(income2018, 15)