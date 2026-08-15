# Name: Matthew Vu
# Due Date: 12/03/2023

# create the database
CREATE DATABASE va_divisions;

# use the database 
USE va_divisions;

# create the table
CREATE TABLE sol_income(
ID INT UNSIGNED NOT NULL AUTO_INCREMENT,
school_year VARCHAR(24),
division_name VARCHAR(128),
subject VARCHAR(64),
grade_level INT UNSIGNED,
pass_rate  FLOAT,
med_income_year INT,
med_income FLOAT,
PRIMARY KEY (ID)
);

# view the dataset
SELECT * FROM sol_income;

# -----
# A.	What are the differences in pass rates between SOL exams of subjects English:Reading and Mathematics and 
# grade levels 3 to 8 between the 2014-2015 to 2018-2019 school years?

# Figure 12:
# compute min, mean, max, and standard deviations of pass rates between English:Reading and Mathematics SOL exams
SELECT subject, MIN(pass_rate), 
ROUND(AVG(pass_rate), 2) AS 'AVG(pass_rate)', 
MAX(pass_rate), 
ROUND(STD(pass_rate), 2) AS 'STD(pass_rate)'
FROM sol_income
GROUP BY subject;

# Figure 13:
# compute min, mean, max, and standard deviations of pass rates by grade level (3-8) SOL exams
SELECT grade_level, MIN(pass_rate), 
ROUND(AVG(pass_rate), 2) AS 'AVG(pass_rate)', 
MAX(pass_rate), 
ROUND(STD(pass_rate), 2) AS 'STD(pass_rate)'
FROM sol_income
GROUP BY grade_level;

# Figure 14:
# compute min, mean, max, and standard deviations of pass rates by subject and grade level
SELECT subject, grade_level, MIN(pass_rate), 
ROUND(AVG(pass_rate), 2) AS 'AVG(pass_rate)', 
MAX(pass_rate), 
ROUND(STD(pass_rate), 2) AS 'STD(pass_rate)'
FROM sol_income
GROUP BY subject, grade_level;

# -----
# C.	How are median household incomes trending across Virginia school divisions from 2014 to 2018?
# Figure 18:
# compute min, average, max, and standard deviations of median household incomes for each year
SELECT school_year, 
med_income_year, 
MIN(med_income), 
round(AVG(med_income), 2) AS 'AVG(med_income)', 
MAX(med_income), 
round(STD(med_income), 2) AS 'STD(med_income)'
FROM sol_income
GROUP BY school_year, med_income_year;