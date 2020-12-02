library(tidyverse)
library(utils)
library(RColorBrewer)

## Load educational attainment data from the last five years
education_data <- read.csv("EducationalAttainmentData.csv")
education_data$NonHighSchoolGrad <- rowSums(education_data[,3:9])
education_data$PostSecondary <- rowSums(education_data[,10:13])
education_data$FourYearCollegeGraduate <- education_data[,14]
education_data$GraduateDegree <- rowSums(education_data[,15:17])
## Add column for count of college graduates

## Load US population data, caluclate populations by race
us_population_data <- read.csv("USPopulationData.csv")
us_pop_by_year <- select(us_population_data, TotalPopulation)
hispanic_pop <- select(us_population_data, Hispanic) * us_pop_by_year
black_pop <- select(us_population_data, Black) * us_pop_by_year
white_pop <- select(us_population_data, White) * us_pop_by_year
asian_pop <- select(us_population_data, Asian) * us_pop_by_year

## Calculate number and percentage of non high school grads by race
## Numbers are in thousands, so need to adjust
white_non_high_school_grads <- education_data %>% 
  filter(Race == "White") %>% 
  select(NonHighSchoolGrad) * 1000
white_non_high_school_grad_percentage <- white_non_high_school_grads / white_pop
black_non_high_school_grads <- education_data %>% 
  filter(Race == "Black") %>% 
  select(NonHighSchoolGrad) * 1000
black_non_high_school_grad_percentage <- black_non_high_school_grads / black_pop
asian_non_high_school_grads <- education_data %>% 
  filter(Race == "Asian") %>% 
  select(NonHighSchoolGrad) * 1000
asian_non_high_school_grad_percentage <- asian_non_high_school_grads / asian_pop
hispanic_non_high_school_grads <- education_data %>% 
  filter(Race == "Hispanic") %>% 
  select(NonHighSchoolGrad) * 1000
hispanic_non_high_school_grad_percentage <- hispanic_non_high_school_grads / hispanic_pop

## Calculate number and percentage of high school grads and post secondary 
## education
## Numbers are in thousands, so need to adjust
white_post_secondary <- education_data %>% 
  filter(Race == "White") %>% 
  select(PostSecondary) * 1000
white_post_secondary_percentage <- white_post_secondary / white_pop
black_post_secondary <- education_data %>% 
  filter(Race == "Black") %>% 
  select(PostSecondary) * 1000
black_post_secondary_percentage <- black_post_secondary / black_pop
asian_post_secondary <- education_data %>% 
  filter(Race == "Asian") %>% 
  select(PostSecondary) * 1000
asian_post_secondary_percentage <- asian_post_secondary / asian_pop
hispanic_post_secondary <- education_data %>% 
  filter(Race == "Hispanic") %>% 
  select(PostSecondary) * 1000
hispanic_post_secondary_percentage <- hispanic_post_secondary / hispanic_pop

## Calculate number and percentage of four year college degrees
## Numbers are in thousands, so need to adjust
white_four_year_college <- education_data %>% 
  filter(Race == "White") %>% 
  select(FourYearCollegeGraduate) * 1000
white_four_year_college_percentage <- white_four_year_college / white_pop
black_four_year_college <- education_data %>% 
  filter(Race == "Black") %>% 
  select(FourYearCollegeGraduate) * 1000
black_four_year_college_percentage <- black_four_year_college / black_pop
asian_four_year_college <- education_data %>% 
  filter(Race == "Asian") %>% 
  select(FourYearCollegeGraduate) * 1000
asian_four_year_college_percentage <- asian_four_year_college / asian_pop
hispanic_four_year_college <- education_data %>% 
  filter(Race == "Hispanic") %>% 
  select(FourYearCollegeGraduate) * 1000
hispanic_four_year_college_percentage <- hispanic_four_year_college / hispanic_pop

## Calculate number and percentage of graduate degrees
## Numbers are in thousands, so need to adjust
white_grad_degrees <- education_data %>% 
  filter(Race == "White") %>% 
  select(GraduateDegree) * 1000
white_grad_degrees_percentage <- white_grad_degrees / white_pop
black_grad_degrees <- education_data %>% 
  filter(Race == "Black") %>% 
  select(GraduateDegree) * 1000
black_grad_degrees_percentage <- black_grad_degrees / black_pop
asian_grad_degrees <- education_data %>% 
  filter(Race == "Asian") %>% 
  select(GraduateDegree) * 1000
asian_grad_degrees_percentage <- asian_grad_degrees / asian_pop
hispanic_grad_degrees <- education_data %>% 
  filter(Race == "Hispanic") %>% 
  select(GraduateDegree) * 1000
hispanic_grad_degrees_percentage <- hispanic_grad_degrees / hispanic_pop

## Prepare data for bar plot
bar_plot_non_high_school_grad <- 
              matrix(c(as.vector(unlist(asian_non_high_school_grad_percentage)),
              as.vector(unlist(white_non_high_school_grad_percentage)),
              as.vector(unlist(black_non_high_school_grad_percentage)),
              as.vector(unlist(hispanic_non_high_school_grad_percentage))),
              ncol = 4, byrow = TRUE)
colnames(bar_plot_non_high_school_grad) <- c(2016, 2017, 2018, 2019)
rownames(bar_plot_non_high_school_grad) <- c("Asian", "White", "Black", "Hispanic")

barplot(bar_plot_non_high_school_grad, 
        main = "Percentage of Population without High School Degrees",
        xlab = "Year", ylab = "Percentage", 
        args.legend = list(x = 'topright', bty = 'n', inset = c(0.10, -0.05)),
        col = brewer.pal(n = 4, name = "YlOrRd"), 
        legend = rownames(bar_plot_non_high_school_grad), beside = TRUE)

bar_plot_post_secondary <- 
  matrix(c(as.vector(unlist(asian_post_secondary_percentage)),
           as.vector(unlist(white_post_secondary_percentage)),
           as.vector(unlist(black_post_secondary_percentage)),
           as.vector(unlist(hispanic_post_secondary_percentage))),
           ncol = 4, byrow = TRUE)

colnames(bar_plot_post_secondary) <- c(2016, 2017, 2018, 2019)
rownames(bar_plot_post_secondary) <- c("Asian", "White", "Black", "Hispanic")

barplot(bar_plot_post_secondary, 
        main = "Percentage of Population with Post Secondary Education",
        xlab = "Year", ylab = "Percentage", 
        args.legend = list(x = 'topright', bty = 'n', inset = c(-0.075, -0.05)),
        col = brewer.pal(n = 4, name = "YlOrRd"), 
        legend = rownames(bar_plot_post_secondary), beside = TRUE)

bar_plot_four_year_graduate <- 
  matrix(c(as.vector(unlist(asian_four_year_college_percentage)),
           as.vector(unlist(white_four_year_college_percentage)),
           as.vector(unlist(black_four_year_college_percentage)),
           as.vector(unlist(hispanic_four_year_college_percentage))),
         ncol = 4, byrow = TRUE)

colnames(bar_plot_four_year_graduate) <- c(2016, 2017, 2018, 2019)
rownames(bar_plot_four_year_graduate) <- c("Asian", "White", "Black", "Hispanic")

barplot(bar_plot_four_year_graduate, 
        main = "Percentage of Population with Four Year College Degree",
        xlab = "Year", ylab = "Percentage", 
        args.legend = list(x = 'topright', bty = 'n', inset = c(-0.025, -0.05)),
        col = brewer.pal(n = 4, name = "YlOrRd"), 
        legend = rownames(bar_plot_four_year_graduate), beside = TRUE)

bar_plot_graduate_degree <- 
  matrix(c(as.vector(unlist(asian_grad_degrees_percentage)),
           as.vector(unlist(white_grad_degrees_percentage)),
           as.vector(unlist(black_grad_degrees_percentage)),
           as.vector(unlist(hispanic_grad_degrees_percentage))),
         ncol = 4, byrow = TRUE)

colnames(bar_plot_graduate_degree) <- c(2016, 2017, 2018, 2019)
rownames(bar_plot_graduate_degree) <- c("Asian", "White", "Black", "Hispanic")

barplot(bar_plot_graduate_degree, 
        main = "Percentage of Population with Graduate Degree",
        xlab = "Year", ylab = "Percentage", 
        args.legend = list(x = 'topright', bty = 'n', inset = c(-0.025, -0.05)),
        col = brewer.pal(n = 4, name = "YlOrRd"), 
        legend = rownames(bar_plot_graduate_degree), beside = TRUE)
