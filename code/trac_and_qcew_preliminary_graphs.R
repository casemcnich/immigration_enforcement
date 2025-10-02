# this file is not integral to the work
# it includes some peliminary graphs on the merged QCEW, TRAC, and NHGIS data
# the data used in this file are
# last modified on 10.01.25

# prerequisites ----
#loading packages
library('stringr')
library('tidyr')
library('dplyr')
library('usdata')
library(lubridate)
library(vtable)
library(sf)
library(zoo)
library(ggplot2)

# loading data ----
load("trac_employment.Rdata")
load("trac_wages.Rdata")

# for the sake of clarity I will call all - construction and restaurant as "other" 
#employment calculation
trac_employment$monthly_emplvl_other = trac_employment$monthly_emplvl_all - trac_employment$monthly_emplvl_const - trac_employment$monthly_emplvl_rest

# building out additional variables
#creating a yearmon variable and qtr_mon variable
trac_employment$year_mon <- as.yearmon(paste(trac_employment$year, " ", trac_employment$month), "%Y %m")
trac_wages$year_qtr <- as.yearqtr(paste(trac_wages$year, " ", trac_wages$qtr), "%Y %q")

#switching  counts to numeric
trac_employment$count <- as.numeric(trac_employment$count)
trac_wages$total_value <- as.numeric(trac_wages$total_value)

#making demeaned version, z score version, and inverse hyperbolic sine version
trac_employment = trac_employment %>%
  group_by(area_title) %>%
  #z score  version
  mutate(z_score_other = scale(monthly_emplvl_other)) %>%
  mutate(z_score_rest = scale(monthly_emplvl_rest)) %>%  
  mutate(z_score_const = scale(monthly_emplvl_const)) %>%
  #demeaned version
  mutate(demeaned_other = monthly_emplvl_other - mean(monthly_emplvl_other)) %>%
  mutate(demeaned_rest = monthly_emplvl_rest - mean(monthly_emplvl_rest)) %>%
  mutate(demeaned_const = monthly_emplvl_const - mean(monthly_emplvl_const))%>%
  #ihs version
  mutate(ihs_emp_other = log(monthly_emplvl_other + ((monthly_emplvl_other^2 +1)^0.5))) %>%
  mutate (ihs_emp_rest = log(monthly_emplvl_rest + ((monthly_emplvl_rest^2 +1)^0.5))) %>%
  mutate( ihs_emp_const = log(monthly_emplvl_const + ((monthly_emplvl_const^2 +1)^0.5))) %>%
  mutate( ihs_count = log(count + ((count^2 +1)^0.5))) %>%
  mutate( ihs_non_cap = log(no_cap_arrests + ((no_cap_arrests^2 +1)^0.5)))


#making demeaned version, z score version, and inverse hyperbolic sine version
trac_wages = trac_wages %>%
  group_by(area_title) %>%
  #z score  version
  mutate(z_score_other = scale(avg_wkly_wage_other)) %>%
  mutate(z_score_rest = scale(avg_wkly_wage_rest)) %>%  
  mutate(z_score_const = scale(avg_wkly_wage_const)) %>%
  #demeaned version
  mutate(demeaned_other = avg_wkly_wage_other - mean(avg_wkly_wage_other)) %>%
  mutate(demeaned_rest = avg_wkly_wage_rest - mean(avg_wkly_wage_rest)) %>%
  mutate(demeaned_const = avg_wkly_wage_const - mean(avg_wkly_wage_const))%>%
  #ihs version
  mutate(ihs_wage_other = log(avg_wkly_wage_other + ((avg_wkly_wage_other^2 +1)^0.5))) %>%
  mutate (ihs_wage_rest = log(avg_wkly_wage_rest + ((avg_wkly_wage_rest^2 +1)^0.5))) %>%
  mutate( ihs_wage_const = log(avg_wkly_wage_const + ((avg_wkly_wage_const^2 +1)^0.5))) %>%
  mutate( ihs_wage_count = log(total_value + ((total_value^2 +1)^0.5))) %>%
  mutate( ihs_no_cap = log(total_nocap + ((total_nocap^2 +1)^0.5)))

#################### plotting employment - full dataset - subset to include counties that had at least one arrest ########################################
# subset to counties that ever had an arrest
trac_employment <- trac_employment %>%
  group_by(county) %>%
  filter(any(count >= 1))

trac_wages <- trac_wages %>%
  group_by(county) %>%
  filter(any(total_value >=1))

##### making subsets #####
#mkaing subsets
trac_wages_treatment <- trac_wages %>%
  group_by(area_title) %>%
  filter(any(total_value >= 1))

trac_employ_treatment <- trac_employment %>%
  group_by(area_title) %>%
  filter(any(count >= 1))
########* employment, z score ##############
ggplot(subset(trac_employment)) +
  aes(count, z_score_other) + geom_point(size = .5) +  
  geom_smooth(method = "lm", se = FALSE) +  # Linear regression line, no confidence interval
  labs(x = "Arrest Count", y = "Z-score Employment in Other Industries",  
    title = "Arrest Count vs Other Industries"
  ) + 
  theme_bw() + stat_smooth(
    method = "lm")
#regression
summary(lm(z_score_other ~ count, data = trac_employment))


ggplot(subset(trac_employment)) +
  aes(count, z_score_rest) + geom_point(size = .5) +  
  geom_smooth(method = "lm", se = FALSE) +  # Linear regression line, no confidence interval
  labs(x = "Arrest Count", y = "Z-score Employment Restaurants",  
       title = "Arrest Count vs Restaurants"
  ) + 
  theme_bw() + stat_smooth(
    method = "lm")
#regression
summary(lm(z_score_rest ~ count, data = trac_employment))


ggplot(subset(trac_employment)) +
  aes(count, z_score_const) + geom_point(size = .5) +  
  geom_smooth(method = "lm", se = FALSE) +  # Linear regression line, no confidence interval
  labs(x = "Arrest Count", y = "Z-score Employment construction",  
       title = "Arrest Count vs Construction"
  ) + 
  theme_bw() + stat_smooth(
    method = "lm")
#regression
summary(lm(z_score_const ~ count, data = trac_employment))

########* employment, demeaned ##############
ggplot(subset(trac_employment )) +
  aes(count, demeaned_other ) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm") +
  labs(
    x = "arrest count", 
    y = "demeaned employment in other industries",  
    title = "arrest count vs other industries" ) + theme_bw()
#regression
summary(lm(demeaned_other ~ count, data = trac_employment))

ggplot(subset(trac_employment)) +
  aes(count, demeaned_rest) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm") +
  labs(
    x = "arrest count", 
    y = "demeaned employment in restaurants",  
    title = "arrest count vs restaurants employment") + theme_bw()
#regression
summary(lm(demeaned_rest ~ count, data = trac_employment))

ggplot(subset(trac_employment)) +
  aes(count, demeaned_const) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm") +
  labs(
    x = "arrest count", 
    y = "demeaned employment in construction",  
    title = "arrest count vs construction employment" ) + theme_bw()
#regression
summary(lm(demeaned_const ~ count, data = trac_employment))

########* employment, log log  ##############
ggplot(subset(trac_employment, count > 0 )) +
  aes(ihs_count, ihs_emp_other ) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm") +
  labs(
    x = "logged arrest count", 
    y = "logged employment in other industries",  
    title = "arrest count vs other industries" ) + theme_bw()
summary(lm(ihs_emp_other ~ ihs_count, data = trac_employ_treatment))
summary(lm(ihs_emp_rest ~ ihs_count, data = trac_employ_treatment))
summary(lm(ihs_emp_const ~ ihs_count, data = trac_employ_treatment))


ggplot(subset(trac_employment, count > 0 )) +
  aes(ihs_count, ihs_emp_rest) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm") +
  labs(
    x = "logged arrest count", 
    y = "logged employment in restaurants",  
    title = "arrest count vs restaurants employment") + theme_bw()

ggplot(subset(trac_employment, count > 0 )) +
  aes(ihs_count, ihs_emp_const) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm") +
  labs(
    x = "logged arrest count", 
    y = "logged employment in construction",  
    title = "arrest count vs construction employment" ) + theme_bw()
mean(trac_wages$count)
################ plotting wages - full dataset ##################################
########*  wages, z score ##############
ggplot(subset(trac_wages)) +
  aes(total_value, z_score_other) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm") +
  labs(
    x = "arrest count", 
    y = "z score wages in other industries",  
    title = "arrest count vs other industries" ) + theme_bw()
summary(lm(z_score_other ~ total_value, data = trac_wages))

ggplot(subset(trac_wages)) +
  aes(total_value, z_score_rest) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm") +
  labs(
    x = "arrest count", 
    y = "z score wages in restaurants",  
    title = "arrest count vs restaurants wages") + theme_bw()
summary(lm(z_score_rest ~ total_value, data = trac_wages))

ggplot(subset(trac_wages)) +
  aes(total_value, z_score_const) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm") +
  labs(
    x = "arrest count", 
    y = "z score wages in construction",  
    title = "arrest count vs construction wages" ) + theme_bw()
summary(lm(z_score_const ~ total_value, data = trac_wages))

########* wages, demeaned ##############
ggplot(subset(trac_wages)) +
  aes(total_value, demeaned_other ) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm") +
  labs(
    x = "arrest count", 
    y = "demeaned wage in other industries",  
    title = "arrest count vs other industries" ) + theme_bw()
summary(lm(demeaned_other ~ total_value, data = trac_wages))

ggplot(subset(trac_wages)) +
  aes(total_value, demeaned_rest) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm") +
  labs(
    x = "arrest count", 
    y = "demeaned wage in restaurants",  
    title = "arrest count vs restaurants wage") + theme_bw()
summary(lm(demeaned_rest ~ total_value, data = trac_wages))

ggplot(subset(trac_wages)) +
  aes(total_value, demeaned_const) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm") +
  labs(
    x = "arrest count", 
    y = "demeaned wage in construction",  
    title = "arrest count vs construction wage" ) + theme_bw()
summary(lm(demeaned_const ~ total_value, data = trac_wages))


########* wages, log log  ##############
ggplot(subset(trac_wages, total_value > 0 )) +
  aes(ihs_wage_count, ihs_wage_other ) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm") +
  labs(
    x = "logged arrest count", 
    y = "logged wage in other industries",  
    title = "arrest count vs other industries" ) + theme_bw()

ggplot(subset(trac_wages, total_value > 0 )) +
  aes(ihs_wage_count, ihs_wage_rest) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm") +
  labs(
    x = "logged arrest count", 
    y = "logged wage in restaurants",  
    title = "arrest count vs restaurants wage") + theme_bw()

ggplot(subset(trac_wages, total_value > 0 )) +
  aes(ihs_wage_count, ihs_wage_const) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm") +
  labs(
    x = "logged arrest count", 
    y = "logged wage in construction",  
    title = "arrest count vs construction wage" ) + theme_bw()

##### plotting wages, only treated subset #####
###* treated wages, z score ####
ggplot(trac_wages_treatment) +
  aes(total_value, z_score_other) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm") +
  labs(
    x = "arrest count", 
    y = "z score wages in other industries",  
    title = "arrest count vs other industries" ) + theme_bw()

ggplot(trac_wages_treatment) +
  aes(total_value, z_score_rest) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm") +
  labs(
    x = "arrest count", 
    y = "z score wages in restaurants",  
    title = "arrest count vs restaurants wages") + theme_bw()

ggplot(trac_wages_treatment) +
  aes(total_value, z_score_const) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm") +
  labs(
    x = "arrest count", 
    y = "z score wages in construction",  
    title = "arrest count vs construction wages" ) + theme_bw()

########* treated wages, demeaned ##############
ggplot(trac_wages_treatment) +
  aes(total_value, demeaned_other ) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm") +
  labs(
    x = "arrest count", 
    y = "demeaned wage in other industries",  
    title = "arrest count vs other industries" ) + theme_bw()

ggplot(trac_wages_treatment) +
  aes(total_value, demeaned_rest) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm") +
  labs(
    x = "arrest count", 
    y = "demeaned wage in restaurants",  
    title = "arrest count vs restaurants wage") + theme_bw()

ggplot(trac_wages_treatment) +
  aes(total_value, demeaned_const) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm") +
  labs(
    x = "arrest count", 
    y = "demeaned wage in construction",  
    title = "arrest count vs construction wage" ) + theme_bw()

##### plotting employment, only treated subset #####
###* treated employment, z score ####

ggplot(trac_employ_treatment) +
  aes(count, z_score_other) + geom_point(size = .5) +  
  geom_smooth(method = "lm", se = FALSE) +  # Linear regression line, no confidence interval
  labs(x = "Arrest Count", y = "Z-score Employment in Other Industries",  
       title = "Arrest Count vs Other Industries"
  ) + 
  theme_bw() + stat_smooth(
    method = "lm")

ggplot(trac_employ_treatment) +
  aes(count, z_score_rest) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm") +
  labs(
    x = "arrest count", 
    y = "z score employment in restaurants",  
    title = "arrest count vs restaurants employment") + theme_bw()

ggplot(trac_employ_treatment) +
  aes(count, z_score_const) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm") +
  labs(
    x = "arrest count", 
    y = "z score employment in construction",  
    title = "arrest count vs construction employment" ) + theme_bw()

###* treated employment, demeaned ####
ggplot(trac_employ_treatment) +
  aes(count, demeaned_other ) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm") +
  labs(
    x = "arrest count", 
    y = "demeaned employment in other industries",  
    title = "arrest count vs other industries" ) + theme_bw()

ggplot(trac_employ_treatment) +
  aes(count, demeaned_rest) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm") +
  labs(
    x = "arrest count", 
    y = "demeaned employment in restaurants",  
    title = "arrest count vs restaurants employment") + theme_bw()

ggplot(trac_employ_treatment) +
  aes(count, demeaned_const) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm") +
  labs(
    x = "arrest count", 
    y = "demeaned employment in construction",  
    title = "arrest count vs construction employment" ) + theme_bw()

#model
summary(lm(demeaned_other ~ count, trac_employ_treatment))

trac_employ_treatment <- trac_employ_treatment %>% 
  group_by(area_title) %>%
  mutate(change_emp_other = monthly_emplvl_other - lag(monthly_emplvl_other)) %>%
  mutate(change_emp_rest = monthly_emplvl_rest - lag(monthly_emplvl_rest)) %>%
  mutate(change_emp_const = monthly_emplvl_const - lag(monthly_emplvl_const)) 

#dropping super strange numbers from censoring
trac_employ_treatment <- subset(trac_employ_treatment, change_emp_other <= 10000)
trac_employ_treatment <- subset(trac_employ_treatment, change_emp_rest <= 10000)
trac_employ_treatment <- subset(trac_employ_treatment, change_emp_const <= 10000)

trac_wages_treatment <- trac_wages_treatment %>% 
  group_by(area_title) %>%
  mutate(change_wage_other = avg_wkly_wage_other - lag(avg_wkly_wage_other)) %>%
  mutate(change_wage_rest = avg_wkly_wage_rest - lag(avg_wkly_wage_rest)) %>%
  mutate(change_wage_const = avg_wkly_wage_const - lag(avg_wkly_wage_const)) 

#dropping super strange numbers from censoring
trac_wages_treatment <- subset(trac_wages_treatment, change_wage_other <= 5000)
trac_wages_treatment <- subset(trac_wages_treatment, change_wage_rest <= 5000)
trac_wages_treatment <- subset(trac_wages_treatment, change_wage_const <= 5000)

##### plotting CHANGE in employment and arrests, only treated subset #####
#### employment ####
ggplot(trac_employ_treatment) +
  aes(x = count, y = change_emp_other) + 
  geom_point(size = .5) +  # Scatterplot for the first count variable
  geom_smooth(aes(x = count), method = "lm", se = FALSE, color = "blue") +  # Linear regression line for count
  geom_smooth(aes(x = no_cap_arrests), method = "lm", se = FALSE, color = "red") +  # Linear regression line for count_new
  labs(x = "Arrest Counts", 
       y = "Change in Employment in Other Industries",  
       title = "Arrest Count vs Change in Employment in Other Industries") + 
  theme_bw() + 
  coord_cartesian(xlim = c(0, 1000), ylim = c(-1000, 5000))  # Adjust x and y limits for zoom

summary(lm(change_emp_other ~ count, trac_employ_treatment))
summary(lm(change_emp_other ~ no_cap_arrests, trac_employ_treatment))

ggplot(trac_employ_treatment) +
  aes(x = count, y = change_emp_rest) + 
  geom_point(size = .5) +  # Scatterplot for the first count variable
  geom_smooth(aes(x = count), method = "lm", se = FALSE, color = "blue") +  # Linear regression line for count
  geom_smooth(aes(x = no_cap_arrests), method = "lm", se = FALSE, color = "red") +  # Linear regression line for count_new
  labs(x = "Arrest Counts", 
       y = "Change in Employment in Rest",  
       title = "Arrest Count vs Change in Employment in Rest") + 
  theme_bw() + 
  coord_cartesian(xlim = c(0, 1000), ylim = c(-1000, 5000))  # Adjust x and y limits for zoom

summary(lm(change_emp_rest ~ count, trac_employ_treatment))
summary(lm(change_emp_rest ~ no_cap_arrests, trac_employ_treatment))

ggplot(trac_employ_treatment) +
  aes(x = count, y = change_emp_const) + 
  geom_point(size = .5) +  # Scatterplot for the first count variable
  geom_smooth(aes(x = count), method = "lm", se = FALSE, color = "blue") +  # Linear regression line for count
  geom_smooth(aes(x = no_cap_arrests), method = "lm", se = FALSE, color = "red") +  # Linear regression line for count_new
  labs(x = "Arrest Counts", 
       y = "Change in Employment in Const",  
       title = "Arrest Count vs Change in Employment in Const") + 
  theme_bw() + 
  coord_cartesian(xlim = c(0, 1000), ylim = c(-1000, 5000))  # Adjust x and y limits for zoom

summary(lm(change_emp_const ~ count, trac_employ_treatment))
summary(lm(change_emp_const ~ no_cap_arrests, trac_employ_treatment))

###### * wages ########
ggplot(trac_wages_treatment) +
  aes(x = total_value, y = change_wage_other) + 
  geom_point(size = .5) +  # Scatterplot for the first count variable
  geom_smooth(aes(x = total_value), method = "lm", se = FALSE, color = "blue") +  # Linear regression line for count
  geom_smooth(aes(x = total_nocap), method = "lm", se = FALSE, color = "red") +  # Linear regression line for count_new
  labs(x = "Arrest Counts", 
       y = "Change in Wage in Other Industries",  
       title = "Arrest Count vs Change in Wage in Other Industries") + 
  theme_bw() + 
  coord_cartesian(xlim = c(0, 1000), ylim = c(-500, 500))  # Adjust x and y limits for zoom

summary(lm(change_wage_other ~ total_value, trac_wages_treatment))
summary(lm(change_wage_other ~ total_nocap, trac_wages_treatment))

ggplot(trac_wages_treatment) +
  aes(x = total_value, y = change_wage_rest) + 
  geom_point(size = .5) +  # Scatterplot for the first count variable
  geom_smooth(aes(x = total_value), method = "lm", se = FALSE, color = "blue") +  # Linear regression line for count
  geom_smooth(aes(x = total_nocap), method = "lm", se = FALSE, color = "red") +  # Linear regression line for count_new
  labs(x = "Arrest Counts", 
       y = "Change in Wage in Rest",  
       title = "Arrest Count vs Change in Wage in Rest") + 
  theme_bw() + 
  coord_cartesian(xlim = c(0, 1000), ylim = c(-300, 300))  # Adjust x and y limits for zoom

summary(lm(change_wage_rest ~ total_value, trac_wages_treatment))
summary(lm(change_wage_rest ~ total_nocap, trac_wages_treatment))


ggplot(trac_wages_treatment) +
  aes(x = total_value, y = change_wage_const) + 
  geom_point(size = .5) +  # Scatterplot for the first count variable
  geom_smooth(aes(x = total_value), method = "lm", se = FALSE, color = "blue") +  # Linear regression line for count
  geom_smooth(aes(x = total_nocap), method = "lm", se = FALSE, color = "red") +  # Linear regression line for count_new
  labs(x = "Arrest Counts", 
       y = "Change in Wage in Const",  
       title = "Arrest Count vs Change in Wage in Const") + 
  theme_bw() + 
  coord_cartesian(xlim = c(0, 1000), ylim = c(-300, 300))  # Adjust x and y limits for zoom

summary(lm(change_wage_const ~ total_value, trac_wages_treatment))
summary(lm(change_wage_const ~ total_nocap, trac_wages_treatment))

############ quadratics ###############
########### * employment #############
ggplot(subset(trac_employ_treatment )) +
  aes(count, demeaned_other ) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm", formula = y ~ I(x^2)) +
  labs(
    x = "arrest count", 
    y = "demeaned employment in other industries",  
    title = "arrest count vs other industries" ) + theme_bw()
summary(lm(demeaned_other ~ count^2, trac_employ_treatment))

ggplot(subset(trac_employ_treatment )) +
  aes(count, demeaned_rest ) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm", formula = y ~ I(x^2)) +
  labs(
    x = "arrest count", 
    y = "demeaned employment in other industries",  
    title = "arrest count vs other industries" ) + theme_bw()
summary(lm(demeaned_rest ~ count^2, trac_employ_treatment))

ggplot(subset(trac_employ_treatment )) +
  aes(count, demeaned_const ) + 
  geom_point(size = .5) +
  geom_smooth(method = "lm", formula = y ~ I(x^2)) +
  labs(
    x = "arrest count", 
    y = "demeaned employment in other industries",  
    title = "arrest count vs other industries" ) + theme_bw()
summary(lm(demeaned_const ~ count^2, trac_employ_treatment))

############ pulling out year_mon fixed effects ###############
########### * employment #############
#adding a yearmon column
trac_employ_treatment$yearmon <- as.yearmon(paste(trac_employ_treatment$year, trac_employ_treatment$month, sep = "-"))

model <- lm(change_emp_rest ~ count + factor(year_mon), data = trac_employ_treatment)
# Generate the predicted values based on the model
trac_employ_treatment$predicted_rest <- predict(model)
slope <- coef(model)["count"]
slope_label <- paste0("Slope = ", round(slope, 3))
# Now, create the plot with a linear regression line using predicted values
ggplot(trac_employ_treatment) +
  aes(x = count, y = predicted_rest) + 
  geom_point(size = .5) +  # Scatterplot for the count variable
  geom_smooth(aes(y = predicted_rest), color = "blue", linewidth = 1) +  # Linear regression line with fixed effects
  annotate("text", x = Inf, y = -Inf, label = slope_label, hjust = 1.1, vjust = -1.1, size = 4) +
  labs(x = "Arrest Counts", 
       y = "Change in Employment in Rest",  
       title = "Arrest Count vs Change in Employment in Rest with Time Fixed Effect") + 
  theme_bw() 

model <- lm(change_emp_const ~  count + factor(yearmon), data = trac_employ_treatment)
summary(model)
# Generate the predicted values based on the model
trac_employ_treatment$predicted_const <- predict(model)
slope <- coef(model)["count"]
slope_label <- paste0("Slope = ", round(slope, 3))
# Now, create the plot with a linear regression line using predicted values
ggplot(trac_employ_treatment) +
  aes(x = count, y = predicted_const) + 
  geom_point(size = .5) +  # Scatterplot for the count variable
  geom_smooth(aes(y = predicted_const), color = "blue", linewidth = 1) +  # Linear regression line with fixed effects
  annotate("text", x = Inf, y = -Inf, label = slope_label, hjust = 1.1, vjust = -1.1, size = 4) +
  labs(x = "Arrest Counts", 
       y = "Change in Employment in Rest",  
       title = "Arrest Count vs Change in Employment in Const with Time Fixed Effect") + 
  theme_bw() 

model <- lm(change_emp_other ~  count + factor(yearmon), data = trac_employ_treatment)
summary(model)
# Generate the predicted values based on the model
trac_employ_treatment$predicted_oth <- predict(model)
slope <- coef(model)["count"]
slope_label <- paste0("Slope = ", round(slope, 3))
# Now, create the plot with a linear regression line using predicted values
ggplot(trac_employ_treatment) +
  aes(x = count, y = predicted_oth) + 
  geom_point(size = .5) +  # Scatterplot for the count variable
  geom_smooth(aes(y = predicted_oth), color = "blue", linewidth = 1) +  # Linear regression line with fixed effects
  annotate("text", x = Inf, y = -Inf, label = slope_label, hjust = 1.1, vjust = -1.1, size = 4) +
  labs(x = "Arrest Counts", 
       y = "Change in Employment in Rest",  
       title = "Arrest Count vs Change in Employment in Other industries with Time Fixed Effect") + 
  theme_bw() 

########### * wages #############
model <- lm(change_wage_rest ~ total_value + factor(year_qtr), data = trac_wages_treatment)
# Generate the predicted values based on the model
trac_wages_treatment$predicted_rest <- predict(model)
slope <- coef(model)["total_value"]
slope_label <- paste0("Slope = ", round(slope, 3))
# Now, create the plot with a linear regression line using predicted values
ggplot(trac_wages_treatment) +
  aes(x = total_value, y = predicted_rest) + 
  geom_point(size = .5) +  # Scatterplot for the count variable
  geom_smooth(aes(y = predicted_rest), color = "blue", linewidth = 1) +  # Linear regression line with fixed effects
  annotate("text", x = Inf, y = -Inf, label = slope_label, hjust = 1.1, vjust = -1.1, size = 4, color = "blue") +
  labs(x = "Arrest Counts", 
       y = "Change in Wage in Rest",  
       title = "Arrest Count vs Change in Wage in Rest with Time Fixed Effect") + 
  theme_bw() 

model <- lm(change_wage_const ~  total_value + factor(year_qtr), data = trac_wages_treatment)
summary(model)
# Generate the predicted values based on the model
trac_wages_treatment$predicted_const <- predict(model)
slope <- coef(model)["total_value"]
slope_label <- paste0("Slope = ", round(slope, 3))
# Now, create the plot with a linear regression line using predicted values
ggplot(trac_wages_treatment) +
  aes(x = total_value, y = predicted_const) + 
  geom_point(size = .5) +  # Scatterplot for the count variable
  geom_smooth(aes(y = predicted_const), color = "blue", linewidth = 1) +  # Linear regression line with fixed effects
  annotate("text", x = Inf, y = -Inf, label = slope_label, hjust = 1.1, vjust = -1.1, size = 4) +
  labs(x = "Arrest Counts", 
       y = "Change in Wage in Rest",  
       title = "Arrest Count vs Change in Wage in Const with Time Fixed Effect") + 
  theme_bw() 

model <- lm(change_wage_other ~  total_value + factor(year_qtr), data = trac_wages_treatment)
summary(model)
# Generate the predicted values based on the model
trac_wages_treatment$predicted_oth <- predict(model)
slope <- coef(model)["total_value"]
slope_label <- paste0("Slope = ", round(slope, 3))
# Now, create the plot with a linear regression line using predicted values
ggplot(trac_wages_treatment) +
  aes(x = total_value, y = predicted_oth) + 
  geom_point(size = .5) +  # Scatterplot for the count variable
  geom_smooth(aes(y = predicted_oth), color = "blue", linewidth = 1) +  # Linear regression line with fixed effects
  annotate("text", x = Inf, y = -Inf, label = slope_label, hjust = 1.1, vjust = -1.1, size = 4, color = "blue") +
  labs(x = "Arrest Counts", 
       y = "Change in Wage in Other",  
       title = "Arrest Count vs Change in Wage in Other industries with Time Fixed Effect") + 
  theme_bw() 

#############################################################################
save(trac_employment, file ="trac_employment.Rdata")
save (trac_employment, file = "trac_wages.Rdata")

######### loading the I247a data ############






