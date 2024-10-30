install.packages("survival")
install.packages("survminer")  # Optional for better plots
library(survival)
library(survminer)  # Optional for better plots
library(tidyverse)


library(haven)
 khdss_snakebiites_survival <- read_dta("C:/Users/Dlagat/Downloads/khdss_snakebiites_survival.dta")

khdss_snakebiites_survival$sex<-as.factor(khdss_snakebiites_survival$sex)
 
install.packages("forcats")
library(forcats)

# Assuming khdss_snakebites_survival is your data frame and sex is the factor variable
khdss_snakebiites_survival <- khdss_snakebiites_survival %>%
  mutate(sex = fct_recode(sex, "F" = "f", "M" = "m"))


 
 
#Create a time to event column.
khdss_snakebiites_survival <- khdss_snakebiites_survival %>%
  mutate(
    time_to_event = case_when(
      !is.na(dod) ~ as.numeric(difftime(dod, dob1, units = "days")) /365.25
)
  )

 
 
# Replace missing values in all_deaths with 0
khdss_snakebiites_survival <- khdss_snakebiites_survival %>%
  mutate(all_deaths = if_else(is.na(all_deaths), 0, all_deaths))

 glimpse(khdss_snakebiites_survival$all_deaths
         )        
      

fit <- survfit(Surv(time_to_event, all_deaths) ~ sex, data = khdss_snakebiites_survival)

#fit <- survfit(Surv(time_to_event, all_deaths) ~1 , data = khdss_snakebiites_survival)





ggsurvplot(fit,data = khdss_snakebiites_survival,
           xlab="Time(years)",
           ylab="Survival probability",
           title="Survival Curves (all_deaths)",
           legend= "bottom",
           legend.title="Sex",
           legend.labs=c("Male",
                         "Female"))




#non-stratified (all_deaths)

fit <- survfit(Surv(time_to_event, all_deaths) ~1 , data = khdss_snakebiites_survival)

# Create the survival plot
# No legend since we have a single survival curve
 ggsurvplot(
  fit, 
  data = khdss_snakebiites_survival,
  xlab = "Time (years)", 
  ylab = "Survival Probability",
  title = "Survival Curve (all_deaths)",
  legend= "bottom",
  legend.title="Sex",
 )



glimpse( khdss_snakebiites_survival$household_snakebite_deaths)


###############################################################################
##HOUSEHOLD SNAKEBITE DEATHS
# Convert labelled data to numeric
 khdss_snakebiites_survival$household_snakebite_deaths<-as.numeric(khdss_snakebiites_survival$household_snakebite_deaths) 
#Converts labelled data to numeric


 
# Replace 999 with 0 and keep 1 as 1
khdss_snakebiites_survival <- khdss_snakebiites_survival %>%
  mutate(household_snakebite_deaths = case_when(
    is.na(household_snakebite_deaths) ~ 0,
    household_snakebite_deaths == 999 ~ 0,
    household_snakebite_deaths == 1 ~ 1,
    TRUE ~ as.numeric(household_snakebite_deaths)   
  ))


  
  # Fit the survival model with stratification
  fit_stratified <- survfit(Surv(time_to_event, household_snakebite_deaths) ~ sex, data = khdss_snakebiites_survival)
  
  # Plot the stratified survival curves
  surv_plot_stratified <- ggsurvplot(
    fit_stratified, 
    data = khdss_snakebiites_survival,
    xlab = "Time (years)", 
    ylab = "Survival Probability",
    title = "Survival Curves  (household snakebite deaths)",
    legend.title = "Sex",
    legend= "bottom",
    legend.labs=c("Male","Female"),
     pval=TRUE,
    risk.table = TRUE)
      
  # Print the plot
  print(surv_plot_stratified)
  
  
 
##non-stratified plot(household_snakebite_deaths)
  fit_1 <- survfit(Surv(time_to_event, household_snakebite_deaths) ~ 1, data = khdss_snakebiites_survival)
  # Plot the stratified survival curves
  surv_plot_stratified_1 <- ggsurvplot(
    fit_1, 
    data = khdss_snakebiites_survival,
    xlab = "Time (years)", 
    ylab = "Survival Probability",
    title = "Survival Curves (household snakebite deaths)",
    legend.title = "Sex",
    legend= "bottom"
    )
  
  
  # Print the plot
  print(surv_plot_stratified_1)
  
  
  
  

  ###################################################
  ##household snakebites va
  glimpse(khdss_snakebiites_survival$household_snakebite_va)
  
  ## Replace missing values in household_snakebite_va with 0 (r cannot perform the analysis with NAs)
  khdss_snakebiites_survival <- khdss_snakebiites_survival %>%
    mutate(household_snakebite_va = if_else(is.na(household_snakebite_va), 0, household_snakebite_va)) 
  
  
  fit <- survfit(Surv(time_to_event, all_deaths) ~ sex, data = khdss_snakebiites_survival)
  
  
  # Plot the stratified survival curves
  surv_plot_stratified <- ggsurvplot(
    fit_stratified, 
    data = khdss_snakebiites_survival,
    xlab = "Time (years)", 
    ylab = "Survival Probability",
    title = "Survival Curves  (household snakebite va)",
    legend.title = "Sex",
    legend= "bottom",
    legend.labs=c("Male","Female"),
    pval=TRUE,
    risk.table = TRUE)
print(surv_plot_stratified)  
  
  
  
##non-stratified plot(household_snakebite_va)
fit_1 <- survfit(Surv(time_to_event, household_snakebite_deaths) ~ 1, data = khdss_snakebiites_survival)
# Plot the stratified survival curves
surv_plot_stratified_1 <- ggsurvplot(
  fit_1, 
  data = khdss_snakebiites_survival,
  xlab = "Time (years)", 
  ylab = "Survival Probability",
  title = "Survival Curves (household snakebite va)",
  legend.title = "Sex",
  legend= "bottom")

  
  
  
  

