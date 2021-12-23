#Glovo - Data Analyst Test

library(tidyverse)

charges_data_path = c("/charges_data.csv")
personal_data_path = c("/personal_data.csv")
plan_data_path = c("/plan_data.csv")


explanatory_analysis <- function(charges_data_path,
                                 personal_data_path,
                                 plan_data_path){
  directory = c("C:/Users/Stefano/Desktop/Stefano/Glovo Data Analyst - Test")
  
  #CSV up-loading
  
  personal_data <- #Personal Data CSV   #1. read csv
    readr::read_csv(file = stringr::str_c(directory, personal_data_path))

  plan_data <- #Plan Data CSV #1. read csv
    readr::read_csv(file = stringr::str_c(directory, plan_data_path))
  
  charges_data <- #Dharges Data CSV #1. read csv
    readr::read_csv(file = stringr::str_c(directory, charges_data_path)) 
  
  
#...........................
#Modified Charges Data - "Charges Data Updated"
charges_data_updated <- 
  charges_data %>% 
  dplyr::mutate(monthlyCharges = tidyr::replace_na(data = monthlyCharges, replace = round(mean(charges_data %>% monthlyCharges, trim = 0.2), digits = 2)),
                totalCharges = monthlyCharges * tenure, #2. totalCharges
                tenureBinned = case_when(tenure > 0 & tenure <= 24 ~ "group1", #3. tenureBinned
                                         tenure > 24 & tenure <= 48 ~ "group2",
                                         tenure > 48 & tenure <= 60 ~ "group3",
                                         tenure > 60 & tenure < Inf ~ "group4"))

data_merged <- #5. join data sets
charges_data_updated %>% 
  dplyr::inner_join(personal_data, by = "customerID") %>% #5. join datasets ... so that the resulting data set has only rows with common customer IDs
  dplyr::left_join(plan_data, by = "customerID") #5. Then join the resulting data set with plan data... this time leaving all rows from the first dataset


#.............................................
#Vector Values (single vector value integers)
#2. Monthly Charges Mean
monthly_charges_mean <- round(mean(charges_data %>% monthlyCharges, trim = 0.2), digits = 2) #2.monthly_charges_mean


#4. churn rate
churn <- charges_data %>% forcats::as_factor(churn)
churn_pct <- round(x = n(dplyr::filter(churn == "Yes"))/nrow(churn), digits = 2) * 100

#6. % customers WHERE age > 60 FROM charges_data_updated
age_chages_data_updated <- charges_data_updated %>% dplyr::select(age)
pct_age_above_60 <- round(x = n(dplyr::filter(age == 60)) / nrow(age_chages_data_updated), digits = 2) * 100

#7. dictionary containing counts of unique values in the internetService column, 
#where keys are the unique values of items an their counts.


#the function returns EDA list (the closest thing to a python dictionary)
  
  EDA = list(monthly_charges_mean, #2.
             charges_data_updated, #3.
             churn_pct, #4.
             data_merged, #5.
             pct_age_above_60, #6.
             internet_service_counts ) #7.
               
  return(EDA)
}

