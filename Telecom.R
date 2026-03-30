install.packages('tidyverse')
install.packages('janitor')
install.packages('moments')
library(tidyverse)
library(janitor)
library(moments)

library(readxl)
#1. read the data 
telco <- read_excel("C:\\Users\\user\\Desktop\\Telco_customer_churn.xlsx")

#2.View the data
View(telco)

#3. perform a summary of the entire data
summary(telco)

#4. check for missing values across all columns
telco %>% map(~sum(is.na(.)))

#5. clean the column names with the help of the janitor library
telco <- clean_names(telco)

#6. View the columns names to make sure everything is alright
colnames(telco)

#7. Remove the churn reason column because it has lots of missing values and would be bad for prediction
telco <- telco %>% select(-churn_reason)

#8.confirm if the churn reasin column is actually gone
colnames(telco)

#9. drop all rows in the Total charges rows that have missing values
telco %>% drop_na(total_charges)

#10. confirm the removal 
telco %>% map(~sum(is.na(.)))

telco$total_charges <- as.numeric(telco$total_charges)

#11. drop all rows in the Total charges rows that have missing values and save it
telco <- telco %>% drop_na(total_charges)

#12. confirm the removal 
telco %>% map(~sum(is.na(.)))

View(telco)
summary(telco)

No_churn <- telco %>% filter(churn_label == 'No')
# there are 5163 customers who are going to stay

Yes_churn <- telco %>% filter(churn_label == 'Yes')
# then we have 1869 customers who are not going to stay

table(telco$churn_value)

table(telco$churn_label)
