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

ggplot(data=telco, mapping = aes(x= contract, fill = churn_label)) + geom_bar(position = 'fill')
# from the bar chart created we find out that customers who are on a Month-to-Month contract are the ones the company is most likely to loose

ggplot(data=telco, mapping = aes(x = monthly_charges, fill = churn_label)) + geom_density(alpha = 0.5) + labs(title = 'Churn By Monthly Charges')
# the question we are trying to ask here is do customers with high charges churn

colnames(telco)

# check if the tenure_month affects the churn
ggplot(data = telco, mapping = aes(x = tenure_months, fill = churn_label)) + geom_histogram(bins = 30, alpha = 0.6) + labs(title = "Churn By Tenure")
# the graph is telling us that the more the tenure increases the customers are likely to stay

ggplot(data = telco, mapping = aes(x = payment_method, fill = churn_label)) + geom_bar(position = 'fill') + labs(title = 'payment method by churn')


# build a model, The Goal is will a customer churn or not 

telco <- telco %>% select(-customer_id)

telco$churn_label <- as.factor(telco$churn_label)

# Train or Test Split

set.seed(123)

train_index <- sample(nrow(telco), 0.7 * nrow(telco))

train_data <- telco[train_index,]
test_data <- telco[-train_index,] 

model <- glm(churn_label~., data = train_data, family = 'binomial')

sapply(train_data, function(x) length(unique(x)))
