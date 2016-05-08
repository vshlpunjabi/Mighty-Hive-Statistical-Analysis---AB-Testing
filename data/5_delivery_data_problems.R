delivery_data <- read.csv('Delivery_Data_Problem.csv', header = TRUE, stringsAsFactors = FALSE)
head(delivery_data)
View(delivery_data)
nrow(delivery_data)
delivery_data$pickup_name[120]
delivery_data$purchase_price[2355]
length(delivery_data$avg_courier_rating[delivery_data$avg_courier_rating ==5])
mean(delivery_data$duration[delivery_data$rating_by_customer == 5])
mean(delivery_data$duration[delivery_data$rating_by_customer == 4])
mean(delivery_data$duration[delivery_data$rating_by_customer == 3])
mean(delivery_data$duration[delivery_data$rating_by_customer == 2])
mean(delivery_data$duration[delivery_data$rating_by_customer == 1])
index = as.logical(delivery_data$reassigned)
avg_duration_reassigned = mean(delivery_data$duration[index])
avg_duration_not_reassigned = mean(delivery_data$duration[!index])
additional_avg_duration = round((avg_duration_reassigned - avg_duration_not_reassigned)/60)
avg_reassigned_customer_rating = round(mean(delivery_data$rating_by_customer[index] != "None"),2)
reassigned_customer_rating = delivery_data$rating_by_customer[index]
reassigned_customer_rating = as.numeric(reassigned_customer_rating)
avg_reassigned_customer_rating = round(mean(reassigned_customer_rating[reassigned_customer_rating != "None"],na.rm = TRUE),2)
nonreassigned_customer_rating = delivery_data$rating_by_customer[!index]
nonreassigned_customer_rating = as.numeric(nonreassigned_customer_rating)
avg_nonreassigned_customer_rating = round(mean(nonreassigned_customer_rating[nonreassigned_customer_rating != "None"],na.rm = TRUE),2)
#visualization to show the rating by customer based on delivery duarion
library(UsingR)
library(reshape2)
ggplot(delivery_data,aes(x=rating_by_customer,y=duration))+geom_point()

