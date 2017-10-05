## This file is dedicated to the exploratory analysis done
library(ggplot2)
library(dplyr)
setwd("D:\\Projects\\Airbnb")
# loading data
train = read.csv("train_users_2.csv")

## Destination Countries Count
ggplot(data = train, aes(train$country_destination)) +geom_bar(col="black",fill="lightblue") +
  labs(x="Destination Country", y="Count") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 16), axis.title = element_text(size = 16, face = "bold"))
ggsave("Counts_Countries.png",plot = last_plot(),width = 12,height = 6,dpi=300)


## Destination Countries Breakdown (Males/Females)
traingender <- train[(train$gender=='MALE'|train$gender=='FEMALE'),]
ggplot(data = traingender, aes(traingender$country_destination, fill=factor(traingender$gender)))+
  geom_bar(position="dodge",col = "black") +
  labs(x="Destination Country", y="Count") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 16),
        axis.title = element_text(size = 16, face = "bold"))
ggsave("Counts_Countries_Gender.png",plot = last_plot(),width = 12,height = 6,dpi=300)


# Age Distribution
trainage <- train[(train$age>18 & train$age < 100),]
ggplot(data = trainage, aes(trainage$age)) +geom_bar(col="black",fill="lightblue") +
  labs(x="Age", y="Count") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 16), axis.title = element_text(size = 16, face = "bold"))
ggsave("Age.png",plot = last_plot(),width = 12,height = 6,dpi=300)

# Age Breakdown per country
not.na <- !is.na(train$age)
ggplot(train[not.na,], aes(age, fill=country_destination)) + geom_bar(position="identity", alpha=0.5) +
  xlim(14, 100) + scale_y_log10() + facet_wrap(~country_destination) +
  labs(x="Age", y="Count") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 16), axis.title = element_text(size = 16, face = "bold"))
ggsave("Age_Country.png",plot = last_plot(),width = 12,height = 6,dpi=300)


# Sign-up Method breakdown per country
ggplot(train, aes(signup_method, fill=country_destination)) + geom_bar(position="identity", alpha=0.5) + 
  scale_y_log10() + facet_wrap(~country_destination)+
  labs(x="Sign-up Methid", y="Count") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 13), axis.title = element_text(size = 12, face = "bold"))
ggsave("Sign_Up_Country.png",plot = last_plot(),width = 12,height = 6,dpi=300)






