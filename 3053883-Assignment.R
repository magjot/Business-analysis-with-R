#Data Uploading and Preview
Project<- read.csv("http://bit.ly/2OMHgFi")


#Data preview
head(Project)

#A plot graph of Ice-cream against the price was plotted to see the relationship between Sales and Price 
plot(x = Project$icecream_sales, y = Project$Price)

#Using of Histogram is viewing the sales in Ice cream
hist(x = Project$icecream_sales)

hist(x = Project$icecream_sales, breaks = 50)

#Bar chart was used to view the sales in the Location Country A and Location Country B.
barplot(table(Project$icecream_sales),
        xlab = "Country",
        ylab = "icecream_sales")
#Also, a box plot was used to view the sales in two countries.

boxplot(x = Project$icecream_sales,
        main = "Boxplot of Sales in the two Country",
        ylab = "icecream_sales",
        xlab = "country", 
        col = "lightblue")



#Installing ggplot and library
install.packages("ggplot2")

library(ggplot2)
#Ggplot used plot a graph of the country, sales, season, and price

ggplot(data = Project, 
       aes(x =icecream_sales , y = price, by=country, colour = season)) + 
  geom_line() + 
  facet_wrap( ~ season) # Apply facets to create a separate plot for each season.

#Another data of interest is the data is Sales in the seasons. This is to help find a relationship between the two.

ggplot(data = Project, aes(x = seasons, y = iceicream_sales)) + 
  geom_boxplot()+ # creates box plots
  xlab("seasons") + 
  ggtitle("Box plots of icecream sales in different seasons")


# View the relationship temperature,country to the sales of ice cream
ggplot(data = Project , aes(x = temperature, y = icecream_sales )) + 
  geom_point(aes(colour = country)) + 
  scale_x_log10() + 
  geom_smooth(method = "lm", size = 1.5)

#	Using box plot to analyse data of interquartile median in the data of Sales in relation to seasons. This is to help find a relationship between the tw
ggplot(data = Project, aes(x = seasons, y = icecream_sales)) + 
  geom_boxplot()+ # creates box plots
  xlab("seasons") + 
  ggtitle("Box plots of icecream_sales in different seasons")

#Descriptive analysis

#Installing dplyr package
install.packages("dplyr")
library(dplyr)
#Using glimpse to see the variable of the data set
glimpse(Project)

#Using summary
summary(Project) 

#Creating a table to check categorical variable.
table(Project$country)

table(Project$seasons)

#Data observation for Hypothesis

data = na.omit(Project)
boxplot(data$temperature ~ data$country ,
        main = "Temperature vs Country",
        ylab = "temperature",
        xlab = "country",
        col = c("red", "green")
)
legend("topleft", c("A", "B"), fill = c("red", "green"))

#Sample Distribution for Inference
statsr::inference(y = temperature, x = country, data = Project, 
                  statistic = c("mean"), 
                  type = c("ci"), 
                  null = 0,
                  alternative = c("twosided"), 
                  method = c("theoretical"), 
                  conf_level = 0.95,
                  order = c("A","B"))
#Installing statsr
install.packages("statsr")


#Checking the Test score
data = na.omit(Project) # let's omit missing values from our calculations

statsr::inference(y = temperature, x = country, data = data, 
          statistic = c("mean"), 
          type = c("ht"), 
          null = 0,
          alternative = c("twosided"), 
          method = c("theoretical"), 
          conf_level = 0.95,
          order = c("A","B"))

#Modeling of Data set

#Answering question 2, finding the co-efficent
Sales_model <- read.csv("http://bit.ly/2OMHgFi")
Sales_model <- lm(icecream_sales ~ income + price+ temperature + country +seasons, data = Company)
summary(Sales_model)

#Answer to Question number 3
Change_in_sales <- data.frame(country = "A", temperature = 5)
Sales_model3 <- lm(icecream_sales ~ + country = "A" * seasons, data = Project)
predict(Sales_model3, Change_in_sales)


#Question 4 answer

Change_in_sales <- data.frame(price = 0.25, temperature = 5)
Sales_model2 <- lm(icecream_sales ~ price + temperature, data = Project)
predict(Sales_model2, Change_in_sales)

#Answering Question 5:What percentage of the variance is described by the model?
(summary(Sales_model)$sigma)**2

# Testing the Regression conditions for Regression
Sales_model <- lm(icecream_sales ~ income + price, data = Project)
# Residual plots
plot(Sales_model$residuals ~ Project$price)


#investigating the normality of the residuals via a normal probability plot (QQ-plot)
log_transform <- Project %>% mutate(logincome = log(Income))
# Run regression using the log(income)
Sales_model_log <- lm(icecream_sales ~ logincome + price, data = log_transform)

hist(Sales_model_log$residuals)

#Prediction 
Sales_data <- data.frame(income = 30000, price = 5, temperature = 3, country = "B",seasons = "Winter")
Sales_model <- lm(icecream_sales ~ price + income + temperature + country + seasons, data = Project)
predict(Sales_model, Sales_data, interval = "prediction", level = 0.90)

