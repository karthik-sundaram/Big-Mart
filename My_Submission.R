
# 1)working directory
path <- "C:/Users/Karthik/Desktop/Coursera/Ex_Files_RStats_EssT/Exercise Files/analytics vidhya"

# 2)set working directory
setwd(path)

#3) read
train <- read.csv("Train_UWu5bXk.csv")
test <- read.csv("Test_u94Q5KV.csv")

#4) dim / str
dim(train)
dim(test)
str(train)

#5)empty?
table(is.na(train))
colSums(is.na(train)) # checks which all cols have missing data as a summary  

#6)
summary(train)

# 7)PLOTS
require(ggplot2)
ggplot(train, aes(x= Item_Visibility, y = Item_Outlet_Sales)) + geom_point(size = 2.5, color="navy") + xlab("Item Visibility") + ylab("Item Outlet Sales") + ggtitle("Item Visibility vs Item Outlet Sales")

{
  mean<-aggregate(Item_Outlet_Sales~Outlet_Identifier, data=train,FUN=sum)
  mean.data <- t(mean[-1])  # Removes first columns, transposes second
  colnames(mean.data) <- mean[, 1]  
  barplot(mean.data,width=0.01)
  
  (or)
  
  ggplot(train, aes(Outlet_Identifier, Item_Outlet_Sales)) + 
    geom_bar(stat = "identity") #ggplot auto assumes mean or 1d/ group data....
}

ggplot(train, aes(Item_Type, Item_MRP)) +geom_boxplot()  # this part is enough
+ggtitle("Box Plot") + theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "red")) + xlab("Item Type") + ylab("Item MRP") + ggtitle("Item Type vs Item MRP")

plot(Item_Outlet_Sales~Item_Visibility,data=new_train)
plot(Item_Outlet_Sales~Item_MRP,data=new_train)
abline(lm(Item_Outlet_Sales~Item_Visibility,data=new_train),col='red',lwd=2)

#8) missing data and incorrect data need are dealt with (check in summary())

#8a)missing data being dealt with
test$Item_Outlet_Sales <-  1 #create a new col and add a value of 1 (cols from 11 becomes 12)
combi <- rbind(train, test)
combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE) 
levels(combi$Outlet_Size)[1] <- "Other" #adjusting for missing aggregate factor field

#8b)incorrect data being dealt with
combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0, median(combi$Item_Visibility), combi$Item_Visibility) 



combi$Year <- 2013 - combi$Outlet_Establishment_Year #no. of yrs and not yr itself is needed


# 9)FACTOR LEVEL CORRECTION
require(plyr)
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content,c("LF" = "Low Fat", "reg" = "Regular"))
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content, c("low fat" = "Low Fat"))
summary(combi)

library(dplyr)
combi_anal <- select(combi, -c(Item_Identifier, Outlet_Identifier, Outlet_Establishment_Year))# choosing variables that are categorical/continuous and leave out ID,...


new_train <- combi_anal[1:nrow(train),]
new_test <- combi_anal[-(1:nrow(train)),]

#10)  Backward Regression
  linear_model1 <- lm(log(Item_Outlet_Sales) ~ .+I(Item_MRP^2)+I(Item_MRP^3)+I(Item_MRP^4)+I(Item_MRP^5), data = new_train)
  linear_modelb <- step(linear_model1,direction = "backward",trace = 0)  # Don't print the steps
  summary(linear_modelb) #coefficients so got, is used in the formula to calculate the Output_Sales
  plot(linear_modelb)

#RESULT:
#[     Call:
#      lm(formula = log(Item_Outlet_Sales) ~ Item_MRP + Outlet_Size + 
#          Outlet_Location_Type + Outlet_Type + Year + I(Item_MRP^2) + 
#          I(Item_MRP^3) + I(Item_MRP^4) + I(Item_MRP^5), data = new_train)
#    
#    Residuals:      Min       1Q   Median       3Q      Max 
#    -2.13924 -0.27434  0.05605  0.36404  1.34805 
#    
#    Coefficients:
#      Estimate Std. Error t value Pr(>|t|)    
#    (Intercept)                   2.960e+00  2.781e-01  10.642  < 2e-16 ***
#      Item_MRP                      6.712e-02  1.149e-02   5.841 5.38e-09 ***
#      Outlet_SizeHigh               3.488e-01  1.173e-01   2.973 0.002960 ** 
#      Outlet_SizeMedium             7.459e-02  3.255e-02   2.292 0.021951 *  
#     Outlet_SizeSmall              6.409e-02  2.092e-02   3.064 0.002188 ** 
#     Outlet_Location_TypeTier 2   -4.645e-02  4.130e-02  -1.125 0.260711    
#    Outlet_Location_TypeTier 3   -1.551e-01  7.090e-02  -2.187 0.028747 *  
#      Outlet_TypeSupermarket Type1  1.749e+00  6.416e-02  27.269  < 2e-16 ***
#      Outlet_TypeSupermarket Type2  1.548e+00  6.212e-02  24.922  < 2e-16 ***
#      Outlet_TypeSupermarket Type3  2.615e+00  8.141e-02  32.126  < 2e-16 ***
#      Year                         -1.499e-02  4.803e-03  -3.122 0.001805 ** 
#      I(Item_MRP^2)                -7.196e-04  1.926e-04  -3.736 0.000188 ***
#      I(Item_MRP^3)                 4.383e-06  1.472e-06   2.977 0.002916 ** 
#      I(Item_MRP^4)                -1.323e-08  5.230e-09  -2.530 0.011433 *  
#      I(Item_MRP^5)                 1.539e-11  7.000e-12   2.199 0.027936 *  
#      ---
#      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#    
#    Residual standard error: 0.5171 on 8508 degrees of freedom
#    Multiple R-squared:  0.7419,	Adjusted R-squared:  0.7415 
#    F-statistic:  1747 on 14 and 8508 DF,  p-value: < 2.2e-16
#  ]
# 11) dummy data frame for regression on categorical variables (done at the end so
#that the excel sheet has all the expanded columns without disturbing the analysis or regression)
library(dummies) # if this shows error, type package name in 'install package'
require(dummies)
combi <- dummy.data.frame(combi, names = c('Item_Fat_Content','Outlet_Size','Outlet_Location_Type','Outlet_Type', 'Item_Type'),  sep='_')

#12)Write the file 
#require(xlsx)
write.csv(combi,file="combi4.csv")


install.packages("Metrics")
 library(Metrics)
 rmse(new_train$Item_Outlet_Sales, exp(linear_modelb$fitted.values))

rm(list = ls())  # Clean up