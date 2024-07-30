### Desription: EDA & Inference
### Date: 07/23/2024

realtor_data<- read.csv("~/Desktop/ANLY502/Project/data/realtor-data.csv")
library(tidyverse)
library(dplyr)
library(multcomp)

sold<-realtor_data %>% subset(status=='sold')

str(sold)
summary(sold)

################## Data Wrangling ##################
### Step 1: clean NA's
na_counts <- sapply(sold, function(x) sum(is.na(x)))
print(na_counts)
#only keep records with all variables not missing
sold1<-sold[complete.cases(sold),]

### Step 2: Date: only keep month of date
sold1$date <- substr(sold1$prev_sold_date, 6, 7)
sold1<-sold1[,-1]


#the median price per square feet of house in USA in 2023 is $224. 
#the median sales price of house in USA in 2023 in around $426525 (423200, 435400, 418500, 429000)
#https://fred.stlouisfed.org/series/MSPUS

#lower_price<-10000
#upper_price<-2000000
q1_price <- quantile(sold1$price, 0.25)
q3_price <- quantile(sold1$price, 0.75)
iqr_price <- IQR(sold1$price)
lower_price<-max(q1_price-0.5*iqr_price,0)
upper_price<-q3_price+0.5*iqr_price

lower_bed<-1
upper_bed<-10
lower_bath<-1
upper_bath<-10
acre_lot_max<-10
house_size_max<-10000
data<-sold1 %>% 
   filter(price>=lower_price & price<=upper_price) %>%
   filter(bed>=lower_bed & bed<=upper_bed)  %>%
  filter(bath>=lower_bath & bath<=upper_bath) %>%
  filter(house_size<=house_size_max  &  acre_lot<=acre_lot_max)

summary(data)

data1<-data %>%
        mutate(street=as.character(street),
               zip_code=as.character(zip_code)) %>%
    dplyr::select(-prev_sold_date, -status)



################## EDA ##################
library(corrplot)
data2<-data1 %>%
    mutate_if(is.character,as.factor) %>%
    mutate_if(is.factor, as.numeric) 
M <- cor(data2)
M
options(repr.plot.width =15, repr.plot.height = 7)
M[lower.tri(M,diag=TRUE)] <- NA  #remove lower triangle values
M[M == 1] <- NA

M <- as.data.frame(as.table(M)) 
M <- na.omit(M) 
#M <- subset(M, abs(Freq) )   
M <- M[order(-abs(M$Freq)),]        #sort by highest correlation

mtx_corr <- reshape2::acast(M, Var1~Var2, value.var="Freq")  #turn M back into matrix in order to plot with corrplot
corrplot(mtx_corr, is.corr=TRUE, tl.col="black", 
         na.label=" ")
title(main="Correlation Plot for House Price and Factors",line=0) 



################## Inference ##################
#Is there a significant difference in average house prices between properties with different numbers of bedrooms?

ggplot(data1, aes(x = price)) +
  geom_histogram(binwidth = 40000, fill = "lightblue", color = "black") +
  labs(title = "Histogram of House Price", x = "Value", y = "Frequency") +
  theme_minimal()
data1$logprice<-log(data1$price)
ggplot(data1, aes(x = logprice)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Histogram of log House Price", x = "Value", y = "Frequency") +
  theme_minimal()


# Fit the ANOVA model
data1$bed<-as.factor(data1$bed)
anova_result <- aov(logprice ~ bed, data = data1)
summary(anova_result)

# Extract residuals
residuals <- residuals(anova_result)

# Plot Q-Q plot
qqnorm(residuals)
qqline(residuals)

# Perform Tukey's HSD test
library(multcomp)
tukey_result <- TukeyHSD(anova_result)
plot(tukey_result)
# Find the most significant difference
df<-tukey_result$bed
signif<- df[which.min(df[, "p adj"]), ]
signif_pos <- rownames(df)[which.min(df[, "p adj"])]
print(signif_pos)






