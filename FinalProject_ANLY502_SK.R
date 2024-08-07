realtor_data <- read.csv("realtor-data.csv",header = TRUE)

### State ###
library(plotly)
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

#Lower bound: $10000 is often used to exclude extremely low prices due to errors
#Upper bound: $2000000 is often used to 
#https://www.rocketmortgage.com/learn/average-square-footage-of-a-house
lower_price<-10000
upper_price<-2000000
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
         zip_code=as.character(zip_code)) 
# %>%
#   select(-contains("prev_sold_date"),
#          -contains("status"))
table(data1$state)
state_avgprice <- tapply(data1$price,data1$state,mean)
state_avgprice
data<-as.data.frame(state_avgprice)
# state_abbr <- c(state.abb, "DC")
# names(state_abbr) <- c(state.name, "District of Columbia")
# Example: 
us_agexp <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")

data$state <- names(state_avgprice)
data$price <- data$state_avgprice
state_abbr <- c(us_agexp$code,"DC","PR")
names(state_abbr) <- c(us_agexp$state,"District of Columbia","Puerto Rico")
data$state_abbr <- state_abbr[data$state]

# Create the interactive heatmap, didn't work
plot <- plot_ly(
  data,
  type = 'choropleth',
  locations = ~state_abbr,
  locationmode = 'USA-states',
  z = ~price,
  text = ~paste(
    "State: ", state, "<br>",
    "Mean House Price: ", price
  ),
  colorscale = list(
    c(0, 'white'),
    c(500000, 'lightblue'),
    c(1000000, 'darkblue')
  ),
  colorbar=list(
    title=list(
      text = "Average House Prices across US States",
      side = "right"
    )
  )
) %>%
  layout(
    title = list(
      text = " Average House Prices across US States",
      y = 0.95,  # Move title closer to the plot
      x = 0.5,
      xanchor = "center",
      yanchor = "top"
    ),
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
  )

# Show the plot
plot

# Using plot_geo
data$hover <- with(data, paste(state, '<br>', "Mean House Price", price))

fig <- plot_geo(data, locationmode = 'USA-states')
fig <- fig %>% add_trace(
  z = ~price, text = ~hover, locations = ~state_abbr,
  color = ~price, colors = 'Purples'
)
fig <- fig %>% colorbar(title = "Price in USD")
fig <- fig %>% layout(
  title = 'Average House Prices across US States<br>(Hover for breakdown)'
)

fig
# Basic State Plot
barplot(state_avgprice,  xlab = "State", 
        ylab = "MEAN HOUSE PRICE", main = "MEAN HOUSE PRICES PER STATE")

# Is State a Significant Factor for Price?
ggplot(data1, aes(x = price)) +
  geom_histogram(binwidth = 40000, fill = "lightblue", color = "black") +
  labs(title = "Histogram of House Price", x = "Value", y = "Frequency") +
  theme_minimal()

# Fit the ANOVA model
data1$state<-as.factor(data1$state)
anova_result <- aov(price ~ state, data = data1)
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
df<-tukey_result$state
signif<- df[which.min(df[, "p adj"]), ]
signif_pos <- rownames(df)[which.min(df[, "p adj"])]
print(signif_pos) # "Arizona-Alabama"

summary(df)

data1$logprice<-log(data1$price)
data1$state1<-as.factor(data1$state)
statelm_result <- lm(logprice ~ state1, data = data1)
summary(statelm_result)

plot(data1$state1,data1$logprice)
# meanlogprice = tapply(data1$logprice,)

meanlogprice = tapply(data1$logprice,data1$state1,mean)
sdlogprice = tapply(data1$logprice,data1$state1,sd)
nprice = tapply(data1$logprice,data1$state1,length)

lblogprice = meanlogprice - sdlogprice/sqrt(nprice)
ublogprice = meanlogprice + sdlogprice/sqrt(nprice)
cilogprice <- rbind(lblogprice,ublogprice)
print(cilogprice)
par(mfrow = c(1, 3))
plot(lblogprice,  xlab = "State",
     ylab = "LB LOG HOUSE PRICE", col = "red", main = "LB LOG HOUSE PRICE PER STATE")
plot(meanlogprice,  xlab = "State",
     ylab = "MEAN LOG HOUSE PRICE", col = "green", main = "MEAN LOG HOUSE PRICE PER STATE")
plot(ublogprice,  xlab = "State",
     ylab = "UB LOG HOUSE PRICE", col = "blue", main = "UB LOG HOUSE PRICE PER STATE")

plot(names(lblogprice), lblogprice, type="l", lty=3, col = c("red","blue"), xlab ="State",
     ylab="Count",main ="CI Log Price for State")

