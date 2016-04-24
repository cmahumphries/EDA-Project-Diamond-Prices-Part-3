library(ggplot2)
library(dplyr)
library(tidyr) 

#1. Price Histograms with Facet and Color

# Create a histogram of diamond prices.
# Facet the histogram by diamond color and use cut to color the histogram bars.

ggplot(aes(x = price), data = diamonds) + 
  geom_histogram(aes(fill = cut), binwidth = 1000) +
  facet_wrap(~ color)

#2. Price vs. Table Colored by Cut

# Create a scatterplot of diamond price vs.table and color the points by the cut of
# the diamond.

ggplot(aes(y = price, x = table), data = diamonds) + 
  geom_point(aes(color = cut))

#3. Typical Table Value

#using the graph created:
#What is the typical table range for the majority of diamonds of ideal cut? 53 - 57
#What is the typical table range for the majority of diamonds of premium cut? 58 - 62

#4. Price vs. Volume and Diamond Clarity

# Create a scatterplot of diamond price vs.volume (x * y * z)
# color the points by the clarity of diamonds. 
# Use scale on the y-axis to take the log10 of price. 
# Omit the top 1% of diamond volumes from the plot.

diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
ggplot(aes(y = price, x = volume), data = diamonds) + 
  geom_point(aes(color = clarity)) +
  scale_y_log10() +
  xlim(1, quantile(diamonds$volume, 0.99))

#5. Proportion of Friendships Initiated

# Create a new variable called 'prop_initiated'in the Pseudo-Facebook data set. 
# The variable should contain the proportion of friendships that the user initiated.

pf <- read.csv("pseudo_facebook.tsv", sep = "\t")
pf$prop_initiated <- ifelse(pf$friend_count == 0, 0, 
                            pf$friendships_initiated/pf$friend_count)
summary(pf$prop_initiated)

#6. prop_initiated vs. tenure

# Create a line graph of the median proportion of friendships initiated ('prop_initiated') vs.
# tenure and color the line segment by year_joined.bucket.

# Recall, we created year_joined.bucket in Lesson 5 by first creating year_joined from 
# the variable tenure. Then, we used the cut function on year_joined to create
# four bins or cohorts of users

# (2004, 2009]
# (2009, 2011]
# (2011, 2012]
# (2012, 2014]

pf$year_joined <- floor(2014 - pf$tenure/365)
pf$year_joined.bucket <- cut(pf$year_joined, 
                             breaks = c(2004, 2009, 2011, 2012, 2014))

pf.median_prop <- pf %>%
  filter(!is.na(tenure)) %>%
  filter(tenure > 0) %>%
  group_by(tenure, year_joined.bucket) %>%
  summarise(median_prop_fr_init = median(prop_initiated),
            n = n()) %>%
  ungroup() %>%
  arrange(tenure)
head(pf.median_prop)

ggplot(aes(y = median_prop_fr_init, x = tenure), data = pf.median_prop) + 
  geom_line(aes(color = year_joined.bucket))

#7. Smoothing prop_initiated vs. tenure

# Smooth the last plot you created of of prop_initiated vs tenure colored by
# year_joined.bucket. You can bin together ranges of tenure or add a smoother to the plot.

ggplot(aes(y = median_prop_fr_init, x = tenure), data = pf.median_prop) + 
  geom_smooth(aes(color = year_joined.bucket))

#8. Greatest prop_initiated Group

#On average, which group initiated the greatest propoertion of its Facebook friendships?
#People who joined after 2012

#9. Largest Group Mean prop_initiated

#For the group with the largest proportion of friendships initiated, 
#what is the group's average proportion of friendships initiated?

with(subset(pf, year_joined.bucket == "(2012,2014]"), mean(prop_initiated))

#Why do you think this group's proportion of frienships initiated is higher than others?
#New users: they are more likely to request friendship from existing users.

#10. Price/Carat Binned, Faceted, & Colored

# Create a scatter plot of the price/carat ratio of diamonds. 
# The variable x should be assigned to cut. 
# The points should be colored by diamond color.
# The plot should be faceted by clarity.

ggplot(aes(y = price/carat, x = cut), data = diamonds) + 
  geom_point(aes(color = color), alpha = 1/10, position = position_jitter(h = 5)) +
  facet_wrap(~ clarity)

#11. Gapminder Multivariate Analysis

# Create 2-5 plots that make use of the techniques from Lesson 5
# ODA aid per person (constant 2007 US$)
app <- read.csv("app.csv")
colnames(app)[1] <- "country" #rename 1st column
app[is.na(app)] <- 0 #set NA values to zero
app[1:5, 1:5]

app.long <- gather(app, "year", "amount_per_person", X1960:X2007)
app.long$year <- as.numeric(substring(app.long$year, 2))
head(app.long)

#chart amount per person by year for each country
ggplot(aes(x = year, y = amount_per_person), data = app.long)  +   
  geom_line(aes(colour = country))

#chart average amount per person by year for all countries
app.long.mean_year <- app.long %>%
  group_by(year) %>%
  summarise(mean_amount_per_person = mean(amount_per_person))
head(app.long.mean_year)

ggplot(aes(x = year, y = mean_amount_per_person), data = app.long.mean_year)  +   
  geom_smooth()

#chart average amount per person by year by groups(group by avg amounts from countries)

app.long.mean_country <- app.long %>%
  group_by(country) %>%
  summarise(mean_country = mean(amount_per_person))
head(app.long.mean_country)

app.long.mean_country$country_bucket <- cut(app.long.mean_country$mean_country, breaks = 5)

app.long <- full_join(app.long, app.long.mean_country, by = "country") 

summary(app.long$country_bucket)

ggplot(aes(x = year, y = amount_per_person), data = app.long)  +   
  geom_smooth(aes(colour = country_bucket)) +
  geom_line(stat = "summary", fun.y = mean, linetype = 2)



