# Assignment: data transformations
# Name: Aquino, Jackson
# Date: 2022-04-14

library(readxl)
library(ggplot2)
library(stringr)
library(dplyr)
library(purrr)

# Using either the same dataset(s) you used in the previous weeks' exercise or a 
#     brand-new dataset of your choosing, perform the following transformations 
#     (Remember, anything you learn about the Housing dataset in these two weeks can be used for a later exercise!)

setwd("C:/Users/jackson_aquino/dsc520")

housing_df <- read_xlsx("data/week-6-housing.xlsx",1)

head(housing_df)
nrow(housing_df)
str(housing_df)


# Using the dplyr package, use the 6 different operations to analyze/transform the data 
#      - GroupBy, Summarize, Mutate, Filter, Select, and Arrange - 
#        Remember this isn't just modifying data, you are learning about your data also - 
#        so play around and start to understand your dataset in more detail
housing_df %>% summarize(unique_reason = n_distinct(sale_reason))
housing_df1 <- arrange(housing_df, desc(housing_df$year_built)) # this way I can see the prices of some of the newest houses

#the overall median for sale price
summarise(housing_df, medianPrice = median(`Sale Price`))

#the median when grouped by site type
housing_df2 <- group_by(housing_df,sitetype)
summarise(housing_df2, medianPrice = median(`Sale Price`))

#same summary but not this time sorted by median price to show which sitetype tends to have a more expensive sale price
arrange(summarise(housing_df2, medianPrice = median(`Sale Price`)),medianPrice)

cheap_medium_homes_df <- filter(housing_df, `Sale Price` < 200000, bedrooms == 2)

housing_df3 <- mutate(housing_df, total_toilet_count = bath_full_count + bath_half_count + bath_3qtr_count) # counting the total number

small_df <- select(housing_df3,bedrooms,total_toilet_count)

# Using the purrr package - perform 2 functions on your dataset.  You could use zip_n, keep, discard, compact, etc.
housing_df4 <- housing_df3 %>% discard(housing_df3$total_toilet_count<2)
housing_df_warnings <- compact(housing_df3,housing_df3$sale_warning)

# Use the cbind and rbind function on your dataset
address_list<-cbind(housing_df$addr_full, housing_df$zip5)
head(address_list)


# Split a string, then concatenate the results back together

#with these two I found out that site types always have two characters
min(str_count(housing_df$sitetype))
max(str_count(housing_df$sitetype))

length(unique(housing_df$sitetype))
unique(housing_df$sitetype)

#As there are several numbers with the R types, it could be useful to split  to categorize them:
housing_df$sitetype_letter <- str_sub(housing_df$sitetype,1,1)
housing_df$sitetype_number <- str_sub(housing_df$sitetype,2,2)

#if for some reason I had to concatenate those two new fields:
str_c(housing_df$sitetype_letter[1], housing_df$sitetype_number[1],sep="")
