library("jsonlite")
library("ggplot2")
library("readr")
library("ggplot2")
#install.packages("MCMCpack")
library("MCMCpack")
library(dplyr)
DATA_PATH="~/code/AppliedStatisticalModelling/Assignment3/dataset/"
#DATA_PATH="H:/TCD/Semester 2/AppliedStatisticalModelling/Assignments/Assignment3/Yelp-Dataset-Statistical-Modelling/dataset/"
VISUALS="visuals/"
BUSINESS_TORONTO_FILE="Business_Toronto_Restaurant.json"
REVIEW_TORONTO="Review_Toronto_Restaurant.json"
STARS_COLUMN="stars"
NEIGHBORHOOD_COLUMN="neighborhood"
CATEGORIES="categories"
MISSING_VALUE_THRESHOLD=1000
PRUNING_THRESHOLD=5

BUSINESS_TORONTO=paste(DATA_PATH,BUSINESS_TORONTO_FILE,sep = "")
REVIEWS_TORONTO=paste(DATA_PATH,REVIEW_TORONTO,sep = "")

business = stream_in(file(BUSINESS_TORONTO),flatten=TRUE)
review= stream_in(file(REVIEWS_TORONTO))
mean_count_reviews = review%>% 
  group_by("business_id"=business_id) %>% 
  summarise("Count"=n(),"Mean Stars"=mean(stars),"Sum Useful"=sum(useful),"Sum Funny"=sum(funny),"Sum Cool"=sum(cool))
business_review_merge=merge(business,mean_count_reviews,by.x="business_id",by.y="business_id")

business_review_merge[business_review_merge==""]  <- NA 

ggplot(business_review_merge) + stat_bin(aes(business_review_merge$review_count),bins=100)

business_review_pruned=subset(business_review_merge,review_count>PRUNING_THRESHOLD)

missing_count_merged=business_review_merge %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))))
View(t(missing_count_merged))

business_review_filtered=data.frame(business_review_merge[ , ! apply( business_review_merge , 2 , function(x) sum(is.na(x))>MISSING_VALUE_THRESHOLD)])

missing_count_filtered=business_review_filtered %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))))
View(t(missing_count_filtered))
