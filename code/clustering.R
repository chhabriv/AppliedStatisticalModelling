library("jsonlite")
library("ggplot2")
library("readr")
library("ggplot2")
#install.packages("dpylr")
library("MCMCpack")
library("mclust")
library("dplyr")

DATA_PATH="~/code/AppliedStatisticalModelling/Assignment3/dataset/"
#DATA_PATH="H:/TCD/Semester 2/AppliedStatisticalModelling/Assignments/Assignment3/Yelp-Dataset-Statistical-Modelling/dataset/"
VISUALS="visuals/"
BUSINESS_TORONTO_FILE="Business_Toronto_Restaurant.json"
REVIEW_TORONTO="Review_Toronto_Restaurant.json"
NEIGHBORHOOD_1="Scarborough"
NEIGHBORHOOD_2="Etobicoke"
STARS_COLUMN="stars"
NEIGHBORHOOD_COLUMN="neighborhood"
LAT_COLUMN="latitude"
LONG_COLUMN="longitude"
CATEGORIES="categories"
INDIAN_TAG="Indian"

BUSINESS_TORONTO=paste(DATA_PATH,BUSINESS_TORONTO_FILE,sep = "")

business = stream_in(file(BUSINESS_TORONTO),flatten=TRUE)
lat_long_cols=c(LONG_COLUMN,LAT_COLUMN)
business_lat_long=business[,lat_long_cols]
plot(business_lat_long)
cluster_check_1=Mclust(business_lat_long)
cluster_check_2=Mclust(business_lat_long,G = c(10:19))
cluster_check_3=Mclust(business_lat_long,G = c(20:29))
cluster_check_4=Mclust(business_lat_long,G = c(30:39))

plot(cluster_check_1, what = "BIC")
plot(cluster_check_2, what = "BIC")
plot(cluster_check_3, what = "BIC")
plot(cluster_check_4, what = "BIC")

cluster_check_1$BIC
cluster_check_2$BIC
cluster_check_3$BIC
cluster_check_4$BIC

cluster_check_1$parameters$mean
cluster_check_2$parameters$mean
cluster_check_3$parameters$mean
cluster_check_4$parameters$mean

cluster_best_VVV=Mclust(business_lat_long, G = 9 ,modelNames = "VVV")
cluster_best_VVV$parameters$mean
plot(cluster_best_VVV, what = "classification")
plot(cluster_best_VVV, what = "uncertainty")
table(cluster_check_1$classification, cluster_best_VVV$classification)

cluster_numbers=data.frame(cluster_best_VVV$classification,cluster_best_VVV$data)
business_cluster=merge(business,cluster_numbers,by=0)[-1]
avg_size=data.frame(size=tapply(business_cluster$stars, 
                                business_cluster$cluster_best_VVV.classification, length))
avg_size$row=row.names(avg_size)
avg_mean_rating=data.frame(mean_rating=tapply(business_cluster$stars, 
                                              business_cluster$cluster_best_VVV.classification, mean))
avg_mean_rating$row=row.names(avg_mean_rating)

avg=merge(avg_size,avg_mean_rating,by.x="row",
          by.y="row")
ggplot(avg, 
       aes(as.numeric(factor(avg$row)), mean_rating))+
  geom_point(aes(size=size))+
  scale_x_continuous(breaks=row_number(avg$row))

                     