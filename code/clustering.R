library("jsonlite")
library("ggplot2")
library("readr")
library("ggplot2")
#install.packages("dpylr")
library("MCMCpack")
library("mclust")
library("dplyr")

DATA_PATH="../dataset/"
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

cluster_best_VVV=Mclust(business_lat_long, G = 19 ,modelNames = "VVV")
cluster_best_VVV$parameters$mean
plot(cluster_best_VVV, what = "classification")
plot(cluster_best_VVV, what = "uncertainty")
table(cluster_check_1$classification, cluster_best_VVV$classification)

cluster_numbers=data.frame(cluster_best_VVV$classification,cluster_best_VVV$data)
business_cluster=merge(business,cluster_numbers,by=0)[-1]
avg_size=data.frame(size=tapply(business_cluster$stars, 
                                business_cluster$cluster_best_VVV.classification, length))
avg_size$row=row.names(avg_size)
max_restaurants_in_a_cluster <- max(avg_size$size)
avg_num_restaurants_in_a_cluster <- mean(avg_size$size)

avg_mean_rating=data.frame(mean_rating=tapply(business_cluster$stars, 
                                              business_cluster$cluster_best_VVV.classification, mean))
mean(avg_mean_rating$mean_rating)
avg_median_rating=data.frame(median_rating=tapply(business_cluster$stars, 
                                              business_cluster$cluster_best_VVV.classification, median))
mean(avg_median_rating$median_rating)
hist(avg_median_rating)

avg_mean_rating$row=row.names(avg_mean_rating)
avg_median_rating$row = row.names(avg_median_rating)

avg=merge(avg_size,avg_mean_rating,by.x="row",
          by.y="row")
avg=merge(avg,avg_median_rating,by.x="row",
          by.y="row")
ggplot(avg, 
       aes(as.numeric(factor(avg$row)), mean_rating))+
  geom_point(aes(size=size,color=row))+
  scale_x_continuous(breaks=row_number(avg$row))+
  geom_hline(aes(yintercept = mean(mean_rating)),color="black",linetype="dashed") + 
  geom_hline(aes(yintercept = mean(median_rating)),color="blue",linetype="dashed") +
  xlab("Cluster number")+
  ylab("Mean rating per cluster")

ggsave("cluster_mean_rating_size.jpeg")
