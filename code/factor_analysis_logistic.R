library("jsonlite")
library("ggplot2")
library("readr")
library("ggplot2")
#install.packages("glmnet")
library("MCMCpack")
library("dplyr")
library("VIM")
library("glmnet")
library("caret")

DATA_PATH="~/code/AppliedStatisticalModelling/Assignment3/dataset/"
#DATA_PATH="H:/TCD/Semester 2/AppliedStatisticalModelling/Assignments/Assignment3/Yelp-Dataset-Statistical-Modelling/dataset/"
VISUALS="visuals/"
BUSINESS_TORONTO_FILE="Business_Toronto_Restaurant.json"
REVIEW_TORONTO="Review_Toronto_Restaurant.json"
STARS_COLUMN="stars"
NEIGHBORHOOD_COLUMN="neighborhood"
CATEGORIES="categories"
MISSING_VALUE_THRESHOLD=715
PRUNING_THRESHOLD=5
KNN_VALUE=7
RELAVENT_CATEGORIES=c(2:11) ## 1 is Restaurants - we don't need this

BUSINESS_TORONTO=paste(DATA_PATH,BUSINESS_TORONTO_FILE,sep = "")
REVIEWS_TORONTO=paste(DATA_PATH,REVIEW_TORONTO,sep = "")

get_class_dataframe=function(dataframe){
  class_variables_filtered=dataframe %>%
    select(everything()) %>%  # replace to your needs
    summarise_all(funs(class((.))))
  class_variables_filtered=data.frame(t(class_variables_filtered))
  colnames(class_variables_filtered)=c("type")
  class_variables_filtered$index=seq(1:nrow(class_variables_filtered))
  class_variables_filtered
}

get_missing_count=function(dataframe){
  missing_filtered=dataframe %>%
    select(everything()) %>%  # replace to your needs
    summarise_all(funs(sum(is.na(.))))
  missing_filtered=data.frame(t(missing_filtered))
  colnames(missing_filtered)=c("missing_count")
  missing_filtered
}

business = stream_in(file(BUSINESS_TORONTO),flatten=TRUE)
review= stream_in(file(REVIEWS_TORONTO))
mean_count_reviews = review%>% 
  group_by("business_id"=business_id) %>% 
  summarise("Count"=n(),"Mean Stars"=mean(stars),"Sum Useful"=sum(useful),"Sum Funny"=sum(funny),"Sum Cool"=sum(cool))
business_review_merge=merge(business,mean_count_reviews,by.x="business_id",by.y="business_id")

business_review_merge[business_review_merge==""]  <- NA 

ggplot(business_review_merge) + stat_bin(aes(business_review_merge$review_count),bins=100)

missing_count_merged=get_missing_count(business_review_merge)
View(missing_count_merged)

business_review_pruned=subset(business_review_merge,review_count>PRUNING_THRESHOLD)
business_review_filtered=data.frame(business_review_pruned[ , ! apply( business_review_pruned , 2 , function(x) sum(is.na(x))>MISSING_VALUE_THRESHOLD)])

missing_count_filtered=get_missing_count(business_review_merge)
View(missing_count_filtered)

class_variables_filtered=get_class_dataframe(business_review_merge)
View(class_variables_filtered)
#We have Lat and Long, removing unnnecessary columns
cols_to_remove=c(1:6)
business_review_filtered=business_review_filtered[-cols_to_remove]

class_variables_filtered=get_class_dataframe(business_review_filtered)
View(class_variables_filtered)

#unlisting categories
cat_total <- unlist(business_review_filtered$categories)
class(cat_total)
length(cat_total)
cat_total <- factor(cat_total)
nlevels(cat_total)
cat_names_sort <- sort(table(cat_total), decreasing = TRUE)
head(cat_names_sort, n = 25)
cat_names <- names(cat_names_sort)[RELAVENT_CATEGORIES] 

cat_bus_ind_mat <- sapply(business_review_filtered$categories, function(y) as.numeric(cat_names %in% y))
cat_bus_ind_mat <- t(cat_bus_ind_mat)
colnames(cat_bus_ind_mat) <- cat_names
business_review_filtered_factorised_cat <- cbind(business_review_filtered, cat_bus_ind_mat)

View(get_class_dataframe(business_review_filtered_factorised_cat))

cols_to_remove_categories=c(6,35:36)
business_review_to_use=business_review_filtered_factorised_cat[-cols_to_remove_categories]
View(get_class_dataframe(business_review_to_use))
View(get_missing_count(business_review_to_use))

#cols_to_factor=c(5:33,37:46)
cols_to_factor=c(5,9,37:46)
business_review_to_use[cols_to_factor] <- lapply(business_review_to_use[cols_to_factor], factor)  ## as.factor() could also be used

business_review_impute=kNN(business_review_to_use,k=KNN_VALUE)
View(get_missing_count(business_review_impute))
View(get_class_dataframe(business_review_impute))

#remove excessive imputed cols
cols_imp_remove=c(47:92)
business_review_impute=business_review_impute[-cols_imp_remove]

cols_to_scale=c(4,34:36)
business_review_impute[cols_to_scale] <- apply(business_review_impute[cols_to_scale],2, scale)

library(randomForest)
RF_DF=RF_DF[-40]
View(get_class_dataframe(RF_DF))
cols_to_factor=c(7:8)
RF_DF[cols_to_factor] <- lapply(RF_DF[cols_to_factor], factor)  ## as.factor() could also be used
RF=randomForest(RF_DF$is_open ~ .,data=RF_DF)
print(RF)
importance(RF)
varImpPlot(RF)
plot(RF)


glm1 <- glm(is_open ~ . , data = business_review_impute, family = binomial())
head(predict(glm1)) ## values on log-odds scale
pred_glm <- plogis(predict(glm1)) ## this is logistic function, maps to [0,1]
boxplot(pred_glm  ~ business_review_impute$is_open)
table(pred_glm > 0.5, business_review_impute$is_open) ## 0.5 is an arbitrary threshold

business_features=model.matrix(is_open ~ . , business_review_impute)[,-1]
is_open <- business_review_impute$is_open
table(is_open, business_review_impute$is_open)
glm2 <- glmnet(business_features, is_open, family = "binomial")
plot(glm2, label = TRUE)

glm_cv <- cv.glmnet(business_features, is_open, family = "binomial")
plot(glm_cv)
coef(glm_cv, s="lambda.min")
coef(glm_cv, s="lambda.1se")

pred1 <- predict(glm_cv, newx = business_features, s="lambda.min", type = "class")
table(pred1, is_open)
confusionMatrix(table(pred1, is_open))

pred2 <- predict(glm_cv, newx = business_features, s="lambda.1se", type = "response")
boxplot(pred2 ~ is_open)


glm_imp <- glm(is_open ~ review_count+longitude+latitude+Sum.Useful+Sum.Cool+Sum.Funny+stars+attributes.RestaurantsPriceRange2 , data = business_review_impute, family = binomial())
head(predict(glm_imp)) ## values on log-odds scale
pred_glm_imp <- plogis(predict(glm_imp)) ## this is logistic function, maps to [0,1]
boxplot(pred_glm_imp  ~ business_review_impute$is_open)
table(pred_glm_imp > 0.5, business_review_impute$is_open) ## 0.5 is an arbitrary threshold
