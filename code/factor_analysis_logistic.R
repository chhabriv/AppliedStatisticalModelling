library("jsonlite")
library("ggplot2")
library("readr")
library("ggplot2")
#install.packages("MCMCpack")
library("MCMCpack")
library("dplyr")
library("glmnet")
library("VIM")


#DATA_PATH="~/code/AppliedStatisticalModelling/Assignment3/dataset/"
DATA_PATH = "../dataset/"
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
#impute <- kNN(business,k=5)
mean_count_reviews = review%>% 
  group_by("business_id"=business_id) %>% 
  summarise("Count"=n(),"Mean Stars"=mean(stars),"Sum Useful"=sum(useful),"Sum Funny"=sum(funny),"Sum Cool"=sum(cool))
business_review_merge=merge(business,mean_count_reviews,by.x="business_id",by.y="business_id")

business_review_merge[business_review_merge==""]  <- NA 

ggplot(business_review_merge) + stat_bin(aes(business_review_merge$review_count),bins=100)

#business_review_pruned=subset(business_review_merge,review_count>PRUNING_THRESHOLD)

missing_count_merged=business_review_merge %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))))
#View(t(missing_count_merged))

business_review_filtered=data.frame(business_review_merge[ , ! apply( business_review_merge , 2 , function(x) sum(is.na(x))>MISSING_VALUE_THRESHOLD)])

#business_review_pruned=subset(business_review_filtered,review_count>PRUNING_THRESHOLD)

missing_count_filtered=business_review_filtered %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))))
#View(t(missing_count_filtered))

summary(business_review_filtered)

names(business_review_filtered)
without_category_df <- business_review_filtered[,-which(names(business_review_filtered)=="categories")]

to_impute_cols <- c("attributes.RestaurantsPriceRange2",
                    "attributes.GoodForKids","attributes.RestaurantsGoodForGroups",
                    "attributes.RestaurantsTakeOut")
impute <- kNN(without_category_df,variable =to_impute_cols,k=5)

summary(impute)

names(business)

head(business_review_filtered$categories)

#without_category_df["categories"] <- business_review_pruned["categories"]



without_category_df$scaled_review_count <-scale(without_category_df$review_count)
without_category_df$scaled_useful_sum <- scale(without_category_df$Sum.Useful)
without_category_df$scaled_funny_sum <- scale(without_category_df$Sum.Funny)
without_category_df$scaled_cool_sum <- scale(without_category_df$Sum.Cool)

df_factor_analysis_model <- without_category_df[,-which(names(without_category_df)=="review_count")]
df_factor_analysis_model <- df_factor_analysis_model[,-which(names(df_factor_analysis_model)=="address")]
df_factor_analysis_model <- df_factor_analysis_model[,-which(names(df_factor_analysis_model)=="city")]
df_factor_analysis_model <- df_factor_analysis_model[,-which(names(df_factor_analysis_model)=="state")]
df_factor_analysis_model <- df_factor_analysis_model[,-which(names(df_factor_analysis_model)=="postal_code")]
df_factor_analysis_model <- df_factor_analysis_model[,-which(names(df_factor_analysis_model)=="business_id")]
df_factor_analysis_model <- df_factor_analysis_model[,-which(names(df_factor_analysis_model)=="name")]
df_factor_analysis_model <- df_factor_analysis_model[,-which(names(df_factor_analysis_model)=="scaled_review_count")]
df_factor_analysis_model <- df_factor_analysis_model[,-which(names(df_factor_analysis_model)=="scaled_useful_sum")]
df_factor_analysis_model <- df_factor_analysis_model[,-which(names(df_factor_analysis_model)=="scaled_funny_sum")]
df_factor_analysis_model <- df_factor_analysis_model[,-which(names(df_factor_analysis_model)=="scaled_cool_sum")]

#business_review_pruned=subset(df_factor_analysis_model,review_count>PRUNING_THRESHOLD)

cat_total <- as.factor(unlist(df_factor_analysis_model$categories))
class(cat_total)
length(cat_total)
cat_total <- factor(cat_total)
nlevels(cat_total)
cat_total <- as.factor(unlist(df_factor_analysis_model$categories))
cat_names_sort <- sort(table(cat_total), decreasing = TRUE)
head(cat_names_sort, n = 25)
tail(cat_names_sort, n = 25)
cat_names_sort
cat_names <- names(cat_names_sort)[-1] ## 1 is Restaurants - we don't need this
cat_bus_ind_mat <- sapply(df_factor_analysis_model$categories, function(y) as.factor(cat_names %in% y))
cat_bus_ind_mat
cat_bus_ind_mat <- t(cat_bus_ind_mat)
colnames(cat_bus_ind_mat) <- cat_names
df_TO_tidy_cat <- cbind(df_factor_analysis_model, cat_bus_ind_mat)
tail(df_TO_tidy_cat)
df_TO_tidy_cat <- df_TO_tidy_cat[,-which(names(df_TO_tidy_cat)=="categories")]


##Logistic regression
length(df_factor_analysis_model$is_open)
View(df_factor_analysis_model)
cols=c(4,5:8,11:13)
df_factor_analysis_model[cols]=lapply(df_factor_analysis_model[cols], factor)
glm1 <- glm(is_open ~ ., data = impute, family = binomial())
glm1$coefficients
sum(is.na(df_factor_analysis_model))
pred_glm <- plogis(predict(glm1)) ## this is logistic function, maps to [0,1]
boxplot(pred_glm  ~ impute$is_open)
View(df_factor_analysis_model$is_open)
table(pred_glm > 0.5, df_factor_analysis_model$is_open)
impute <- kNN(df_factor_analysis_model,variable =to_impute_cols,k=5)



##Bayesian Logistic

attributes <-  "longitude + latitude + scaled_review_count + scaled_cool_sum +scaled_funny_sum+ scaled_useful_sum"

fit_open_closed <- MCMClogit(is_open ~ ., data=df_factor_analysis_model, burnin = 10, mcmc=5000, thin = 1, tune = 0.5, beta.start = 0)

plot(fit_open_closed)

acf(fit_open_closed)

raftery.diag(fit_open_closed)

fit_open_closed <- MCMClogit(is_open ~ longitude + latitude + scaled_review_count + scaled_cool_sum +scaled_funny_sum+ scaled_useful_sum, data=business_review_filtered, burnin = 10, mcmc=50000, thin = 10, tune = 0.5, beta.start = 0)

plot(fit_open_closed)

acf(fit_open_closed)

raftery.diag(fit_open_closed)

##Logistic with Lasso
y_open_closed <- df_factor_analysis_model$is_open
cols <- c("longitude","latitude","scaled_review_count","scaled_cool_sum","scaled_funny_sum","scaled_useful_sum")
new_df <- df_factor_analysis_model[,-which(names(df_factor_analysis_model)=="is_open")]
new_df <- subset(df_factor_analysis_model,select = -"is_open")
x_open_closed <- as.matrix(new_df)
#x_open_cor <- cor(new_df)

corr(new_df,cols,0,call)

open_closed_fit <- glmnet(x_open_closed, y_open_closed, family = "binomial")
plot(open_closed_fit, label = TRUE)
open_closed_fit
coef(open_closed_fit, s=0.02)

open_closed_fitcv <- cv.glmnet(x_open_closed, y_open_closed, family = "binomial")
plot(open_closed_fitcv)

coef(open_closed_fitcv, s="lambda.min")

coef(open_closed_fitcv, s="lambda.1se")

pred1 <- predict(open_closed_fitcv, newx = x_open_closed, s="lambda.min", type = "class")
table(pred1, y_open_closed)

pred2 <- predict(open_closed_fitcv, newx = x_open_closed, s="lambda.1se", type = "response")
boxplot(pred2 ~ y_open_closed)

coef(open_closed_fitcv, s="lambda.1se")




corr = function(Data,cols,exclude,call){
  if (exclude == 1) {
    cormat <- round(cor(Data[-cols],use="complete.obs"),2)
  }
  else
  {
    cormat <- round(cor(Data[cols],use="complete.obs"),2)
  }
  print(cormat)
  melted_cormat <- melt(cormat)
  print(melted_cormat)
  ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()
  #ggsave(filename=paste("Correlation Heatmap",call,".jpg",sep=""),plot=last_plot(),
         #device="jpeg",path=path)
  #,width = 4.55, height = 2.4)
}
