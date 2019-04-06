library("jsonlite")
library("ggplot2")
library("readr")
library("ggplot2")
#install.packages("MCMCpack")
library("MCMCpack")

#DATA_PATH="~/code/AppliedStatisticalModelling/Assignment3/dataset/"
DATA_PATH="H:/TCD/Semester 2/AppliedStatisticalModelling/Assignments/Assignment3/Yelp-Dataset-Statistical-Modelling/dataset/"
VISUALS="visuals/"
BUSINESS_TORONTO_FILE="Business_Toronto_Restaurant.json"
REVIEW_TORONTO="Review_Toronto_Restaurant.json"
NEIGHBORHOOD_1="Scarborough"
NEIGHBORHOOD_2="Etobicoke"
STARS_COLUMN="stars"
NEIGHBORHOOD_COLUMN="neighborhood"
CATEGORIES="categories"
INDIAN_TAG="Indian"

INITIAL_MEAN=2.5
INITIAL_VARIANCE=1.5625
INITIAL_DIFFERENCE=0
A_0=4
B_0=1.6
ITERATIONS=5000

INDIA_NEIGHBORHOOD_DISTRIBUTION="INDIAN RESTAURANT RATING DISTRIBUTION PER NEIGHBORHOOD"
INDIAN_NEIGHBORHOOD_RATING="INDIAN RESTAURANTS COUNT OF RATINGS PER NEIGHBORHOOD"
HISTOGRAM_DIFF_SIM_NEIGHBORHOOD_RATINGS="INDIAN RESTAURANT SIMULATED RATING DIFFERENCE"

BUSINESS_TORONTO=paste(DATA_PATH,BUSINESS_TORONTO_FILE,sep = "")

business = stream_in(file(BUSINESS_TORONTO))

names(business)
dim(business)

required_neighborhoods <- business$neighborhood == NEIGHBORHOOD_1 | business$neighborhood == NEIGHBORHOOD_2 
all_restaurant_ratings <- subset(business, neighborhood == NEIGHBORHOOD_1 | neighborhood == NEIGHBORHOOD_2, select =  c(STARS_COLUMN, NEIGHBORHOOD_COLUMN))


## Make a new indicator variable for neighborhood
restaurant_ratings$indicator <- as.numeric(factor(restaurant_ratings$neighborhood))

neighborhood_vs_rating=as.data.frame(table(restaurant_ratings$neighborhood, 
                                           restaurant_ratings$stars,dnn = c(NEIGHBORHOOD_COLUMN, STARS_COLUMN)))
ggplot(data=neighborhood_vs_rating,aes(neighborhood,stars))+
  geom_point(aes(size = Freq))

class(business$categories)
total_categories=unlist(business$categories)
total_categories=as.factor(total_categories)
number_of_categories=nlevels(total_categories)

## Which categories are most popular?business
all_categories=unlist(business$categories)
categories_count <- table(all_categories)
top25_category=as.data.frame(head(categories_count, n = 25))
business$IndianFlag=0

for(i in c(1:nrow(business))){
  if(INDIAN_TAG %in% unlist(business[i,]$categories)){
          business[i,]$IndianFlag<-1
  }
  else
  {
  business[i,]$IndianFlag<-0
}
}

indian_restaurants=subset(business,IndianFlag==1 & (neighborhood==NEIGHBORHOOD_1 | neighborhood==NEIGHBORHOOD_2))

ggplot(indian_restaurants) + geom_boxplot(aes(neighborhood, stars, fill = neighborhood)) + 
  geom_jitter(aes(neighborhood, stars, shape = neighborhood))+
  ggtitle(INDIA_NEIGHBORHOOD_DISTRIBUTION)

indian_neighborhood_vs_rating=as.data.frame(table(indian_restaurants$neighborhood, 
                                           indian_restaurants$stars,dnn = c(NEIGHBORHOOD_COLUMN, STARS_COLUMN)))

ggplot(data=indian_neighborhood_vs_rating,aes(neighborhood,stars,col=neighborhood))+
  geom_point(aes(size = Freq))+
  ggtitle(INDIAN_NEIGHBORHOOD_RATING)

tapply(indian_restaurants$stars, indian_restaurants$neighborhood, mean)
tapply(indian_restaurants$stars, indian_restaurants$neighborhood, median)
tapply(indian_restaurants$stars, indian_restaurants$neighborhood, sd)

indian_restaurants$indicator <- as.numeric(factor(indian_restaurants$neighborhood))

t.test(stars ~ neighborhood, data=indian_restaurants, var.equal = TRUE)
  
gibbs_sampler <- function(y, ind, mu0 = INITIAL_MEAN, tau0 = 1/INITIAL_VARIANCE, del0 = INITIAL_DIFFERENCE
                          , gamma0 = 1/INITIAL_VARIANCE, a0 = A_0, b0 = B_0, maxiter = ITERATIONS)
{
  y1 <- y[ind == 1]
  y2 <- y[ind == 2]
  
  n1 <- length(y1) 
  n2 <- length(y2)
  
  ##### starting values
  mu <- (mean(y1) + mean(y2)) / 2
  del <- (mean(y1) - mean(y2)) / 2
  
  mat_store <- matrix(0, nrow = maxiter, ncol = 3)
  #####
  
  ##### Gibbs sampler
  an <- a0 + (n1 + n2)/2
  
  for(s in 1 : maxiter) 
  {
    
    ##update tau
    bn <- b0 + 0.5 * (sum((y1 - mu - del) ^ 2) + sum((y2 - mu + del) ^ 2))
    tau <- rgamma(1, an, bn)
    ##
    
    ##update mu
    taun <-  tau0 + tau * (n1 + n2)
    mun <- (tau0 * mu0 + tau * (sum(y1 - del) + sum(y2 + del))) / taun
    mu <- rnorm(1, mun, sqrt(1/taun))
    ##
    
    ##update del
    gamman <-  gamma0 + tau*(n1 + n2)
    deln <- ( del0 * gamma0 + tau * (sum(y1 - mu) - sum(y2 - mu))) / gamman
    del<-rnorm(1, deln, sqrt(1/gamman))
    ##
    
    ## store parameter values
    mat_store[s, ] <- c(mu, del, tau)
  }
  colnames(mat_store) <- c("mu", "del", "tau")
  return(mat_store)
}

hierarchial_model=gibbs_sampler(indian_restaurants$stars,as.factor(indian_restaurants$indicator))
plot(as.mcmc(hierarchial_model))
raftery.diag(as.mcmc(hierarchial_model))
apply(hierarchial_model, 2, mean)
apply(hierarchial_model, 2, sd)
mean(1/sqrt(hierarchial_model[, 3])) 
sd(1/sqrt(hierarchial_model[, 3])) 

#1=Etobicoke, 2=Scarborough
#simulation
y1_sim <- rnorm(ITERATIONS, hierarchial_model[, 1] + hierarchial_model[, 2], sd = 1/sqrt(hierarchial_model[, 3]))
y2_sim <- rnorm(ITERATIONS, hierarchial_model[, 1] - hierarchial_model[, 2], sd = 1/sqrt(hierarchial_model[, 3]))

ggplot(data.frame(y_sim_diff=y1_sim - y2_sim)) + stat_bin(aes(y1_sim - y2_sim),bins=12)+
  ggtitle(HISTOGRAM_DIFF_SIM_NEIGHBORHOOD_RATINGS)#CHANGE COLOR AND ADD BOUNDARY

mean(y1_sim > y2_sim)

ggplot(data.frame(y1_sim, y2_sim)) + geom_point(aes(y1_sim, y2_sim,col=TRUE, alpha = 0.3))+geom_abline(slope = 1, intercept = 0)


#Multiple Models
business_with_neighborhood=subset(business,business$neighborhood!="")
business_with_neighborhood$neighborhood_indicator = as.numeric(factor(business_with_neighborhood$neighborhood))
#Think about how to show this
ggplot(business_with_neighborhood) + 
  geom_boxplot(aes(x = reorder(neighborhood_indicator, stars, median), 
                                    stars, 
                                    fill = reorder(neighborhood_indicator, stars, median)), show.legend=FALSE)
ggplot(business_with_neighborhood, 
       aes(x = reorder(neighborhood_indicator, stars, length))) + stat_count()

ggplot(business_with_neighborhood, aes(stars)) + stat_bin(bins = 10)

avg_size=data.frame(size=tapply(business_with_neighborhood$stars, 
           business_with_neighborhood$neighborhood, length))
avg_size$neighborhood=row.names(avg_size)
avg_mean_rating=data.frame(mean_rating=tapply(business_with_neighborhood$stars, 
                business_with_neighborhood$neighborhood, mean))
avg_mean_rating$neighborhood=row.names(avg_mean_rating)

avg=merge(avg_size,avg_mean_rating,by.x=NEIGHBORHOOD_COLUMN,
          by.y=NEIGHBORHOOD_COLUMN)
ggplot(avg, 
       aes(as.numeric(factor(neighborhood)), mean_rating))+
  geom_point(aes(size=size))+
  scale_x_continuous(breaks=row_number(avg$size))

ggplot(avg, 
       aes(size, mean_rating,col=(factor(neighborhood))))+
  geom_point()


             