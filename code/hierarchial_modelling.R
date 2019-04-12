library("jsonlite")
library("ggplot2")
library("readr")
#install.packages("MCMCpack")
library("MCMCpack")

#DATA_PATH="~/code/AppliedStatisticalModelling/Assignment3/dataset/"
<<<<<<< HEAD
#PATH="H:/TCD/Semester 2/AppliedStatisticalModelling/Assignments/Assignment3/Yelp-Dataset-Statistical-Modelling/"
PATH="../dataset/"
=======
PATH="~/code/AppliedStatisticalModelling/Assignment3/"
#PATH="H:/TCD/Semester 2/AppliedStatisticalModelling/Assignments/Assignment3/Yelp-Dataset-Statistical-Modelling/"
>>>>>>> final code and changes
DATA_FOLDER="dataset/"
DATA_PATH=paste(PATH,DATA_FOLDER,sep="")
PLOT_FOLDER="visuals/"
VISUALS=paste(PATH,PLOT_FOLDER,sep="")
NEIGHBORHOOD_MAPPING_FILENAME="NeighborhoodMapping.csv"
BUSINESS_TORONTO_FILE="Business_Toronto_Restaurant.json"
REVIEW_TORONTO="Review_Toronto_Restaurant.json"
NEIGHBORHOOD_1="Scarborough"
NEIGHBORHOOD_2="Etobicoke"
STARS_COLUMN="stars"
NEIGHBORHOOD_COLUMN="neighborhood"
CATEGORIES="categories"
INDIAN_TAG="Indian"

save_plot=function(title){
  ggsave(filename=paste(gsub(" ","",title),".jpeg",sep=""),plot=last_plot(),
         device="jpeg",path=VISUALS,width=12,height = 8,dpi=300)
}

INITIAL_MEAN=2.5
INITIAL_VARIANCE=1.5625
INITIAL_DIFFERENCE=0
A_0=2
B_0=2*INITIAL_VARIANCE
ITERATIONS=5000
KAPPA=2

NEIGHBORHOOD_RATING_DISTRIBUTION="NEIGHBORHOOD Vs RESTAURANT RATINGS"
INDIA_NEIGHBORHOOD_DISTRIBUTION="INDIAN RESTAURANT RATING DISTRIBUTION PER NEIGHBORHOOD"
INDIAN_NEIGHBORHOOD_RATING="INDIAN RESTAURANTS COUNT OF RATINGS PER NEIGHBORHOOD"
HISTOGRAM_DIFF_SIM_NEIGHBORHOOD_RATINGS="INDIAN RESTAURANT SIMULATED RATING DIFFERENCE"
MEAN_SAMPLE_RATING_VS_NUMBER_RESTAURANTS_NEIGHBORHOOD="MEAN RATING FROM SAMPLES VS NUMBER OF RESTAURANTS PER NEIGHBORHOOD"
MEAN_SIM_RATING_NEIGHBORHOOD_SIZE="MEAN RATING FROM SAMPLES AGAINST NUMBER OF RESTAURANTS PER NEIGHBORHOOD"
Y1SIM_VS_Y2SIM="Simulated ratings for Etobicoke(Y1) and Scarborough(Y2)"
NEIGHBORHOOD_MEDIAN_RATING_NEIGHBORHOOD_SIZE="Median ratings of restaurants in a neighborhood by size"
NEIGHBORHOOD_MEAN_RATING_NEIGHBORHOOD_SIZE="Mean ratings of restaurants in a neighborhood by size"
MEAN_RATING_NEIGHBORHOOD_SIZE="Mean ratings of each neighborhood by size"
MCMC_PLOT_2_NEIGHBORHOOD="Question1MCMC"
HISTOGRAM_NEIGHBORHOOD_RATING_DISTRIBUTION="Histogram of Restaurant Rating Distribution"
NEIGHBORHOOD_SIZE_RATING="Number of Restaurants per Neighborhood"
BUSINESS_TORONTO=paste(DATA_PATH,BUSINESS_TORONTO_FILE,sep = "")

business = stream_in(file(BUSINESS_TORONTO))

names(business)
dim(business)

required_neighborhoods <- business$neighborhood == NEIGHBORHOOD_1 | business$neighborhood == NEIGHBORHOOD_2 
all_restaurant_ratings <- subset(business, neighborhood == NEIGHBORHOOD_1 | neighborhood == NEIGHBORHOOD_2, select =  c(STARS_COLUMN, NEIGHBORHOOD_COLUMN))


## Make a new indicator variable for neighborhood
all_restaurant_ratings$indicator <- as.numeric(factor(all_restaurant_ratings$neighborhood))

neighborhood_vs_rating=as.data.frame(table(all_restaurant_ratings$neighborhood, 
                                           all_restaurant_ratings$stars,dnn = c(NEIGHBORHOOD_COLUMN, STARS_COLUMN)))
ggplot(data=neighborhood_vs_rating,aes(neighborhood,stars))+
  geom_point(aes(size = Freq))+
  ggtitle(NEIGHBORHOOD_RATING_DISTRIBUTION)
save_plot(NEIGHBORHOOD_RATING_DISTRIBUTION)

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

indian_restaurants=subset(business,IndianFlag==1 & 
                            (neighborhood==NEIGHBORHOOD_1 | neighborhood==NEIGHBORHOOD_2) &
                            is_open==1)

ggplot(indian_restaurants) + geom_boxplot(aes(neighborhood, stars, fill = neighborhood)) + 
  geom_jitter(aes(neighborhood, stars, shape = neighborhood))+
  ggtitle(INDIA_NEIGHBORHOOD_DISTRIBUTION)+
  theme(plot.title = element_text(face="bold",hjust = 0.5))
save_plot(INDIA_NEIGHBORHOOD_DISTRIBUTION)

indian_neighborhood_vs_rating=as.data.frame(table(indian_restaurants$neighborhood, 
                                           indian_restaurants$stars,dnn = c(NEIGHBORHOOD_COLUMN, STARS_COLUMN)))

ggplot(data=indian_neighborhood_vs_rating,aes(neighborhood,stars,col=neighborhood))+
  geom_point(aes(size = Freq))+
  ggtitle(INDIAN_NEIGHBORHOOD_RATING)+
  theme(plot.title = element_text(face="bold",hjust = 0.5))
save_plot(INDIAN_NEIGHBORHOOD_RATING)

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
jpeg(paste(VISUALS,MCMC_PLOT_2_NEIGHBORHOOD,".jpeg",sep=""))
plot(as.mcmc(hierarchial_model))
dev.off()
raftery.diag(as.mcmc(hierarchial_model))
apply(hierarchial_model, 2, mean)
apply(hierarchial_model, 2, sd)
mean(1/sqrt(hierarchial_model[, 3])) 
sd(1/sqrt(hierarchial_model[, 3])) 

#1=Etobicoke, 2=Scarborough
#simulation
y1_sim <- rnorm(ITERATIONS, hierarchial_model[, 1] + hierarchial_model[, 2], sd = 1/sqrt(hierarchial_model[, 3]))
y2_sim <- rnorm(ITERATIONS, hierarchial_model[, 1] - hierarchial_model[, 2], sd = 1/sqrt(hierarchial_model[, 3]))

ggplot(data.frame(y_sim_diff=y1_sim - y2_sim)) + 
  stat_bin(aes(y1_sim - y2_sim),bins=12,fill="#619CFF",color="black")+
  ggtitle(HISTOGRAM_DIFF_SIM_NEIGHBORHOOD_RATINGS)+
  theme(plot.title = element_text(face="bold",hjust = 0.5))
save_plot(HISTOGRAM_DIFF_SIM_NEIGHBORHOOD_RATINGS)

mean(y1_sim > y2_sim)

ggplot(data.frame(y1_sim, y2_sim)) + 
  geom_point(aes(y1_sim, y2_sim, alpha = 0.3, colour = y1_sim)) +
  geom_abline(slope = 1, intercept = 0)+
  theme(plot.title = element_text(face="bold",hjust = 0.5),
        legend.position = "none")+
  ggtitle(Y1SIM_VS_Y2SIM)
save_plot(Y1SIM_VS_Y2SIM)

#Multiple Models
business_with_neighborhood=subset(business,business$neighborhood!="")
business_with_neighborhood$neighborhood_indicator = as.numeric(factor(business_with_neighborhood$neighborhood))
#Think about how to show this
ggplot(business_with_neighborhood) + 
  geom_boxplot(aes(x = reorder(neighborhood, stars, median), 
                                    stars, 
                                    fill = reorder(neighborhood, stars, median)), 
               show.legend=FALSE)+  
  theme(plot.title = element_text(hjust = 0.5),
  axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle(NEIGHBORHOOD_MEDIAN_RATING_NEIGHBORHOOD_SIZE)
save_plot(NEIGHBORHOOD_MEDIAN_RATING_NEIGHBORHOOD_SIZE)

ggplot(business_with_neighborhood, 
       aes(x = reorder(neighborhood, stars, length))) + stat_count(fill="#619CFF",color="black")+  
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle(NEIGHBORHOOD_SIZE_RATING)
save_plot(NEIGHBORHOOD_SIZE_RATING)

write.csv(unique(data.frame("Neighborhood"=business_with_neighborhood$neighborhood,
                       "Neighborhood Code"=business_with_neighborhood$neighborhood_indicator,
                       check.names = FALSE)),
          paste(DATA_FOLDER,NEIGHBORHOOD_MAPPING_FILENAME,sep=""))

ggplot(business_with_neighborhood, aes(stars)) + 
  stat_bin(bins = 10,fill="#619CFF",color="black")+
ggtitle(HISTOGRAM_NEIGHBORHOOD_RATING_DISTRIBUTION)+
  theme(plot.title = element_text(face="bold",hjust = 0.5))
save_plot(HISTOGRAM_NEIGHBORHOOD_RATING_DISTRIBUTION)

avg_size=data.frame(size=tapply(business_with_neighborhood$stars, 
           business_with_neighborhood$neighborhood, length))
avg_size$neighborhood=row.names(avg_size)
avg_restaurant_count_per_neighborhood <- mean(avg_size)
avg_mean_rating=data.frame(mean_rating=tapply(business_with_neighborhood$stars, 
                business_with_neighborhood$neighborhood, mean))
avg_mean_rating$neighborhood=row.names(avg_mean_rating)

avg=merge(avg_size,avg_mean_rating,by.x=NEIGHBORHOOD_COLUMN,
          by.y=NEIGHBORHOOD_COLUMN)

ggplot(avg, 
       aes(size, mean_rating))+
  geom_point()+
  geom_hline(yintercept = mean(avg$mean_rating), colour="blue", linetype="dashed")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle(MEAN_RATING_NEIGHBORHOOD_SIZE)
save_plot(MEAN_RATING_NEIGHBORHOOD_SIZE)

ggplot(avg, 
       aes(neighborhood, mean_rating))+
  geom_point(aes(size=size))+
  geom_hline(yintercept = mean(avg$mean_rating), colour="blue", linetype="dashed")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle(NEIGHBORHOOD_MEAN_RATING_NEIGHBORHOOD_SIZE)
save_plot(NEIGHBORHOOD_MEAN_RATING_NEIGHBORHOOD_SIZE)

gibbs_sampler_multiple <- function(y, ind, maxiter = ITERATIONS)
{
  ### weakly informative priors
  a0 <- KAPPA ; b0 <- KAPPA*(INITIAL_VARIANCE^2) ## tau_w hyperparameters
  eta0 <-KAPPA ; t0 <-KAPPA*(INITIAL_VARIANCE^2) ## tau_b hyperparameters
  mu0<-INITIAL_MEAN ; gamma0 <- 1/INITIAL_VARIANCE
  ###
  

  ### starting values
  m <- nlevels(ind)
  ybar <- theta <- tapply(y, ind, mean)
  a <- tapply(y, ind, var)
  tau_w <- mean(1 / tapply(y, ind, var)) ##within group precision
  mu <- mean(theta)
  tau_b <-var(theta) ##between group precision
  n_m <- tapply(y, ind, length)
  an <- a0 + sum(n_m)/2
  ###
  
  ### setup MCMC
  theta_mat <- matrix(0, nrow=maxiter, ncol=m)
  mat_store <- matrix(0, nrow=maxiter, ncol=3)
  ###
  
  ### MCMC algorithm
  for(s in 1:maxiter) 
  {
    
    # sample new values of the thetas
    for(j in 1:m) 
    {
      taun <- n_m[j] * tau_w + tau_b
      #print(tau_w)
      thetan <- (ybar[j] * n_m[j] * tau_w + mu * tau_b) / taun
      theta[j]<-rnorm(1, thetan, 1/sqrt(taun))
    }
    
    #sample new value of tau_w
    ss <- 0
    for(j in 1:m){
      ss <- ss + sum((y[ind == j] - theta[j])^2)
    }
    bn <- b0 + ss/2
    tau_w <- rgamma(1, an, bn)
    
    #sample a new value of mu
    gammam <- m * tau_b + gamma0
    mum <- (mean(theta) * m * tau_b + mu0 * gamma0) / gammam
    mu <- rnorm(1, mum, 1/ sqrt(gammam)) 
    
    # sample a new value of tau_b
    etam <- eta0 + m/2
    tm <- t0 + sum((theta-mu)^2)/2
    tau_b <- rgamma(1, etam, tm)
    
    #store results
    theta_mat[s,] <- theta
    mat_store[s, ] <- c(mu, tau_w, tau_b)
  }
  colnames(mat_store) <- c("mu", "tau_w", "tau_b")
  return(list(params = mat_store, theta = theta_mat))
}


business_neighborhood_open=subset(business_with_neighborhood,(!(business_with_neighborhood$neighborhood_indicator == 14
                                                                |business_with_neighborhood$neighborhood_indicator == 38)) & is_open==1)

multi_hierarchial_model=gibbs_sampler_multiple(business_neighborhood_open$stars, factor(business_neighborhood_open$neighborhood_indicator))
apply(multi_hierarchial_model$params, 2, mean)
apply(multi_hierarchial_model$params, 2, sd)
mean(1/sqrt(multi_hierarchial_model$params[, 3]))
sd(1/sqrt(multi_hierarchial_model$params[, 3]))
theta_hat <- apply(multi_hierarchial_model$theta, 2, mean)

result=data.frame(size = tapply(business_neighborhood_open$stars, business_neighborhood_open$neighborhood, length), 
                  theta_hat = theta_hat)
result$neighborhood=row.names(result)

ggplot(data.frame(size = tapply(business_neighborhood_open$stars, business_neighborhood_open$neighborhood, length), 
                  theta_hat = theta_hat), aes(size, theta_hat)) + 
  geom_point()+
  ggtitle(MEAN_SAMPLE_RATING_VS_NUMBER_RESTAURANTS_NEIGHBORHOOD)+
  theme(plot.title = element_text(hjust = 0.5))
save_plot(MEAN_SAMPLE_RATING_VS_NUMBER_RESTAURANTS_NEIGHBORHOOD)

ggplot(result, aes(neighborhood, theta_hat)) + 
  geom_point(aes(size=result$size))+
  ggtitle(MEAN_SIM_RATING_NEIGHBORHOOD_SIZE)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_hline(yintercept = mean(result$theta_hat), colour="blue", linetype="dashed")+
save_plot(MEAN_SIM_RATING_NEIGHBORHOOD_SIZE)
