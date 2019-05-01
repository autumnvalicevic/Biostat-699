##Aim 1: Determine the number of patients to be enrolled to detect a reduction of the 50% historical rate of aGVHD to 40%, 35%, and 30%.

##Aim 2: Design a study to include one interim analysis of futility, where a 50% rate of aGVHD is considered to be ineffective and the study should be terminated early, and assess how this affects the sample size from aim 1.


#Sample size simulation for aim 1
## Function to see what sample size is needed for 80% power with type II error rate of 0.05
sample_size_sim <- function(reduction, begin, end, sims = 10000){
  #Set range of sample size for the simulation
  N <- c(begin:end)
  #Set compliers of potato starch vector
  compliers <- c()
  #Set non-compliers of potato starch vector
  non_compliers <- c()
  #Power vector
  power <- c()
  for (n in 1:length(N)){
    sig <- c()
    for (i in 1:sims){
      compliers <- rbinom(N[n]*0.8, 1, reduction)
      non_compliers <- rbinom(N[n]*0.2, 1, runif(1,reduction,0.5))
      total_pop <- c(compliers, non_compliers)
      proportions.test <- prop.test(sum(total_pop),N[n],0.5)
      if (proportions.test$p.value < 0.05) {sig[i] = 1} else {sig[i] = 0}
    }
    power[n] <- sum(sig)/sims
  }
}

#Sample Size for Aim 2. We add in an interim analysis. We chose that under the null of 0.5 we want to only accept 20% of those. 
#Under the alternative we want to accept at least 70%

#Sample size at 30% calculated in aim 1 without the increase from dropout
N <- 66
#Sample size at 35% calculated in aim 1 without the increase from dropout
#N <- 116
#Sample size at 40% calculated in aim 1 without the increase from dropout
#N <- 254
#Set compliers of potato starch vector
compliers <- c()
#Set non-compliers of potato starch vector
non_compliers <- c()
#Set reduction we want to see, changed from 0.4, 0.35, and 0.3 and changed to 50% to get p-value for the interim analysis
reduction <- 0.3
#Power vector
power <- c()
power.i <- c()
#Set number of simulations
sims <- 10000
for (n in 1:length(N)){
  sig <- c()
  stop <- c()
  for (i in 1:sims){
    #### Interim analysis
    ## compliers of eating potato starch
    compliers.i <- rbinom(round(N[n]*0.5*0.8), 1, reduction)
    ## Non-compliers of eating potato starch
    non_compliers.i <- rbinom(round(N[n]*0.5*0.2), 1, runif(1,reduction,0.5))
    ## Merging compliers and non-compliers together
    total_pop.i <- c(compliers.i, non_compliers.i)
    ## Test of proportions to see if it's significantly different from 0.5
    proportions.test.i <- prop.test(sum(total_pop.i),length(total_pop.i),0.5)
    # Adjusted p-value to accept at least 70% under the alternative of either 0.3, 0.35, or 0.4 and accept less than 20% under the alternative of 0.5
    if (proportions.test.i$p.value > 0.45) {stop[i] = 1} else {stop[i] = 0}
    
    #aGVHD outcome for complete compliers with potato startch
    compliers <- rbinom(round((N[n] - length(total_pop.i))*0.8), 1, reduction)
    non_compliers <- rbinom(round((N[n] - length(total_pop.i))*0.2), 1, runif(1,reduction,0.5))
    total_pop <- c(compliers, compliers.i, non_compliers, non_compliers.i)
    proportions.test <- prop.test(sum(total_pop),N[n],0.5)
    # We could have changed this last p value cut off if we did not obtain 0.05 type II error rate but the error rate was still 0.05
    if (proportions.test$p.value < 0.05) {sig[i] = 1} else {sig[i] = 0}
    if (stop[i] == 1 & sig[i] == 1) {incorrect[i] = 1} else {incorrect[i] = 0}
    if (stop[i] == 1) {sig[i] = 0} 
  }
  # Want to see the percent of times that we stop at interim
  pct.stop[n] <- sum(stop)/sims
  #Calculate power
  power[n] <- sum(sig)/sims
  #Calcuate power of interim analysis
  power.i[n] <- (length(stop)-sum(stop))/sims
}

reduction <- rep(N,0.3)
power.curve.30 <- data.frame(cbind(power, N,reduction))
#Increase sample size due to 15% dropout rate
power.curve.30$N <- power.curve.30$N * (1/.85)

reduction <- rep(N,0.35)
power.curve.35 <- data.frame(cbind(power, N,reduction))
#Increase sample size due to 15% dropout rate
power.curve.35$N <- power.curve.35$N * (1/.85)

reduction <- rep(N,0.4)
power.curve.40 <- data.frame(cbind(power, N*1/.85,reduction))
#Increase sample size due to 15% dropout rate
power.curve.40$N <- power.curve.40$N * (1/.85)

## Create power curve for each of the different rates
ggplot(power.curve.30, aes(x=N, y=power)) +
  geom_line() +
  geom_point() +
  expand_limits(y=0) +
  xlab("Sample Size") + ylab("Power") + 
  geom_smooth(method = "loess") + 
  theme_bw() +
  scale_y_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
  geom_hline(yintercept = 0.8)

