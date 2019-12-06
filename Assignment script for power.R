##First of all,perfoming balanced one way analysis of variance
##So, equal number of observation within each group
#we have standard alpha level 0.05,i= number of groups, n=participants in the group 
#First we are testing H0
a<- .05
i<-4
n<-10 

#let's plot to see the central F distribution
whr<-8
x<- seq(0,whr,.01)
plot(x,df(x,i-1,i*(n-1)),type = 'l',lwd=3,xlab = 'f values',ylab = 'probability',main = 'one way analysis of variance')

#for ploting the rejection region
polygon(c(qf(1-a,i-1,i*(n-1)),seq(qf(1-a,i-1,i*(n-1)),whr,.01),whr),
        c(0,df(seq(qf(1-a,i-1,i*(n-1)),whr,.01),i-1,1*(n-1)),0),density=50,col="red")
legend("top",paste("Alpha=",a))


#let's assume that the H1 is true
sig2=1
grpmeans <- c(-0.5,0,0,0.5)
lamda <- (i-1)*n*(var(grpmeans)/sig2)
lamda
#let's see the distribution area for non central F distribution
#so the region for power starts right above the rejection region assuming that
#the non central distribution is true and therefore H1 is true.
windows()
polygon(c(qf(1-a,i-1,i*(n-1)),seq(qf(1-a,i-1,i*(n-1)),whr,.01),whr),
        c(0,df(seq(qf(1-a,i-1,i*(n-1)),whr,.01),i-1,1*(n-1)),0),density=50,col="red")
legend("top",paste("Alpha=",a))
plot(x,df(x,i-1,i*(n-1)),type = 'l',lwd=3,xlab = 'f values',ylab = 'probability',main = 'one way analysis of variance')
lines(x,df(x,i-1,i*(n-1),lamda), col=("blue"),lwd=3)

?power.anova.test
#calculating power in r
?pf
1-pf(qf(1-a,i-1,i*(n-1)),i-1,i*(n-1),lamda)
power.anova.test(groups = 4,n=10,between.var = var(grpmeans),within.var = sig2,power = NULL)
power.anova.test(groups = 4,n=40,between.var = var(grpmeans),within.var = sig2,power = NULL)
power.anova.test(groups = 4,n=50,between.var = var(grpmeans),within.var = sig2,power = NULL)
?qf
qf(1-0.05, 3, 36) #f crit

### calculating sample size for power 50%to90%
power.anova.test(groups = 4,n=NULL,between.var = var(grpmeans),within.var = sig2,power = .5)#group size is 12 for 50% power
power.anova.test(groups = 4,n=NULL,between.var = var(grpmeans),within.var = sig2,power = .6)#group size is 15 for 60%
power.anova.test(groups = 4,n=NULL,between.var = var(grpmeans),within.var = sig2,power = .7)#size 18 for 70%
power.anova.test(groups = 4,n=NULL,between.var = var(grpmeans),within.var = sig2,power = .8)#size 23 for 80%
power.anova.test(groups = 4,n=NULL,between.var = var(grpmeans),within.var = sig2,power = .9)#size 29 for 90%


#following an example from the last part of the script
library(MonteCarlo)
t.test<-function(n,loc,scale){sample<-rnorm(n,loc,scale)
 stat<- sqrt(n)*mean(sample)/sd(sample) 
  decision<-abs(stat)> 1.96
return(list("decision"=decision))}

possible.ns <- seq(from=10, to=50, by=1)
powers <- rep(NA, length(possible.ns))  
alpha <- 0.05   #significance level
sims <- 100 

for(j in 1:length(possible.ns)){
  N <- possible.ns[4]                             
  
  significant.experiments <- rep(NA, sims)}     

loc_grid<-seq(-0.5,0,0,0.5)
scale_grid<-c(1,2)

param_list=list("n"=possible.ns,
                "loc"=loc_grid,
                "scale"=scale_grid)


MS_simulation<-MonteCarlo(func = t.test, nrep = 100, param_list = param_list)
summary(MC_result)
MakeTable(output = MC_result,rows = "n", cols = c("loc","scale"),digits = 2, include_meta = FALSE)




####Just trying things in order to find something to adjust in our case####


?replicate
#monte carlo simulation
#generate some data
runif(100, min=-0.5, max=0.5)
?fnPower
?MonteCarlo
require(MonteCarlo)
MonteCarlo()
?MonteCarlo
ttest<-function(n,loc,scale){
  
  # generate sample:
  sample<-rnorm(n, loc, scale)
  
  # calculate test statistic:
  stat<-sqrt(n)*mean(sample)/sd(sample)
  
  # get test decision:
  decision1.96
  
  # return result:
  return(list("decision"=decision))
}

possible.ns <- seq(from=10, to=50, by=1)# Our group sizes
powers <- rep(NA, length(possible.ns))  
alpha <- 0.05   #significance level
sims <- 100 # Number of simulations to conduct for each n


for(j in 1:length(possible.ns)){
  N <- possible.ns[4]                              # Pick the jth value for N
  
  significant.experiments <- rep(NA, sims)}         # Empty object to count significant experiments
  
  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    Y0 <-  rnorm(n=N, mean=0.5, sd=1)              
    tau <- 5                                      
    Y1 <- Y0 + tau                                 
    Z.sim <- rbinom(n=N, size=1, prob=.5)          
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               
    fit.sim <- lm(Y.sim ~ Z.sim)                   
    p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
    significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
  }
  
  powers[4] <- mean(significant.experiments)  
  plot(possible.ns, powers, ylim=c(0,1))
  
  ###Something else t test for comparing power between two groups
  n <- seq(10, 50, 1)
  power <- sapply(seq_along(n), function(i)
    power.t.test(n = n[i], delta = .15, sd = 1, type = 'two.sample')$power)  
  
  power_df <- data.frame(
    n = 4,
    power = power
  )
  windows()
  ggplot(power_df, aes(x = n, y = power)) +
    geom_line(size = 2) +
    geom_hline(yintercept = 0.8, linetype = 2, color = 'gray30') +
    geom_vline(xintercept = 700, linetype = 2, color = 'gray30') +
    scale_x_continuous("Sample Size", breaks = seq(0, 1000, 200)) +
    scale_y_continuous("Power", breaks = seq(0, 1, .2)) +
    theme_bw(base_size = 14)
  ?simglm
  require(simglm)
  simglm() #doesn't work 
  
  
  #not completely relevant but maybe can give me an idea    
  sims_to_run<-100
  save_p <- vector(length=sims_to_run)
  
  for(i in 1:sims_to_run){
    sim_data <- rnorm(25,100,10)
    t.out <-t.test(sim_data,mu=0.5)
    save_p[i] <- t.out$p.value
  }
  hist(save_p)  #it seems that all p-values aren't equally likely
  
  proportion_sig<-length(save_p[save_p<.05])/sims_to_run
  proportion_sig  #The calculated proportion of significant values
  
  
  #####another thing to get some idea from a simulation
  subject_numbers<-c(10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,
                     30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50)
  
  power<-vector(length=length(subject_numbers))
  for(subnum in 1:length(subject_numbers)){
    
    sims_to_run<-100
    save_p <- vector(length=sims_to_run)
    
    for(i in 1:sims_to_run){
      condition_1 <- rnorm(subject_numbers[subnum])
      condition_2 <- rnorm(subject_numbers[subnum])
      t.out <-t.test(condition_1,condition_2,paired=TRUE,var.equal = TRUE)
      save_p[i] <- t.out$p.value
    }
    power[subnum]<-length(save_p[save_p<.05])/sims_to_run
  }
  
  power_table<-data.frame(subject_numbers,power)
  power_table
  
