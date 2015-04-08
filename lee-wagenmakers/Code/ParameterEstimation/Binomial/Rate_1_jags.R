# R2jags Example;
# When you work through the code for the first time, 
# execute each command one at a time to better understand
# what it does.

# clears workspace:  
rm(list=ls()) 

# sets working directories:

library(R2jags)

k <- 0 # number of heads
n <- 1 # number of samples

data <- list("k", "n") # to be passed on to JAGS

myinits <- list(
  list(theta = 0.1), #chain 1 starting value
  list(theta = 0.9)) #chain 2 starting value

# parameters to be monitored:	
parameters <- c("theta")

# The following command calls JAGS with specific options.
# For a detailed description see the R2jags documentation.
samples <- jags(data, inits=myinits, parameters,
	 			 model.file ="Rate_1.txt", n.chains=2, n.iter=200000, 
         n.burnin=1, n.thin=1, DIC=T)
# Now the values for the monitored parameters are in the "samples" object, 
# ready for inspection.

# The commands below are useful for a quick overview:
print(samples)  # a rough summary
plot(samples)   # a visual representation
traceplot(samples) # traceplot (press <enter> repeatedly to see the chains)
#more info on what is returned:
summary(samples)
summary(samples$BUGSoutput)

chain <- 1
samples$BUGSoutput$sims.array[1:15,chain,]# array: element, chain, column (theta/deviance) 

# Collect posterior samples across all chains:
posterior <- data.frame(samples$BUGSoutput$sims.list$theta)
names(posterior)<-c('theta')
# Now let's plot a histogram for theta. 
ggplot(data=posterior,aes(x=theta))+
  geom_histogram(aes(y=..count../sum(..count..)),binwidth=0.02)+
  theme_bw()+
  xlim(0,1)
