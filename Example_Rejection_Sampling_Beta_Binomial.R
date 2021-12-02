## Assume you observe x_obs=8
n<-10
x_obs<-8

## We use rejection sampling to simulate the posterior distribution
## Step (1): Simulation of parameter "theta" from the prior distribution
## and X from the likelihood
theta<-runif(1)
x<-rbinom(1,size = n,prob=theta)

## Step (2): We accept theta if X=x_obs, otherwise repeat Step (1)
while(x!=x_obs){
  theta<-runif(1)
  x<-rbinom(1,size = n,prob=theta)
}

## theta is a sample from the posterior p(theta|sum(X)=x_obs)
theta

## Reiterate s times
s<-15000
RS_output<-rep(NA,s)
for (i in 1:s){
  theta<-runif(1)
  x<-rbinom(1,size = n,prob=theta)
  while(x!=x_obs){
    theta<-runif(1)
    x<-rbinom(1,size = n,prob=theta)
  }
  RS_output[i]<-theta
}

## Theta was simulated from a uniform distribution
## but the accept/reject mechanism transformed it in Beta(1,2)
## Comparison with theta without accept/reject:
plot(runif(s),main="Samples before accept/reject",xlab="Sample number",ylab="Sample from prior distr.")
plot(RS_output,main="Samples after accept/reject",xlab="Sample number",ylab="Sample from posterior distr.")

## Check that the sample with accept/reject come from the posterior
hist(RS_output,freq = F,main="Histogram of posterior samples")
lines(c(0,1),c(1,1),col="red")
lines(seq(0,1,by=0.01),dbeta(seq(0,1,by=0.01),
                             shape1 = 1+x_obs,
                             shape2 = 1+n-x_obs)
      ,col="blue")
legend(x = "topleft", legend = c("prior","posterior"),
        lty=c(1,1),col=c("red","blue"),cex=2)

