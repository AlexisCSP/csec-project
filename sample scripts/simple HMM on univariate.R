library(mhsmm)

#dataset <- read.csv('file.csv',header=TRUE)
#train <- formatMhsmm(data.frame(TotalFlow$FlowLevel))
#day subset

traindayform <- formatMhsmm(data.frame(train$Global_active_power[1:50000]))
testdayform <- formatMhsmm(data.frame(test$Global_active_power[1:1000]))
#end of data
# 4 states HMM    
k=2
#init probabilities
init <- rep(1/k, k)

#transition matrix
P <- matrix(rep(1/k, k*k), nrow = k)

#emission matrix:  here I used a Gaussian distribution, replace muEst and sigmaEst by your initial estimates of mean and variance
b <- list(mu = c(1,4), sigma = c(2,1)) 

#starting model for EM
startmodel <- hmmspec(init = init, trans = P, parms.emis = b, dens.emis = dnorm.hsmm)
startmodel
#EM algorithm fits an HMM to the data
hmm <- hmmfit(traindayform$x, startmodel , mstep = mstep.norm,maxit = 200)

#print resulting HMM parameters
summary(hmm)
plot(hmm$loglik, type="b", ylab="log-likelihood", xlab="Iteration")

#testhmm1 <- formatMhsmm(test)
yhat1 <- predict (hmm,traindayform$x)
yhat2 <- predict (hmm,testdayform$x)
#modelbased <- predict(startmodel,train,method="smoothed" )
#plot(modelbased)

plot(yhat1)
#addstates(yhat1$s)
plot(yhat2)
#addstates(yhat2$s)
