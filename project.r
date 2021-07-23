# data info: https://www.kaggle.com/argonalyst/sao-paulo-real-estate-sale-rent-april-2019/data#

setwd("C:/Users/julio/Google Drive/coursera/Univ_CA/BayesianStatistics_Models_Tech/Rcode/project")

library(ggplot2)
library(dplyr)
library(rmutil)

df = read.csv("sao-paulo-properties-april-2019.csv",header = TRUE)
y = df$Price[df$Negotiation.Type=="sale"&df$Condo>700]

# get relevant data
#1. y = Price in R$ (Brazilian Real)
#2. Condo = condominium expenses, unknown values are marked as zero (there are missing values)
#3. Size (in m^2)
#4. Rooms - n bedrooms
#5. Toilets - n toilets
#6. Suites - N bedrooms with a private bedroom
#7. Parking - n of parking spots
#8. Furnished - 1 if the property is furnished 
#9. Swimming pool - 1 if there is a swimming pool
#10. New - 1 if new

X = cbind(df$Condo[df$Negotiation.Type=="sale"&df$Condo>700],
          df$Size[df$Negotiation.Type=="sale"&df$Condo>700],
          df$Rooms[df$Negotiation.Type=="sale"&df$Condo>700],
          df$Toilets[df$Negotiation.Type=="sale"&df$Condo>700],
          df$Suites[df$Negotiation.Type=="sale"&df$Condo>700],
          df$Parking[df$Negotiation.Type=="sale"&df$Condo>700],
          df$Furnished[df$Negotiation.Type=="sale"&df$Condo>700],
          df$Swimming.Pool[df$Negotiation.Type=="sale"&df$Condo>700],
          df$New[df$Negotiation.Type=="sale"&df$Condo>700],
          as.numeric(factor(df$District[df$Negotiation.Type=="sale"&df$Condo>700])))
colnames(X) = c("Condo","Size","Rooms","Toilets","Suites","Parking","Furnished",
                "Swimming.Pool","New","District")

# Data exploration 

# plot data
plot(density(y))
plot(density(log(y)))
logy = log(y) 

plot(density(X[,"Suites"]))

cor(logy,X[,"Rooms"])
boxplot(logy~X[,"District"])

# Prior predictive checks
#Before implementing the model, we need to select prior distributions
#for mu and tau, the hyperparameters governing the normal distribution for the b_new parameters

# Modeling
mod_string = " model {
  for (i in 1:length(y)) {
    y[i] ~ dlnorm(muy[i], precy[District[i]])
    muy[i] = b[1] + b[2]*Condo[i] + b[3]*Size[i] + b[4]*Rooms[i] + b[5]*Toilets[i] + b[6]*Suites[i] + b[7]*Parking[i] + b[8]*Furnished[i] + b[9]*SwimmingPool[i] + bnew[District[i]]*New[i]
  }
  
  for (j in 1:9) {
    b[j] ~ dnorm(0.0, 1.0/1.0e6)
  }
  
  for (j in 1:max(District)) {
    bnew[j] ~ ddexp(mu, precb)
    precy[j] ~ dgamma(0.001,0.001)
    sigy[j] ~ sqrt(1/precy[j])
  }
  
  mu ~ dnorm(0.0, 1.0/1.0e6)
  precb ~ dgamma(0.001, 0.001)
  b = sqrt( 1.0 / precb )
  
} "

mod_string = " model {
  for (i in 1:length(y)) {
    y[i] ~ dlnorm(muy[i], precy)
    muy[i] = b[1] + b[2]*Condo[i] + b[3]*Size[i] + b[4]*Rooms[i] +
    b[5]*Toilets[i] + b[6]*Suites[i] + b[7]*Parking[i] + 
    b[8]*Furnished[i] + b[9]*SwimmingPool[i] + bnew[District[i]]*New[i]
  }
  
  for (j in 1:9) {
    b[j] ~ dnorm(0.0, 1.0/1.0e6)
  }
  
  for (j in 1:max(District)) {
    bnew[j] ~ ddexp(mu, precb)
  }
  
  mu ~ dnorm(0.0, 1.0/1.0e6)
  precb ~ dgamma(0.001, 0.001)
  b_bnew = sqrt( 1.0 / precb )
  precy ~ dgamma(0.001,0.001)
  sigy  = sqrt(1/precy)

} "

library(rjags)

set.seed(116)
#data_jags = list(y=y, Condo = X[,"Condo"], Size = X[,"Size"], Rooms = X[,"Rooms"],
#                 Toilets = X[,"Toilets"], Suites = X[,"Suites"], Parking = X[,"Parking"],
#                 Furnished = X[,"Furnished"], SwimmingPool = X[,"Swimming.Pool"],
#                 District = X[,"District"], New = X[,"New"])


data_jags = list(y=y, Condo = X[,"Condo"], Size = X[,"Size"], Rooms = X[,"Rooms"],
                 Toilets = X[,"Toilets"], Suites = X[,"Suites"], Parking = X[,"Parking"],
                 Furnished = X[,"Furnished"], SwimmingPool = X[,"Swimming.Pool"],
                 District = X[,"District"], New = X[,"New"])

params = c("sigy", "b", "bnew","mu", "b_bnew")
mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 50e3) # burn-in
mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=10e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim)) # combine multiple chains
## convergence diagnostics
plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
sample_sizes = effectiveSize(mod_sim)
mean(sample_sizes)

# check our model using residuals
pm_params = colMeans(mod_csim)
pm_params
# residual analysis
y_hat = numeric(length = length(y))
for (i in 1:length(y)){
  y_hat[i] = pm_params[1] + pm_params[2:9]%*%X[i,1:8] + pm_params[10+X[i,10]]*X[i,9]
}
#y_hat = exp(y_hat)
resid = logy - y_hat
plot(resid)
plot(y_hat,logy)
lines(logy,logy,col="red")
R2 = cor(y_hat,logy)^2
mean(resid)
sd(resid)

#get the  two districts with the highest NEW slopes
pm_params[11:82]
order(pm_params[11:82], decreasing = TRUE)
# districts slope_new(45) > slope_new(11) > slope_new(all)

#Posterior predictive simulation
(n_sim = nrow(mod_csim))
X_predA = c(750,160,2,2,1,2,0,0,1)
X_predB = c(750,160,2,2,1,2,0,0,1)
X_predC = c(750,160,2,2,1,2,0,0,1)
y_meanA = mod_csim[,1] + mod_csim[,2:9]%*%X_predA[1:8] + mod_csim[,"bnew[58]"]*X_predA[9]
y_predA = rlnorm(n=n_sim, meanlog=y_meanA, sdlog = mod_csim[,"sigy"])
y_meanB = mod_csim[,1] + mod_csim[,2:9]%*%X_predB[1:8] + mod_csim[,"bnew[45]"]*X_predB[9]
y_predB = rlnorm(n=n_sim, meanlog=y_meanB, sdlog = mod_csim[,"sigy"])
y_meanC = mod_csim[,1] + mod_csim[,2:9]%*%X_predC[1:8] + mod_csim[,"bnew[11]"]*X_predC[9]
y_predC = rlnorm(n=n_sim, meanlog=y_meanC, sdlog = mod_csim[,"sigy"])
plot(density(y_predA), xlim=c(0,5000000))
lines(density(y_predB), col = "red", xlim=c(0,5000000))
lines(density(y_predC), col = "blue", xlim=c(0,5000000))
legend('topright',c('District 58','District 45', 'District 11'), fill = c("black","red","blue"), bty = 'n',
       border = NA)

mean(y_predA > 1500000)
mean(y_predB > 1500000)
mean(y_predC > 1500000)
