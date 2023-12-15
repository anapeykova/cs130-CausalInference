set.seed(130)
rm(list = ls())
#set directory to source folder
library(readr)
data <- read_csv("data.csv")

library(CBPS)
library(systemfit)

############## REPLICATION

### SUR MODEL

m4.s1 <- npCBPS(rf_0622~
                  Turnout15  +
                  UKIP14_pct+
                  femPerc+
                  percDE +  
                  logPop +
                  percDegree + 
                  medianAge +postal_pct+
                  England, 
                method='exact',
                data=data)


w4<- sqrt(m4.s1$weights)
X<- model.matrix(~rf_0622+
                   Turnout15  +
                   UKIP14_pct+
                   femPerc+
                   percDE +  
                   logPop +
                   percDegree + 
                   medianAge + 
                   England, data = data)

X<-diag(w4)%*%X

Leave4 <- diag(w4)%*%log(data$leave_pct/data$abstain_pct)
Remain4 <-diag(w4)%*%log(data$remain_pct/data$abstain_pct)


m4eq1<- Leave4 ~ X-1
m4eq2<- Remain4 ~ X-1

m4 <- systemfit(list(m4eq1,m4eq2),method = 'SUR')

### FIGURE 3
par(mfrow=c(1,1))

plot(x=NULL,y=NULL,main=NULL,
     ylab = expression(paste(Delta,'Election Result (%)')), 
     xlab = '',
     xlim= c(0.2,1.8), ylim= c(-0.05,0.05), axes=F, type = 'n')
axis(1, at= c(0.5,1,1.5), labels = c('Abstain (%)','Leave (%)','Remain (%)'))
axis(2)
par(lend=0)
abline(h=0,lty=2)
box()


# simulated coefficients
coefSimLeave <- mvrnorm(n=1000, m4[1]$eq[[1]]$coefficients, m4[1]$eq[[1]]$coefCov)
coefSimRemain  <- mvrnorm(n=1000, m4[1]$eq[[2]]$coefficients, m4[1]$eq[[2]]$coefCov)


rainHigh      <-  25
rainLow       <-  0


## Vector of Xs
highRainVec <- c(1,rainHigh, mean(data$Turnout15), mean(data$UKIP14_pct),
                 mean(data$femPerc), mean(data$percDE), mean(data$logPop), 
                 mean(data$percDegree), 
                 mean(data$medianAge), 
                 mean(data$England))


lowRainVec <- c(1,rainLow,  mean(data$Turnout15), mean(data$UKIP14_pct),
                mean(data$femPerc), mean(data$percDE), mean(data$logPop), 
                mean(data$percDegree), 
                mean(data$medianAge), 
                mean(data$England))


## Predicted ln(RemainShare/AbstainShare) (for both high and low rain)
pred.remain.low  <- coefSimRemain %*% lowRainVec
pred.remain.high <- coefSimRemain %*% highRainVec

## Predicted ln(LeaveShare/AbstainShare) (for both high and low rain)
pred.leave.low  <- coefSimLeave %*% lowRainVec
pred.leave.high <- coefSimLeave %*% highRainVec

## Predicted Share Values for Remain : Rhat
lowRemainShare  <- exp(pred.remain.low)  / (1 + exp(pred.remain.low)  + exp(pred.leave.low)  )
highRemainShare <- exp(pred.remain.high) / (1 + exp(pred.remain.high) + exp(pred.leave.high) )

## Predicted Share Values for Leave : Lhat
lowLeaveShare  <- exp(pred.leave.low)  / (1 + exp(pred.leave.low)   +  exp(pred.remain.low)   )
highLeaveShare <- exp(pred.leave.high) / (1 + exp(pred.leave.high)  +  exp(pred.remain.high)  )

## Predicted Share Values for Abstain : 100 - Rhat - Lhat
lowAbstainShare  <- 1  / (1 + exp(pred.leave.low)   +  exp(pred.remain.low)   )
highAbstainShare <- 1 / (1 + exp(pred.leave.high)  +  exp(pred.remain.high)  )

## First Differences
fd_abstain <- (highAbstainShare - lowAbstainShare)*100
fd_remain  <- (highRemainShare  - lowRemainShare)*100
fd_leave   <- (highLeaveShare   - lowLeaveShare)*100



#Remove plotmat later
plotmat<- matrix(nrow=1, ncol=9)

modelnumber<-1

plotmat[modelnumber,1]<-mean(fd_abstain)
plotmat[modelnumber,2]<-quantile(fd_abstain, 0.975)
plotmat[modelnumber,3]<-quantile(fd_abstain, 0.025)

plotmat[modelnumber,4]<-mean(fd_remain)
plotmat[modelnumber,5]<-quantile(fd_remain, 0.975)
plotmat[modelnumber,6]<-quantile(fd_remain, 0.025)

plotmat[modelnumber,7]<-mean(fd_leave)
plotmat[modelnumber,8]<-quantile(fd_leave, 0.975)
plotmat[modelnumber,9]<-quantile(fd_leave, 0.025)



#plot

par(mfrow=c(1,1), mai=c(.5,0.7,.3,.3))

plot(x=NULL,y=NULL,main=NULL,
     ylab = '', 
     xlab = '',
     xlim= c(0.2,1.6), ylim= c(-5,5),
     axes=F, type = 'n')
axis(1, at= c(0.5,1,1.5), 
     labels = c('Abstain','Remain',
                'Leave'),
     padj=-0.5)
axis(2, padj=0.5)
par(lend=0)
title(ylab=expression(paste(Delta,'Election Result (%)')), line=1.7)
abline(h=0,lty=2)
box()

#abstain plot
points(x = modelnumber-0.5, y=plotmat[modelnumber,1], pch=19,col='gray50')
segments(modelnumber-0.5,plotmat[modelnumber,2],modelnumber-.5,plotmat[modelnumber,3], lwd=2, col= 'gray50')
text(0.33,plotmat[modelnumber,1],paste(round(mean(fd_abstain), 2)))

#remain plot
points(x = modelnumber, y=plotmat[modelnumber,4], pch=19,col= 'gray50')
segments(modelnumber,plotmat[modelnumber,5],modelnumber,plotmat[modelnumber,6],lwd=2, col= 'gray50')
text(0.8,plotmat[modelnumber,4],paste(round(mean(fd_remain), 2)))

#leave plot
points(x = modelnumber+0.5, y=plotmat[modelnumber,7], pch=19,col= 'gray50')
segments(modelnumber+0.5,plotmat[modelnumber,8],modelnumber+0.5,plotmat[modelnumber,9],lwd=2, col= 'gray50')
text(1.3,plotmat[modelnumber,7],paste(round(mean(fd_leave), 2)))




############# EXTENSION

data$treatment <- ifelse(data$rf_0622 < 20/3, 0, 1)
nrow(data[data$treatment==0,])
nrow(data[data$treatment==1,])

covariates <- cbind(Turnout15, UKIP14_pct, femPerc, percDE, logPop, percDegree, medianAge)
treatment <- data$treatment
outcome <- data$Leave_share

genout <- GenMatch(
  Tr = treatment,
  X = covariates,
  M = 1,
  pop.size = 200,
  max.generations = 100
)

mout <- Match(Tr=treatment, X = covariates, Weight.matrix = genout, Y=outcome)
mb <- MatchBalance(treatment ~ covariates,match.out=mout, nboots=500)

summary(mout)

treated_units <- data[mout$index.treated,]
control_units <- data[mout$index.control,]
matched_data <- rbind(treated_units,control_units)


treatment_effect <- treated_units$Leave_Share - control_units$Leave_Share
quantiles <- quantile(treatment_effect, probs = c(0.025, 0.975))
print(quantiles)

hist(treatment_effect, main="Treatment Effect: Heavy Rainfall on Leave Share")
abline(v=quantiles[1], col='red', ylim=c(0, max(hist(treatment_effect)$counts)))
abline(v=quantiles[2], col='red', ylim=c(0, max(hist(treatment_effect)$counts)))
legend("topright", legend="95% CI", col="red", lty=1)


