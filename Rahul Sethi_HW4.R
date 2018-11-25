library(readstata13)

ivreg2 <- read.dta13("ivreg2.dta")

0.9/(sqrt(2)*1)

ivreg2$e <- ivreg2$y-(3+ivreg2$x)
cov(ivreg2$x,ivreg2$e)/(sd(ivreg2$x)*sd(ivreg2$e))

plot(ivreg2$x,ivreg2$y, col="Blue",xlab = "X", ylab = "Y")
abline(3,1,col="red")

outp <- matrix(rep(NA,12),nrow=4,ncol=3)
samsize <- c(10,20,100,500)
for(i in 1:4){
    model <- lm(y~x, data=ivreg2[1:samsize[i],])
    outp[i,1] <- samsize[i]
    outp[i,2] <- model$coefficients[1]
    outp[i,3] <- model$coefficients[2]
    abline(outp[i,2],outp[i,3],col=c("orange","maroon","cyan","green")[i])
}

