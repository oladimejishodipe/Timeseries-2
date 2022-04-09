library(readxl)
Finalelectricdatares <- read_excel("~/Dissertation/Residential/Finalelectricdata.xlsx",
                                   sheet = "Residential")

which(is.na(Finalelectricdatares))


REP<- ts(Finalelectricdatares[,5], start=c(2001,1),
           frequency=12)

WTI<- ts(Finalelectricdatares[,6], start=c(2001,1),
             frequency=12)

HHG<- ts(Finalelectricdatares[,7], start=c(2001,1),
           frequency=12)


REP1<- window(REP, end=c(2019,12))
WTI1<- window(WTI, end=c(2019,12))
HHG1<- window(HHG, end=c(2019,12))


#Log of the series
lREP<-log(REP1)
lWTI<-log(WTI1)
lHHG<-log(HHG1)

adflREP<-ur.df(y=lREP, type="none", selectlags="AIC")
adflREP
adflREP<-ur.df(y=lREP, type="drift", selectlags="AIC")
adflREP
adflREP<-ur.df(y=lREP, type="trend", selectlags="AIC")
adflREP

adflreWIP<-ur.df(y=lWTI, type="none", selectlags="AIC")
adflreWIP
adflreWIP<-ur.df(y=lWTI, type="drift", selectlags="AIC")
adflreWIP
adflreWIP

adflHHG<-ur.df(y=lHHG, type="none", selectlags="AIC")
adflHHG
adflHHG<-ur.df(y=lHHG, type="drift", selectlags="AIC")
adflHHG
adflHHG<-ur.df(y=lHHG, type="trend", selectlags="AIC")
adflHHG


#Differencing

dlWTI<-diff(lWTI)
dadflreWIP<-ur.df(y=dlWTI, type="none", selectlags="AIC")
dadflreWIP
dadflreWIP<-ur.df(y=dlWTI, type="drift", selectlags="AIC")
dadflreWIP
dadflreWIP<-ur.df(y=dlWTI, type="trend", selectlags="AIC")
dadflreWIP

dlHHG<-diff(lHHG)
dadflHHG<-ur.df(y=dlHHG, type="none", selectlags="AIC")
dadflHHG
dadflHHG<-ur.df(y=dlHHG, type="drift", selectlags="AIC")
dadflHHG
dadflHHG<-ur.df(y=dlHHG, type="trend", selectlags="AIC")
dadflHHG

#Combining the stationary series
Enerdata<-ts.combine(dlWTI, dlHHG, lREP)

# Reduced Form VAR
Enermodel<- VAR(Enerdata, p=1)
Enermodel

# Structural VAR Estimation
Enermodel1<-tsreg(dlWTI, ts.combine(lags(dlWTI,1), lags(lHHG,1), lags(lREP,1)))
Enermodel1

Enermodel2<-tsreg(dlHHG, ts.combine(lags(dlWTI,0:1), lags(dlHHG,1),lags(lREP,1)))
Enermodel2

Enermodel3<-tsreg(lREP, ts.combine(lags(dlWTI,0:1), lags(dlHHG,0:1), lags(lREP,1)))
Enermodel3


Enermodel1
# Structural VAR Residual Matrix
Resid<-matrix(c(Enermodel1$residuals, Enermodel2$residuals, Enermodel3$residuals),ncol=3)

# Structural VAR Covariance Matrix
CV<-cov(Resid)
CV

# Contemporaneous Effect Matrix
ConD<-matrix(c(0,Enermodel2$coefficients[2],Enermodel3$coefficients[2],0,0,Enermodel3$coefficients[4],0,0,0), ncol=3)
ConD

# The Inverse of Reduced Form Contemporaneos matrix
ImP<- diag(3) - ConD
ImP
InverseImp<-solve(ImP)
InverseImp

# Impact Effect Matrix from Structural Shock
ImpactShock<-t(chol(InverseImp%*%CV%*%t(InverseImp)))
ImpactShock

adflreWIP<-ur.df(y=lWTI, type="trend", selectlags="AIC")









#CoeMatrix<-matrix(c(Enermodel1$coefficients[2],Enermodel2$coefficients[3],Enermodel3$coefficients[3],
 #                   Enermodel1$coefficients[3],Enermodel2$coefficients[4],Enermodel3$coefficients[5],
  #                  Enermodel1$coefficients[4],Enermodel2$coefficients[5],Enermodel3$coefficients[6]), ncol=3)


Resid<-(residuals(Enermodel))
cova<-cov(Resid)
Shock<-t(chol(cova))
Shock












































