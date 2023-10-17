#load and display data
data()
dim()
head()

cor(data)

#Estimate a factor model (ML)
f1<-factanal(~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=data,n.factors=1)

formula<-"V1+V2+V3+V4+V5+V6+V7+V8+V9+V10"#on cov/cor matrix
f1<-factanal(formula, covmat = cormat, n.obs = n, factors = 1, rotation = "none")


#Chi-square test.
chisq<-round(c(f1$STATISTIC, f2$STATISTIC, f3$STATISTIC, f4$STATISTIC),3)
df<-round(c(f1$dof,f2$dof,f3$dof, f4$dof),3)
pvalues<-round(c(f1$PVAL,f2$PVAL,f3$PVAL,f4$PVAL),3)
  #uniq>0.5 

#interprete factors
f3$loadings
print(f3, cutoff=0) #all loadings to each observed value
print(f3,cutoff=0.3)

#communalities 
comm<-1-f3$uniq

#%var of the model explained by the three factor model
percvar<-sum(comm)/trace(cormat)

#reproduced correlation matrix 
repcorr<-loadings(f3)%*%t(loadings(f3))
#discrepancy
round(matcor-repcorr,3)

#rotations
library(GPArotation)
varimax(loadings(f3))
#print(varimax(loadings(f3)),cutoff=0.3)
quartimax(loadings(f3))
oblimin(loadings(f3))

# Compute the factor scores
fb<-factanal(~x1+x2+x3+x4+x5+x6+x7+x8+x9, data=data, 3, scores = "Bartlett")$scores
ft<-factanal(~x1+x2+x3+x4+x5+x6+x7+x8+x9, data=data, 3, scores = "regression")$scores
max(fb[,1]) #and min
which.max(fb[,1]) #min
min(abs(fb[,1]))
which.min(abs(fb[,1])) ##first factor

#confirmatory factor analysis
HS.model <- 'visual =∼ x1 + x2 + x3
  textual =∼ x4 + x5 + x6
  speed =∼ x7 + x8 + x9' #look at the rotation interp.
fit<-cfa(HS.model, data = data)
summary(fit,fit.measures=TRUE) #Comparative Fit Index (0,1)
fit$aic
fit$bic #the lower the better

##cov
library(lavaan)
formula2<-"F1=~V8+V9
  F2=~V1+V2+V5+V10
  F3=~V3+V4+V6+V7"
fit<-cfa(formula2, sample.cov = cormat, sample.nobs = n, std.lv=TRUE)
summary(fit, fit.measures=T)

