library(ltm)
data()
dim()
head()

#- which one among the 4 items shows the lowest degree of agreement
dsc<-descript(data)
dsc$perc #proportion of 1 1 1 1

#- distribution of the total score/observed frequency of the pattern .. 
dsc$item

#- if there are problems in some pairs of items ;
dsc$pw.ass #pval <0.5 means items are signif. associated= no prob

#-proportion of correct responses given the same value of total score
plot(dsc)

# fit the 2 IRT model to the data.
m1<-ltm(Abortion~z1) #2pl beta
m1.rip<-ltm(Abortion~z1, IRT.param = F)#latent trait model alpha

#rasch model
m1<-rasch(data,IRT.param =  , constraint = cbind(ncol(data)+1,1)) #all discrim =1
m1<-rasch(data,IRT.param = ) #unconstrained

#evaluate difference between two model
anova(m1,m2)  #aic bic pval

#which item shows the highest positive attitude
m1.rip$coefficients[,1]

#Which item more discriminant
summary(m1.rip)
alpha<-m1.rip$coeff[,2]

#interpretation of the latent var
summary(m1.rip) #z

#relation between the parameter estimates
summary(m1)$coef[1,1]
-summary(m1.rip)$coef[1,1]/summary(m1)$coef[5,1]

#GoF.rasch to estimate the bootstrap pvalue of chisquare.
pvalboot<-GoF.rasch(m1,B=199)
  pvalboot$Tobs

#standardized discriminant parameters
stalpha<-alpha/sqrt(1+alpha^2)

#probability of positive response of the median individual
coef(m1.rip, prob=TRUE, order=TRUE)

#characteristic curves for the estimated model
plot(m1,legend=TRUE, cx= "bottomright", 
     xlab="y", lwd=3, cex.main=1.5, 
     cex.lab=1.3, cex=1.1)

# good fit
E<-fitted(m1)[,5]
O<-m1$patterns$obs
cbind(m1$patterns$X, O, E)

chisq<-sum((O-E)^2/E)
df<-2^p-p*(q+1)-1 #2^p only observed patterns
pvalue<-1-pchisq(chisq,df)

LR<-2*sum(O*log(O/E)) #likelihood ratio test
pvalueLR<-1-pchisq(LR, df)

#residuals caluculated from marginal freq, local fit
margins(m1)
margins(m1, type="three-way", nprint=3) #>4

#estimate the values of the latent variable for each individual (z1)//
#methods of scaling individuals.
fs<-factor.scores(m1, method = "EAP")         #   "
comp<-factor.scores(m1, method = "Component") #estimates values of the latentvar
resp.pattern<-fs$score.dat[,1:4]
total.score<-apply(resp.pattern,1,sum) #of positive resp of individuals for each resp.patt
cp<-comp$score.dat[,7]                          #rank rp based on the val of observed var
tab<-cbind(fs$score.dat,cp,total.score)
round(tab[order(total.score),],)
plot(fs$score.dat$z1)

# response pattern that do not occur
factor.scores(m1,resp.pattern=rbind(c(1,0,0,1), c(1,0,1,0)))

#uva
model.f1<-'f1=~ x1+x3+x4+x7'
t<-cfa(model.f1,data= data[,c(1,3,4,7)],
          ordered=c("x1","x3","x4","x7"), std.lv = TRUE)
summary(fit, fit.measures=TRUE)
#2 factor
model.f2<-'f1=~ x1+x3+x4+x7
    f2=~x2+x5+x6'
t2<-cfa(model.f2,data= data[,c(1,2,3,4,5,6,7)], 
          ordered=c("x1","x2","x3","x4","x5","x6","x7"), std.lv = TRUE)