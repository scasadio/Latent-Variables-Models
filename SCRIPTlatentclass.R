# poLCA to fit the models
formula<-cbind(A,B,C,D,E,F,G)~1 #A lable of var 1
m.2<-poLCA(formula,carcinoma,nclass=2,nrep=10,verbose=FALSE)

#Recode the data for poLCA.
names(Mobility)<-c("Item1","Item2","Item3","Item4", "Item5","Item6","Item7","Item8")
Mobility[Mobility==1]<-2
Mobility[Mobility==0]<-1

#Compare the estimates of the expected frequencies
freq.estim<-data.frame(m.2$predcell[1:9],m.3$predcell[9],m.4$predcell[9])

#Compare
K<-c('2', '3', '4')
llik<-c(m.2$llik,m.3$llik,m.4$llik)
npar<-c(m.2$npar,m.3$npar,m.4$npar)
Gsq<-round(c(m.2$Gsq,m.3$Gsq,m.4$Gsq),3) #likelihood ratio statistic, it decreases
Chisq<-round(c(m.2$Chisq,m.3$Chisq,m.4$Chisq),3)
df<-c(m.2$resid.df,m.3$resid.df,m.4$resid.df)
p.value<-round(1-pchisq(Chisq,df),4) #spare= explosive tests(high)
p.value2<-round(1-pchisq(Gsq,df),4)
AIC<-round(c(m.2$aic,m.3$aic,m.4$aic),3) #lowest
BIC<-round(c(m.2$bic,m.3$bic,m.4$bic),3) .
summary<-data.frame(K,llik,npar,Gsq,Chisq,df,p.value,p.value2,AIC,BIC)

#compare this solution with those obtained with your colleagues
round(m.3$P,4) #prior probabilities #different order cause seed set
lapply(m.3$probs,round,2) 

#solve the indeterminacy of the ranking of the latent classes
probs.start.m3<-m.3$probs.start  #starting values for solutions, order of prior.
new.probs.start.m3<-poLCA.reorder(probs.start.m3,order(m.3$P)) 
m.3.ord<-poLCA(formula,carcinoma,nclass=3,probs.start=new.probs.start.m3,verbose=FALSE)
round(m.3.ord$P,4) #%of diagnosis for each class
lapply(m.3.ord$probs,round,2)

#estimate of the probability of the agreement of the pathologists in 1(negative)
p.dnc1<-m.3.ord$P[1]*m.3.ord$probs$A[1,1]*m.3.ord$probs$B[1,1]*m.3.ord$probs$C[1,1]*m.3.ord$probs$D[1,1]*m.3.ord$probs$E[1,1]*m.3.ord$probs$F[1,1]*m.3.ord$probs$G[1,1]
p.dnc2<-m.3.ord$P[2]*m.3.ord$probs$A[2,1]*m.3.ord$probs$B[2,1]*m.3.ord$probs$C[2,1]*m.3.ord$probs$D[2,1]*m.3.ord$probs$E[2,1]*m.3.ord$probs$F[2,1]*m.3.ord$probs$G[2,1]
p.dnc3<-m.3.ord$P[3]*m.3.ord$probs$A[3,1]*m.3.ord$probs$B[3,1]*m.3.ord$probs$C[3,1]*m.3.ord$probs$D[3,1]*m.3.ord$probs$E[3,1]*m.3.ord$probs$F[3,1]*m.3.ord$probs$G[3,1]
p.dn<-p.dnc1+p.dnc2+p.dnc3  #prob of negative agreement diagnosis for over all classes
round(p.dn,4)

#estimate of the number of units pathologists agree in negative
round(p.dn*m.3$N,4)

#posterior probability estimates of the response pattern (1,..,1) for the three latent classes
a<-as.numeric(row.names(carcinoma[which(carcinoma$A==1&carcinoma$B==1&
  carcinoma$C==1&carcinoma$D==1&carcinoma$E==1&carcinoma$F==1&carcinoma$G==1),]))
post.11<-m.3.ord$posterior[a[1],]
round(post.11,5)

#latent class in which units are allocated according to highest post prob
m.3.ord$predclass
table(m.3.ord$predclass)

#Select the samples units allocated to the class 1 and correspondent pattern.
sel<-carcinoma[m.3.ord$predclass==1,]

#predicted cell probability the response pattern not observed 
#allocate it according to the highest pos prob
poLCA.predcell(m.4.ord,c(2,...,1))
round(poLCA.posterior(m.4.ord,y=c(2,...,1)),3)