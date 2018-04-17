###################################################################
# Name : DATA_Maniacs
# Subject : KDDM
# Type : Project BASE
###################################################################
## Loading the Dataset
rm(list=ls())
getwd()
dataset<-read.csv("/Users/VT/Desktop/DRUG_DATABASE.csv")

## Seperating the Needed Columns 


mydataset_raw<- data.frame( alcfirst=dataset$ALCEVER,alcdays=dataset$ALCDAYS,alcflag=dataset$ALCFQFLG,alcrec=dataset$ALCREC,alctry=dataset$ALCTRY,alcyr=dataset$ALCYRTOT,addalc=dataset$ADDALC,
                            mjage=dataset$MJAGE,mjrec=dataset$MJREC,mjlive=dataset$MJLIVE,mjtot=dataset$MJTOT,mjknown=dataset$MJKNOWN,mrjyr=dataset$MRJYRTOT,mjday30a=dataset$MJDAY30A,mjfirst=dataset$MJMFU,addmj=dataset$ADDMJ,
                            her30use=dataset$HER30USE,herage=dataset$HERAGE,herfirst=dataset$HEREVER,herneedl=dataset$HERDAYPMO,herrec=dataset$HERREC,hertot=dataset$HERMFU,addher=dataset$ADDALC,
                            cocage=dataset$COCDAYPMO,coctot=dataset$COCTOT,cocus30a=dataset$COCUS30A,cocrec=dataset$COCREC,addcoc=dataset$ADDTOB,
                            sex=dataset$SEX, ragegrp= dataset$RAGEGRP, resprace=dataset$RESPRACE, respage=dataset$RESPAGE,income=dataset$INCOME, educ=dataset$EDUC, marital=dataset$MARITAL,employed=dataset$EMPLOYED)

## Creating Columns for OPIOIDS and NONOPIOIDS
mydataset<-mydataset_raw

mydataset["op"]<- NA               ## Field for Opioid Addicts
mydataset["nonop"]<- NA            ## Field for NoN Opioids
mydataset["noadd"]<- NA            ## Field for No Subsatance Addicts
mydataset["nonopbutadd"]<- NA      ## Field for NoN Opioids but Addicts

## Generating Values for Fields
mydataset$op<- as.numeric(mydataset$addher|mydataset$addcoc)
mydataset$nonop<- as.numeric(!(mydataset$addher|mydataset$addcoc))
mydataset$noadd<- as.numeric(!(mydataset$addher|mydataset$addcoc|mydataset$addmj|mydataset$addalc))
mydataset$nonopbutadd<- as.numeric((!mydataset$op)&(!mydataset$noadd))



## Putting every 100th record in test data, and every 70th record in training data
## which leaves us with 73 observations of test data
## and 104 observations of training data 
idx=seq(from=1,to=nrow(mydataset),by=7)
mytestdataset<-mydataset[idx,]
mytrainingdataset<-mydataset[-idx,]
# View(mytestdataset)
# View(mytrainingdataset)


## Creating Datasets for each substance(ALcohol/Marijuana/Heroine/Cocaine)
ALCdataset<- data.frame(alcfirst=mydataset$alcfirst,alcdays=mydataset$alcdays,alcflag=mydataset$alcflag,alcrec=mydataset$alcrec,alctry=mydataset$alctry,alcyr=mydataset$alcyr,addalc=mydataset$addalc)
MJdataset<-data.frame(mjage=mydataset$mjage,mjrec=mydataset$mjrec,mjtot=mydataset$mjtot,mjknown=mydataset$mjknown,mjday30a=mydataset$mjday30a,mjfirst=mydataset$mjfirst,addmj=mydataset$addmj)
HERdataset<-data.frame(her30use=mydataset$her30use,herage=mydataset$herage,herfirst=mydataset$herfirst,herneedl=mydataset$herneedl,herrec=mydataset$herrec,hertot=mydataset$hertot,addher=mydataset$addher)
COCdataset<-data.frame(cocage=mydataset$cocage,coctot=mydataset$coctot,cocus30a=mydataset$cocus30a,cocrec=mydataset$cocrec,addcoc=mydataset$addcoc)

## Applying KNN on ALCOHOL
mmnorm <-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx))
return(z) 
}

#create a new dataset with


ALCnorm<-cbind(  ALCage=mmnorm(ALCdataset[,1],min(ALCdataset[,1]),max(ALCdataset[,1]))
                 , ALCtot=mmnorm(ALCdataset[,2],min(ALCdataset[,2]),max(ALCdataset[,2] ))
                 ,ALCus30a=mmnorm(ALCdataset[,3],min(ALCdataset[,3]),max(ALCdataset[,3] ))
                 , ALCrec=mmnorm(ALCdataset[,4],min(ALCdataset[,4]),max(ALCdataset[,4] ))
                 , ALCrec=mmnorm(ALCdataset[,5],min(ALCdataset[,5]),max(ALCdataset[,5] ))
                 , ALCrec=mmnorm(ALCdataset[,6],min(ALCdataset[,6]),max(ALCdataset[,6] ))
                 , addALC=mmnorm(ALCdataset[,7],min(ALCdataset[,7]),max(ALCdataset[,7] ))
                 
)


idx=seq(from=1,to=7224,by=7)

ALCtest <- ALCnorm[idx,]
ALCtraining <- ALCnorm[-idx,]

?knn()
## you can choose different 'k' to find the best 'k'
Error_Rate <- rep(0, 10)
k <- 1:50
for(x in k){
  ALCpredict<-knn(ALCtraining[,-7],ALCtest[,-7],ALCtraining[,7],k=x)
  Error_Rate[x] <- mean(ALCpredict !=ALCtraining[,7] )
}

plot(k, Error_Rate, type = 'b')
## best k is 10
library(class)
ALCpredict<-knn(ALCtraining[,-7],ALCtest[,-7],ALCtraining[,7],k=10)
ALCresults<-cbind(ALCtest,as.character(ALCpredict) )
head(ALCresults)

table(ALCresults[,7],ALCresults[,8])
ALCwrong<-ALCresults[,7]!=ALCresults[,8]

ALCrate<-sum(ALCwrong)/length(ALCwrong)
ALCrate


##Applying KNN on Marijuana
mmnorm <-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx))
return(z) 
}

#create a new dataset with


MJnorm<-cbind(  MJage=mmnorm(MJdataset[,1],min(MJdataset[,1]),max(MJdataset[,1]))
                , MJrec=mmnorm(MJdataset[,2],min(MJdataset[,2]),max(MJdataset[,2] ))
                
                , MJtot=mmnorm(MJdataset[,3],min(MJdataset[,3]),max(MJdataset[,3] ))
                , MJknown=mmnorm(MJdataset[,4],min(MJdataset[,4]),max(MJdataset[,4] ))
                
                , MJday30a=mmnorm(MJdataset[,5],min(MJdataset[,5]),max(MJdataset[,5] ))
                , MJfirst=mmnorm(MJdataset[,6],min(MJdataset[,6]),max(MJdataset[,6] ))
                , addMJ=mmnorm(MJdataset[,7],min(MJdataset[,7]),max(MJdataset[,7] ))
)


idx=seq(from=1,to=7224,by=7)

MJtest <- MJnorm[idx,]
MJtraining <- MJnorm[-idx,]

?knn()

## you can choose different 'k' to find the best 'k'
Error_Rate <- rep(0, 10)
k <- 1:50
for(x in k){
  MJpredict<-knn(MJtraining[,-7],MJtest[,-7],MJtraining[,7],k=x)
  Error_Rate[x] <- mean(MJpredict !=MJtraining[,7] )
}

plot(k, Error_Rate, type = 'b')

## best k is 48
library(class)
library(class)
MJpredict<-knn(MJtraining[,-7],MJtest[,-7],MJtraining[,7],k=48)
MJresults<-cbind(MJtest,as.character(MJpredict) )
head(MJresults)

table(MJresults[,7],MJresults[,8])
MJwrong<-MJresults[,7]!=MJresults[,8]

MJrate<-sum(MJwrong)/length(MJwrong)
MJrate


##Applying KNN on Heroine
mmnorm <-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx))
return(z) 
}

#create a new dataset with


HERnorm<-cbind(  HER30use=mmnorm(HERdataset[,1],min(HERdataset[,1]),max(HERdataset[,1]))
                 , HERage=mmnorm(HERdataset[,2],min(HERdataset[,2]),max(HERdataset[,2] ))
                 ,HERfirst=mmnorm(HERdataset[,3],min(HERdataset[,3]),max(HERdataset[,3] ))
                 , HERneedl=mmnorm(HERdataset[,4],min(HERdataset[,4]),max(HERdataset[,4] ))
                 , HERrec=mmnorm(HERdataset[,5],min(HERdataset[,5]),max(HERdataset[,5] ))
                 , addHERtot=mmnorm(HERdataset[,6],min(HERdataset[,6]),max(HERdataset[,6] ))
                 , addHER=mmnorm(HERdataset[,7],min(HERdataset[,7]),max(HERdataset[,7] ))
)


idx=seq(from=1,to=7224,by=7)

HERtest <- HERnorm[idx,]
HERtraining <- HERnorm[-idx,]

?knn()


## you can choose different 'k' to find the best 'k'
Error_Rate <- rep(0, 10)
k <- 1:50
for(x in k){
  HERpredict<-knn(HERtraining[,-7],HERtest[,-7],HERtraining[,7],k=x)
  Error_Rate[x] <- mean(HERpredict !=HERtraining[,7] )
}

plot(k, Error_Rate, type = 'b')

## best k is 3
library(class)
HERpredict<-knn(HERtraining[,-7],HERtest[,-7],HERtraining[,7],k=3)
HERresults<-cbind(HERtest,as.character(HERpredict) )
head(HERresults)

table(HERresults[,7],HERresults[,8])
HERwrong<-HERresults[,7]!=HERresults[,8]

HERrate<-sum(HERwrong)/length(HERwrong)
HERrate



## Applying KNN on Cocaine
mmnorm <-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx))
return(z) 
}

#create a new dataset with


COCnorm<-cbind(  cocage=mmnorm(COCdataset[,1],min(COCdataset[,1]),max(COCdataset[,1]))
                 , coctot=mmnorm(COCdataset[,2],min(COCdataset[,2]),max(COCdataset[,2] ))
                 ,cocus30a=mmnorm(COCdataset[,3],min(COCdataset[,3]),max(COCdataset[,3] ))
                 , cocrec=mmnorm(COCdataset[,4],min(COCdataset[,4]),max(COCdataset[,4] ))
                 , addcoc=mmnorm(COCdataset[,5],min(COCdataset[,5]),max(COCdataset[,5] ))
                 
)


idx=seq(from=1,to=7224,by=7)

COCtest <- COCnorm[idx,]
COCtraining <- COCnorm[-idx,]

?knn()


## you can choose different 'k' to find the best 'k'
Error_Rate <- rep(0, 10)
k <- 1:50
for(x in k){
  COCpredict<-knn(COCtraining[,-5],COCtest[,-5],COCtraining[,5],k=x)
  Error_Rate[x] <- mean(COCpredict !=COCtraining[,5] )
}

plot(k, Error_Rate, type = 'b')


## best k is 20
library(class)
COCpredict<-knn(COCtraining[,-5],COCtest[,-5],COCtraining[,5],k=20)
COCresults<-cbind(COCtest,as.character(COCpredict) )
head(COCresults)

table(COCresults[,5],COCresults[,6])
COCwrong<-COCresults[,5]!=COCresults[,6]

COCrate<-sum(COCwrong)/length(COCwrong)
COCrate



## Applying C5.0 ALgorithm on ALchol
library(C50)
ALCdataset$addalc[ALCdataset$addalc==0]<-"Not Addicted"
ALCdataset$addalc[ALCdataset$addalc==1]<-"Addicted"
table(ALCdataset$addalc)
set.seed(9850)
g<-runif(nrow(ALCdataset))
ALCdataset_r <- ALCdataset[order(g),]

id=seq(from=1,to=nrow(ALCdataset_r),by=7)
ALCdataset_r_test<-ALCdataset_r[id,]
ALCdataset_r_train<-ALCdataset_r[-id,]

ALCmodel <- C5.0(ALCdataset_r_train[,-7],as.factor(ALCdataset_r_train[,7]))
ALCmodel
summary(ALCmodel)
plot(ALCmodel)
ALCmodel$y
ALCpredict<-predict(ALCmodel,ALCdataset_r_test[,-7])
ALCpredict
summary.C5.0(ALCModel)
table(Actual=ALCdataset_r_test[,7], predicted=ALCpredict)
ALCresults<-cbind(ALCdataset_r_test,as.character(ALCpredict) )

ALCwrong<-ALCresults[,7]!=ALCresults[,8]

ALCrate<-sum(ALCwrong)/length(ALCwrong)
ALCrate

summary.C5.0(ALCModel)


##Applying C5.0 on Marijuana
library(C50)
MJdataset$addmj[MJdataset$addmj==0]<-"Not Addicted"
MJdataset$addmj[MJdataset$addmj==1]<-"Addicted"
table(ALCdataset$addmj)
set.seed(9850)
g<-runif(nrow(MJdataset))
MJdataset_r <- MJdataset[order(g),]

id=seq(from=1,to=nrow(ALCdataset_r),by=7)
MJdataset_r_test<-MJdataset_r[id,]
MJdataset_r_train<-MJdataset_r[-id,]

MJmodel <- C5.0(MJdataset_r_train[,-7],as.factor(MJdataset_r_train[,7]))
MJmodel
summary(MJmodel)
plot(MJmodel)

MJpredict<-predict(MJmodel,MJdataset_r_test[,-7])
MJpredict
summary.C5.0(MJModel)
table(Actual=MJdataset_r_test[,7], predicted=MJpredict)
MJresults<-cbind(MJdataset_r_test,as.character(MJpredict) )

MJwrong<-MJresults[,7]!=MJresults[,8]

MJrate<-sum(MJwrong)/length(MJwrong)
MJrate

summary.C5.0(MJModel)

## Applying C5.0 on Heroine
library(C50)
HERdataset$addher[HERdataset$addher==0]<-"Not Addicted"
HERdataset$addher[HERdataset$addher==1]<-"Addicted"
table(ALCdataset$addher)
set.seed(9850)
g<-runif(nrow(HERdataset))
HERdataset_r <- HERdataset[order(g),]

id=seq(from=1,to=nrow(HERdataset_r),by=7)
HERdataset_r_test<-HERdataset_r[id,]
HERdataset_r_train<-HERdataset_r[-id,]

HERmodel <- C5.0(HERdataset_r_train[,-7],as.factor(HERdataset_r_train[,7]))
HERmodel
summary(HERmodel)
plot(HERmodel)

HERpredict<-predict(HERmodel,HERdataset_r_test[,-7])
HERpredict
summary.C5.0(HERModel)
table(Actual=HERdataset_r_test[,7], predicted=HERpredict)
HERresults<-cbind(HERdataset_r_test,as.character(HERpredict) )

HERwrong<-HERresults[,7]!=HERresults[,8]

HERrate<-sum(HERwrong)/length(HERwrong)
HERrate

summary.C5.0(HERModel)

## Applying C5.0 on Cocaine
library(C50)
COCdataset$addcoc[COCdataset$addcoc==0]<-"Not Addicted"
COCdataset$addcoc[COCdataset$addcoc==1]<-"Addicted"
table(ALCdataset$addcoc)
set.seed(9850)
g<-runif(nrow(COCdataset))
COCdataset_r <- COCdataset[order(g),]

id=seq(from=1,to=nrow(COCdataset_r),by=7)
COCdataset_r_test<-COCdataset_r[id,]
COCdataset_r_train<-COCdataset_r[-id,]

COCmodel <- C5.0(COCdataset_r_train[,-5],as.factor(COCdataset_r_train[,5]))
COCmodel
summary(COCmodel)
plot(COCmodel)

COCpredict<-predict(COCmodel,COCdataset_r_test[,-5])
COCpredict
summary.C5.0(COCModel)
table(Actual=COCdataset_r_test[,5], predicted=COCpredict)
COCresults<-cbind(COCdataset_r_test,as.character(COCpredict) )

COCwrong<-COCresults[,5]!=HERresults[,6]

COCrate<-sum(COCwrong)/length(COCwrong)
COCrate

summary.C5.0(COCModel)
## Aplying K-means on substances
#Marijuana

mmj <- MJdataset
mmj$mjage <- mmnorm(MJdataset[,1],min(MJdataset[,1]),max(MJdataset[,1]))
mmj$mjrec <- mmnorm(MJdataset[,2],min(MJdataset[,2]),max(MJdataset[,2]))
mmj$mjtot <- mmnorm(MJdataset[,3],min(MJdataset[,3]),max(MJdataset[,3]))
mmj$mjknown<- mmnorm(MJdataset[,4],min(MJdataset[,4]),max(MJdataset[,4]))
mmj$mjday30a<- mmnorm(MJdataset[,5],min(MJdataset[,5]),max(MJdataset[,5]))
mmj$mjfirst<- mmnorm(MJdataset[,6],min(MJdataset[,6]),max(MJdataset[,6]))
mmj$addmj <- NULL

m <- kmeans(mmj,2)
m
table(m$cluster,MJdataset$addmj)

#Cocaine

ncoc <- COCdataset
ncoc$cocage <-mmnorm(COCdataset[,1],min(COCdataset[,1]),max(COCdataset[,1]))
ncoc$coctot <-mmnorm(COCdataset[,2],min(COCdataset[,2]),max(COCdataset[,2]))
ncoc$cocus30a <-mmnorm(COCdataset[,3],min(COCdataset[,3]),max(COCdataset[,3]))
ncoc$cocrec <-mmnorm(COCdataset[,4],min(COCdataset[,4]),max(COCdataset[,4]))
ncoc$addcoc <-mmnorm(COCdataset[,5],min(COCdataset[,5]),max(COCdataset[,5]))
testcoc <- ncoc
testcoc$addcoc <- NULL
c <- kmeans(testcoc,2, ??nstart = 20)
c
table(COCdataset$addcoc , c$cluster)

#Alcohol

atest <- ALCdataset$
  atest[,1] <- mmnorm(ALCdataset[,1],min(ALCdataset[,1]),max(ALCdataset[,1]))
atest[,2] <- mmnorm(ALCdataset[,2],min(ALCdataset[,2]),max(ALCdataset[,2]))
atest[,3] <- mmnorm(ALCdataset[,3],min(ALCdataset[,3]),max(ALCdataset[,3]))
atest[,4] <- mmnorm(ALCdataset[,4],min(ALCdataset[,4]),max(ALCdataset[,4]))
atest[,5] <- mmnorm(ALCdataset[,5],min(ALCdataset[,5]),max(ALCdataset[,5]))
atest[,6] <- mmnorm(ALCdataset[,6],min(ALCdataset[,6]),max(ALCdataset[,6]))
atest[,7] <- NULL
a <- kmeans(atest, 2)
a
table(ACTUAL = ALCdataset$addalc,PREDICTED= a$cluster)
#Heroine
testher <- HERdataset

testher[,1] <- mmnorm(HERdataset[,1],min(HERdataset[,1]),max(HERdataset[,1]))
testher[,2]=mmnorm(HERdataset[,2],min(HERdataset[,2]),max(HERdataset[,2] ))
testher[,3]=mmnorm(HERdataset[,3],min(HERdataset[,3]),max(HERdataset[,3] ))
testher[,4]=mmnorm(HERdataset[,4],min(HERdataset[,4]),max(HERdataset[,4] ))
testher[,5]mmnorm(HERdataset[,5],min(HERdataset[,5]),max(HERdataset[,5] ))
testher[,6]=mmnorm(HERdataset[,6],min(HERdataset[,6]),max(HERdataset[,6] ))
testher[,7]=mmnorm(HERdataset[,7],min(HERdataset[,7]),max(HERdataset[,7]) )

e <- kmeans(testher, 2)
table(HERdataset$addher , e$cluster)

## DATA VISUALIZATION
##Plots the graphs
##Graph for Gender for each (OP/NONOP/NOADD/NONOPBUTADD)
mydataset$op[mydataset$op==1]<-"Opioid Addict"
mydataset$nonop[mydataset$nonop==0]<-"NON Opioid NoN Addict"
mydataset$nonop[mydataset$nonop==1]<-"NON Opioid Addict"
mydataset$noadd[mydataset$noadd==0]<-"Addict"
mydataset$noadd[mydataset$noadd==1]<-"Never Addict"
mydataset$nonopbutadd[mydataset$nonopbutadd==0]<-"NON Opioid NON Addict"
mydataset$nonopbutadd[mydataset$nonopbutadd==1]<-"NoN Opiod Addict"
mydataset$op[mydataset$op==0]<-"Opioid NoN Addict"
## Graphs for Gender
mydataset$sex[mydataset$sex==1]<-"Male"
mydataset$sex[mydataset$sex==2]<-"Female"
counts <- table(mydataset$sex, mydataset$op)
barplot(counts, main="Number of Opioids for each Gender",
        xlab="Gender",ylab="Number of Opioids ", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
counts <- table(mydataset$sex, mydataset$nonop)
barplot(counts, main="Number of NON Opioids for each Gender",
        xlab="Gender",ylab="Number of NONOpioids ", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
counts <- table(mydataset$sex, mydataset$noadd)
barplot(counts, main="Number of Non Addicts for each Gender",
        xlab="Gender",ylab="Number of NON Addicts ", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
counts <- table(mydataset$sex, mydataset$nonopbutadd)
barplot(counts, main="Number of NON Opioid Addicts for each Gender",
        xlab="Gender",ylab="Number of NON Opioid Addicts ", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
summary(mydataset$educ)
## Graph for EDUCATION on each (OP/NONOP/NOADD/NONOPBUTADD)
mydataset$educ[mydataset$educ==1]<-"No schooling"
mydataset$educ[mydataset$educ==2]<-"Elementary-5th Grade"
mydataset$educ[mydataset$educ==3]<-"Some High School"
mydataset$educ[mydataset$educ==4]<-"High School Graduate"
mydataset$educ[mydataset$educ==5]<-"Technical School"
mydataset$educ[mydataset$educ==6]<-"Some College"
mydataset$educ[mydataset$educ==7]<-"College graduate or beyond"
mydataset$educ[mydataset$educ==93]<-"Does not Apply"
mydataset$educ[mydataset$educ==98]<-"Blank"
counts <- table(mydataset$educ, mydataset$op)
barplot(counts, main="Number of Opioids for each Education Level",
        xlab="Education",ylab="Number of Opioids ", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
counts <- table(mydataset$educ, mydataset$nonop)
barplot(counts, main="Number of NON Opioids for each Education Level",
        xlab="Education",ylab="Number of NONOpioids ", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
counts <- table(mydataset$educ, mydataset$noadd)
barplot(counts, main="Number of Non Addicts for each Education Level",
        xlab="Education",ylab="Number of NON Addicts ", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
counts <- table(mydataset$educ, mydataset$nonopbutadd)
barplot(counts, main="Number of NON Opioid Addicts for each Education Level",
        xlab="Education",ylab="Number of NON Opioid Addicts ", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)


## Graph for RACE on each (OP/NONOP/NOADD/NONOPBUTADD)
mydataset$resprace[mydataset$resprace==1]<-"American"
mydataset$resprace[mydataset$resprace==2]<-"Alaskan"
mydataset$resprace[mydataset$resprace==3]<-"Asian"
mydataset$resprace[mydataset$resprace==4]<-"Pacific "
mydataset$resprace[mydataset$resprace==5]<-"White"
mydataset$resprace[mydataset$resprace==6]<-"Black"
mydataset$resprace[mydataset$resprace==7]<-"Other"
mydataset$resprace[mydataset$resprace==98]<-"Blank"
counts <- table(mydataset$resprace, mydataset$op)
barplot(counts, main="Number of Opioids for each Race",
        xlab="Race",ylab="Number of Opioids ", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
counts <- table(mydataset$resprace, mydataset$nonop)
barplot(counts, main="Number of NON Opioids for each Race",
        xlab="Race",ylab="Number of NONOpioids ", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
counts <- table(mydataset$resprace, mydataset$noadd)
barplot(counts, main="Number of Non Addicts for each Race",
        xlab="Race",ylab="Number of NON Addicts ", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
counts <- table(mydataset$resprace, mydataset$nonopbutadd)
barplot(counts, main="Number of NON Opioid Addicts for each Race",
        xlab="Race",ylab="Number of NON Opioid Addicts ", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
## Graph for Income on each (OP/NONOP/NOADD/NONOPBUTADD)
mydataset$income[mydataset$income==1]<-"No income"
mydataset$income[mydataset$income==2]<-"Under $6,999"
mydataset$income[mydataset$income==3]<-"$7,000 - $9,999"
mydataset$income[mydataset$income==4]<-"$10,000 - $14,999 "
mydataset$income[mydataset$income==5]<-"$15,000 - $19,999"
mydataset$income[mydataset$income==6]<-"$20,000 - $24,999	"
mydataset$income[mydataset$income==7]<-"$25,000 - $29,999"
mydataset$income[mydataset$income==8]<-"$30,000 - $34,999"
mydataset$income[mydataset$income==9]<-"$35,000 or more"
mydataset$income[mydataset$income==93]<-"DOES NOT APPLY"

mydataset$income[mydataset$income==98]<-"Blank"
counts <- table(mydataset$income, mydataset$op)
barplot(counts, main="Number of Opioids for each Income",
        xlab="Income",ylab="Number of Opioids ", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
counts <- table(mydataset$income, mydataset$nonop)
barplot(counts, main="Number of NON Opioids for each Income",
        xlab="Income",ylab="Number of NONOpioids ", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
counts <- table(mydataset$income, mydataset$noadd)
barplot(counts, main="Number of Non Addicts for each Income",
        xlab="Income",ylab="Number of NON Addicts ", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
counts <- table(mydataset$income, mydataset$nonopbutadd)
barplot(counts, main="Number of NON Opioid Addicts for each Income",
        xlab="Income",ylab="Number of NON Opioid Addicts ", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

## Graph for AgeGroup on each (OP/NONOP/NOADD/NONOPBUTADD)
mydataset$ragegrp[mydataset$ragegrp==1]<-"18-25"
mydataset$ragegrp[mydataset$ragegrp==2]<-"26-49"
mydataset$ragegrp[mydataset$ragegrp==3]<-"50+"
mydataset$ragegrp[mydataset$ragegrp==4]<-"Blank"
counts <- table(mydataset$ragegrp, mydataset$op)
barplot(counts, main="Number of Opioids for each AgeGroup",
        xlab="AgeGroup",ylab="Number of Opioids ", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
counts <- table(mydataset$ragegrp, mydataset$nonop)
barplot(counts, main="Number of NON Opioids for each AgeGroup",
        xlab="AgeGroup",ylab="Number of NONOpioids ", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
counts <- table(mydataset$ragegrp, mydataset$noadd)
barplot(counts, main="Number of Non Addicts for each AgeGroup",
        xlab="AgeGroup",ylab="Number of NON Addicts ", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
counts <- table(mydataset$ragegrp, mydataset$nonopbutadd)
barplot(counts, main="Number of NON Opioid Addicts for each AgeGroup",
        xlab="AgeGroup",ylab="Number of NON Opioid Addicts ", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
