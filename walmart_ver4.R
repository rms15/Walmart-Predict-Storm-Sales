### Models log
# poisson regression, Random forests  (0.58)
# Boosted gbm models, one for each station nbr. If NAs > 50%, that column dropped (0.28)
# Boosted gbm models by station nbr, excluded items with zero sales
# Boosted gbm models, build separate models for rain vs snowfall. That will eliminate some of the NAs
# Boosted gbm models, separate models for each item number? 110 models.
####
install.packages("MASS")
install.packages("ggplot2")
install.packages("glmnet")
install.packages("stringr")
install.packages("plyr")
install.packages("randomForest")
install.packages("gbm")
install.packages("caret")
print("loading packages")
library(caret)
library(MASS)
library(ggplot2)
library(glmnet)
library(plyr)
library(randomForest)
library(gbm)
setwd("~/walmart")
#setwd("/Volumes/rms15/walmart")
#setwd("~/Rice/Kaggle/Walmart")
#setwd("P:/Staff_Folders/Riaz/walmart")
print("reading training dataset")
train.smp<-read.csv("merge_train_all_rows.csv")
print("reading test dataset")
test.full<-read.csv("test_full_mod.csv")
# test.full$sunrise <-as.character(test.full$sunrise)
# test.full$sunset <-as.character(test.full$sunset)
# test.full$codesum <- as.character(test.full$codesum)
# test.full$date <- as.Date(test.full$date)
# test.full$snowfall <- as.character(test.full$snowfall)
# 
# test.full$depart <- as.numeric(as.character(test.full$depart))
# test.full$cool <- as.numeric(as.character(test.full$cool))

test.full$month<-as.numeric(substr((test.full$date),6,7))
test.full$sunset.num<-as.numeric(test.full$sunset)
test.full$sunrise.num<-as.numeric(test.full$sunrise)

#head(test.full,10000)
test.full$sunrise.fmt<-NA

test.full$sunset.fmt<-NA
test.full$sunset.fmt[which(!is.na(test.full$sunset.num))]<- 
  paste(substr(test.full$sunset.num[which(!is.na(test.full$sunset.num))],1,2),":",substr(test.full$sunset.num[which(!is.na(test.full$sunset.num))],3,4),sep="")



test.full$sunrise.fmt[which(!is.na(test.full$sunrise.num))]<- 
  paste(substr(test.full$sunrise.num[which(!is.na(test.full$sunrise.num))],1,1),":",substr(test.full$sunrise.num[which(!is.na(test.full$sunrise.num))],2,3),sep="")

test.full$sunrise.fmt2<-strptime(test.full$sunrise.fmt,format="%H:%M")

test.full$sunset.fmt2<-strptime(test.full$sunset.fmt,format="%H:%M")

test.full$day.length<- (difftime(test.full$sunset.fmt2, test.full$sunrise.fmt2,units=c("hours")))

#plot(as.numeric(test.full$day.length) ~ test.full$month)

###########################################################################
####  Model ##
###########################################################################
#train.smp <- merge_train_all_rows
train.smp$tmax <- as.numeric(as.character(train.smp$tmax))               
train.smp$tmin <- as.numeric(as.character(train.smp$tmin))               
train.smp$tavg <- as.numeric(as.character(train.smp$tavg))
train.smp$dewpoint <- as.numeric(as.character(train.smp$dewpoint))
train.smp$heat <- as.numeric(as.character(train.smp$heat))
train.smp$cool <- as.numeric(as.character(train.smp$cool))
train.smp$preciptotal <- as.numeric(as.character(train.smp$preciptotal))
train.smp$stnpressure <- as.numeric(as.character(train.smp$stnpressure))
train.smp$sealevel <- as.numeric(as.character(train.smp$sealevel))
train.smp$resultspeed <- as.numeric(as.character(train.smp$resultspeed))
train.smp$resultdir <- as.numeric(as.character(train.smp$resultdir))
train.smp$avgspeed <- as.numeric(as.character(train.smp$avgspeed))
train.smp$wetbulb <- as.numeric(as.character(train.smp$wetbulb))
train.smp$weather.event.bool <- as.numeric((train.smp$weather.event))
train.smp$sunrise <-as.character(train.smp$sunrise)
train.smp$sunset <-as.character(train.smp$sunset)
train.smp$depart <- as.numeric(as.character(train.smp$depart))


train.smp$month<-as.numeric(substr((train.smp$date),6,7))
train.smp[which(train.smp$wthr_ind == -3),]$wthr_ind = 'A'
train.smp[which(train.smp$wthr_ind == -2),]$wthr_ind = 'B'
train.smp[which(train.smp$wthr_ind == -1),]$wthr_ind = 'C'
train.smp[which(train.smp$wthr_ind == 0),]$wthr_ind = 'D'
train.smp[which(train.smp$wthr_ind == 1),]$wthr_ind = 'E'
train.smp[which(train.smp$wthr_ind == 2),]$wthr_ind = 'F'
train.smp[which(train.smp$wthr_ind == 3),]$wthr_ind = 'G'


unique(as.numeric(train.smp$sunset))

train.smp$sunset.num<-as.numeric(train.smp$sunset)
train.smp$sunrise.num<-as.numeric(train.smp$sunrise)
train.smp$sunrise.fmt<-NA
train.smp$sunset.fmt<-NA
train.smp$sunset.fmt[which(!is.na(train.smp$sunset.num))]<- 
  paste(substr(train.smp$sunset.num[which(!is.na(train.smp$sunset.num))],1,2),":",substr(train.smp$sunset.num[which(!is.na(train.smp$sunset.num))],3,4),sep="")


train.smp$sunrise.fmt[which(!is.na(train.smp$sunrise.num))]<- 
  paste(substr(train.smp$sunrise.num[which(!is.na(train.smp$sunrise.num))],1,1),":",substr(train.smp$sunrise.num[which(!is.na(train.smp$sunrise.num))],2,3),sep="")

train.smp$sunrise.fmt2<-strptime(train.smp$sunrise.fmt,format="%H:%M")

train.smp$sunset.fmt2<-strptime(train.smp$sunset.fmt,format="%H:%M")

train.smp$day.length<- (difftime(train.smp$sunset.fmt2, train.smp$sunrise.fmt2))
hist(as.numeric(train.smp$day.length))
plot(as.numeric(train.smp$day.length) ~ train.smp$month)
head(as.numeric(train.smp$sunset) - as.numeric(train.smp$sunrise))
## gbm 

fitControl = trainControl(
  method = "repeatedcv",
  number = 3,
  repeats = 3,
  selectionFunction = "best")
str(fitControl)
gbmGrid <-  expand.grid(interaction.depth = c(1,3,5,7),
                        n.trees = (10:22)*10,
                        shrinkage = 0.1,
                        n.minobsinnode=(1:3)*20)

nrow(gbmGrid)
######## THE LOOP for training ##
print("model building")
train.smp$wthr_ind_factor<-as.factor(train.smp$wthr_ind)

train.smp$snowfall <- as.numeric(as.character(train.smp$snowfall))

### Check stations with no day length
for (i in unique(train.smp$station_nbr))
{
  mod.nam <- paste("lm.mod.st", i, sep = ".")
  print(paste("Training for the station number:",i))
  train.smp.onest<-train.smp[which(train.smp$station_nbr==i),]
  print(length(which(is.na((train.smp.onest$day.length))))*100/nrow(train.smp.onest))
}  

##########Identify items which do not have sales at all for any date in the training dataset.
### These items are not weather sensitive. 
item_zerosales <-NULL
j=1
for (i in 1:111)
{
 # mod.nam <- paste("lm.mod.st", i, sep = ".")
#  print(paste("Sales for the item number:",i))
  train.smp.oneitem<-train.smp[which(train.smp$item_nbr==i),]
 items_zerosale<-length(which(train.smp.oneitem$units ==0))*100/nrow(train.smp.oneitem)
  if (items_zerosale == 100)
  {
    print(paste("Sales are zero for the item number:", i))
    item_zerosales[j] = i
    j = j+1
  }
}  
item_zerosales
# [1] "Sales are zero for the item number: 22"
# [1] "Sales are zero for the item number: 24"
# [1] "Sales are zero for the item number: 34"
# [1] "Sales are zero for the item number: 38"
# [1] "Sales are zero for the item number: 47"
# [1] "Sales are zero for the item number: 63"
# [1] "Sales are zero for the item number: 64"
# [1] "Sales are zero for the item number: 66"
# [1] "Sales are zero for the item number: 95"
# [1] "Sales are zero for the item number: 96"
# [1] "Sales are zero for the item number: 97"
# [1] "Sales are zero for the item number: 99"
# [1] "Sales are zero for the item number: 101"
# [1] "Sales are zero for the item number: 102"
# [1] "Sales are zero for the item number: 103"
# [1] "Sales are zero for the item number: 104"
# [1] "Sales are zero for the item number: 105"
# [1] "Sales are zero for the item number: 106"
# [1] "Sales are zero for the item number: 107"
# [1] "Sales are zero for the item number: 108"
# [1] "Sales are zero for the item number: 109"
# [1] "Sales are zero for the item number: 110"
# [1] "Sales are zero for the item number: 111"


#itemz_nzsales<-unique(train.smp[which(!train.smp$item_nbr %in%  item_zerosales),]$item_nbr)

train.smp.nz<- train.smp[which(!train.smp$item_nbr %in%  item_zerosales),]
nrow(train.smp.nz)
###backup train.smp in .all and load with nz sale data
#train.smp.all<-train.smp
#train.smp<-train.smp.nz
## day.length not present for 1,12,13,16,17,20,7,8,9
## snowfall not present for 1,10,13,16,8,9
## heat,cool  not present for station nbr 8
#####Identify whether snowfall and rainfall are mutually exclusive

str(train.smp.nz)

fitControl = trainControl(
  method = "repeatedcv",
  number = 3,
  repeats = 3,
  selectionFunction = "best")

nrow(train.smp.all)
train.smp$snow_ind = as.numeric(train.smp$snow_ind)
train.smp$rain_ind = as.numeric(train.smp$rain_ind)

for (i in unique(train.smp$station_nbr))
{
  mod.nam <- paste("lm.mod.st", i, sep = ".")
  print(paste("Training for the station number:",i))
  train.smp.onest<-train.smp[which(train.smp$station_nbr==i),]
  
  print(day.length.NAs<-length(which(is.na((train.smp.onest$day.length))))*100/nrow(train.smp.onest))
  print(snowfall.NAs<-length(which(is.na((train.smp.onest$snowfall))))*100/nrow(train.smp.onest))
  print(heat.NAs<-length(which(is.na((train.smp.onest$heat))))*100/nrow(train.smp.onest))
  print(cool.NAs<-length(which(is.na((train.smp.onest$cool))))*100/nrow(train.smp.onest))
  print(preciptotal.NAs<-length(which(is.na((train.smp.onest$preciptotal))))*100/nrow(train.smp.onest))
  print(tavg.NAs <-length(which(is.na((train.smp.onest$tavg))))*100/nrow(train.smp.onest))
  print(depart.NAs <-length(which(is.na((train.smp.onest$depart))))*100/nrow(train.smp.onest))
  print(sealevel.NAs <-length(which(is.na((train.smp.onest$sealevel))))*100/nrow(train.smp.onest))
  
  train.smp.cols<-train.smp.onest[,c("units","store_nbr","item_nbr", "wthr_ind_factor","month", 
                                     "tmax","tavg",
                                     "dewpoint","wetbulb","heat","cool",  
                                     "preciptotal", "stnpressure","sealevel","rain_ind","snow_ind",
                                     "snowfall",  "avgspeed","day.length")]
  if (day.length.NAs > 50 )
  {
    train.smp.cols$day.length<-NULL
    print("Column day.length dropped")
  }
  if (preciptotal.NAs > 50 )
  {
    train.smp.cols$preciptotal.NAs<-NULL
    print("Column preciptotal dropped")
  }
  if (snowfall.NAs > 50)
  {
    train.smp.cols$snowfall<-NULL
    print("Column snowfall dropped")
  }
  if (heat.NAs > 50)
  {
    train.smp.cols$heat<-NULL
    print("Column heat dropped")        
  }
  if (cool.NAs > 50)
  {
    train.smp.cols$cool<-NULL
    print("Column cool dropped")
  }
  if (tavg.NAs > 50)
  {
    train.smp.cols$tavg<-NULL
    print("Column tavg dropped")
  }
  if (depart.NAs > 50)
  {
    train.smp.cols$depart<-NULL
    print("Column depart dropped")
  }
  if (sealevel.NAs > 50)
  {
    train.smp.cols$sealevel<-NULL
    print("Column sealevel dropped")
  }
  train.smp.cols.nz<- train.smp.cols[which(!train.smp.cols$item_nbr %in%  item_zerosales),]
  
                assign(mod.nam,train(units ~ .,data=train.smp.cols.nz,
                                     method="gbm",
                                     trControl=fitControl,
                                     tuneGrid = gbmGrid,
                                     verbose=FALSE,
                                     distribution="poisson",
                                     train.fraction = 0.8,na.action=na.omit))

 
  print("Best model:")
  print(get(mod.nam)$best)
  
}

#### Build a model for every item number

predict.train.merge<-NULL
predict.train.onest<-NULL
for (i in unique(train.smp$item_nbr))
{
  mod.nam <- paste("boost.mod.item", i, sep = ".")
  print(paste("Training for the item number:",i))
  train.smp.oneitem<-train.smp[which(train.smp$item_nbr==i),]
  
  print(day.length.NAs<-length(which(is.na((train.smp.oneitem$day.length))))*100/nrow(train.smp.oneitem))
  print(snowfall.NAs<-length(which(is.na((train.smp.oneitem$snowfall))))*100/nrow(train.smp.oneitem))
  print(heat.NAs<-length(which(is.na((train.smp.oneitem$heat))))*100/nrow(train.smp.oneitem))
  print(cool.NAs<-length(which(is.na((train.smp.oneitem$cool))))*100/nrow(train.smp.oneitem))
  print(preciptotal.NAs<-length(which(is.na((train.smp.oneitem$preciptotal))))*100/nrow(train.smp.oneitem))
  print(tavg.NAs <-length(which(is.na((train.smp.oneitem$tavg))))*100/nrow(train.smp.oneitem))
  print(depart.NAs <-length(which(is.na((train.smp.oneitem$depart))))*100/nrow(train.smp.oneitem))
  print(sealevel.NAs <-length(which(is.na((train.smp.oneitem$sealevel))))*100/nrow(train.smp.oneitem))
  
  train.smp.cols<-train.smp.oneitem[,c("units","store_nbr","item_nbr", "wthr_ind_factor","month", 
                                     "tmax","tavg",
                                     "dewpoint","wetbulb","heat","cool",  
                                     "preciptotal", "stnpressure","sealevel","rain_ind","snow_ind",
                                     "snowfall",  "avgspeed","day.length")]
  if (day.length.NAs > 50 )
  {
    train.smp.cols$day.length<-NULL
    print("Column day.length dropped")
  }
  if (preciptotal.NAs > 50 )
  {
    train.smp.cols$preciptotal.NAs<-NULL
    print("Column preciptotal dropped")
  }
  if (snowfall.NAs > 50)
  {
    train.smp.cols$snowfall<-NULL
    print("Column snowfall dropped")
  }
  if (heat.NAs > 50)
  {
    train.smp.cols$heat<-NULL
    print("Column heat dropped")        
  }
  if (cool.NAs > 50)
  {
    train.smp.cols$cool<-NULL
    print("Column cool dropped")
  }
  if (tavg.NAs > 50)
  {
    train.smp.cols$tavg<-NULL
    print("Column tavg dropped")
  }
  if (depart.NAs > 50)
  {
    train.smp.cols$depart<-NULL
    print("Column depart dropped")
  }
  if (sealevel.NAs > 50)
  {
    train.smp.cols$sealevel<-NULL
    print("Column sealevel dropped")
  }
  assign(mod.nam,train(units ~ .,data=train.smp.cols,
                       method="gbm",
                       trControl=fitControl,
                       tuneGrid = gbmGrid,
                       verbose=FALSE,
                       distribution="poisson",
                       train.fraction = 0.8,na.action=na.omit))
  
  
  print("Best model:")
  print(get(mod.nam)$best)
  
}

# 
test.full$wthr_ind_factor<-NA
test.full[which(test.full$wthr_ind == -3),]$wthr_ind_factor = 'A'
test.full[which(test.full$wthr_ind == -2),]$wthr_ind_factor = 'B'
test.full[which(test.full$wthr_ind == -1),]$wthr_ind_factor = 'C'
test.full[which(test.full$wthr_ind == 0),]$wthr_ind_factor = 'D'
test.full[which(test.full$wthr_ind == 1),]$wthr_ind_factor = 'E'
test.full[which(test.full$wthr_ind == 2),]$wthr_ind_factor = 'F'
test.full[which(test.full$wthr_ind == 3),]$wthr_ind_factor = 'G'


test.full$wthr_ind_factor <- as.factor(test.full$wthr_ind_factor)
test.full$snowfall<- as.numeric(test.full$snowfall)

####################################
### predicting on training dataset
####################################

predict.train.merge<-NULL
predict.train.onest<-NULL
for (i in unique(train.smp$station_nbr))
{
  
  mod.nam <- paste("lm.mod.st", i, sep = ".")
  print(paste("Predicting for the train data for station number:",i))
  train.smp.onest<-train.smp[which(train.smp$station_nbr==i),]
  
  print(day.length.NAs<-length(which(is.na((train.smp.onest$day.length))))*100/nrow(train.smp.onest))
  print(snowfall.NAs<-length(which(is.na((train.smp.onest$snowfall))))*100/nrow(train.smp.onest))
  print(heat.NAs<-length(which(is.na((train.smp.onest$heat))))*100/nrow(train.smp.onest))
  print(cool.NAs<-length(which(is.na((train.smp.onest$cool))))*100/nrow(train.smp.onest))
  print(preciptotal.NAs<-length(which(is.na((train.smp.onest$preciptotal))))*100/nrow(train.smp.onest))
  print(tavg.NAs <-length(which(is.na((train.smp.onest$tavg))))*100/nrow(train.smp.onest))
  print(depart.NAs <-length(which(is.na((train.smp.onest$depart))))*100/nrow(train.smp.onest))
  print(sealevel.NAs <-length(which(is.na((train.smp.onest$sealevel))))*100/nrow(train.smp.onest))
  train.smp.cols<-NULL
  train.smp.cols<-train.smp.onest[,c("station_nbr","store_nbr","item_nbr", "wthr_ind_factor","month", 
                                   "tmax","tavg",
                                   "dewpoint","wetbulb","heat","cool",  
                                   "preciptotal", "stnpressure","sealevel",
                                   "rain_ind","snow_ind",
                                   "snowfall",  "avgspeed","day.length","units")]
#   if (day.length.NAs > 50 )
#   {
#     train.smp.cols$day.length<-NULL
#     print("Column day.length dropped")
#   }
#   if (preciptotal.NAs > 50 )
#   {
#     train.smp.cols$preciptotal.NAs<-NULL
#     print("Column preciptotal dropped")
#   }
#   if (snowfall.NAs > 50)
#   {
#     train.smp.cols$snowfall<-NULL
#     print("Column snowfall dropped")
#   }
#   if (heat.NAs > 50)
#   {
#     train.smp.cols$heat<-NULL
#     print("Column heat dropped")        
#   }
#   if (cool.NAs > 50)
#   {
#     train.smp.cols$cool<-NULL
#     print("Column cool dropped")
#   }
#   if (tavg.NAs > 50)
#   {
#     train.smp.cols$tavg<-NULL
#     print("Column tavg dropped")
#   }
#   if (depart.NAs > 50)
#   {
#     train.smp.cols$depart<-NULL
#     print("Column depart dropped")
#   }
#   if (sealevel.NAs > 50)
#   {
#     train.smp.cols$sealevel<-NULL
#     print("Column sealevel dropped")
#   }
  row.has.na <- apply(train.smp.cols, 1, function(x){any(is.na(x))})
  train.smp.cols$predicted_units<-0
  train.smp.cols$predicted_units[which(!row.has.na)]<-
    round(predict(get(mod.nam),newdata=train.smp.cols,na.action=na.omit))
  print(paste("length of the train subset:",nrow(train.smp.cols)))
  print(paste("length of the predicted outcome:",length(which(!row.has.na)))) 
  print(paste("pct of outcomes  predicted:", length(which(!row.has.na))*100/nrow(train.smp.onest)))
  
#  predict.train.onest<-train.smp.cols[,c("id","units","predicted_units")]
  predict.train.merge<-rbind(predict.train.merge,train.smp.cols)
}
colnames(predict.train.merge)
predict.train.zero<-predict.train.merge[which(predict.train.merge$units > 0 & predict.train.merge$predicted_units == 0),]
ggplot(predict.train.merge) + aes(x=units,y=predicted_units,col=store_nbr) + geom_point()



na.dist.all=NULL
na.one.row<-NULL
na.st.rows<-NULL
cols.list<- c( "tmax","tavg","depart","sunrise","sunset","codesum","resultspeed","resultdir",
              
                                 "dewpoint","wetbulb","heat","cool",  
                                 "preciptotal", "stnpressure","sealevel",
                                 "rain_ind","snow_ind",
                                 "snowfall",  "avgspeed","day.length")
for (k in unique(train.smp$station_nbr))
{
  
#  mod.nam <- paste("lm.mod.st", i, sep = ".")
#  na.dist=data.frame()
  print(paste("Checking NAs for the train data for station number:",k))
  train.smp.onest<-train.smp[which(train.smp$station_nbr==k),]
  na.st.rows<-NULL
  for ( i in (1:length(cols.list)))
  { 
    na.one.row<-cbind(k,cols.list[i],round(length(which(is.na(train.smp.onest[,i])))*100/nrow(train.smp.onest),2))
    na.st.rows<-rbind(na.st.rows,na.one.row)
#    na.dist$station_nbr[i] = k
#    na.dist$var[i] = colnames(train.smp.onest)[i]
#    na.dist$NA.pct[i]=length(which(is.na(train.smp.onest[,i])))/nrow(train.smp.onest)
  }
na.dist.all<-rbind(na.dist.all,na.st.rows)
}
head(na.dist.all)
colnames(na.dist.all) = c("station_nbr","var","NA.pct")
str(na.dist.all)

na.dist.df<-data.frame(na.dist.all)
str(na.dist.df)
na.dist.df$station_nbr= as.numeric(as.character(na.dist.df$station_nbr))
na.dist.df$var= (as.character(na.dist.df$var))
na.dist.df$NA.pct= as.numeric(as.character(na.dist.df$NA.pct))
str(na.dist.df)


ggplot(na.dist.df) + aes(x=var,y=NA.pct) + geom_bar(stat="identity") + facet_wrap(~station_nbr) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####################################
#############By item nbr
####################################

for (k in unique(train.smp$item_nbr))
{
  
  #  mod.nam <- paste("lm.mod.st", i, sep = ".")
  #  na.dist=data.frame()
  print(paste("Checking NAs for the train data for item number:",k))
  train.smp.oneitem<-train.smp[which(train.smp$item_nbr==k),]
  na.st.rows<-NULL
  for ( i in (1:length(cols.list)))
  { 
    na.one.row<-cbind(k,cols.list[i],round(length(which(is.na(train.smp.onest[,i])))*100/nrow(train.smp.onest),2))
    na.st.rows<-rbind(na.st.rows,na.one.row)
    #    na.dist$station_nbr[i] = k
    #    na.dist$var[i] = colnames(train.smp.onest)[i]
    #    na.dist$NA.pct[i]=length(which(is.na(train.smp.onest[,i])))/nrow(train.smp.onest)
  }
  na.dist.all<-rbind(na.dist.all,na.st.rows)
}


ggplot(predict.train.zero) + aes(x=units,y=predicted_units,col=factor(store_nbr)) + geom_bar(stat="identity")
ggplot(predict.train.zero) + aes(x=station_nbr) + geom_histogram()
ggplot(predict.train.zero) + aes(x=preciptotal) + geom_histogram()
length(which(is.na(predict.train.zero$preciptotal)))/nrow(predict.train.zero)
ggplot(predict.train.zero) + aes(x=snowfall) + geom_histogram()
ggplot(predict.train.zero) + aes(x=tavg) + geom_histogram()
ggplot(predict.train.zero) + aes(x=tavg) + geom_histogram()
ggplot(predict.train.zero) + aes(x=tavg) + geom_histogram()
ggplot(predict.train.zero) + aes(x=tavg) + geom_histogram()
ggplot(predict.train.zero) + aes(x=tavg) + geom_histogram()
ggplot(predict.train.zero) + aes(x=tavg) + geom_histogram()
ggplot(predict.train.zero) + aes(x=tavg) + geom_histogram()



nrow(predict.train.zero)
plot(predict.train.zero)




####predict for test dataset
print("predicting for test dataset")

test.full.nz<- test.full[which(!test.full$item_nbr %in%  item_zerosales),]
#nrow(test.full.nz)
#nrow(test.full)
###backup train.smp in .all and load with nz sale data
#test.full.all<-test.full
#test.full<-test.full.all

predict.test.merge<-NULL
predict.test.onest<-NULL
for (i in unique(test.full$station_nbr))
{

  mod.nam <- paste("lm.mod.st", i, sep = ".")
  print(paste("Predicting for the test data for station number:",i))
  test.smp.onest<-test.full[which(test.full$station_nbr==i),]
  
  print(day.length.NAs<-length(which(is.na((test.smp.onest$day.length))))*100/nrow(test.smp.onest))
  print(snowfall.NAs<-length(which(is.na((test.smp.onest$snowfall))))*100/nrow(test.smp.onest))
  print(heat.NAs<-length(which(is.na((test.smp.onest$heat))))*100/nrow(test.smp.onest))
  print(cool.NAs<-length(which(is.na((test.smp.onest$cool))))*100/nrow(test.smp.onest))
  print(preciptotal.NAs<-length(which(is.na((test.smp.onest$preciptotal))))*100/nrow(test.smp.onest))
  print(tavg.NAs <-length(which(is.na((test.smp.onest$tavg))))*100/nrow(test.smp.onest))
  print(depart.NAs <-length(which(is.na((test.smp.onest$depart))))*100/nrow(test.smp.onest))
  print(sealevel.NAs <-length(which(is.na((test.smp.onest$sealevel))))*100/nrow(test.smp.onest))
  test.smp.cols<-NULL
  test.smp.cols<-test.smp.onest[,c("id","store_nbr","item_nbr", "wthr_ind_factor","month", 
                                     "tmax","tavg",
                                     "dewpoint","wetbulb","heat","cool",  
                                     "preciptotal", "stnpressure","sealevel",
                                   "rain_ind","snow_ind",
                                     "snowfall",  "avgspeed","day.length")]
  if (day.length.NAs > 50 )
  {
    test.smp.cols$day.length<-NULL
    print("Column day.length dropped")
  }
  if (preciptotal.NAs > 50 )
  {
    test.smp.cols$preciptotal.NAs<-NULL
    print("Column preciptotal dropped")
  }
  if (snowfall.NAs > 50)
  {
    test.smp.cols$snowfall<-NULL
    print("Column snowfall dropped")
  }
  if (heat.NAs > 50)
  {
    test.smp.cols$heat<-NULL
    print("Column heat dropped")        
  }
  if (cool.NAs > 50)
  {
    test.smp.cols$cool<-NULL
    print("Column cool dropped")
  }
  if (tavg.NAs > 50)
  {
    test.smp.cols$tavg<-NULL
    print("Column tavg dropped")
  }
  if (depart.NAs > 50)
  {
    test.smp.cols$depart<-NULL
    print("Column depart dropped")
  }
  if (sealevel.NAs > 50)
  {
    test.smp.cols$sealevel<-NULL
    print("Column sealevel dropped")
  }
  row.has.na <- apply(test.smp.cols, 1, function(x){any(is.na(x))})
  test.smp.cols$predicted_units<-0
  test.smp.cols$predicted_units[which(!row.has.na)]<-
    round(predict(get(mod.nam),newdata=test.smp.cols,na.action=na.omit)) 
  test.smp.cols$predicted_units[which(test.smp.cols$item_nbr %in%  item_zerosales)] = 0
  print(paste("length of the test subset:",nrow(test.smp.cols)))
  print(paste("length of the predicted outcome:",length(which(!row.has.na)))) 
  print(paste("pct of outcomes  predicted:", round(length(which(!row.has.na))*100/nrow(test.smp.onest),2)))
  
  predict.test.onest<-test.smp.cols[,c("id","predicted_units")]
  predict.test.merge<-rbind(predict.test.merge,predict.test.onest)
}

colnames(predict.test.merge) <- c("id","units")
predict.test.merge$id <- as.character(predict.test.merge$id)
#
print("writing test prediction file for upload to kaggle website")
write.csv(predict.test.merge, "predict.test.merge_boost.csv")



