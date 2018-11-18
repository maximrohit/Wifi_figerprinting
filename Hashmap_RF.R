##############################################
#1.Reading  & sumarizeing data
#2.Data checks and EDA
  #A) Univariant analysis
  #B) segmenetd univariate analysis
  #C) Bivariant analysis
  #D) Wap value sanalysis
#3.Combining and reshuffling data
#4. Modelling with Random Forest

##################################################
#1.Reading  & sumarizeing data
#Reading data
train_base_ori<-read.csv("C:/Users/Morien/Downloads/UJIndoorLoc/UJIndoorLoc/trainingData.csv", header = T, sep = ',',stringsAsFactors=F)
validation_base_ori<-read.csv("C:/Users/Morien/Downloads/UJIndoorLoc/UJIndoorLoc/validationData.csv", header = T, sep = ',',stringsAsFactors=F)

#creating working copies
train_base<-train_base_ori
validation_base<-validation_base_ori

#####################################
#Sumarizing train data
nrow(train_base)#19937
ncol(train_base)#529 .... 520 Wap values
str(train_base[,521:529])
summary(train_base[,521:529])
#3 BUILDINGID 0,1,2 from percentile values we will evalautae further later
#5 FlOOR 0-4 from percentile values we will evalautae further later
#ID's SPACEID USERID PHONEID
#2 RELATIVEPOSITION 1 or 2


#checking for an differnece in the train and validation set
setdiff(names(train_base),names(validation_base))
setdiff(names(validation_base),names(train_base))

#Sumarizing validation data
nrow(validation_base)#1111
ncol(validation_base)#529 .... 520 Wap values
str(validation_base[,521:529])
summary(validation_base[,521:529])
#similar values except all ID's zero'ed



##########################################################
#2.Data checks and EDA
##########################################################
###Duplicate check###
sum(duplicated(train_base))#637 duplicate records in train
sum(duplicated(validation_base))#no duplicate records in validation

#de-duplicating train data
train_base<-train_base[!duplicated(train_base),]
nrow(train_base)#19300=19937-637

###NA/Null check###
#train
na_value_train<-sapply(train_base,function(x) sum(is.na(x)))
na_value_train[na_value_train!=0]#empty set, no na values
#validation
na_value_validation<-sapply(validation_base,function(x) sum(is.na(x)))
na_value_validation[na_value_train!=0]#empty set, no na values

#A) Univariant analysis########################
###Unique values###
#train
unique_value_train<-sapply(train_base[,521:529],function(x) length(unique(x)))
unique_value_train
# LONGITUDE     LATITUDE            FLOOR       BUILDINGID          SPACEID 
# 679              687                5                3              123 
# RELATIVEPOSITION           USERID          PHONEID        TIMESTAMP 
# 2                           18               16             9965 
#we have 18 user id and 16 phone id , seems like some devices were shared
# 679 & 687 unique longitute & LATITUDE
#floor and biuilding match to initial summary observation

#let's plot the logitude vs lattitude
plot(train_base$LONGITUDE,train_base$LATITUDE)
#3 cluster lets plot each building at a time
#building 0
plot(train_base[which(train_base$BUILDINGID==0),]$LONGITUDE,train_base[which(train_base$BUILDINGID==0),]$LATITUDE)
#building 1
plot(train_base[which(train_base$BUILDINGID==1),]$LONGITUDE,train_base[which(train_base$BUILDINGID==1),]$LATITUDE)
#building 2
plot(train_base[which(train_base$BUILDINGID==2),]$LONGITUDE,train_base[which(train_base$BUILDINGID==2),]$LATITUDE)

##################################################
#validation
unique_value_validation<-sapply(validation_base[,521:529],function(x) length(unique(x)))
unique_value_validation
# LONGITUDE       LATITUDE            FLOOR       BUILDINGID          SPACEID 
# 1042             1068                5                3                1 
# RELATIVEPOSITION           USERID          PHONEID        TIMESTAMP 
# 1                            1               11             1052 
#Larger set of longitude & latitude in valation set 1042 & 1068
#as seen in the summary SPACEID, RELATIVEPOSITION & USERID has zero as unique value

#let's plot the logitude vs lattitude
plot(validation_base$LONGITUDE,validation_base$LATITUDE)
#much denser plot representing higher variety of longitude & latitude
#3 building/cluster clearly visible here as well



#B) segmenetd univariate analysis####################################
#FLOOR
train_base$FLOOR<-factor(train_base$FLOOR)
summary(train_base$FLOOR)
plot(train_base$FLOOR)
#less number of figerprint for 4th floor
#rets of teh floor have evenly mathced data
validation_base$FLOOR<-factor(validation_base$FLOOR)
summary(validation_base$FLOOR)
plot(validation_base$FLOOR)
#very few for 4th and less for 0 and 3rd
#BUILDINGID
train_base$BUILDINGID<-factor(train_base$BUILDINGID)
summary(train_base$BUILDINGID)
plot(train_base$BUILDINGID)
#building 2 figureprints is comparitively higher in number
validation_base$BUILDINGID<-factor(validation_base$BUILDINGID)
summary(validation_base$BUILDINGID)
plot(validation_base$BUILDINGID)
#Buildin O is higher in number here
#RELATIVEPOSITION
train_base$RELATIVEPOSITION<-factor(train_base$RELATIVEPOSITION)
summary(train_base$RELATIVEPOSITION)
plot(train_base$RELATIVEPOSITION)
#Very disbalance data set 2n floor people are very high in number
validation_base$RELATIVEPOSITION<-factor(validation_base$RELATIVEPOSITION)
#all zero's
# summary(validation_base$RELATIVEPOSITION)
# plot(validation_base$RELATIVEPOSITION)



#C) Bivariant analysis###########################################
#install.packages("dplyr")
library(dplyr)

##building to floor
#Train
train_building_floor<-train_base%>%
  group_by(BUILDINGID,FLOOR) %>%
  summarise(count_records = n())
train_building_floor
plot(train_building_floor)
#4,4 & 5 fllor for  0,1 &2
# building 3 has more data, even floor wise their are outliners

#validation
validation_building_floor<-validation_base%>%
  group_by(BUILDINGID,FLOOR) %>%
  summarise(count_records = n())
validation_building_floor
plot(validation_building_floor)
#4,4 & 5 floor for  0,1 &2
#building 0 has more data .



# Data is in general imbalanced in the train and validation data set
#combining them and then randomly spliting will be recomended

#######################################################
#D) Wap value sanalysis
#Thisi can be considered under univariate analysis ans indual colum variace is bein validated


###range of maximum and mimnimum wasp values###
#train
unique(sapply(train_base[,1:520],function(x)  max(x[x!=100])))#-93  -29 
unique(sapply(train_base[,1:520],function(x) min(x)))#-104 - -83

#validation
unique(sapply(validation_base[,1:520],function(x) max(x[x!=100])))#-94 -38
unique(sapply(validation_base[,1:520],function(x) min(x)))#-102 - -74

#WAP value are expected to be co-planar which is represneted by their maximum and minmum spread
#max value of 100 represents the misisng singal

###Variance###
#train
variance_train<-sapply(train_base[,1:520],function(x) var(x))
variance_train
plot(variance_train)
#Variance is while spread, 
#there seems ot be column with zero variance too
length(variance_train[variance_train==0])#55 out of 520 columns, ard 10% of data
unique(train_base[,names(variance_train[variance_train==0])])
#constant value of 100 represent missing signal at these access point

#lets plot these varaibel based on their variance
var.plot_train<-quantile(variance_train,probs = seq(0, 1, 0.01))
plot(var.plot_train,xaxt='n',xlab="Percentile", ylab="Variance") 
axis(1, at = seq(5, 100, by = 5), las=2)
#around 40% variable have very less variance 
#the variance elbow is between 50 to 85 % followed by a steep rise

#validation
variance_validation<-sapply(validation_base[,1:520],function(x) var(x))
variance_validation
plot(variance_validation)
#Data spread is wider as compared to train set
length(variance_validation[variance_validation==0])#153 out of 520 columns, ard 30% of data
unique(train_base[,names(variance_train[variance_train==0])])
#as expected all 100, representing missing signal

#lets plot these varaibel based on their variance
var.plot_validation<-quantile(variance_validation,probs = seq(0, 1, 0.01))
plot(var.plot_validation,xaxt='n',xla20b="Percentile", ylab="Variance") 
axis(1, at = seq(5, 100, by = 5), las=2)
#Data spread is wider as compared to train set
#The wider spread of data is represented by a sharper elbow
#but most of variance is above 50% mark unlike train where it was ard 40% mark


###Co-variance between wap values###
#Using only variable with vaiace greater the 0 i.e. not singleton column of 100
cor_train<-cor(train_base[,names(variance_train[variance_train!=0])])
View(cor_train)
length(cor_train[cor_train>0.3 & cor_train !=1])/length(cor_train!=1)
#0.024 data has high variace, which is very low
#plot(cor_train)
max(cor_train[cor_train!=1])#0.9999832
min(cor_train)#-0.2030918
#low co-relation for train data set
col<- colorRampPalette(c("blue", "white", "red"))(20)
#heatmap(x = cor_train, col = col, symm = TRUE)


cor_validation<-cor(validation_base[,names(variance_validation[variance_validation!=0])])
View(cor_validation)
length(cor_validation[cor_validation>0.3 & cor_validation !=1])/length(cor_validation!=1)
#.035 slightly higher but still very low co-realtion
#has a wider spread as compared to train
#plot(cor_validation)
max(cor_validation[cor_validation!=1])#0.9999832
min(cor_validation)#-0.2077201

#In both data set co-relation is on a smaller side, as compared to MNIST dataset
#Naive bayes might be an an good alretaive based on this info


#install.packages('corrplot')
# library(corrplot)
#corrplot(cor_train,type = "upper", order = "hclust", 
#tl.col = "black", tl.srt = 45)


###Noarmality test for WAP values###
#install.packages('nortest')
  library(nortest)
#Using only variable with vaiace greater the 0 i.e. not singleton column of 100
#train
norm_train<-sapply(train_base[,names(variance_train[variance_train!=0])],function(x) ad.test(x)$p.value)
max(norm_train)#3.7e-24
min(norm_train)#3.7e-24
# all p-value are pretty low , 
#implying the null hypothesis of data being nomraly ditributed can be rejected for this
#since the data is not normally distributed PCA is out of question since it assumes the data to be  Gaussian distribution.
#Also finding orthogonal component for co-plannar data is futile

norm_validation<-sapply(validation_base[,names(variance_validation[variance_validation!=0])],function(x) ad.test(x)$p.value)
max(norm_validation)#3.7e-24
min(norm_validation)#3.7e-24
#similar results


#3.Combining and reshuffling data
#We will combine and reshuffle train and validation set for 
#better ditribution building,longitude & latitude wise 

###reshuffling data###
train_base$Differntiator<-1
validation_base$Differntiator<-2

nrow(train_base)#19300 95%
nrow(validation_base)#1111 5%

Complete_data_base<- rbind(train_base,validation_base)

library(caTools)
set.seed(100)

indices <- sample(1:nrow(Complete_data_base), nrow(Complete_data_base)*.8)

New_train <- Complete_data_base[indices,]
New_validation <- Complete_data_base[-indices,]

##################################################
new_variance_train<-sapply(New_train[,1:520],function(x) var(x))
new_variance_train
plot(new_variance_train)
#Variance is while spread, 
#there seems ot be column with zero variance too
length(new_variance_train[new_variance_train==0])#now only 4 colus are all zero
#remarkable impreovemnt rom 55 of train in teh previous set
unique(New_train[,names(new_variance_train[new_variance_train==0])])
#constant value of 100 represent missing signal at these access point

#lets plot these varaibel based on their variance
new_var.plot_train<-quantile(new_variance_train,probs = seq(0, 1, 0.01))
plot(new_var.plot_train,xaxt='n',xlab="Percentile", ylab="Variance") 
axis(1, at = seq(5, 100, by = 5), las=2)
#the overall variace graph still matches the original tria  data
#around 40% variable have very less variance 
#the variance elbow is between 50 to 85 % followed by a steep rise



###############################################################################
#Based on my previous experience with mnist dataset fordigit prediction which was similar in nature 
#we will use feature with high variace for predcition
#In case that doesn't work out we will user factor analysis for dimensionalty redcution
#I saw best result with SVM model with polynomial kernal with mnist data set 
#since the pixels will be apprearing in sets for a number
#Similarly the Wap values would be appearing in sets as highlighet by the co-realtion plot
###############################################################################

###4. Modelling with Random Forest######################
#We need to predict
# 1.logitute - Regression
# 2.Latitude - Regression
# 3.Floor - Classification
# 4.Building - Classification(incase we create common model for all three building)
# 5. Relative position? - Classification 

#We will be writing a custom funtion for cross validation 
#and are not using the train inbuilt function as we want to play around with the 
#variace values as well at the same time.
#also train funtion chooses teh final model by itself , 
#we may want to choose a simiplar model with marginally high error

#we will use 60% and above for prediction
#pertile<-c("60%","70%","80%","90%")
pertile<-c("75%")
520*.25# 130 variables
#sampsize<-c(5000,7500,10000)#1 is for refernce
nrow(New_train)/3# 5442.667
sampsize<-c(5400)
#ntree<-c(100,250,500,40,30,20,15)
ntree<-c(20)
#mtry<-c(50,100,150,40,35,30,25,20,15)
mtry<-c(20)
grid<-expand.grid("pertile"=pertile,"sampsize"=sampsize,"ntree"=ntree,"mtry"=mtry)
#grid[1,"pertile"]
#This is an expansive grid to check what are optimal values for ourdata set

####################################
#install.packages("randomForest")
library(randomForest)
library(caret)

RF_EVAL<- function(x,train_data,model_formula,inde_var,type,validation_data){
  percentile_val<-grid[x,"pertile"]
  sampsize_val<-grid[x,"sampsize"]
  ntree_val<-grid[x,"ntree"]
  mtry_val<-grid[x,"mtry"]
  print(percentile_val)
  var.value<-var.plot_train[names(new_var.plot_train)==percentile_val]
  var_wise_train_col<-names(new_variance_train[new_variance_train>var.value ])
  
  Model_RF <- randomForest(model_formula, 
                             data = train_data[,c(var_wise_train_col,inde_var)], 
                             proximity=FALSE,
                             sampsize=sampsize_val,
                             ntree=ntree_val, mtry=mtry_val, do.trace=TRUE)
  
  col_num<-which(names(validation_data)==inde_var)
  Eval_RF<- predict(Model_RF, validation_data[,-col_num])
  
  if(type=='Classification'){
    
    POLY.confMat<-confusionMatrix(Eval_RF,validation_data[,inde_var])
    print(POLY.confMat)
    c("percentile"=as.character(percentile_val),"sampsize"=sampsize_val,'mtry'=mtry_val,'ntree'=ntree_val,"Accuracy"=POLY.confMat$overall["Accuracy"])
  }else{
    error<-Eval_RF-validation_data[,inde_var]
    rsme<-sqrt(sum(error^2))
    r_sqaured_formula<-1-(sum(error^2)/sum((validation_data[,inde_var]-mean(validation_data[,inde_var]))^2) )
    
    R_squared<- cor(Eval_RF,validation_data[,inde_var])^2
    c("percentile"=as.character(percentile_val),"sampsize"=sampsize_val,'mtry'=mtry_val,'ntree'=ntree_val,
      "R_squared"=R_squared,"r_sqaured_formula"=r_sqaured_formula,"rsme"=rsme)
  }
}



#predcition across buildings

#floor
RF_floor<-sapply(1:nrow(grid),RF_EVAL,New_train,as.formula('FLOOR ~ .'),'FLOOR',
                   'Classification',New_validation)
RF_floor
# percentile        "75%"              
# sampsize          "5400"             
# mtry              "20"               
# ntree             "20"  
#Accuracy.Accuracy "0.961058045554739"
# Statistics by Class:
#   
#                        Class: 0 Class: 1 Class: 2 Class: 3 Class: 4
# Sensitivity            0.9851   0.9552   0.9348   0.9587  0.95918
# Specificity            0.9807   0.9880   0.9904   0.9862  0.99949


# a very simple model is giving us 95% accuracy,
#we can take it to a higher level but we will prefer keeping the model simple






#LONGITUDE
RF_LONGITUDE<-sapply(1:nrow(grid),RF_EVAL,New_train,as.formula('LONGITUDE ~ .'),'LONGITUDE',
                 'Regression',New_validation)
RF_LONGITUDE
# R_squared         "0.991664042436765"
# r_sqaured_formula "0.991609868799052"
# rsme              "725.636817023573" 

#LATITUDE
RF_LATITUDE<-sapply(1:nrow(grid),RF_EVAL,New_train,as.formula('LATITUDE ~ .'),'LATITUDE',
                     'Regression',New_validation)
RF_LATITUDE
# R_squared         "0.983504034198189"
# r_sqaured_formula "0.983364477137941"
# rsme              "557.020755316488" 

#BUILDINGID
RF_BUILDINGID<-sapply(1:nrow(grid),RF_EVAL,New_train,as.formula('BUILDINGID ~ .'),'BUILDINGID',
                 'Classification',New_validation)
RF_BUILDINGID
#Accuracy.Accuracy "0.997305902522655"
# Statistics by Class:
#   
#                        Class: 0 Class: 1 Class: 2
# Sensitivity            1.0000   1.0000   0.9942
# Specificity            1.0000   0.9963   1.0000


