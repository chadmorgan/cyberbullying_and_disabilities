
# ------------------------------------------------------------------------------------------
#
# Name: CBD_data_prep.R
# Author: Chad Morgan
# Purpose: Data cleaning and prep for cyberbullying/disabilities analysis
#
# ------------------------------------------------------------------------------------------

library(gdata)
library(foreign)
library(psych)
library(gbm)
library(ggplot2)
library(plyr)

# ---- Data Import ---- #

# read data from excel spreadsheet
rawdata <- read.xls('collegedisabilitydatacombonew110514.xls'
                    ,stringsAsFactors=FALSE,na.strings=c('9','99'))
rawdata <- rawdata[,2:228]
rawdata$subject.ID <- seq(1:nrow(rawdata))                        # number subjects with an ID

# read SPSS data too as a reference to level names
refdat <- read.spss('collegedisabilitydatacombonew110514.sav'
                    ,to.data.frame=TRUE)

# ---- Quick data format changes ---- #
rawdata$race <- factor(rawdata$race,labels=c('Black','MiddleEastern','Hispanic','Asian','White','Other'))
rawdata$race = factor(ifelse(as.character(rawdata$race) %in% c("MiddleEastern","Other"),"White",as.character(rawdata$race)))
rawdata$sex <- ifelse(rawdata$sex==1,0,1)                         # recode sex to 0=male, 1=female
rawdata$freqintuse <- (rawdata$freqintuse-1)*2                    # recode internet use to hours scale
rawdata$havedisability <- ifelse(rawdata$havedisability==2,1,0)   # recode havedisability to 0=no, 1=yes
rawdata$accomodations <- ifelse(rawdata$accomodations==1,1,0)     # recode accomodations to 0=no, 1=yes
rawdata$takemeds <- ifelse(rawdata$takemeds==1,1,0)               # recode takemeds to 0=no, 1=yes


# ------------------------------------------------------------------------------------------
# Check for missing data

# count number of missing variables in vector
count_missing <- function(z){
  return(length(which(is.na(z))))
}
rawdata$subject_missing <- apply(rawdata,MARGIN=1,FUN=count_missing)
rawdata$victim_missing  <- ifelse(is.na(rawdata$victim),1,0)
rawdata$perp_missing  <- ifelse(is.na(rawdata$perp),1,0)
rawdata$cybervic_missing <- ifelse(is.na(rawdata$cybervic),1,0)
rawdata$cyberperp_missing <- ifelse(is.na(rawdata$cyberperp),1,0)

# examine records with large n's of missing data or missing key variables
rawdata[rawdata$subject_missing>100
        | rawdata$victim_missing==1
        | rawdata$perp_missing==1
        | rawdata$cybervic_missing==1
        | rawdata$cyberperp_missing==1
          ,c('subject_missing','victim_missing','perp_missing','cybervic_missing','cyberperp_missing')]

    # row 19: appears to have quit survey after the initial cyber victim questions (drop)
    # row 35: missing cyberperp but otherwise very complete
    # row 36: quit after symptoms
    # row 61: quit after cybervic (drop)
    # row 63: quit after symptoms
    # row 73: quit after symptoms
    # row 196: missing cyberperp but otherwise very complete

# some subjects are missing too much data for imputation. We will need to be careful about when to impute...



# ------------------------------------------------------------------------------------------
# Missing data replacement with machine learning regression algorithm


# ---- define function to replace missing values with regression-based approach
missing_values_boosted_regression <- function(var.block){
  # narrow data to the block of variables
  blockdat <- rawdata[,c(var.block)]
  
  # detect and remove subjects missing all variables in the block
  missing.block <- ifelse(apply(blockdat,MARGIN=1,FUN=count_missing)==length(blockdat),1,0)
  blockdat <- blockdat[missing.block==0,]
  block.IDs <- rawdata[missing.block==0,'subject.ID']
  
  # detect remaining variables in the block that need replacement
  missing.vars <- apply(blockdat,MARGIN=2,FUN=count_missing)
  
  # loop over variables that need replacement and fit model based on remaining variables
  for (i in names(missing.vars[missing.vars>0])){
    # drop missing subjects from focus variable
    limit.dat <- blockdat[is.na(blockdat[,i])==FALSE,]
    # fit nonparametric regression algorithm (gradient boosting with trees)
    miss.gbm <- gbm(limit.dat[,i]~.
                    ,data=limit.dat[,setdiff(colnames(limit.dat),i)]
                    ,n.trees=400
                    ,cv.folds=5
                    ,shrinkage = 0.01
                    ,interaction.depth=5
                    ,distribution='gaussian')
    predict.i <- data.frame(predict(miss.gbm,blockdat))
    # if CV model error is worse than the mean, just use median replacement
    rsq <- 1-(min(miss.gbm$cv.error)/var(limit.dat[,i]))
    if(rsq<=0){
      predict.i<-rep(median(limit.dat[,i]),nrow(blockdat))
    }
    # hold on to predictions but don't impute yet (so we don't learn on imputed values for the next variable)
    if(i==names(missing.vars[missing.vars>0])[1]){
      replace.dat <- predict.i
    }else{
      replace.dat <- cbind(replace.dat,predict.i)
    }
  
    cat(i,rsq,'\n')
  }
  colnames(replace.dat) <- names(missing.vars[missing.vars>0])
  
  # loop again and use GBM predictions to replace missings
  for (i in names(missing.vars[missing.vars>0])){
    blockdat[is.na(blockdat[,i]),i] <- replace.dat[is.na(blockdat[,i]),i]
  }
  names <- c('subject.ID',colnames(blockdat))
  blockdat <- cbind(block.IDs,blockdat)
  colnames(blockdat) <- names
  return(blockdat)
}


# ---- get data that is complete or will not need the regression-based imputation method
cleandata <- rawdata[,c('subject.ID','havedisability','accomodations','takemeds','noticeable'
                        ,"victim","told", "perp","cybervic", "bulliedim", "bulliedchatroom", "bulliedsns", 
                        "bulliedwebsite", "bulliedemail", "bulliedtxt", "bulliedgaming", "bulliedanohterway", 
                        "cyberperp", "cyberwitness", "cyberheard")]

# logical missing value replacement for non-disabled subjects
cleandata[cleandata$havedisability==0 & is.na(cleandata$accomodations),'accomodations'] <- 0
cleandata[cleandata$havedisability==0 & is.na(cleandata$takemeds),'takemeds'] <- 0
cleandata[cleandata$havedisability==0 & is.na(cleandata$noticeable),'noticeable'] <- 1

# if bullying activity is missing, impute a 1 (the median for all of these variables)
for (i in 6:19){
  cleandata[is.na(cleandata[,i]),i] = 1
}

# add grades 
cleandata$grades <- rawdata$grades


# ---- use imputation function above to do missing value replacement for blocks of related quantitative variables
vars <- colnames(rawdata)
demos_internet <- c("age", "sex", "year", "race", "freqintuse", 
                    "parentsknow", "safe", "parentscomm", "texting", "facebook", 
                    "myspace", "formspring", "twitter", "gaming", "google", "youtube", 
                    "itunes", "chatrooms", "im", "email", "tumblr", "instagram")
impute.demos_internet <- missing_values_boosted_regression(demos_internet)
cleandata <- merge(cleandata,impute.demos_internet,by='subject.ID',all.x=TRUE)


symptoms <- c("symptom1", "symptom2", "symptom3", "symptom4", "symptom5", 
              "symptom6", "symptom7", "symptom8", "symptom9", "symptom10")
impute.symptoms <- missing_values_boosted_regression(symptoms)
cleandata <- merge(cleandata,impute.symptoms,by='subject.ID',all.x=TRUE)

self_esteem <- c("se1", "se2", "se3", "se4", "se5", "se6", "se7", "se8", "se9", "se10")
impute.se <- missing_values_boosted_regression(self_esteem)
cleandata <- merge(cleandata,impute.se,by='subject.ID',all.x=TRUE)

ostracism <- c("ost1", "ost2", "ost3", "ost4", "ost5", "ost6", 
                "ost7", "ost8", "ost9", "ost10", "ost11", "ost12", "ost13", "ost14", "ost15")
impute.ostr <- missing_values_boosted_regression(ostracism)
cleandata <- merge(cleandata,impute.ostr,by='subject.ID',all.x=TRUE)

socanxiety <- c("sa1", "sa2", "sa3", "sa4", "sa5", "sa6", "sa7", "sa8", 
                "sa9", "sa10", "sa11", "sa12", "sa13", "sa14", "sa15")
impute.socanx <- missing_values_boosted_regression(socanxiety)
cleandata <- merge(cleandata,impute.socanx,by='subject.ID',all.x=TRUE)

depress <- c("depress1", "depress2", "depress3", "depress4", "depress5", "depress6", "depress7", 
              "depress8", "depress9", "depress10", "depress11", "depress12", 
              "depress13", "depress14", "depress15", "depress16", "depress17", 
              "depress18", "depress19", "depress20")
impute.depress <- missing_values_boosted_regression(depress)
cleandata <- merge(cleandata,impute.depress,by='subject.ID',all.x=TRUE)

bigfive <- c("bf1", "bf2", "bf3", "bf4", 
              "bf5", "bf6", "bf7", "bf8", "bf9", "bf10", "bf11", "bf12", "bf13", 
              "bf14", "bf15", "bf16", "bf17", "bf18", "bf19", "bf20", "bf21", 
              "bf22", "bf23", "bf24", "bf25", "bf26", "bf27", "bf28", "bf29", 
              "bf30", "bf31", "bf32", "bf33", "bf34", "bf35", "bf36", "bf37", 
              "bf38", "bf39", "bf40", "bf41", "bf42", "bf43", "bf44")
impute.bigfive <- missing_values_boosted_regression(bigfive)
cleandata <- merge(cleandata,impute.bigfive,by='subject.ID',all.x=TRUE)

# clean up workspace
rm(demos_internet)
rm(impute.demos_internet)
rm(symptoms)
rm(impute.symptoms)
rm(impute.se)
rm(impute.ostr)
rm(impute.socanx)
rm(impute.depress)
rm(impute.bigfive)


# ------------------------------------------------------------------------------------------
# Scale reliability analysis and compute scale composites



# ---- Self esteem: reliability = .92 ---- #
alpha(cleandata[,c(self_esteem)])

# recode reverse-worded items using: (z*-1)+max(z+1)
cleandata$se1 <- (cleandata$se1*-1)+5
cleandata$se3 <- (cleandata$se3*-1)+5
cleandata$se4 <- (cleandata$se4*-1)+5
cleandata$se7 <- (cleandata$se7*-1)+5
cleandata$se10 <- (cleandata$se10*-1)+5

# compute composite for self esteem
cleandata$selfesteem <- (cleandata$se1+cleandata$se2+cleandata$se3+cleandata$se4+cleandata$se5+cleandata$se6
                         +cleandata$se7+cleandata$se8+cleandata$se9+cleandata$se10)/10

# ---- Ostracism: reliability = .91 ---- #
alpha(cleandata[,c(ostracism)])

# recode reverse-worded items
cleandata$ost2 <- (cleandata$ost2*-1)+6
cleandata$ost4 <- (cleandata$ost4*-1)+6
cleandata$ost7 <- (cleandata$ost7*-1)+6
cleandata$ost9 <- (cleandata$ost9*-1)+6
cleandata$ost12 <- (cleandata$ost12*-1)+6
cleandata$ost14 <- (cleandata$ost14*-1)+6

cleandata$ostracism <- (cleandata$ost1+cleandata$ost2+cleandata$ost3+cleandata$ost4+cleandata$ost5+cleandata$ost6
                        +cleandata$ost7+cleandata$ost8+cleandata$ost9+cleandata$ost10+cleandata$ost11+cleandata$ost12
                        +cleandata$ost13+cleandata$ost14+cleandata$ost15)/15

# ---- Social anxiety: alpha = .89 ---- #
alpha(cleandata[,c(socanxiety)])

# recode reverse-worded items
cleandata$sa3 <- (cleandata$sa3*-1)+6
cleandata$sa6 <- (cleandata$sa6*-1)+6
cleandata$sa10 <- (cleandata$sa10*-1)+6
cleandata$sa15 <- (cleandata$sa15*-1)+6

cleandata$socialanxiety <- (cleandata$sa1+cleandata$sa2+cleandata$sa3+cleandata$sa4+cleandata$sa5+cleandata$sa6+cleandata$sa7
                            +cleandata$sa8+cleandata$sa9+cleandata$sa10+cleandata$sa11+cleandata$sa12+cleandata$sa13+
                              cleandata$sa14+cleandata$sa15)/15

# ---- Depression: alpha = .9 ---- #
alpha(cleandata[,c(depress)])

cleandata$depress4 <- (cleandata$depress4*-1)+5
cleandata$depress8 <- (cleandata$depress8*-1)+5
cleandata$depress12 <- (cleandata$depress12*-1)+5
cleandata$depress16 <- (cleandata$depress16*-1)+5

cleandata$depression <- (cleandata$depress1+cleandata$depress2+cleandata$depress3+cleandata$depress4+cleandata$depress5
                         +cleandata$depress6+cleandata$depress7+cleandata$depress8+cleandata$depress9+cleandata$depress10
                         +cleandata$depress11+cleandata$depress12+cleandata$depress13+cleandata$depress14
                         +cleandata$depress15+cleandata$depress16+cleandata$depress17+cleandata$depress18
                         +cleandata$depress19+cleandata$depress20)/20

# ---- Extraversion: alpha = .86 ---- #
alpha(cleandata[,c('bf1','bf6','bf11','bf16','bf21','bf26','bf31','bf36')])

# recode reverse worded items
cleandata$bf6 <- (cleandata$bf6*-1)+6
cleandata$bf21 <- (cleandata$bf21*-1)+6
cleandata$bf31 <- (cleandata$bf31*-1)+6

cleandata$extraversion <- (cleandata$bf1+cleandata$bf6+cleandata$bf11+cleandata$bf16+cleandata$bf21+cleandata$bf26
                           +cleandata$bf31+cleandata$bf36)/8

# ---- Agreeableness: alpha = .79 ---- #
alpha(cleandata[,c('bf2','bf7','bf12','bf17','bf22','bf27','bf32','bf37','bf42')])

cleandata$bf2 <- (cleandata$bf2*-1)+6
cleandata$bf12 <- (cleandata$bf12*-1)+6
cleandata$bf27 <- (cleandata$bf27*-1)+6
cleandata$bf37 <- (cleandata$bf37*-1)+6

cleandata$agreeableness <- (cleandata$bf2+cleandata$bf7+cleandata$bf12+cleandata$bf17+cleandata$bf22+cleandata$bf27
                            +cleandata$bf32+cleandata$bf37+cleandata$bf42)/9

# ---- Conscientiousness: alpha = .76 ---- #
alpha(cleandata[,c('bf3','bf8','bf13','bf18','bf23','bf28','bf33','bf38','bf43')])

cleandata$bf8 <- (cleandata$bf8*-1)+6
cleandata$bf18 <- (cleandata$bf18*-1)+6
cleandata$bf23 <- (cleandata$bf23*-1)+6
cleandata$bf43 <- (cleandata$bf43*-1)+6

cleandata$conscien <- (cleandata$bf3+cleandata$bf8+cleandata$bf13+cleandata$bf18+cleandata$bf23+cleandata$bf28
                       +cleandata$bf33+cleandata$bf38+cleandata$bf43)

# ----- Neuroticism: alpha = .84 ---- #
alpha(cleandata[,c('bf4','bf9','bf14','bf19','bf24','bf29','bf34','bf39')])

cleandata$bf9 <- (cleandata$bf9*-1)+6
cleandata$bf24 <- (cleandata$bf24*-1)+6
cleandata$bf34 <- (cleandata$bf34*-1)+6

cleandata$neurotic <- (cleandata$bf4+cleandata$bf9+cleandata$bf14+cleandata$bf19+cleandata$bf24+cleandata$bf29
                       +cleandata$bf34+cleandata$bf39)/8

# ---- Openness to experience: alpha = .78 ---- #
alpha(cleandata[,c('bf5','bf10','bf15','bf20','bf25','bf30','bf35','bf40','bf41','bf44')])

cleandata$bf35 <- (cleandata$bf35*-1)+6
cleandata$bf41 <- (cleandata$bf41*-1)+6

cleandata$openness <- (cleandata$bf5+cleandata$bf10+cleandata$bf15+cleandata$bf20+cleandata$bf25+cleandata$bf30
                       +cleandata$bf35+cleandata$bf40+cleandata$bf41+cleandata$bf44)/10

# ---- Physical symptoms: get 1st principal component
symptom.dat <- na.omit(cleandata[,c('subject.ID','symptom1','symptom2','symptom3','symptom4','symptom5','symptom6','symptom7',
                                    'symptom8','symptom9','symptom10')])
symptom.pca <- princomp(symptom.dat[,-1])
symptom.dat$symptom.score <- -1*symptom.pca$scores[,1]

cleandata <- merge(cleandata,symptom.dat[,c('subject.ID','symptom.score')],by='subject.ID',all.x=TRUE)

# reduce data by dropping scale items
finaldat <- cleandata[,c("subject.ID", "havedisability", "accomodations", "takemeds", 
                          "noticeable", "victim", "perp", "cybervic", "bulliedim", "bulliedchatroom", 
                          "bulliedsns", "bulliedwebsite", "bulliedemail", "bulliedtxt", 
                          "bulliedgaming", "bulliedanohterway", "cyberperp", "cyberwitness", 
                          "cyberheard", "grades", "age", "sex", "year", "race", "freqintuse", 
                          "parentsknow", "safe", "parentscomm", "texting", "facebook", 
                          "myspace", "formspring", "twitter", "gaming", "google", "youtube", 
                          "itunes", "chatrooms", "im", "email", "tumblr", "instagram", 
                          "selfesteem", "ostracism", "socialanxiety", "depression", 'agreeableness',"extraversion", 
                          "conscien", "neurotic", "openness", "symptom.score")]





# ----------------------------------------------------------------------------------------------------------------
# Code common disabilities from text descriptions

disability <- rawdata[,c('subject.ID','disability')]
disability$disability <- tolower(disability$disability)   # convert to lowercase

# ---- Use GREP to search string for disability types
disability$ADHD <- ifelse(grepl(pattern='add',x=disability[,2]),1
                          ,ifelse(grepl(pattern='adhd',x=disability[,2]),1
                          ,ifelse(grepl(pattern='a.d.d.',x=disability[,2]),1
                          ,ifelse(grepl(pattern='attention deficit',x=disability[,2]),1,0))))
disability[1,3] <- 0
disability$AnxietyDisorders <- ifelse(grepl(pattern='anxiety',x=disability[,2]),1,
                             ifelse(grepl(pattern='ocd',x=disability[,2]),1,
                             ifelse(grepl(pattern='obsessive compulsive',x=disability[,2]),1,
                             ifelse(grepl(pattern='panic',x=disability[,2]),1,0))))
disability$LearningDisorders <-  ifelse(grepl(pattern='learning',x=disability[,2]),1,
                                    ifelse(grepl(pattern='lurning',x=disability[,2]),1,
                                    ifelse(grepl(pattern='math',x=disability[,2]),1,
                                    ifelse(grepl(pattern='spelling',x=disability[,2]),1,
                                    ifelse(grepl(pattern='processing',x=disability[,2]),1,
                                    ifelse(grepl(pattern='dyslexia',x=disability[,2]),1,
                                    ifelse(grepl(pattern='dislexia',x=disability[,2]),1,
                                    ifelse(grepl(pattern='reading',x=disability[,2]),1,0))))))))
disability$PhysicalDisability <- ifelse(grepl(pattern='physical',x=disability[,2]),1,
                              ifelse(grepl(pattern='kidney',x=disability[,2]),1,
                              ifelse(grepl(pattern="crohn's",x=disability[,2]),1,
                              ifelse(grepl(pattern='allerg',x=disability[,2]),1,
                              ifelse(grepl(pattern="thoracic",x=disability[,2]),1,
                              ifelse(grepl(pattern="cystic fibrosis",x=disability[,2]),1,
                              ifelse(grepl(pattern="immune deficiency",x=disability[,2]),1,
                              ifelse(grepl(pattern="cerebral palsy",x=disability[,2]),1,
                              ifelse(grepl(pattern="hearing",x=disability[,2]),1,
                              ifelse(grepl(pattern="muscular dystrophy",x=disability[,2]),1,
                              ifelse(grepl(pattern="pain",x=disability[,2]),1,
                              ifelse(grepl(pattern="diabetes",x=disability[,2]),1,
                              ifelse(grepl(pattern="ibs",x=disability[,2]),1,
                              ifelse(grepl(pattern='epilepsy',x=disability[,2]),1,
                              ifelse(grepl(pattern='sickle sell anemia',x=disability[,2]),1,
                              ifelse(grepl(pattern="metabolism",x=disability[,2]),1,0))))))))))))))))
disability$OtherPsychDisorder <- ifelse(grepl(pattern='asperger',x=disability[,2]),1,
                                    ifelse(grepl(pattern='autism',x=disability[,2]),1,
                                    ifelse(grepl(pattern='stress',x=disability[,2]),1,
                                    ifelse(grepl(pattern='ptsd',x=disability[,2]),1,
                                    ifelse(grepl(pattern='sleep',x=disability[,2]),1,
                                    ifelse(grepl(pattern='narcolepsy',x=disability[,2]),1,
                                    ifelse(grepl(pattern='depress',x=disability[,2]),1,
                                    ifelse(grepl(pattern='bipolar',x=disability[,2]),1,0))))))))
apply(disability[,3:7],MARGIN=2,FUN=sum)
  
# merge disability categories to finaldat
finaldat <- merge(finaldat,disability[,-2],by='subject.ID')

save.image('data_cleaning.RData')
save(finaldat,file='finaldat.RData')
write.csv(finaldat,'finaldat.csv',row.names=FALSE)
