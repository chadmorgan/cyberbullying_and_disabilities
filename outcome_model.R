# ----------------------------------------------------------------------
# name:     outcome_model.R
# author:   Chad Morgan
# project:  cyber bullying and disabilities
# purpose:  model effect of bullying and disabilities on various outcomes
# ----------------------------------------------------------------------

library(glmnet)
library(plyr)

# read in prepared dataset
load("finaldat.RData")


# list of outcome variables
outcome_vars = c('grades','symptom.score','depression','selfesteem','ostracism')

# design matrix for models
x.var = c('cybervic','cyberperp','victim','perp','cyberwitness','cyberheard' 
          ,'havedisability','accomodations','noticeable'                      
          ,'ADHD','AnxietyDisorders','LearningDisorders','PhysicalDisability','OtherPsychDisorder'
          ,'age','sex','year','race'
          ,'parentsknow','safe','parentscomm'
          ,'freqintuse','texting','facebook','myspace','twitter','gaming','google','youtube'
          ,'itunes','chatrooms','im','email','tumblr','instagram'
          ,'socialanxiety','agreeableness','extraversion','conscien','neurotic','openness')



elnet_infer <- function(y.var,x.vars){

  train.y = na.omit(finaldat[,c(y.var,'cybervic','cyberheard','openness')])[,y.var]
  train.x <- model.matrix(~ 0
                          # bullying features
                          + cybervic + cyberperp + victim + perp + cyberwitness + cyberheard 
                          # disability
                          + havedisability + accomodations + noticeable
                          # moderating terms
                          + noticeable*cybervic + noticeable*cyberperp + noticeable*victim + noticeable*perp                         
                          # disability type
                          + ADHD + AnxietyDisorders + LearningDisorders + PhysicalDisability +OtherPsychDisorder
                          # demographics 
                          + age + sex + year + race
                          # parental involvement
                          + parentsknow + safe + parentscomm
                          # internet use
                          + freqintuse + texting + facebook + myspace + twitter + gaming + google + youtube
                          + itunes + chatrooms + im + email + tumblr + instagram
                          # social anxiety and personality factors
                          + socialanxiety + extraversion + conscien + neurotic + openness
                          ,data=na.omit(finaldat[,c(y.var,x.var)]))
  train.x = scale(train.x)
  
  lambda = exp(seq(-8,-.01,.01))
  repeat.cv.results <- NULL
  for (b in 1:50){
    result = tryCatch({
      elnet.cv <- cv.glmnet(x=data.matrix(train.x)
                            ,y=train.y
                            ,nfolds=10
                            ,alpha=.5
                            ,lambda=lambda)
      repeat.cv.results <- rbind(repeat.cv.results,data.frame(rep=b,lambda=elnet.cv$lambda,mse=elnet.cv$cvm))
    })
  }
  
  cv.results <- ddply(repeat.cv.results,'lambda',function(data){data.frame(mse=mean(data$mse))})
  best.lambda = cv.results[cv.results$mse==min(cv.results$mse),'lambda']
  cv.rsq = 1-(min(cv.results$mse)[1]/var(train.y))
  
  # fit model with best lambda to get coefficients
  elnet.model <- glmnet(x=data.matrix(train.x),y=train.y,alpha=.5,lambda=lambda)
  coef.path = cbind(data.frame(lambda = elnet.model$lambda),t(data.matrix(coef(elnet.model))))
  
  # bootstrap to get 95% CI for coefficients
  el.dat = cbind(train.y,train.x)
  boot.coef.sample = data.matrix(matrix(nrow=1000,ncol=dim(train.x)[2]+1))
  for(b in 1:1000){
    boot.dat = el.dat[sample(nrow(el.dat),replace=TRUE),]
    boot.model <- glmnet(x=data.matrix(boot.dat[,2:(dim(boot.dat)[2])]),y=boot.dat[,1],alpha=.5,lambda=lambda)#,standardize=FALSE)
    boot.coef = coef(boot.model,s=best.lambda)
    coef.names = dimnames(boot.coef)[[1]]
    boot.coef = as.numeric(data.matrix(boot.coef))
    boot.coef.sample[b,] <- boot.coef
  }
  colnames(boot.coef.sample) = dimnames(coef(elnet.model,s=best.lambda))[[1]]
  coefficients = data.frame(feature = dimnames(coef(elnet.model,s=best.lambda))[[1]]
                            ,estimate = as.numeric(t(unlist(as.matrix(coef(elnet.model,s=best.lambda)))))
                            ,lower.ci = apply(boot.coef.sample,MARGIN=2,FUN=quantile,probs=.025)
                            ,upper.ci = apply(boot.coef.sample,MARGIN=2,FUN=quantile,probs=.975))
  return(list(cv.results,best.lambda,elnet.model,coef.path,coefficients,boot.coef.sample,cv.rsq))
}


grades_model = elnet_infer('grades',x.var)
selfest_model = elnet_infer('selfesteem',x.var)
symptom_model = elnet_infer('symptom.score',x.var) 
depression_model = elnet_infer('depression',x.var)
ostracism_model = elnet_infer('ostracism',x.var)



# visualization of interaction effects

depression_coefs = depression_model[5][[1]][,c('feature','estimate')]
depression_coefs = subset(depression_coefs,estimate != 0)
feature_sd = apply(finaldat[,setdiff(as.character(depression_coefs$feature),'(Intercept)')],2,sd,na.rm=TRUE)
depression_sim = expand.grid(cyberperp = seq(1,4),noticeable=seq(1:5),cybervic=1, PhysicalDisability=0, OtherPsychDisorder=0
                             ,raceHispanic=0,parentsknow=3,freqintuse=4,texting=5,myspace=1,chatrooms=1,socialanxiety=2.8
                             ,extraversion=3.375,conscien=32,neurotic=3)
