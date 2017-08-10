
# ------------------------------------------------------------------------------------------
#
# Name: bullying_model.R
# Author: Chad Morgan
# Purpose: Exploratory analysis and model development for bullying involvement
#
#   This script will develop models for traditional bullying (victimization and perpetration)
#   and cyber bullying (victimization and perpetration) that will account for disability
#   type and status
#
#   Risk factors considered (in line with Kowalski, Giumetti, Schroeder and Lattaner, 2014):
#     - Disability status and type
#     - Demographics
#     - Frequency and pattern of internet use
#     - Social anxiety
#     - Personality
#     - Parental involvement
#     - Other bullying involvement (where appropriate depending on the perpetration/victimization type)
# ------------------------------------------------------------------------------------------

library(ggplot2)
library(ggthemes)
library(glmnet)
library(dummies)
library(plyr)
library(reshape2)

# read in prepared data
load('finaldat.RData')
colnames(finaldat)[3] = 'accommodations'
colnames(finaldat)[7] = 'perpetration'
colnames(finaldat)[49] = 'conscientiousness'
#finaldat$race = factor(ifelse(as.character(finaldat$race) %in% c("MiddleEastern","Other"),"White",as.character(finaldat$race)))


elnet_infer <- function(y.var,x.vars){
  
  # set up X
  naomit.data <- na.omit(finaldat[,c('subject.ID',y.var,x.vars)])
  x.frame <- model.frame(~.,data=naomit.data[,setdiff(x.vars,'race')])
  race.dummy <- dummy(naomit.data$race)[,-5] # use white as reference race
  x.frame <- data.matrix(cbind(x.frame,race.dummy))
  x.frame = scale(x.frame)
  
  # repeated CV to choose lambda
  lambda = exp(seq(-8,-.01,.01))
    #glmnet(x=data.matrix(x.frame),y=naomit.data[,y.var],lambda.min.ratio=.00001,nlambda=200)$lambda
  repeat.cv.results <- NULL
  for (b in 1:50){
    result = tryCatch({
      elnet.cv <- cv.glmnet(x=data.matrix(x.frame)
                            ,y=naomit.data[,y.var]
                            ,nfolds=10
                            ,alpha=.5
                            #,standardize=FALSE
                            ,lambda=lambda)
      repeat.cv.results <- rbind(repeat.cv.results,data.frame(rep=b,lambda=elnet.cv$lambda,mse=elnet.cv$cvm))
    })
  }
  
  cv.results <- ddply(repeat.cv.results,'lambda',function(data){data.frame(mse=mean(data$mse))})
  best.lambda = cv.results[cv.results$mse==min(cv.results$mse),'lambda']
  cv.rsq = 1-(min(cv.results$mse)[1]/var(naomit.data[,y.var]))
  
  # fit model with best lambda to get coefficients
  elnet.model <- glmnet(x=data.matrix(x.frame),y=naomit.data[,y.var],alpha=.5,lambda=lambda)#,standardize=FALSE)
  coef.path = cbind(data.frame(lambda = elnet.model$lambda),t(data.matrix(coef(elnet.model))))
  
  # bootstrap to get 95% CI for coefficients
  el.dat = cbind(naomit.data[,y.var],x.frame)
  boot.coef.sample = data.matrix(matrix(nrow=1000,ncol=dim(x.frame)[2]+1))
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


ggplot_model = function(model){
  
  cv_plot = ggplot(data.frame(model[1]),aes(log(lambda),mse))+geom_line()+
    geom_vline(xintercept=log(model[2][[1]]),lty=2)+
    ylab("10-fold CV MSE")+
    theme_bw()+theme(legend.position="none")
  
  est.frame=data.frame(model[5])
  est.frame = subset(est.frame,estimate!=0 & feature!='(Intercept)')
  est.frame$l=log(model[2][[1]])
  path= subset(melt(model[4],id.vars = 'lambda'),variable!='(Intercept)')
  
  path_plot = ggplot(data=path,aes(log(lambda),value))+geom_line(aes(group=variable))+
    geom_vline(xintercept=log(model[2][[1]]),lty=2)+
    geom_text(data=est.frame,aes(l,estimate,color=feature,label=feature,hjust=-.1,size=8))+
    geom_point(data=est.frame,aes(l,estimate,color=feature))+
    ylab("scaled estimate")+theme_bw()+theme(legend.position="none")
  
  boot.sample = suppressMessages(melt(data.frame(model[6])[,as.character(est.frame$feature)]))
  boot_plot= ggplot(boot.sample,aes(value,fill=variable))+geom_histogram(binwidth=.008)+
    facet_wrap(~variable)+geom_vline(xintercept=0,linetype='dashed',size=.25)+
    xlab("scaled estimate")+ylab('frequency')+theme_bw()+theme(legend.position="none")
  #scale_x_continuous(breaks=c(-.3,-.2,-.1,0,.1,.2,.3))
  
  return(list(cv_plot,path_plot,boot_plot))
}

# ------------------------------------------------------------------------------------------
# Cyber victimization modeling

cybervic.x = c('victim','perpetration','cyberwitness','cyberheard'
           ,'havedisability','accommodations','takemeds','noticeable'
           ,'ADHD','AnxietyDisorders','LearningDisorders','PhysicalDisability','OtherPsychDisorder'
           ,'age','sex','year','race'
           ,'parentsknow','safe','parentscomm','freqintuse'
           ,'texting','facebook','myspace','twitter','gaming','google','youtube','itunes','chatrooms','im','email','tumblr','instagram'
           ,'socialanxiety','agreeableness','extraversion','conscientiousness','neurotic','openness')

cybervic.model = elnet_infer('cybervic',cybervic.x)
cybervic.plots = ggplot_model(cybervic.model)

ggsave(filename = 'fig1_cybervic_lambda.jpeg'
      ,plot=cybervic.plots[1][[1]],dpi=2500,width=8,height=6)

ggsave(filename = 'fig3_cybervic_bootstrap.jpeg'
       ,plot=cybervic.plots[3][[1]],dpi=2500,width=8,height=6)

# ------------------------------------------------------------------------------------------
# Traditional victimization modeling


tvic.x = c('havedisability','accommodations','takemeds','noticeable'
               ,'ADHD','AnxietyDisorders','LearningDisorders','PhysicalDisability','OtherPsychDisorder'
               ,'age','sex','year','race'
               ,'parentsknow','safe','parentscomm','freqintuse'
               ,'socialanxiety','agreeableness','extraversion','conscientiousness','neurotic','openness')

tvic.model = elnet_infer('victim',tvic.x)
tvic.plots = ggplot_model(tvic.model)


# ------------------------------------------------------------------------------------------
# Cyber perpetration

cyberperp.x =c('victim','perpetration','cyberwitness','cyberheard','cybervic'
               ,'havedisability','accommodations','takemeds','noticeable'
               ,'ADHD','AnxietyDisorders','LearningDisorders','PhysicalDisability','OtherPsychDisorder'
               ,'age','sex','year','race'
               ,'parentsknow','safe','parentscomm','freqintuse'
               ,'texting','facebook','myspace','twitter','gaming','google','youtube','itunes','chatrooms','im','email','tumblr','instagram'
               ,'socialanxiety','agreeableness','extraversion','conscientiousness','neurotic','openness')

cyberper.model = elnet_infer('cyberperp',cyberperp.x)
cyberper.plots = ggplot_model(cyberper.model)


ggsave(filename = 'fig2_cyberperp_lambda.jpeg'
       ,plot=cyberper.plots[1][[1]],dpi=2500,width=8,height=6)

ggsave(filename = 'fig4_cyberperp_bootstrap.jpeg'
       ,plot=cyberper.plots[3][[1]],dpi=2500,width=8,height=9)


# ------------------------------------------------------------------------------------------
# Traditional perpetration

perp.x =c('victim'
               ,'havedisability','accommodations','takemeds','noticeable'
               ,'ADHD','AnxietyDisorders','LearningDisorders','PhysicalDisability','OtherPsychDisorder'
               ,'age','sex','year','race'
               ,'parentsknow','safe','parentscomm','freqintuse'
               ,'socialanxiety','agreeableness','extraversion','conscientiousness','neurotic','openness')

perp.model = elnet_infer('perp',perp.x)
perp.plots = ggplot_model(perp.model)
