
library(glmnet)
load('finaldat.RData')

# design matrix for models
x.var = c('cybervic','cyberperp','victim','perp','cyberwitness','cyberheard' 
          ,'havedisability','accomodations','noticeable'                      
          ,'ADHD','AnxietyDisorders','LearningDisorders','PhysicalDisability','OtherPsychDisorder'
          ,'age','sex','year','race'
          ,'parentsknow','safe','parentscomm'
          ,'freqintuse','texting','facebook','myspace','twitter','gaming','google','youtube'
          ,'itunes','chatrooms','im','email','tumblr','instagram'
          ,'socialanxiety','agreeableness','extraversion','conscien','neurotic','openness')

y.var = 'symptom.score'
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
coefs1 = data.matrix(coef(elnet.model,s=best.lambda))
coefs = data.frame(coefs1[coefs1!=0])
rownames(coefs) = dimnames(coefs1)[[1]][coefs1!=0]

# ----- sim data ----- #

sim_dat = expand.grid(int=1,cyberperp=0,AnxietyDisorders=0,OtherPsychDisorder=0,age=19
                     ,sex=0,raceBlack=0,raceHispanic=0,raceWhite=1,parentsknow=3,parentscomm=3,freqintuse=4
                     ,texting=5,facebook=5,myspace=1,gaming=1,chatrooms=1,extraversion=3.375
                     ,conscien=32,neurotic=3,noticeable=c(1,3,5),cybervic=c(1,2,3,4,5))
sim_dat$cybervic_noticeable = sim_dat$noticeable*sim_dat$cybervic
sim_dat$pred.symptom = as.numeric(t(t(data.matrix(coefs)) %*% t(data.matrix(sim_dat[,c(1:20,23)]))))

# -- plot predictions -- #

library(ggplot2)

sim_dat$noticeable2 = ifelse(sim_dat$noticeable==1,"not at all noticeable",ifelse(sim_dat$noticeable==3,"moderately noticeable","extremely noticeable"))

interact_plot = ggplot(sim_dat,aes(cybervic,pred.symptom,linetype=noticeable2,color=noticeable2))+geom_line()+geom_point()+
  theme_bw()+xlab("Cyberbullying vicitimization")+ylab("Predicted physical symptoms")+ 
  scale_linetype_discrete(name="Noticeability of disability")+scale_color_discrete(name="Noticeability of disability")


ggsave(filename = 'fig5_outcome_interact.jpeg'
       ,plot=interact_plot,dpi=2500,width=10,height=7)
