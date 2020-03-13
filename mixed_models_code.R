###MIXED EFFECTS MODELLING OF CORAL RESILIENCE AND RESISTANCE 


library(tidyverse)
library(lme4)
#library(car) #For checking multicollinearity, using performance instead
library(performance) #for checking R2 or ICC, and comparing model performances
library(ggeffects)

###USEFUL Tutotials on Mixed Effects and assumptions
#https://ourcodingclub.github.io/tutorials/mixed-models/
#https://easystats.github.io/performance/index.html
#https://jonlefcheck.net/2012/12/28/dealing-with-multicollinearity-using-variance-inflation-factors/
#https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/j.2041-210X.2009.00001.x

#read in data
a1=read.csv('coral_recov_master.csv') 
nrow(a1)

#################################
#RECOVERY MIXED EFFECTS MODELS
#################################
# mixed effect model run using lmer()

#remove NAs from data
recov<-a1 %>% drop_na(calculated.recovery.rate) %>% droplevels()
tail(recov)
nrow(recov)
str(recov)
#exploring the structure of the data
hist(recov$calculated.recovery.rate) #positively skewed normal

#center (standardise) explanatory variable(s) (mean of zero=centering, sd=1 = scaling --doing both here)
recov$hii100km2<-scale(recov$hii100km, center=TRUE, scale=TRUE) #only works for numeric variables
hist(recov$hii100km2)

####EXPLORING DATA BEFORE MODEL TESTING####
#basic lm
lm1<-lm(calculated.recovery.rate ~ hii100km2, data=recov)
summary(lm1)

plot1<-ggplot(recov, aes(x=hii100km, y=calculated.recovery.rate))+
  geom_point()+
  geom_smooth(method="lm")
plot1

#are assumptions met?
#residuals
plot(lm1, which=1) #gray line is flat, red should be nearly flat (mimicking gray)
#looks ok

#qq
plot(lm1, which=2) #points should be close to the line. They diverge at the ends A LOT

#check for observation independence (use categorical vars here)
#if data from within each category are more similar to each other than to data from different categories then they are correlated!

#region
boxplot(calculated.recovery.rate ~ region, data=recov) #maybe correlated? Possibly not.

#disturbance
boxplot(calculated.recovery.rate ~ disturbance, data= recov) #most likely correlated though sample sizes might be small

#plot w/ colors by category to see
color1<-ggplot(recov, aes(x=hii100km2, y=calculated.recovery.rate, color=region))+
  geom_point(size=2)+
  theme_bw()
color1
#regions vary by recovery rate and hii, so observations within are not independent

color2<-ggplot(recov, aes(x=hii100km2, y=calculated.recovery.rate, color=disturbance))+
  geom_point(size=2)+
  theme_bw()
color2
#disturbances vary by recovery rate AND hii100km, so observations within these are NOT INDEPENDENT 

#add fixed effects into the model
lm2<-lm(calculated.recovery.rate ~ hii100km2 + region + disturbance, data=recov)
summary(lm2)
#hii100km2 is not significant, we also now have recov rate vs hii in each region and disturbance
#We ARE interested in this, so we can use these as fixed factors.
#ARE THERE ANY RANDOM FACTORS?-- in this case, these would be variables that we'd like to control the variation for. Confounding vars, for example.

#POTENTIAL RANDOM FACTORS (MUST BE CATEGORICAL)
#pre.dist.cc, recovery_time, resistance.time, country-this is a weird one because Australia can be multiple regions!
#############################################################################################

######MODEL TESTING#######
###GLM MODELS FOR RECOVERY
#RUN ONCE! JUST READ .csv IN FOR GRAPHING!
# mixed effect model run using lmer()
#model testing (REML=FALSE for this step)
#REML=restricted (or residual) maximum likelihood
#fixed effects fo after the ~, random go after that var ~ fixed + (1| random)

#figuring out the random effects
r0.1<-lmer(calculated.recovery.rate ~ hii100km2+ (1|region) , data = recov, REML=FALSE)
summary(r0.1) 

r0.2<-lmer(calculated.recovery.rate ~ hii100km2+region + (1|disturbance) , data = recov, REML=FALSE)
summary(r0.2) 

r0.3<-lmer(calculated.recovery.rate ~ hii100km2+disturbance + (1|region) , data = recov, REML=FALSE)
summary(r0.3)

# r1<-lmer(calculated.recovery.rate ~ hii100km2*region*recovery_time*MPA_status + (1|disturbance) , data = recov, REML=FALSE)
# summary(r1)

#######
#Building the "full" model. Can remove  variables one by one from here to find optimal model 
#######

r2.0<-lmer(calculated.recovery.rate ~ region*disturbance*hii100km2*MPA_status + (1|study), data = recov, REML=FALSE)
summary(r2.0)
#if modeled slope is < modeled error, then the effect cannot be distinguished from zero!
#variance from random effects / total variance #Tells us how much var left over AFTER fixed effects is explained by random effects


r2.1<-lmer(calculated.recovery.rate ~ region*disturbance*hii100km2*MPA_status*recovery_time + (1|study), data = recov, REML=FALSE)#singular fit
r2.2<-lmer(calculated.recovery.rate ~ region*disturbance*hii100km2*MPA_status+recovery_time + (1|study), data = recov, REML=FALSE)
check_model(r2.2)
check_collinearity(r2.2) #VIFs are very high, indicating collinearity is an issue. Remove VIFs > ~ 2 and retest. 
#can use the vif function in the cars package to see the VIFS as well
vif(r2.2)
#REMOVE THE INTERACTION TERMS 1 at a time and compare VIFs
r2.2.1<-lmer(calculated.recovery.rate ~ region*disturbance*hii100km2+MPA_status+recovery_time + (1|study), data = recov, REML=FALSE)
check_collinearity(r2.2.1) #still high
performance::check_model(r2.2.1)

r2.2.2<-lmer(calculated.recovery.rate ~ region*disturbance+hii100km2+MPA_status+recovery_time + (1|study), data = recov, REML=FALSE)
check_collinearity(r2.2.2) #still high
performance::check_model(r2.2.2) 

r2.2.3<-lmer(calculated.recovery.rate ~ region+disturbance+hii100km2+MPA_status+recovery_time + (1|study), data = recov, REML=FALSE)
check_collinearity(r2.2.3) #This finally looks good!
performance::check_model(r2.2.3) 
summary(r2.2.3)


AIC(r2.0,r2.1,r2.2,r2.2.1,r2.2.2,r2.2.3)
anova(r2.2, r2.2.3) #likelihood ratio test
perf1<-performance::compare_performance(r2.0,r2.1,r2.2,r2.2.1,r2.2.2,r2.2.3, rank=TRUE) #compares models and gives assesment of best performance
perf1
plot(perf1)

#AIC, BIC are both criterion for model selection-> smaller is better
#R2_marginal is R2 consdiering the fixed factors only
#R2 condtiional takes both fixed and random factors into account
#ICC is Intraclass correlation coefficient - "the proportion of the variance explained by the grouping structure in the population" (Hox 2010)
#RMSE= root mean squared error (smaller is generally better)
#BF is bayes factor (not sure it is relevant)


#add REML back in to best model and test for convergence (singular fit= does not converge and cannot use) 
#use best fit model
finalmodel<-lmer(calculated.recovery.rate ~ region+disturbance+hii100km2+MPA_status+recovery_time + (1|study), data = recov, REML=TRUE)
summary(finalmodel)
perf2<-performance::compare_performance(r2.2.3, finalmodel)
perf2
plot(perf2)


#write model summary info to dfs for saving later (if desired)
ps1<-summary(finalmodel)
ps2<-summary(finalmodel)$coefficients
ps3<-anova(finalmodel)



### Performing the parametric bootstrapping of the model:
#NOTE: If model reports singular fit during this step that is OK! This is what bootstrapping is for :)

bootnum = 1500 # set number of iterations (we used 1500) between 999 and 9999
seed = 3 #seed to make results replicatable (our seed was 3)

sapply(recov, function(x) length(x)) #recov has 184 rows

mod_out <- simulate(finalmodel,nsim=bootnum,seed=seed,re.form=NULL) # simulate your model set number of times in a dataframe (samples using random effects)
mod_boots <- apply(mod_out,2,function(x) predict(lmer(x ~  region+disturbance+hii100km2+MPA_status+recovery_time + (1|study), data = recov),re.form=NA, REML=TRUE)) # applies the predict (does not use random effects) FUNCTION to the columns of the 'out' dataframe to ...
mod_boots <- cbind(predict(finalmodel,re.form=NA), mod_boots) #combines boots matrix created above with the predicted values based on the model into a single matrix

mod.a <- (cbind(recov, as.data.frame(t(apply(mod_boots, 1, function(x) c(mean(x), quantile(x, c(0.025, 0.975)))))))) #estimates mean and 95% confidence intercals for the prediction values and adds them to your new dataframe
head(mod.a)

#rename columns 
ncol(mod.a)
colnames(mod.a)[54:56] <- c("recov_mean", "recovlowerci", "recovupperci") # rename mean/CI columns
head(mod.a)

#Save data so you don't have to rerun the model everytime
write.csv(mod.a, file='recov_glm.csv')




####VISUALIZATIONS

#read in data
mod.a=read.csv('recov_glm.csv')

pd=position_dodge(width=0.4)

#plot of recovery by region/disturbance, etc
recov_fig<-ggplot(mod.a, aes(x=disturbance, y=recov_mean, group=region, fill=region))+
  geom_point(data=mod.a, aes(x=disturbance, y=calculated.recovery.rate),position=pd, color='lightgrey', shape=1)+
  geom_point(aes(color=region),position=pd, size=3)+
  #geom_line(aes(color=timepoint))+
  scale_color_manual(values=c('firebrick', 'forestgreen', 'darkorange4', "dodgerblue", "purple"))+
  geom_errorbar(aes(x=disturbance, ymin=recovlowerci, ymax=recovupperci, color=region),position=pd, lwd=0.5, width=0.3)+
  facet_wrap(~region, scales='free_y')+
  theme_bw()+
  ylab('Coral Recovery Rate (%/Year')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

recov_fig


#HOW DO I VISUALIZE THIS?
head(mod.a)

recov_fig1<-ggplot(mod.a, aes(x=disturbance, y=recov_mean, group=recovery_time, fill=hii100km2))+
  geom_point(data=mod.a, aes(x=disturbance, y=calculated.recovery.rate),position=pd, color='lightgrey', shape=1)+
  geom_point(aes(color=hii100km2),position=pd, size=3)+
  #geom_line(aes(color=timepoint))+
  #scale_color_manual(values=c('firebrick', 'forestgreen', 'darkorange4', "dodgerblue", "purple"))+
  geom_errorbar(aes(x=disturbance, ymin=recovlowerci, ymax=recovupperci),position=pd, lwd=0.5, width=0.3)+
  facet_grid(region~MPA_status, scales='free_y')+
  theme_bw()+
  ylab('Coral Recovery Rate (%/Year')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8))+
  geom_hline(aes(yintercept=0),linetype='dashed')

recov_fig1

#####other plotting attempts
#finalmodel<-lmer(calculated.recovery.rate ~ region+disturbance+hii100km2+MPA_status+recovery_time + (1|study), data = recov, REML=TRUE)

mm_plot<-ggplot(recov, aes(x=recovery_time, y=calculated.recovery.rate, color=disturbance))+
  facet_grid(region~MPA_status)+
  geom_point(alpha=0.5)+
  theme_bw()+
  geom_line(data = cbind(recov, pred = predict(finalmodel)), aes(y = pred), size = 1)

mm_plot


###Plot of predictions

###RECOVERY TIME
# Extract the prediction data frame
pred.rec <- ggpredict(finalmodel, terms = c("recovery_time"))  # this gives overall predictions for the model

# Plot the predictions 
ggpred<-ggplot(pred.rec) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = recov,                      # adding the raw data (scaled values)
               aes(x = recovery_time, y = calculated.recovery.rate, colour = region)) +
    theme_minimal()+
  labs(x = "Recovery Time (years)", y = "Recovery Rate")+
  #facet_grid(region~MPA_status)+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8))+
  geom_hline(aes(yintercept=0),linetype='dashed')

ggpred

####Human Influence Index
# Extract the prediction data frame
pred.hii <- ggpredict(finalmodel, terms = c("hii100km2"))  # this gives overall predictions for the model

# Plot the predictions 
ggpredhii<-ggplot(pred.hii) + 
  geom_line(aes(x = x, y = predicted)) +          # slope
  geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
              fill = "lightgrey", alpha = 0.5) +  # error band
  geom_point(data = recov,                      # adding the raw data (scaled values)
             aes(x = hii100km2, y = calculated.recovery.rate, colour = region)) +
  theme_minimal()+
  labs(x = "Human Influence Index", y = "Recovery Rate")+
  #facet_grid(region~MPA_status)+
  #theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8))+
  geom_hline(aes(yintercept=0),linetype='dashed')

ggpredhii




