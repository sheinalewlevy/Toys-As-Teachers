library(testthat)
library(dplyr)
library(rgdal)
library(geos)
library(httr)
library(rnaturalearth)
library(sf)
library(rnaturalearthdata)
library(ggplot2)
library(brms)
library(rethinking)
library(ggpubr)
library(bayesplot)
library(tidyr)
library(lme4)

#################
###Import data###
#################

d<-read.csv("data_complete.csv") 

######################
###Recode variables###
######################
d$scale.recode<-as.factor(ifelse(d$Scale=="ChildOnly",0, 1))
d$play.recode<-ifelse(d$Play=="Play",1,0)
d$play.iv<-d$play.recode%>%replace_na("unknown")
d$play.recode<-as.factor(d$play.recode)
d$play.iv<-as.factor(d$play.iv)
d$simpleComp.recode<-as.factor(ifelse(d$simpleComp=="Comp",1,0))
d$risk.recode<-as.factor(ifelse(d$risk=="risky",1,0))
d$Social.recode<-as.factor(ifelse(d$Social=="Social",1,0))
d$ref<-coerce_index(d$Reference)
d$Gender<-factor(d$Gender,levels=c("Girls","Boys","Both/Unspecified"))
d$age.recode<-recode(d$Age,ADOLESCENCE="Older",`MIDDLE CHILDHOOD`="Older",`EARLY CHILDHOOD`="Younger",Unspecified="Unknown")
d$age.recode<-as.factor(d$age.recode)
d$age.recode<-relevel(d$age.recode,ref="Younger")
d$Type.recode<-recode(d$Type,`Animal Figure`="Figures",`Human figure`="Figures",Instrument="Subsistence",`Tended facility`="Subsistence",`Untended facility`="Subsistence",Game="Games",PhysGame="Games")
d$mult<-ifelse(d$Multiple.Uses=="Multiple",1,0)
d$toycon<-ifelse(d$Toycon=="Toycon",1,0)

#########################
###General descriptive###
#########################
expect_true(nrow(d)==434)
expect_true(length(unique(d$Society))==54)
d$one<-1
t<-d%>%group_by(Society)%>%summarise(count=sum(one),Continent=unique(Continent))
expect_true(max(t$count)==38)
expect_true(round(mean(t$count),2)==8.04)
expect_true(round(sd(t$count),2)==8.07)
expect_true(length(unique(d$entryNumber))==272)
expect_true(length(unique(d$Reference))==124)
round((table(t$Continent)/54)*100,0)
round(table(d$Continent)/nrow(d)*100,0)
rm(t)

########################################
###Set Priors and make psign function###
########################################
prior1<-c(prior(normal(0,1),class=b,nlpar=a), prior(normal(0,1),class=b,nlpar=b), prior(exponential(1),class=sd,nlpar=d))

prior2<-c(prior(normal(0,1),class=b,nlpar=a), prior(normal(0,1),class=b,nlpar=b),prior(normal(0,1),class=b,nlpar=c),  prior(exponential(1),class=sd,nlpar=d))

prior3<-c(prior(normal(0,1),class=b), prior(exponential(1),class=sd))

prior4<-c(prior(normal(0,1),class=Intercept),prior(exponential(1),class=sd))

## This function is courtesy of Bret Beheim
psign <- function(samples, n_digits = 3) {
  if (all(samples == 0)) stop("all samples are zero!")
  if (mean(samples) > 0) output <- round(mean(samples < 0), n_digits)
  if (mean(samples) < 0) output <- round(mean(samples > 0), n_digits)
  float_digits <- paste0("%.", n_digits, "f")
  output <- sprintf(float_digits, output)
  null_entry <- paste0("0.", paste0(rep("0", n_digits), collapse = ""))
  output[output  ==  null_entry] <- paste0("$<0.", paste0(rep("0", n_digits - 1), collapse = ""),"1$")
  return(output)
}

##########################
###Fit Intercept Models###
##########################
dp<-subset(d,play.recode==1)
dt<-subset(d,Play_vs_use!="Play")

E1<-brm(play.recode~ 1 +  (1| Continent) + (1|Society) + (1|ref)+ (1|randomNumber),family=bernoulli(), data = d, cores=4, iter=5000 ,prior=prior4,control=list(adapt_delta=0.99) )

E2<-brm(mult~ 1 +  (1| Continent) + (1|Society) + (1|ref)+ (1|randomNumber),family=bernoulli(), data = dp, cores=4, prior=prior4, iter=5000 ,control=list(adapt_delta=0.99) )

E3<-brm(toycon~ 1 +  (1| Continent) + (1|Society) + (1|ref)+ (1|randomNumber),family=bernoulli(), data = dt, cores=4, prior=prior4, iter=5000 ,control=list(adapt_delta=0.99) )

E4<-brm(simpleComp.recode ~ 1 + (1| Continent) + (1|Society) + (1|ref)+ (1|randomNumber),family=bernoulli(), data = d, cores=4, prior=prior4, iter=5000 ,control=list(adapt_delta=0.99) )

E5<-brm(risk.recode ~ 1 +  (1| Continent) + (1|Society) + (1|ref)+ (1|randomNumber),family=bernoulli(), data = d, cores=4, prior=prior4, iter=5000 ,control=list(adapt_delta=0.99) )

E6<-brm(Social.recode ~ 1 +  (1| Continent) + (1|Society) + (1|ref)+ (1|randomNumber),family=bernoulli(), data = d, cores=4, prior=prior4, iter=5000 ,control=list(adapt_delta=0.99) )

E7<-brm(scale.recode ~ 1 +  (1| Continent) + (1|Society) + (1|ref)+ (1|randomNumber),family=bernoulli(), data = d, cores=4, prior=prior4, iter=5000 ,control=list(adapt_delta=0.99) )


######################
###Fit Index Models###
######################

##https://rsconnect.calvin.edu:3939/Rethinking2-ggformula/god-spiked-the-integers.html

M1<-brm(bf(play.recode ~ a + b + d,
           a ~ 0 + age.recode ,
           b ~ 0 + Gender,
           d ~ 0 +  (1| Continent) + (1|Society) + (1|ref)+ (1|randomNumber),
           nl=TRUE),family=bernoulli(), data = d, cores=4, prior=prior1, iter=5000 ,control=list(adapt_delta=0.99))

M2<-brm(bf(simpleComp.recode ~ a + b + c  + d,
           a ~ 0 + age.recode ,
           b ~ 0 + Gender,
           c ~ 0 + play.iv,
           d ~ 0 + (1| Continent) + (1|Society) + (1|ref)+ (1|randomNumber),
           nl=TRUE),family=bernoulli(), data = d, cores=4, prior=prior2, iter=5000 ,control=list(adapt_delta=0.99))

M3<-brm(bf(risk.recode~ a + b + c  + d,
           a ~ 0 + age.recode ,
           b ~ 0 + Gender,
           c ~ 0 + play.iv,
           d ~ 0 + (1| Continent) + (1|Society) + (1|ref)+ (1|randomNumber),
           nl=TRUE),family=bernoulli(), data = d, cores=4, prior=prior2, iter=5000 ,control=list(adapt_delta=0.99))

M4<-brm(bf(Social.recode~ a + b + c  + d,
           a ~ 0 + age.recode ,
           b ~ 0 + Gender,
           c ~ 0 + play.iv,
           d ~ 0 + (1| Continent) + (1|Society) + (1|ref)+ (1|randomNumber),
           nl=TRUE),family=bernoulli(), data = d, cores=4, prior=prior2, iter=5000 ,control=list(adapt_delta=0.99))

M5<-brm(bf(scale.recode~ a + b + c  + d,
           a ~ 0 + age.recode ,
           b ~ 0 + Gender,
           c ~ 0 + play.iv,
           d ~ 0 + (1| Continent) + (1|Society) + (1|ref)+ (1|randomNumber),
           nl=TRUE),family=bernoulli(), data = d, cores=4, prior=prior2, iter=5000 ,control=list(adapt_delta=0.99))


#####################
###In Text Results###
#####################
##########MISSING DATA###################
table(is.na(d$play.recode))
expect_true(round(416/434,2)*100==96) ##96% of objects are coded for play/use

table(is.na(d$simpleComp.recode)) 
expect_true(round(330/434,2)*100==76) ##76% of objects are coded for complexity

table(is.na(d$risk.recode))

table(is.na(d$Social.recode)) 
expect_true(round(227/434,2)*100==52) ##52% of objects had info on social context

table(is.na(d$scale.recode))

###########PLAY############################################
post_E1<-posterior_samples(E1)
post_E2<-posterior_samples(E2)
post_E3<-posterior_samples(E3)

round(median(inv_logit(post_E1$b_Intercept)),2)
round(median(inv_logit(post_E2$b_Intercept)),2)
round(median(inv_logit(post_E3$b_Intercept)),2)

post1<-posterior_samples(M1)
post1$prob_Older<-inv_logit(post1$b_a_age.recodeOlder)
post1$prob_Younger<-inv_logit(post1$b_a_age.recodeYounger)
post1$prob_age_dif<-post1$prob_Older-post1$prob_Younger
round(median(post1$prob_age_dif),2)
round(PI(post1$prob_age_dif),2)

##############COMPLEXITY#############################
post_E4<-posterior_samples(E4)
round(median(inv_logit(post_E4$b_Intercept)),2)

post2<-posterior_samples(M2)

post2$prob_Boys<-inv_logit(post2$b_b_GenderBoys)
post2$prob_Girls<-inv_logit(post2$b_b_GenderGirls)
post2$prob_gender_dif<-post2$prob_Boys-post2$prob_Girls
round(median(post2$prob_gender_dif),2)

######################RISK################################
post_E5<-posterior_samples(E5)
round(median(inv_logit(post_E5$b_Intercept)),2)

post3<-posterior_samples(M3)

post3$prob_Boys<-inv_logit(post3$b_b_GenderBoys)
post3$prob_Girls<-inv_logit(post3$b_b_GenderGirls)
post3$prob_gender_dif<-post3$prob_Boys-post3$prob_Girls
round(median(post3$prob_gender_dif),2)

post3$prob_Play<-inv_logit(post3$b_c_play.iv1)
post3$prob_Instrument<-inv_logit(post3$b_c_play.iv0)
post3$prob_activity_dif<-post3$prob_Play-post3$prob_Instrument
round(median(post3$prob_activity_dif),2)

post3$prob_Older<-inv_logit(post3$b_a_age.recodeOlder)
post3$prob_Younger<-inv_logit(post3$b_a_age.recodeYounger)
post3$prob_age_dif<-post3$prob_Older-post3$prob_Younger
round(median(post3$prob_age_dif),2)
round(PI(post3$prob_age_dif),2)

##############SOCIAL CONTEXT##############################
post_E6<-posterior_samples(E6)
round(median(inv_logit(post_E6$b_Intercept)),2)

post4<-posterior_samples(M4)

post4$prob_Play<-inv_logit(post4$b_c_play.iv1)
post4$prob_Instrument<-inv_logit(post4$b_c_play.iv0)
post4$prob_activity_dif<-post4$prob_Play-post4$prob_Instrument
round(median(post4$prob_activity_dif),2)

######################LEARNING############################
post_E7<-posterior_samples(E7)
round(median(inv_logit(post_E7$b_Intercept)),2)

post5<-posterior_samples(M5)

post5$prob_Boys<-inv_logit(post5$b_b_GenderBoys)
post5$prob_Girls<-inv_logit(post5$b_b_GenderGirls)
post5$prob_gender_dif<-post5$prob_Boys-post3$prob_Girls
round(median(post5$prob_gender_dif),2)


post5$prob_Play<-inv_logit(post5$b_c_play.iv1)
post5$prob_Instrument<-inv_logit(post5$b_c_play.iv0)
post5$prob_activity_dif<-post5$prob_Play-post5$prob_Instrument
round(median(post5$prob_activity_dif),2)

#############################
###Make Table of Contrasts###
#############################
post1$age_dif<-post1$b_a_age.recodeOlder-post1$b_a_age.recodeYounger
round(median(post1$age_dif),2)
round(median(exp(post1$age_dif)),2)
round(PI(post1$age_dif),2)
psign(post1$age_dif)

post1$gender_dif<-post1$b_b_GenderBoys-post1$b_b_GenderGirls
round(median(post1$gender_dif),2)
round(median(exp(post1$gender_dif)),2)
round(PI(post1$gender_dif),2)
psign(post1$gender_dif)

post2$age_dif<-post2$b_a_age.recodeOlder-post2$b_a_age.recodeYounger
round(median(post2$age_dif),2)
round(median(exp(post2$age_dif)),2)
round(PI(post2$age_dif),2)
psign(post2$age_dif)

post2$gender_dif<-post2$b_b_GenderBoys-post2$b_b_GenderGirls
round(median(post2$gender_dif),2)
round(median(exp(post2$gender_dif)),2)
round(PI(post2$gender_dif),2)
psign(post2$gender_dif)

post2$play_dif<-post2$b_c_play.iv1-post2$b_c_play.iv0
round(median(post2$play_dif),2)
round(median(exp(post2$play_dif)),2)
round(PI(post2$play_dif),2)
psign(post2$play_dif)

post3$age_dif<-post3$b_a_age.recodeOlder-post3$b_a_age.recodeYounger
round(median(post3$age_dif),2)
round(median(exp(post3$age_dif)),2)
round(PI(post3$age_dif),2)
psign(post3$age_dif)

post3$gender_dif<-post3$b_b_GenderBoys-post3$b_b_GenderGirls
round(median(post3$gender_dif),2)
round(median(exp(post3$gender_dif)),2)
round(PI(post3$gender_dif),2)
psign(post3$gender_dif)

post3$play_dif<-post3$b_c_play.iv1-post3$b_c_play.iv0
round(median(post3$play_dif),2)
round(median(exp(post3$play_dif)),2)
round(PI(post3$play_dif),2)
psign(post3$play_dif)

post4$age_dif<-post4$b_a_age.recodeOlder-post4$b_a_age.recodeYounger
round(median(post4$age_dif),2)
round(median(exp(post4$age_dif)),2)
round(PI(post4$age_dif),2)
psign(post4$age_dif)

post4$gender_dif<-post4$b_b_GenderBoys-post4$b_b_GenderGirls
round(median(post4$gender_dif),2)
round(median(exp(post4$gender_dif)),2)
round(PI(post4$gender_dif),2)
psign(post4$gender_dif)

post4$play_dif<-post4$b_c_play.iv1-post4$b_c_play.iv0
round(median(post4$play_dif),2)
round(median(exp(post4$play_dif)),2)
round(PI(post4$play_dif),2)
psign(post4$play_dif)

post5$age_dif<-post5$b_a_age.recodeOlder-post5$b_a_age.recodeYounger
round(median(post5$age_dif),2)
round(median(exp(post5$age_dif)),2)
round(PI(post5$age_dif),2)
psign(post5$age_dif)

post5$gender_dif<-post5$b_b_GenderBoys-post5$b_b_GenderGirls
round(median(post5$gender_dif),2)
round(median(exp(post5$gender_dif)),2)
round(PI(post5$gender_dif),2)
psign(post5$gender_dif)

post5$play_dif<-post5$b_c_play.iv1-post5$b_c_play.iv0
round(median(post5$play_dif),2)
round(median(exp(post5$play_dif)),2)
round(PI(post5$play_dif),2)
psign(post5$play_dif)

##########################
###Supplementary models###
##########################
d$test_play<-ifelse(is.na(d$play.recode),1,0)
d$test_complexity<-ifelse(is.na(d$simpleComp.recode),1,0)
d$test_social<-ifelse(is.na(d$Social.recode),1,0)
d$test_age.recode<-ifelse(is.na(d$age.recode),1,0)

MS1<-brm(test_play~Gender+age.recode+ (1| randomNumber)+ (1|Society) + (1|ref) + (1|Continent),family=bernoulli(), data = d, cores=4, prior=prior3, iter=4000 ,control=list(adapt_delta=0.99))

MS2<-brm(test_complexity~Gender+age.recode+ (1| randomNumber)+ (1|Society) + (1|ref) + (1|Continent),family=bernoulli(), data = d, cores=4, prior=prior3, iter=4000 ,control=list(adapt_delta=0.99))

MS3<-brm(test_social~Gender+age.recode+ (1| randomNumber)+ (1|Society) + (1|ref) + (1|Continent),family=bernoulli(), data = d, cores=4, prior=prior3, iter=4000 ,control=list(adapt_delta=0.99))

MS4<-brm(test_age.recode~Gender+ (1| randomNumber)+ (1|Society) + (1|ref) + (1|Continent),family=bernoulli(), data = d, cores=4, prior=prior3, iter=4000 ,control=list(adapt_delta=0.99))

summary(MS1,prob=0.89)
summary(MS2,prob=0.89)
summary(MS3,prob=0.89)
summary(MS4,prob=0.89)

##############################
###Make Table of Intercepts###
##############################
summary(E1,prob=0.89)
summary(E2,prob=0.89)
summary(E3,prob=0.89)
summary(E4,prob=0.89)
summary(E5,prob=0.89)
summary(E6,prob=0.89)
summary(E7,prob=0.89)

###############################
###Make Table of Main Models###
###############################
summary(M1,prob=0.89)
summary(M2,prob=0.89)
summary(M3,prob=0.89)
summary(M4,prob=0.89)
summary(M5,prob=0.89)

##################
###Make the map###
##################
points<-d%>%group_by(Society)%>%summarise(Latitude=unique(Latitude),Longitude=unique(Longitude))
societies<-as.data.frame(unique(d$Society))
colnames(societies)[1] <- "Society"

societies<-merge(societies,points,by="Society")
points <- st_as_sf(societies, coords = c("Longitude", "Latitude"), remove = FALSE, 
                   crs = 4326, agr = "constant")
theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf()+
  geom_sf(data=points,color="black",fill="red",pch=21,size=2)

rm(societies)
rm(points)
rm(world)

########################################
###Make Probability Percent Estimates###
########################################
a<-c("Any Play (% Activity)","Multifunctional (% Any Play)","Toy Construction (% Any Instrumental)","Composite (% Complexity)", "Risky (% Associated Risk)","Social (% Context of Use)", "Adult Culture (% Learning Function)")
b<-c(median(inv_logit(post_E1$b_Intercept))*100,median(inv_logit(post_E2$b_Intercept))*100,median(inv_logit(post_E3$b_Intercept))*100,median(inv_logit(post_E4$b_Intercept))*100,median(inv_logit(post_E5$b_Intercept))*100,median(inv_logit(post_E6$b_Intercept))*100,median(inv_logit(post_E7$b_Intercept))*100)

c<-cbind(PI(inv_logit(post_E1$b_Intercept))*100,PI(inv_logit(post_E2$b_Intercept))*100,PI(inv_logit(post_E3$b_Intercept))*100,PI(inv_logit(post_E4$b_Intercept))*100,PI(inv_logit(post_E5$b_Intercept))*100,PI(inv_logit(post_E6$b_Intercept))*100,PI(inv_logit(post_E7$b_Intercept))*100)

f<-as.data.frame(t(rbind(a,b,c)))
f$b<-as.numeric(f$b)
f$`5%`<-as.numeric(f$`5%`)
f$`94%`<-as.numeric(f$`94%`)
f$a<-factor(f$a,levels=c("Adult Culture (% Learning Function)","Social (% Context of Use)","Risky (% Associated Risk)","Composite (% Complexity)","Toy Construction (% Any Instrumental)","Multifunctional (% Any Play)","Any Play (% Activity)"))


P0<-ggplot(f, aes(x = a, y = b)) + geom_linerange(aes(x = a,  ymin = `5%`, ymax = `94%`), lwd = 1, colour = gray(1/2))+ geom_point(aes(x = a, y = b),size=3.5)   + scale_y_continuous(breaks=c(25,50,75))+ coord_flip() + theme_bw()+theme(axis.title.y=element_blank())+labs(y="(A) Percent of Objects")

##########################################
###Make Probability Distribution Figure###
##########################################

color_scheme_set("viridis")
P1<-mcmc_plot(M1,prob=0.89,prob_outer=0.89,pars="^b_",type="areas",rhat=c(1.1,1.1,1.1,1.2,1.2,1.2))
P1<-P1+scale_y_discrete(labels=c("b_a_age.recodeYounger" ="6 Years and Under","b_a_age.recodeOlder"="7 Years and Over","b_a_age.recodeUnknown"="Age Unknown","b_b_GenderGirls"="Girls","b_b_GenderBoys"="Boys","b_b_GenderBothDUnspecified"="Both/Unknown"),
                        limits=c("b_b_GenderBothDUnspecified","b_b_GenderBoys","b_b_GenderGirls","b_a_age.recodeUnknown","b_a_age.recodeOlder","b_a_age.recodeYounger"))
P1<-P1+scale_x_continuous(limits=c(-3,3),breaks=c(-2,0,2))+labs(y="Activity", x="Instrumental Only vs Any Play") + theme_bw()+legend_none()
P1

P2<-mcmc_plot(M2,prob=0.89,prob_outer=0.89,pars="^b_",type="areas", rhat=c(1.1,1.1,1.1,1.2,1.2,1.2,1,1,1))
P2<-P2+scale_y_discrete(labels=c("b_a_age.recodeYounger" ="6 Years and Under","b_a_age.recodeOlder"="7 Years and Over","b_a_age.recodeUnknown"="Age Unknown","b_b_GenderGirls"="Girls","b_b_GenderBoys"="Boys","b_b_GenderBothDUnspecified"="Both/Unknown","b_c_play.iv0"="Instrumental Only","b_c_play.iv1"="Any Play","b_c_play.ivunknown"="Unknown Activity"),
                        limits=c("b_c_play.ivunknown","b_c_play.iv0","b_c_play.iv1","b_b_GenderBothDUnspecified","b_b_GenderBoys","b_b_GenderGirls","b_a_age.recodeUnknown","b_a_age.recodeOlder","b_a_age.recodeYounger"))
P2<-P2+scale_x_continuous(limits=c(-3,3),breaks=c(-2,0,2))+labs(y="Object Complexity",x="Simple vs Composite")+ theme_bw()+legend_none()
P2

P3<-mcmc_plot(M3,prob=0.89,prob_outer=0.89,pars="^b_",type="areas", rhat=c(1.1,1.1,1.1,1.2,1.2,1.2,1,1,1))
P3<-P3+scale_y_discrete(labels=c("b_a_age.recodeYounger" ="6 Years and Under","b_a_age.recodeOlder"="7 Years and Over","b_a_age.recodeUnknown"="Age Unknown","b_b_GenderGirls"="Girls","b_b_GenderBoys"="Boys","b_b_GenderBothDUnspecified"="Both/Unknown","b_c_play.iv0"="Instrumental Only","b_c_play.iv1"="Any Play","b_c_play.ivunknown"="Unknown Activity"),
                        limits=c("b_c_play.ivunknown","b_c_play.iv0","b_c_play.iv1","b_b_GenderBothDUnspecified","b_b_GenderBoys","b_b_GenderGirls","b_a_age.recodeUnknown","b_a_age.recodeOlder","b_a_age.recodeYounger"))
P3<-P3+scale_x_continuous(limits=c(-3,3),breaks=c(-2,0,2))+labs(y="Associated Risk",x="Safe vs Risky")+ theme_bw()+legend_none()
P3

P4<-mcmc_plot(M4,prob=0.89,prob_outer=0.89,pars="^b_",type="areas", rhat=c(1.1,1.1,1.1,1.2,1.2,1.2,1,1,1))
P4<-P4+scale_y_discrete(labels=c("b_a_age.recodeYounger" ="6 Years and Under","b_a_age.recodeOlder"="7 Years and Over","b_a_age.recodeUnknown"="Age Unknown","b_b_GenderGirls"="Girls","b_b_GenderBoys"="Boys","b_b_GenderBothDUnspecified"="Both/Unknown","b_c_play.iv0"="Instrumental Only","b_c_play.iv1"="Any Play","b_c_play.ivunknown"="Unknown Activity"),
                        limits=c("b_c_play.ivunknown","b_c_play.iv0","b_c_play.iv1","b_b_GenderBothDUnspecified","b_b_GenderBoys","b_b_GenderGirls","b_a_age.recodeUnknown","b_a_age.recodeOlder","b_a_age.recodeYounger"))
P4<-P4+scale_x_continuous(limits=c(-3,3),breaks=c(-2,0,2),labels=c("-2","0","2"))+labs(y="Context of Use",x="Solitary vs Social")+ theme_bw()+legend_none()
P4

P5<-mcmc_plot(M5,prob=0.89,prob_outer=0.89,pars="^b_",type="areas", rhat=c(1.1,1.1,1.1,1.2,1.2,1.2,1,1,1))
P5<-P5+scale_y_discrete(labels=c("b_a_age.recodeYounger" ="6 Years and Under","b_a_age.recodeOlder"="7 Years and Over","b_a_age.recodeUnknown"="Age Unknown","b_b_GenderGirls"="Girls","b_b_GenderBoys"="Boys","b_b_GenderBothDUnspecified"="Both/Unknown","b_c_play.iv0"="Instrumental Only","b_c_play.iv1"="Any Play","b_c_play.ivunknown"="Unknown Activity"),
                        limits=c("b_c_play.ivunknown","b_c_play.iv0","b_c_play.iv1","b_b_GenderBothDUnspecified","b_b_GenderBoys","b_b_GenderGirls","b_a_age.recodeUnknown","b_a_age.recodeOlder","b_a_age.recodeYounger"))
P5<-P5+scale_x_continuous(limits=c(-3,3),breaks=c(-2,0,2),labels=c("-2","0","2"))+labs(y="Learning Function",x="Child vs Adult Culture")+ theme_bw()+legend_none()
P5