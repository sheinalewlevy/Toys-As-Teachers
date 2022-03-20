##load packages
library(readxl)
library(stringi)
library(dplyr)
library(irr)
library(rel)
library(brms)
library(tidyr)
library(ggplot2)
library(vcd)
library(ggmosaic)
library(forcats)
library(ggpubr)
library(tidyverse)
library(rgdal)
library(rgeos)
library(httr)
library(rnaturalearth)
library(sf)
library(rnaturalearthdata)
library(brms)
library(rethinking)


###############################
###General Descriptive Stats###
###############################

##import the dataset
d<-read.csv("data_pub.csv")

d$ref<-coerce_index(d$Reference)

##Look at child age names
unique(d$childAge_coded)

##Recode and rename variables to make them more appealing
d$Age<-ifelse(d$childAge_coded=="ADOLESCENCE"|d$childAge_coded=="MIDDLE CHILDHOOD"|d$childAge_coded=="OLD","OLD","YOUNG")

d$Age<-recode(d$Age, YOUNG="Approx. ≤6 years", OLD="Approx. ≥7 years")
d$Age[is.na(d$Age)]<-"Unknown"

d$Gender<-recode(d$Gender, GIRLS="Girls", BOYS="Boys",BOTH="Both")
d$Gender[is.na(d$Gender)]<-"Unknown"

d$Continent<-recode(d$Continent, `Southern South America` = "South America")

d$Society<-as.factor(d$Society)

##Number of unique paragraphs
length(unique(d$randomNumber)) ##272 paragraphs

##number of objects
nrow(d) ##434 objects

##number of societies
length(unique(d$Society)) ##54

##tally the number of tools per culture, and generate descriptive stats
d$count<-1
total<-d%>%group_by(Society, Continent) %>% summarise(subsistence=unique(Subsistence), n_pubs=length(unique(Reference)), min_date=min(Date), max_date=max(Date),n=sum(count))
total$percent<-round(total$n/sum(total$n),4)*100
round(mean(total$n),2) ##8.04
round(sd(total$n),2) ##8.07
range(total$n) ##1-38

##write.csv(total, "TableS1.csv")

length(unique(d$Reference)) ##124 references
round(mean(total$n_pubs),2) ##2.3
round(sd(total$n_pubs),2) ##1.78
range(total$n_pubs) ##1-9

range(d$Date) ##1854-2019

##Regions
round(table(d$Continent)/nrow(d),2)*100 ##60% of objects come from North America

round(table(total$Continent)/nrow(total),2)*100 ##52% of societies come from north america


rm(total)

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
  geom_sf(data=points,color="black",fill="white",pch=21,size=2.5)

rm(societies)
rm(points)
rm(world)

############
###Gender###
############

d$Gender<-relevel(as.factor(d$Gender), ref="Unknown")

prior<-c(prior(normal(0,1),class=Intercept),prior(exponential(1),class=sd, dpar="muGirls"),prior(exponential(1),class=sd, dpar="muBoys"), prior(exponential(1),class=sd, dpar="muBoth"))

M1<-brm(Gender~1 +  (1| Continent) + (1|Society) + (1|ref)+ (1|randomNumber), family=categorical(),prior=prior, data = d, iter=5000, cores=4, control=list(adapt_delta=0.99) )

post1<-posterior_samples(M1)

post1<-post1%>% dplyr::select(starts_with("b_mu"))
post1$uknown<-0

post1<-as.data.frame(softmax(post1))

round(mean(post1$b_muBoys_Intercept),2)*100
round(mean(post1$b_muGirls_Intercept),2)*100
round(mean(post1$b_muBoth_Intercept),2)*100
round(mean(post1$uknown),2)*100

fig3a_mean<-c(mean(post1$b_muBoys_Intercept)*100,
                  mean(post1$b_muGirls_Intercept)*100,
                  mean(post1$b_muBoth_Intercept)*100,
                  mean(post1$uknown)*100)

fig3a_PI<-cbind(PI(post1$b_muBoys_Intercept)*100,
                PI(post1$b_muGirls_Intercept)*100,
                PI(post1$b_muBoth_Intercept)*100,
                PI(post1$uknown)*100)

fig3a_title<-c("Boys","Girls","Both Boys\n & Girls","Gender\n Unknown")

fig3a_data<-as.data.frame(t(rbind(fig3a_title,fig3a_mean,fig3a_PI)))

fig3a_data$fig3a_title<-factor(fig3a_data$fig3a_title,levels=c("Gender\n Unknown","Both Boys\n & Girls","Girls","Boys"))
fig3a_data$fig3a_mean<-as.numeric(fig3a_data$fig3a_mean)
fig3a_data$`5%`<-as.numeric(fig3a_data$`5%`)
fig3a_data$`94%`<-as.numeric(fig3a_data$`94%`)
                              
fig3a<-ggplot(fig3a_data, aes(x = fig3a_title, y = fig3a_mean)) + geom_linerange(aes(x = fig3a_title,  ymin = `5%`, ymax = `94%`), lwd = 1, colour = gray(1/2))+ geom_point(aes(x = fig3a_title, y = fig3a_mean),size=3.5)   + scale_y_continuous(limits=c(0,100),breaks=c(0,25,50,75,100),labels=c("0%","25%","50%","75%","100%"))+coord_flip() + theme_bw(base_size=18)+theme(axis.title.y=element_blank())+labs(y="(A) User Gender")

fig3a

#########
###Age###
#########

d$Age<-relevel(as.factor(d$Age), ref="Unknown")

prior<-c(prior(normal(0,1),class=Intercept),prior(exponential(1),class=sd, dpar="muApprox7years"),prior(exponential(1),class=sd, dpar="muApprox6years"))

M2<-brm(Age~1 +  (1| Continent) + (1|Society) + (1|ref)+ (1|randomNumber), family=categorical(),prior=prior, data = d, iter=5000, cores=4, control=list(adapt_delta=0.99) )

post2<-posterior_samples(M2)

post2<-post2%>% dplyr::select(starts_with("b_mu"))
post2$uknown<-0

post2<-as.data.frame(softmax(post2))

round(mean(post2$b_muApprox6years_Intercept),2)*100
round(mean(post2$b_muApprox7years_Intercept),2)*100
round(mean(post2$uknown),2)*100

fig3b_mean<-c(mean(post2$b_muApprox6years_Intercept)*100,
              mean(post2$b_muApprox7years_Intercept)*100,
              mean(post2$uknown)*100)

fig3b_PI<-cbind(PI(post2$b_muApprox6years_Intercept)*100,
            PI(post2$b_muApprox7years_Intercept)*100,
            PI(post2$uknown)*100)

fig3b_title<-c("Infancy\n & Early Childhood","Middle Childhood\n & Adolescence","Age Unknown")

fig3b_data<-as.data.frame(t(rbind(fig3b_title,fig3b_mean,fig3b_PI)))

fig3b_data$fig3b_title<-factor(fig3b_data$fig3b_title,levels=c("Age Unknown","Middle Childhood\n & Adolescence","Infancy\n & Early Childhood"))
fig3b_data$fig3b_mean<-as.numeric(fig3b_data$fig3b_mean)
fig3b_data$`5%`<-as.numeric(fig3b_data$`5%`)
fig3b_data$`94%`<-as.numeric(fig3b_data$`94%`)

fig3b<-ggplot(fig3b_data, aes(x = fig3b_title, y = fig3b_mean)) + geom_linerange(aes(x = fig3b_title,  ymin = `5%`, ymax = `94%`), lwd = 1, colour = gray(1/2))+ geom_point(aes(x = fig3b_title, y = fig3b_mean),size=3.5)   + scale_y_continuous(limits=c(0,100),breaks=c(0,25,50,75,100),labels=c("0%","25%","50%","75%","100%"))+coord_flip() + theme_bw(base_size=18)+theme(axis.title.y=element_blank())+labs(y="(B) User Age Category")

fig3b

################################################
###Frequency and percent for each object type###
################################################
table(d$Type_recode)

table(d$Type_recode,d$Gender)

###################
###Make Table 3####
###################
##Recode types so that we have enough in each category
d$Type_recode<-recode(d$Type,`Animal Figure`="Figures",`Human figure`="Figures",Instrument="Subsistence",`Tended facility`="Subsistence",`Untended facility`="Subsistence",Game="Games",PhysGame="Games")

t3a<-as.data.frame(table(d$Type_recode))

##Frequency by sex
t3b<-as.data.frame.matrix(table(d$Type_recode, d$Gender))

##Frequency and percent for each scale type
t3c<-as.data.frame.matrix(table(d$Type_recode,d$Scale))

t3d<-as.data.frame.matrix(table(d$Type_recode,d$PresLikelihood))

t3e<-as.data.frame.matrix(table(d$Type_recode,d$simpleComp))

t3f<-as.data.frame.matrix(table(d$Type_recode,d$Play))

table3<-bind_cols(t3a,t3b, t3f,t3c,t3d, t3e)
##write.csv(table3,"table3.csv")

##########
###Play###
##########
prior<-c(prior(normal(0,1),class=Intercept),prior(exponential(1),class=sd))

d_play<-subset(d,!is.na(Play))

nrow(d_play) ##416 objects 
length(unique(d_play$Society)) ##53 societies

M3<-brm(Play~1 +  (1| Continent) + (1|Society) + (1|ref)+ (1|randomNumber), family=bernoulli(),prior=prior, data = d_play, iter=5000, cores=4, control=list(adapt_delta=0.99) )

post3<-posterior_samples(M3)

round(mean(1-inv_logit(post3$b_Intercept)),2)*100

###########
###Scale###
###########

prior<-c(prior(normal(0,1),class=Intercept),prior(exponential(1),class=sd, dpar="muChildOnly"),prior(exponential(1),class=sd, dpar="muMini"))

M4<-brm(Scale~1 +  (1| Continent) + (1|Society) + (1|ref)+ (1|randomNumber), family=categorical(),prior=prior, data = d, iter=5000, cores=4, control=list(adapt_delta=0.99) )

post4<-posterior_samples(M4)

post4<-post4%>% dplyr::select(starts_with("b_mu"))
post4$AdultVersion<-0

post4<-as.data.frame(softmax(post4))

round(mean(post4$b_muMini_Intercept),2)*100
round(mean(post4$b_muChildOnly_Intercept),2)*100
round(mean(post4$AdultVersion),2)*100

###################
###Raw materials###
###################

d_pres<-subset(d, !is.na(PresLikelihood))

nrow(d_pres) ##202
length(unique(d_pres$Society)) ##46

##Make dataframe of materials
m<-d_pres%>%select(toolName,materialMetal,materialPlant,materialStone,materialBone,materialAntler,materialOther)

##Recode yes into 1 and no into 0
m <- m %>%
  mutate_at(vars(materialMetal,materialPlant,materialStone,materialBone,materialAntler,materialOther), ~ifelse(. == "YES", 1, 0))

##Calculate number of materials used per object
sum_m<-rowSums(m[,-1])
round(mean(sum_m),2) ##1.29
round(sd(sum_m),2) ##0.51
range(sum_m) ##1-3

##Make dataset for pie chart
cm<-as.data.frame(colSums(m[,-1]))
colnames(cm)[1] <- "value"
cm$Names<-c("Metal","Plant","Stone","Bone","Antler","Other")
cm<-subset(cm,Names!="Antler")

cm$Percent<-paste(cm$Names,paste(round(cm$value/sum(cm$value)*100),"%",sep=""))
pie(cm$value,labels=cm$Percent,col=grey.colors(length(cm$Percent)))

##################
###Preservation###
##################

prior<-c(prior(normal(0,1),class=Intercept),prior(exponential(1),class=sd))

M5<-brm(PresLikelihood~1 +  (1| Continent) + (1|Society) + (1|ref)+ (1|randomNumber), family=bernoulli(),prior=prior, data = d_pres, iter=5000, cores=4, control=list(adapt_delta=0.99) )

post5<-posterior_samples(M5)

mean(inv_logit(post5$b_Intercept))

################
###Complexity###
################
d_comp<-subset(d, !is.na(simpleComp))

nrow(d_comp) ##330
length(unique(d_comp$Society)) ##all 54

M6<-brm(simpleComp~1 +  (1| Continent) + (1|Society) + (1|ref)+ (1|randomNumber), family=bernoulli(),prior=prior, data = d_comp, iter=5000, cores=4, control=list(adapt_delta=0.99) )

post6<-posterior_samples(M6)
round(mean(1-inv_logit(post6$b_Intercept)),2)*100


######################
###Manufacturer Age###
######################

d_man_age<-subset(d, !is.na(manufacturerAge))

nrow(d_man_age) ##99
length(unique(d_man_age$Society)) ##32

prior<-c(prior(normal(0,1),class=Intercept),prior(exponential(1),class=sd, dpar="muCHILD"),prior(exponential(1),class=sd, dpar="muBOTH"))

M7<-brm(manufacturerAge~1 +  (1| Continent) + (1|Society) + (1|ref)+ (1|randomNumber), family=categorical(), prior=prior, data = d_man_age, iter=5000, cores=4, control=list(adapt_delta=0.99) )

post7<-posterior_samples(M7)

post7<-post7%>% dplyr::select(starts_with("b_mu"))
post7$Adult<-0

post7<-as.data.frame(softmax(post7))

round(mean(post7$b_muCHILD_Intercept),2)*100
round(mean(post7$b_muBOTH_Intercept),2)*100
round(mean(post7$Adult),2)*100

fig3d_mean<-c(mean(post7$b_muCHILD_Intercept)*100,
              mean(post7$b_muBOTH_Intercept)*100,
              mean(post7$Adult)*100)
              
fig3d_PI<-cbind(PI(post7$b_muCHILD_Intercept)*100,
              PI(post7$b_muBOTH_Intercept)*100,
              PI(post7$Adult)*100)    

fig3d_title<-c("Child","Both Child\n & Adult","Adult")

fig3d_data<-as.data.frame(t(rbind(fig3d_title,fig3d_mean,fig3d_PI)))

fig3d_data$fig3d_title<-factor(fig3d_data$fig3d_title,levels=c("Both Child\n & Adult","Adult","Child"))
fig3d_data$fig3d_mean<-as.numeric(fig3d_data$fig3d_mean)
fig3d_data$`5%`<-as.numeric(fig3d_data$`5%`)
fig3d_data$`94%`<-as.numeric(fig3d_data$`94%`)

fig3d<-ggplot(fig3d_data, aes(x = fig3d_title, y = fig3d_mean)) + geom_linerange(aes(x = fig3d_title,  ymin = `5%`, ymax = `94%`), lwd = 1, colour = gray(1/2))+ geom_point(aes(x = fig3d_title, y = fig3d_mean),size=3.5)   + scale_y_continuous(limits=c(0,100),breaks=c(0,25,50,75,100),labels=c("0%","25%","50%","75%","100%"))+coord_flip() + theme_bw(base_size=18)+theme(axis.title.y=element_blank())+labs(y="(D) Manufacturer Age Category")

fig3d

#########################
###Manufacturer Gender###
#########################

d_man_gen<-subset(d, !is.na(manufacturerSex))
nrow(d_man_gen) ##98
length(unique(d_man_gen$Society)) ##32

prior<-c(prior(normal(0,1),class=Intercept),prior(exponential(1),class=sd, dpar="muFEMALE"),prior(exponential(1),class=sd, dpar="muMALE"))

M8<-brm(manufacturerSex~1 +  (1| Continent) + (1|Society) + (1|ref)+ (1|randomNumber), family=categorical(), prior=prior, data = d_man_gen, iter=5000, cores=4, control=list(adapt_delta=0.99) )

post8<-posterior_samples(M8)

post8<-post8%>% dplyr::select(starts_with("b_mu"))
post8$both<-0

post8<-as.data.frame(softmax(post8))

round(mean(post8$b_muFEMALE_Intercept),2)*100
round(mean(post8$b_muMALE_Intercept),2)*100
round(mean(post8$both),2)*100

#################
###Same Gender###
#################

omg<-subset(d_man_gen,Gender!="Unknown")
omg<-subset(omg,manufacturerSex!="Unknown")

nrow(omg) ##79 objects
length(unique(omg$Society)) ##30 societies

omg$sexmatch<-ifelse(omg$Gender=="Boys"&omg$manufacturerSex=="MALE","Same Gender",ifelse(omg$Gender=="Girls"&omg$manufacturerSex=="FEMALE","Same Gender",ifelse(omg$Gender=="Both"&omg$manufacturerSex=="BOTH","Same Gender","Opposite Gender")))

prior<-c(prior(normal(0,1),class=Intercept),prior(exponential(1),class=sd))

M9<-brm(sexmatch~1 +  (1| Continent) + (1|Society) + (1|ref)+ (1|randomNumber), family=bernoulli(),prior=prior, data = omg, iter=5000, cores=4, control=list(adapt_delta=0.99) )

post9<-posterior_samples(M9)

round(mean(inv_logit(post9$b_Intercept)),2)*100


fig3e_mean<-c(mean(post8$b_muFEMALE_Intercept)*100,
              mean(post8$b_muMALE_Intercept)*100,
             mean(post8$both)*100,
             mean(inv_logit(post9$b_Intercept))*100)

fig3e_PI<-cbind(PI(post8$b_muFEMALE_Intercept)*100,
              PI(post8$b_muMALE_Intercept)*100,
              PI(post8$both)*100,
              PI(inv_logit(post9$b_Intercept))*100)

fig3e_title<-c("Girls/Women","Boys/Men","Both Girls/Women\n & Boys/Men","Same\n Gender")

fig3e_data<-as.data.frame(t(rbind(fig3e_title,fig3e_mean,fig3e_PI)))

fig3e_data$fig3e_title<-factor(fig3e_data$fig3e_title,levels=c("Same\n Gender","Both Girls/Women\n & Boys/Men","Boys/Men","Girls/Women"))
fig3e_data$fig3e_mean<-as.numeric(fig3e_data$fig3e_mean)
fig3e_data$`5%`<-as.numeric(fig3e_data$`5%`)
fig3e_data$`94%`<-as.numeric(fig3e_data$`94%`)

fig3e<-ggplot(fig3e_data, aes(x = fig3e_title, y = fig3e_mean)) + geom_linerange(aes(x = fig3e_title,  ymin = `5%`, ymax = `94%`), lwd = 1, colour = gray(1/2))+ geom_point(aes(x = fig3e_title, y = fig3e_mean),size=3.5)   + scale_y_continuous(limits=c(0,100),breaks=c(0,25,50,75,100),labels=c("0%","25%","50%","75%","100%"))+coord_flip() + theme_bw(base_size=18)+theme(axis.title.y=element_blank())+labs(y="(E) Manufacturer Gender")

fig3e

##############
###Learning###
##############
##Make dataset
l<-subset(d,learn=="YES")

nrow(l) ##35
length(unique(l$Society)) ##16

##Recode mode & process
l$Mechanism<-recode(l$Mode,`COLLABORATIVE LEARNING`="Collaborative",`COLLABORATIVE LEARNING PLAY`="Collaborative",`COLLABORATIVE LEARNING-ROLE PLAYING`="Collaborative",`TEACHING-ENCOURAGEMENT`="Teaching",`TEACHING-INSTRUCTION`="Teaching",`TEACHING-OPPORTUNITY PROVISONING`="Teaching",`TEACHING-STORYTELLING`="Teaching",`TEACHING-UNSPECIFIED`="Teaching",`OBSERVATION-IMITATION`="Observation/Imitation",UNKNOWN="Unknown")

l$Pathway<-recode(l$Process,HORIZONTAL="Horizontal",OBLIQUE="Oblique",`OBLIQUE-PRESTIGE`="Oblique",UNKNOWN="Unknown",VERTICAL="Vertical")

##Who transmits?
table(l$Pathway)
round((table(l$Pathway)/nrow(l))*100)

##How do they transmit?
table(l$Mechanism)
round((table(l$Mechanism)/nrow(l))*100)


##Reorder the factors for beauty
l$Pathway <- ordered(l$Pathway, levels = c("Vertical", "Horizontal", "Oblique","Unknown"))
l$Mechanism<-ordered(l$Mechanism,levels=c("Teaching","Collaborative","Observation/Imitation","Unknown"))

table(l$Pathway,l$Mechanism)

##Make Mosaic Plot
plot5<-ggplot(data = l) +
  geom_mosaic(aes(x = product(Pathway, Mechanism), fill=Pathway),show.legend = FALSE) 
plot5<-plot5+theme_classic()+scale_fill_grey()
plot5<-plot5+ theme(axis.text.x = element_text(angle = 45, hjust=1))
plot5

################
###Figure 3C###
###############


fig3c_mean<-c(mean(1-inv_logit(post3$b_Intercept))*100,
              mean(post4$b_muMini_Intercept)*100,
              mean(post4$b_muChildOnly_Intercept)*100,
              mean(post4$AdultVersion)*100,
              mean(inv_logit(post5$b_Intercept))*100,
              mean(1-inv_logit(post6$b_Intercept))*100)
              
              
fig3c_PI<-cbind(PI(1-inv_logit(post3$b_Intercept))*100,
              PI(post4$b_muMini_Intercept)*100,
              PI(post4$b_muChildOnly_Intercept)*100,
              PI(post4$AdultVersion)*100,
              PI(inv_logit(post5$b_Intercept))*100,
              PI(1-inv_logit(post6$b_Intercept))*100)

fig3c_title<-c("Play","Miniatures","Child\n Only","Adult\n Versions","High\n Preservation","Composite")

fig3c_data<-as.data.frame(t(rbind(fig3c_title,fig3c_mean,fig3c_PI)))

fig3c_data$fig3c_title<-factor(fig3c_data$fig3c_title,levels=c("Composite","High\n Preservation","Adult\n Versions","Child\n Only","Miniatures","Play"))
fig3c_data$fig3c_mean<-as.numeric(fig3c_data$fig3c_mean)
fig3c_data$`5%`<-as.numeric(fig3c_data$`5%`)
fig3c_data$`94%`<-as.numeric(fig3c_data$`94%`)

fig3c<-ggplot(fig3c_data, aes(x = fig3c_title, y = fig3c_mean)) + geom_linerange(aes(x = fig3c_title,  ymin = `5%`, ymax = `94%`), lwd = 1, colour = gray(1/2))+ geom_point(aes(x = fig3c_title, y = fig3c_mean),size=3.5)   + scale_y_continuous(limits=c(0,100),breaks=c(0,25,50,75,100),labels=c("0%","25%","50%","75%","100%"))+coord_flip() + theme_bw(base_size=18)+theme(axis.title.y=element_blank())+labs(y="(C) Object Characteristics")

fig3c

#######################
###Figure 3 composed###
#######################

col1<-ggarrange(fig3a,fig3b,ncol=1,align="v")
col3<-ggarrange(fig3d,fig3e,ncol=1, align="v")

ggarrange(col1,fig3c,col3,ncol=3) ##save 7 X 17"

#########################
###Supplementary Model###
#########################

sd<-d%>%group_by(randomNumber)%>%summarise(count=sum(count),ref=unique(ref),Society=unique(Society), Continent=unique(Continent),Pages=unique(Pages),Date=unique(Date))

sd$Pages_z<-standardize(sd$Pages)
sd$Date_z<-standardize(sd$Date)

prior<-c(prior(normal(0,1),class=Intercept),prior(normal(0,1),class=b),prior(exponential(1),class=sd))

SM1<-brm(count~Pages_z +Date_z +  (1| Continent) + (1|Society) + (1|ref)+ (1|randomNumber),prior=prior,family=poisson(), data = sd, iter=10000, cores=4, control=list(adapt_delta=0.99))

summary(SM1,prob=0.89)

pp_check(SM1)

figs1<-conditional_effects(SM1, effect="Date_z", prob=0.89)

plot(figs1, plot=FALSE)[[1]]+ylab("Number of Objects")+xlab("Publication Year (z-score standardized)")


###########################
##Random effect estimates##
###########################

summary(M1, prob=0.89)
summary(M2, prob=0.89)
summary(M3, prob=0.89)
summary(M4, prob=0.89)
summary(M5, prob=0.89)
summary(M6, prob=0.89)
summary(M7, prob=0.89)
summary(M8, prob=0.89)
summary(M9, prob=0.89)
summary(SM1, prob=0.89)
