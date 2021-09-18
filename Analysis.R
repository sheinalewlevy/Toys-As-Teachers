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


###############################
###General Descriptive Stats###
###############################

##import the dataset
d<-read_excel("data_pub.xlsx",na="NA")

##Look at child age names
unique(d$childAge_coded)

##Recode and rename variables to make them more appealing
d$Age<-ifelse(d$childAge_coded=="ADOLESCENCE"|d$childAge_coded=="MIDDLE CHILDHOOD"|d$childAge_coded=="OLD","OLD","YOUNG")

d$Age<-recode(d$Age, YOUNG="Approx. ≤6 years", OLD="Approx. ≥7 years")

d$Gender<-recode(d$Gender, GIRLS="Girls", BOYS="Boys",BOTH="Both")

d$Society<-as.factor(d$Society)

##Number of unique paragraphs
length(unique(d$randomNumber)) ##272 paragraphs

##number of objects
nrow(d) ##434 objects

##number of societies
length(unique(d$Society)) ##54

##tally the number of tools per culture, and generate descriptive stats
total<-d%>%group_by(Society) %>%tally()
round(mean(total$n),2) ##8.04
round(sd(total$n),2) ##8.07
range(total$n) ##1-38

##Regions
round(table(d$Continent)/nrow(d),2)*100 ##60% of objects come from North America

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
  geom_sf(data=points,color="black",fill="red",pch=21,size=2)

rm(societies)
rm(points)
rm(world)

##########################################
###Sex differences in number of objects###
##########################################

##Calculate how many objects have the sex of the user reported
s<-d%>%group_by(Society,Gender)%>%tally()%>%na.omit() 

sum(s$n) ##302 objects had information on the gender of the user
round(sum(s$n)/nrow(d),2)*100 #for 70% of objects
length(unique(s$Society)) ##from 54 societies

##Spread dataset
spread_s<-spread(s,Gender,n,fill=0)
sum(spread_s$Girls)##93 objects used exclusively by girls
round(sum(spread_s$Girls)/sum(s$n),2)*100 ##31%
sum(spread_s$Boys)##171 objects used exclusively by boys
round(sum(spread_s$Boys)/sum(s$n),2)*100 ##57%
sum(spread_s$Both)##38 objects used by both
round(sum(spread_s$Both)/sum(s$n),2)*100 ##13%

##Look at the number of societies for each ratio
spread_s$ratio_mf<-spread_s$Girls/spread_s$Boys
sum(spread_s$ratio_mf>1) ##6
sum(spread_s$ratio_mf==1) ##9
sum(spread_s$ratio_mf<1) ##39

##Reorder Culture by the ratio
s$Society<-factor(s$Society,levels=spread_s$Society[order(spread_s$ratio_mf)], ordered=TRUE)
s$Gender<-factor(s$Gender,levels=c("Girls","Boys","Both"))

##Make figure 2 TOP
plot1<-ggplot(s, aes(x=Society, y=n, fill=Gender)) + geom_bar(stat="identity",position=position_dodge(preserve = "single"),width=0.7)+scale_fill_manual(values=c("#000000", "#E69F00", "#56B4E9"))+geom_vline(xintercept = "Canela",linetype="dashed")+geom_vline(xintercept = "Warao",linetype="dashed")+annotate(x="Aleut",y=+Inf,label="Boys>Girls",vjust=2,geom="label")+annotate(x="Nuxalk",y=+Inf,label="Girls=Boys",vjust=2,geom="label")+annotate(x="Klamath",y=+Inf,label="Girls>Boys",vjust=2,geom="label")
plot1<-plot1+ylab("Number of Objects (n=302)")+xlab("Society (n=54)")
plot1<-plot1+theme_classic()
plot1<-plot1+theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1))
plot1
rm(s)
rm(spread_s)

##########################################
###Age differences in number of objects###
##########################################

##Make dataframe from objects by Age
a<-d%>%group_by(Society,Age)%>%tally()%>%na.omit()

##How many objects have age data?
sum(a$n) ##65
round(sum(a$n)/nrow(d),2)*100 ##15%

##How many societies have age info?
length(unique(a$Society)) ##25

##Make spread
spread_a<-spread(a,Age,n,fill=0)

##How many objects are for older and younger children?
sum(spread_a$`Approx. ≤6 years`) ##37 for younger
round(sum(spread_a$`Approx. ≤6 years`)/sum(a$n),2)*100 ##57% for younger
sum(spread_a$`Approx. ≥7 years`) ##28 for older
round(sum(spread_a$`Approx. ≥7 years`)/sum(a$n),2)*100 ##43% for older

##Look at the number of societies for each proportion
spread_a$ratio<-spread_a$`Approx. ≤6 years`/spread_a$`Approx. ≥7 years`
sum(spread_a$ratio>1) ##14
sum(spread_a$ratio==1) ##1
sum(spread_a$ratio<1) ##10

##Reorder and rename variables for the figure
a$Society<-factor(a$Society,levels=spread_a$Society[order(-spread_a$ratio)], ordered=TRUE)

##Make figure 2 BOTTOM
plot2<-ggplot(a,aes(x=Society,y=n,fill=Age))+ geom_bar(stat="identity",position=position_dodge(preserve = "single"),width=0.7)+ scale_fill_manual(values=c("#B4B4B4FF","#0062B4FF"))+geom_vline(xintercept = "Tlingit",linetype="dashed")+annotate(x="San",y=+Inf,label="Younger>Older",vjust=2,geom="label")+annotate(x="Kaska",y=+Inf,label="Older>Younger",vjust=2,geom="label")
plot2<-plot2+ylab("Number of Objects (n=65)")+xlab("Society (n=25)")
plot2<-plot2+theme_classic()
plot2<-plot2+theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1))
plot2

##Put the figures together
fig2<-ggarrange(plot1, plot2, nrow=2,ncol=1)
fig2

rm(a)
rm(spread_a)

##################
###Object Types###
##################
##Recode types so that we have enough in each category
d$Type_recode<-recode(d$Type,`Animal Figure`="Figures",`Human figure`="Figures",Instrument="Subsistence",`Tended facility`="Subsistence",`Untended facility`="Subsistence",Game="Games",PhysGame="Games")

###Frequency and percent for each object type
table(d$Type_recode)
t3a<-as.data.frame(table(d$Type_recode))
round(table(d$Type_recode)/nrow(d),2)*100

##Frequency by sex
table(d$Type_recode, d$Gender)
t3b<-as.data.frame.matrix(table(d$Type_recode, d$Gender))

##Frequency and percent for each scale type
table(d$Scale)
round(table(d$Scale)/nrow(d),2)*100
table(d$Type_recode,d$Scale)
t3c<-as.data.frame.matrix(table(d$Type_recode,d$Scale))

sc<-d%>%group_by(Society,Scale)%>%tally()%>%na.omit()
spread_sc<-spread(sc,Scale,n,fill=0)

spread_sc$ratio_mc<-spread_sc$Mini/spread_sc$ChildOnly
sum(spread_sc$ratio_mc>1,na.rm=T) ##30
sum(spread_sc$ratio_mc==1,na.rm=T) ##8
sum(spread_sc$ratio_mc<1,na.rm=T) ##15

sum(spread_sc$AdultVersion==0) ##34 societies have no adult version reported

##Rename columns for aesthtic reasons
sc$Scale<-recode(sc$Scale, Mini="Miniature", AdultVersion="Adult Version",ChildOnly="Child Only")

##Reorder Culture by the ratio
sc$Society<-factor(sc$Society,levels=spread_sc$Society[order(spread_sc$ratio_mc)], ordered=TRUE)

##Make figure 3
plot3<-ggplot(sc, aes(x=Society, y=n, fill=Scale)) + geom_bar(stat="identity",position=position_dodge(preserve = "single"),width=0.7)+scale_fill_manual(values=c("#000000", "#E69F00", "#56B4E9"))+geom_vline(xintercept = "Aranda",linetype="dashed")+annotate(x="Nuxalk",y=+Inf,label="Child Only>Miniature",vjust=2,geom="label")+geom_vline(xintercept = "Yuki",linetype="dashed")+annotate(x="Kaska",y=+Inf,label="Child Only=Miniature",vjust=2,geom="label")+annotate(x="Canela",y=+Inf,label="Miniature>Child Only",vjust=2,geom="label")
plot3<-plot3+ylab("Number of Objects (n=434)")+xlab("Society (n=54)")
plot3<-plot3+theme_classic()
plot3<-plot3+theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1))
plot3

rm(sc)
rm(spread_sc)

###############
###Materials###
###############
##How many objects have material descriptions?
sum(d$materialDescribed=="YES") ##202
round((sum(d$materialDescribed=="YES")/nrow(d))*100,0) ##47

##Make dataframe of materials
m<-subset(d,materialDescribed=="YES")
m<-m%>%select(toolName,materialMetal,materialPlant,materialStone,materialBone,materialAntler,materialOther)

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
pie(cm$value,labels=cm$Percent,col=rainbow(length(cm$Percent)))

rm(m)
rm(cm)
rm(sum_m)

m<-subset(d,materialDescribed=="YES")
table(m$PresLikelihood) ##178 objects unlikely to preserve
round((table(m$PresLikelihood)/nrow(m))*100,0) ##88%

table(m$PresLikelihood,m$Type_recode) ##No category was much more likely to preserve
t3d<-as.data.frame.matrix(table(m$Type_recode,m$PresLikelihood))
rm(m)

#################
###Composition###
#################
sum(!is.na(d$simpleComp)) ##330 objects had data on composition
round(sum(!is.na(d$simpleComp))/nrow(d),2)*100 ##76% of all objects

cs<-d%>%group_by(Society,simpleComp)%>%tally()%>%na.omit()
length(unique(cs$Society)) ##54 societies

table(d$simpleComp) ##206 comp
round(table(d$simpleComp)/sum(!is.na(d$simpleComp)),2)*100 ##62%
table(d$Type_recode,d$simpleComp)
t3e<-as.data.frame.matrix(table(d$Type_recode,d$simpleComp))

spread_cs<-spread(cs,simpleComp,n,fill=0)

spread_cs$ratio_cs<-spread_cs$Simple/spread_cs$Comp
sum(spread_cs$ratio_cs>1,na.rm=T) ##10
sum(spread_cs$ratio_cs==1,na.rm=T) ##9
sum(spread_cs$ratio_cs<1,na.rm=T) ##35

##Reorder Culture by the ratio
cs$Society<-factor(cs$Society,levels=spread_cs$Society[order(spread_cs$ratio_cs)], ordered=TRUE)
cs$simpleComp<-ordered(cs$simpleComp,levels=c("Simple","Comp"))
cs$simpleComp<-recode(cs$simpleComp,Comp="Composite")
cs<-cs%>%rename(Complexity=simpleComp)

##Make figure 5
plot5<-ggplot(cs, aes(x=Society, y=n, fill=Complexity)) + geom_bar(stat="identity",position=position_dodge(preserve = "single"),width=0.7)+scale_fill_manual(values=c("#B4B4B4FF","#0062B4FF"))+geom_vline(xintercept = "Barama River Carib",linetype="dashed")+annotate(x="Crow",y=+Inf,label="Composite>Simple",vjust=2,geom="label")+geom_vline(xintercept = "Ticuna",linetype="dashed")+annotate(x="Manus",y=+Inf,label="Composite=Simple",vjust=2,geom="label")+annotate(x="Kiribati",y=+Inf,label="Simple>Composite",vjust=2,geom="label")
plot5<-plot5+ylab("Number of Objects (n=330)")+xlab("Society (n=54)")
plot5<-plot5+theme_classic()
plot5<-plot5+theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1))
plot5

rm(cs)
rm(spread_cs)

###################
###Manufacturers###
###################
##Number of objects with info on tool manufacturer
sum(table(d$manufacturerAge))
round((sum(table(d$manufacturerAge))/nrow(d))*100,0)

##Make dataset 
om<-subset(d,!is.na(d$manufacturerAge))
nrow(om)
length(unique(om$Society)) ##32 societies

##Recode
om$manufacturerSex[is.na(om$manufacturerSex)]<-"Unknown"
om$manufacturerAge<-recode(om$manufacturerAge,CHILD="Child",ADULT="Adult",BOTH="Both")
om$manufacturerSex<-recode(om$manufacturerSex,MALE="Male",FEMALE="Female",BOTH="Both")
om$manufacturerKin[is.na(om$manufacturerKin)]<-"Unknown"
om$Gender[is.na(om$Gender)]<-"Unknown"

##Objects manufacturer info
table(om$manufacturerAge)
round((table(om$manufacturerAge)/sum(table(om$manufacturerAge)))*100)

table(om$manufacturerSex)
round((table(om$manufacturerSex)/sum(table(om$manufacturerSex)))*100)

##Calculate sex match
omg<-subset(om,Gender!="Unknown")
omg<-subset(omg,manufacturerSex!="Unknown")
nrow(omg) ##79 objects
length(unique(omg$Society)) ##30 societies
omg$sexmatch<-ifelse(omg$Gender=="Boys"&omg$manufacturerSex=="Male","Same Gender",ifelse(omg$Gender=="Girls"&omg$manufacturerSex=="Female","Same Gender",ifelse(omg$Gender=="BOTH"&omg$manufacturerSex=="Both","Same Gender","Opposite Gender")))

table(omg$sexmatch)
round((table(omg$sexmatch)/sum(table(omg$sexmatch)))*100)

##Make datasets for figure
OM1<-om%>%group_by(Society,manufacturerAge)%>%tally()%>%na.omit()
OM2<-om%>%group_by(Society,manufacturerSex)%>%tally()%>%na.omit()

spread_OM1<-spread(OM1,manufacturerAge,n,fill=0)
spread_OM2<-spread(OM2,manufacturerSex,n,fill=0)

##Calculate ratios for child vs. adult manufacturers
spread_OM1$ratio<-spread_OM1$Child/spread_OM1$Adult
spread_OM1$ratio[is.na(spread_OM1$ratio)]<-1
sum(spread_OM1$ratio>1,na.rm=T) ##18
sum(spread_OM1$ratio==1,na.rm=T) ##6
sum(spread_OM1$ratio<1,na.rm=T) ##8

##Calculate ratios for f vs. m manufacturers
spread_OM2$ratio<-spread_OM2$Female/spread_OM2$Male
spread_OM2$ratio[is.na(spread_OM2$ratio)]<-1
sum(spread_OM2$ratio>1,na.rm=T) ##11
sum(spread_OM2$ratio==1,na.rm=T) ##8
sum(spread_OM2$ratio<1,na.rm=T) ##13

##Make Figure
OM1$Society<-factor(OM1$Society,levels=spread_OM1$Society[order(spread_OM1$ratio)], ordered=TRUE)
OM1$`Manufacturer Age`<-factor(OM1$manufacturerAge,levels=c("Adult","Child","Both"))

plot6<-ggplot(OM1,aes(x=Society,y=n,fill=`Manufacturer Age`))+ geom_bar(stat="identity",position=position_dodge(preserve = "single"),width=0.7)+ scale_fill_manual(values=c("red","blue","green","purple"))+geom_vline(xintercept = "Bororo",linetype="dashed")+annotate(x="Nivkh",y=+Inf,label="Adult>Child",vjust=2,geom="label")+geom_vline(xintercept = "Yuki",linetype="dashed")+annotate(x="Manus",y=+Inf,label="Adult=Child",vjust=2,geom="label")+annotate(x="Hadza",y=+Inf,label="Child>Adult",vjust=2,geom="label")
plot6<-plot6+ylab("Number of Objects (n=99)")+xlab("Society (n=32)") 
plot6<-plot6+theme_classic()
plot6<-plot6+theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1))
plot6


OM2$Society<-factor(OM2$Society,levels=spread_OM2$Society[order(spread_OM2$ratio)], ordered=TRUE)
OM2$`Manufacturer Sex`<-factor(OM2$manufacturerSex,levels=c("Male","Female","Both","Unknown"))
OM2<-subset(OM2,`Manufacturer Sex`!="Unknown")
sum(OM2$n)

plot7<-ggplot(OM2,aes(x=Society,y=n,fill=`Manufacturer Sex`))+ geom_bar(stat="identity",position=position_dodge(preserve = "single"),width=0.7)+ scale_fill_manual(values=c("red","blue","green","purple"))+geom_vline(xintercept = "Aranda",linetype="dashed")+annotate(x="Mataco",y=+Inf,label="Male>Female",vjust=2,geom="label")+geom_vline(xintercept = "Vedda",linetype="dashed")+annotate(x="Kaska",y=+Inf,label="Male=Female",vjust=2,geom="label")+annotate(x="Canela",y=+Inf,label="Female>Male",vjust=2,geom="label")
plot7<-plot7+ylab("Number of Objects (n=98)")+xlab("Society (n=32)") 
plot7<-plot7+theme_classic()
plot7<-plot7+theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1))
plot7

Fig3<-ggarrange(plot6, plot7, nrow=2,ncol=1)
Fig3

##############
###Learning###
##############
##How many objects are embedded within teaching?
sum(d$learn=="YES",na.rm=T) ##35

##Make dataset
l<-subset(d,learn=="YES")

##Number of societies
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

##Make Mosaic Plot
plot7<-ggplot(data = l) +
  geom_mosaic(aes(x = product(Pathway, Mechanism), fill=Pathway)) 
plot7<-plot7+theme_classic()
plot7<-plot7+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot7

#############
###Table 3###
#############
table3<-bind_cols(t3a,t3b, t3c,t3d,t3e)
write.csv(table3,"table3.csv")

