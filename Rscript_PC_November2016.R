##################################################################################################
####SCREENING INTERVIEW - November 22, 2016                                                      #
##################################################################################################

##In this script: Analyses for screening interview 
#R version: 3.1.2 (Pumpkin Helmet)

##Last update: Nov. 20, 2016
##Github: 

##################################################################################################

##################################################################################################
####PACKAGES                                                                                     #
##################################################################################################
library(permute)
library(vegan)
library(boot)
library(rich)
library(Iso)
library(vegan)
library(plyr)
library(reshape)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(party) 
library(rpart.plot) 
library(partykit)
library(tree) 

##################################################################################################

##################################################################################################
####WORKING DIRECTORY                                                                            #
##################################################################################################
setwd("C:/Users/Winegardner/Dropbox/ParksCanada_PC2")

##################################################################################################

##################################################################################################
####DATA                                                                                         #
##################################################################################################

##Fish community data
fish.raw<- read.csv("superior_wetlands_fish.csv")

summary(fish.raw)


##Wetland env data
env.raw<- read.csv("superior_wetlands_env.csv")

summary(env.raw)

##################################################################################################

##################################################################################################
####ENVIRONMENTAL VARIABLES                                                                      #
##################################################################################################

##Tracking environmental change through time 

#TEMPERATURE

temp.time<- ggplot(env.raw, aes(x=Year, y=TEMP)) + geom_point(size=4) + geom_path() + facet_wrap(~Site, nrow=2)
temp.time<- temp.time + labs(x = "Sampling year", y = "Temperature (deg C)") + theme_bw() 
temp.time<- temp.time + theme(axis.text.x = element_text(colour="black", size=16))
temp.time<- temp.time + theme(axis.text.y = element_text(colour="black", size=16))
temp.time<- temp.time + theme(axis.title.x = element_text(size = rel(2), angle=00))
temp.time<- temp.time + theme(axis.title.y = element_text(size = rel(2), angle=90)) 
temp.time<- temp.time + theme(strip.text.x = element_text(size=25, face="bold"), panel.background=element_rect(fill="white")) 

#TN
TN.time<- ggplot(env.raw, aes(x=Year, y=TN)) + geom_point(size=4) + geom_path() + facet_wrap(~Site, nrow=2)
TN.time<- TN.time + labs(x = "Sampling year", y = "Total Nitrogen (mg/L)") + theme_bw() 
TN.time<- TN.time + theme(axis.text.x = element_text(colour="black", size=16))
TN.time<- TN.time + theme(axis.text.y = element_text(colour="black", size=16))
TN.time<- TN.time + theme(axis.title.x = element_text(size = rel(2), angle=00))
TN.time<- TN.time + theme(axis.title.y = element_text(size = rel(2), angle=90)) 
TN.time<- TN.time + theme(strip.text.x = element_text(size=25, face="bold"), panel.background=element_rect(fill="white")) 

#TP
TP.time<- ggplot(env.raw, aes(x=Year, y=TP)) + geom_point(size=4) + geom_path() + facet_wrap(~Site, nrow=2)
TP.time<- TP.time + labs(x = "Sampling year", y = "Total Phosphorus (ug/L)") + theme_bw() 
TP.time<- TP.time + theme(axis.text.x = element_text(colour="black", size=16))
TP.time<- TP.time + theme(axis.text.y = element_text(colour="black", size=16))
TP.time<- TP.time + theme(axis.title.x = element_text(size = rel(2), angle=00))
TP.time<- TP.time + theme(axis.title.y = element_text(size = rel(2), angle=90)) 
TP.time<- TP.time + theme(strip.text.x = element_text(size=25, face="bold"), panel.background=element_rect(fill="white")) 


#COND
cond.time<- ggplot(env.raw, aes(x=Year, y=COND)) + geom_point(size=4) + geom_path() + facet_wrap(~Site, nrow=2)
cond.time<- cond.time + labs(x = "Sampling year", y = "Specific conductivity (uS/cm)") + theme_bw() 
cond.time<- cond.time + theme(axis.text.x = element_text(colour="black", size=16))
cond.time<- cond.time + theme(axis.text.y = element_text(colour="black", size=16))
cond.time<- cond.time + theme(axis.title.x = element_text(size = rel(2), angle=00))
cond.time<- cond.time + theme(axis.title.y = element_text(size = rel(2), angle=90)) 
cond.time<- cond.time + theme(strip.text.x = element_text(size=25, face="bold"), panel.background=element_rect(fill="white")) 


#CHL A
chla.time<- ggplot(env.raw, aes(x=Year, y=CHL)) + geom_point(size=4) + geom_path() + facet_wrap(~Site, nrow=2)
chla.time<- chla.time + labs(x = "Sampling year", y = "Chlorophyll a (ug/L)") + theme_bw() #Units not specified in data, assume ug/L
chla.time<- chla.time + theme(axis.text.x = element_text(colour="black", size=16))
chla.time<- chla.time + theme(axis.text.y = element_text(colour="black", size=16))
chla.time<- chla.time + theme(axis.title.x = element_text(size = rel(2), angle=00))
chla.time<- chla.time + theme(axis.title.y = element_text(size = rel(2), angle=90)) 
chla.time<- chla.time + theme(strip.text.x = element_text(size=25, face="bold"), panel.background=element_rect(fill="white")) 


#pH
pH.time<- ggplot(env.raw, aes(x=Year, y=pH)) + geom_point(size=4) + geom_path() + facet_wrap(~Site, nrow=2)
pH.time<- pH.time + labs(x = "Sampling year", y = "pH") + theme_bw() #Units not specified in data, assume ug/L
pH.time<- pH.time + theme(axis.text.x = element_text(colour="black", size=16))
pH.time<- pH.time + theme(axis.text.y = element_text(colour="black", size=16))
pH.time<- pH.time + theme(axis.title.x = element_text(size = rel(2), angle=00))
pH.time<- pH.time + theme(axis.title.y = element_text(size = rel(2), angle=90)) 
pH.time<- pH.time + theme(strip.text.x = element_text(size=25, face="bold"), panel.background=element_rect(fill="white")) 


#TURB
turb.time<- ggplot(env.raw, aes(x=Year, y=TURB)) + geom_point(size=4) + geom_path() + facet_wrap(~Site, nrow=2)
turb.time<- turb.time + labs(x = "Sampling year", y = "Turbidity (NTU)") + theme_bw() 
turb.time<- turb.time + theme(axis.text.x = element_text(colour="black", size=16))
turb.time<- turb.time + theme(axis.text.y = element_text(colour="black", size=16))
turb.time<- turb.time + theme(axis.title.x = element_text(size = rel(2), angle=00))
turb.time<- turb.time + theme(axis.title.y = element_text(size = rel(2), angle=90)) 
turb.time<- turb.time + theme(strip.text.x = element_text(size=25, face="bold"), panel.background=element_rect(fill="white")) 


##PCA of environmental variables 
#PCA + standardize env variables
env.pca<- rda(decostand(env.raw[,3:9], "standardize"))

#PCA scores and plots
#Species scores (env variables)
env.pca.spscr<- as.data.frame(scores(env.pca, choices=1:2, display="sp"))
colnames(env.pca.spscr)<- c("Env_Sp_PC1", "Env_Sp_PC2")

#Site scores 
env.pca.sitescr<- as.data.frame(scores(env.pca, choices=1:2, display="sites"))
colnames(env.pca.sitescr)<- c("Env_Site_PC1", "Env_Site_PC2")
#Bind site scores with site information 
env.pca.sitescr<- as.data.frame(cbind(env.raw[,1:2], env.pca.sitescr))

#PCA plot - with years 
env.pca.plot<- ggplot()
env.pca.plot<- env.pca.plot + geom_vline(x=0,colour="grey50")
env.pca.plot<- env.pca.plot + geom_hline(y=0,colour="grey50") 
env.pca.plot<- env.pca.plot + geom_segment(data=env.pca.spscr, aes(x=0, y=0, xend=Env_Sp_PC1, yend=Env_Sp_PC2), size=0.7, colour="black") 
env.pca.plot<- env.pca.plot + geom_text(data=env.pca.spscr, aes(x=Env_Sp_PC1, y=Env_Sp_PC2, label=rownames(env.pca.spscr)), size=6, colour="black")
env.pca.plot<- env.pca.plot + geom_text(data=env.pca.sitescr, aes(x=Env_Site_PC1, y=Env_Site_PC2, label=env.pca.sitescr$Year, colour=env.pca.sitescr$Site), size=6)
env.pca.plot<- env.pca.plot + scale_colour_manual(values= c("CD" = "#1b9e77", "CV" = "#d95f02", "HE" = "#7570b3", "HS" = "#e7298a", "LN" = "#66a61e"))  
env.pca.plot<- env.pca.plot + labs(x= "Env PC1 = 0.34", y= "Env PC2 = 0.21")+ theme_bw()  
env.pca.plot<- env.pca.plot + theme(axis.text.x = element_text(colour="black", size=16))
env.pca.plot<- env.pca.plot + theme(axis.text.y = element_text(colour="black", size=16))
env.pca.plot<- env.pca.plot + theme(axis.title.x = element_text(size = rel(2), angle=00))
env.pca.plot<- env.pca.plot + theme(axis.title.y = element_text(size = rel(2), angle=90)) 

#PCA plot - with trajectories 
env.pca.plot2<- ggplot()
env.pca.plot2<- env.pca.plot2 + geom_vline(x=0,colour="grey50")
env.pca.plot2<- env.pca.plot2 + geom_hline(y=0,colour="grey50") 
env.pca.plot2<- env.pca.plot2 + geom_segment(data=env.pca.spscr, aes(x=0, y=0, xend=Env_Sp_PC1, yend=Env_Sp_PC2), size=0.7, colour="black") 
env.pca.plot2<- env.pca.plot2 + geom_text(data=env.pca.spscr, aes(x=Env_Sp_PC1, y=Env_Sp_PC2, label=rownames(env.pca.spscr)), size=6, colour="black")
env.pca.plot2<- env.pca.plot2 + geom_point(data=env.pca.sitescr, aes(x=Env_Site_PC1, y=Env_Site_PC2, colour=env.pca.sitescr$Site), size=4)
env.pca.plot2<- env.pca.plot2 + scale_colour_manual(values= c("CD" = "#1b9e77", "CV" = "#d95f02", "HE" = "#7570b3", "HS" = "#e7298a", "LN" = "#66a61e"))  
env.pca.plot2<- env.pca.plot2 + labs(x= "Env PC1 = 0.34", y= "Env PC2 = 0.21")+ theme_bw()  
env.pca.plot2<- env.pca.plot2 + theme(axis.text.x = element_text(colour="black", size=16))
env.pca.plot2<- env.pca.plot2 + theme(axis.text.y = element_text(colour="black", size=16))
env.pca.plot2<- env.pca.plot2 + theme(axis.title.x = element_text(size = rel(2), angle=00))
env.pca.plot2<- env.pca.plot2 + theme(axis.title.y = element_text(size = rel(2), angle=90)) 
env.pca.plot2<- env.pca.plot2 + geom_path(data = env.pca.sitescr, aes(x = Env_Site_PC1, y= Env_Site_PC2, colour= env.pca.sitescr$Site), size =0.2, arrow=arrow(length= unit(0.4,"cm"), ends="first", type="open" ))
        

##################################################################################################

##################################################################################################
####FISH                                                                                         #
##################################################################################################

##Rarefied richness for fish species
#Merge first 2 columns so have unique rownames
fish.raw$SiteID<- paste(fish.raw$Site, fish.raw$Year) #SiteID now as last column 

#Rarefy
row.names(fish.raw) <- fish.raw[,41]
fish.red<- fish.raw[,-1] #Remove Site column
fish.red<- fish.red[,-1] #Remove Year
fish.red<- fish.red[,-39] #SiteID column

#Run the rarefy function wich uses the fish abundance table and the least number of individuals caught in all the lakes i.e. "min(rowSums(fish.red))"
Srar <- rarefy(fish.red, min(rowSums(fish.red)))
Srar
Srar<-as.data.frame(Srar)

#Bind Srar back with a separate Site and Year column
Srar.comb<- as.data.frame(cbind(fish.raw$Site, fish.raw$Year, Srar))
colnames(Srar.comb)<- c("Site", "Year", "Srar")

#Boxplot of rarefied richness across sites --> showing variation across the years
srar.boxplot<- ggplot(Srar.comb, aes(x=Site, y=Srar, fill=Site)) +geom_boxplot()
srar.boxplot<- srar.boxplot + labs(x = "Site", y = "Rarefied species richness") + theme_bw() 
srar.boxplot<- srar.boxplot + theme(axis.text.x = element_text(colour="black", size=16))
srar.boxplot<- srar.boxplot + theme(axis.text.y = element_text(colour="black", size=16))
srar.boxplot<- srar.boxplot + theme(axis.title.x = element_text(size = rel(2), angle=00))
srar.boxplot<- srar.boxplot + theme(axis.title.y = element_text(size = rel(2), angle=90))
srar.boxplot<- srar.boxplot + scale_fill_manual(values= c("CD" = "#1b9e77", "CV" = "#d95f02", "HE" = "#7570b3", "HS" = "#e7298a", "LN" = "#66a61e")) 
srar.boxplot<- srar.boxplot + theme(legend.position="none")


##Fish prevlance through time 
#Melt into long-form
fish.raw.long<- melt(fish.raw, id=c("Site", "Year", "SiteID"))
colnames(fish.raw.long)<- c("Site", "Year", "SiteID", "Fish", "Count")

allfish.facet<- ggplot(fish.raw.long, aes(x=Year, y=Count, colour=Site)) + geom_point() + facet_wrap(~Fish)

#Reduced fish set--> focus fish for analyses 
#RG- invasive
#LT- community objective
#BLC, BTN, CSH, MSH- variation across sites, smaller 

fish.long.red<- as.data.frame(subset(fish.raw.long, Fish == "RG" | Fish == "LT" | Fish == "BLC" | Fish == "BTN" | Fish == "CSH" | Fish == "MSH", drop=T))

subsetfish.facet<- ggplot(fish.long.red, aes(x=Year, y=Count, shape=Fish, colour=Fish)) + geom_point(size=3) + geom_path() + facet_wrap(~Site)
subsetfish.facet<- subsetfish.facet + labs(x = "Sampling year", y = "Species count") + theme_bw()
subsetfish.facet<- subsetfish.facet + scale_colour_manual(values = c("BLC" = "black", "BTN" = "black", "CSH" = "black", "LT" = "darkblue", "MSH" = "black", "RG" = "red"))
subsetfish.facet<- subsetfish.facet + theme(axis.text.x = element_text(colour="black", size=16))
subsetfish.facet<- subsetfish.facet + theme(axis.text.y = element_text(colour="black", size=16))
subsetfish.facet<- subsetfish.facet + theme(axis.title.x = element_text(size = rel(2), angle=00))
subsetfish.facet<- subsetfish.facet + theme(axis.title.y = element_text(size = rel(2), angle=90)) 
subsetfish.facet<- subsetfish.facet + theme(strip.text.x = element_text(size=25, face="bold"), panel.background=element_rect(fill="white"))


##Fish RDA
#Explanatory variables = env + round goby abundance

env.rg<- as.data.frame(cbind(env.raw, fish.raw$RG))
colnames(env.rg)[10]<- 'RG'

#Fish data = all but round goby (raw)
fish.no.rg<- subset(fish.raw, select = -c(RG))

#RDA 
#Standardize env variables 
#Hellinger transform fish 

env.rg.std<- as.data.frame(decostand(env.rg[,3:10], "standardize"))
fish.no.rg.hell<- decostand(fish.no.rg[,3:39], "hellinger")

fish.rda<- rda(fish.no.rg.hell ~ TEMP + TN + TP + COND + CHL + pH + TURB + RG, env.rg.std)

#Scores
#Species scores (fish)
fish.rda.spscr<- as.data.frame(scores(fish.rda, choices=1:2, display="sp"))
colnames(fish.rda.spscr)<- c("Fish_Sp_RDA1", "Fish_Sp_RDA2")

#Site scores (sites)
fish.rda.sitscr<- as.data.frame(scores(fish.rda, choices=1:2, display="sites"))
colnames(fish.rda.sitscr)<- c("Fish_Site_RDA1", "Fish_Site_RDA2")

#BP scores (env variables)
fish.rda.bpscr<- as.data.frame(scores(fish.rda, choices=1:2, display="bp"))
colnames(fish.rda.bpscr)<- c("Fish_Env_RDA1", "Fish_Env_RDA2")

#Bind site scores with site information 
fish.rda.sitscr<- as.data.frame(cbind(fish.raw[,1:2], fish.rda.sitscr))

#RDA- backs up line graphs --> including ALL fish species 
fish.rda.plot<- ggplot()
fish.rda.plot<- fish.rda.plot + geom_vline(x=0,colour="grey50")
fish.rda.plot<- fish.rda.plot + geom_hline(y=0,colour="grey50") 
fish.rda.plot<- fish.rda.plot + geom_segment(data=fish.rda.bpscr, aes(x=0, y=0, xend=Fish_Env_RDA1, yend=Fish_Env_RDA2), size=0.7, colour="black") 
fish.rda.plot<- fish.rda.plot + geom_text(data=fish.rda.bpscr, aes(x=Fish_Env_RDA1, y=Fish_Env_RDA2, label=rownames(fish.rda.bpscr)), size=6, colour="black")
#fish.rda.plot<- fish.rda.plot + geom_text(data=fish.rda.sitscr, aes(x=Fish_Site_RDA1, y=Fish_Site_RDA2, label=fish.rda.sitscr$Year, colour=fish.rda.sitscr$Site), size=6)
fish.rda.plot<- fish.rda.plot + geom_point(data=fish.rda.sitscr, aes(x=Fish_Site_RDA1, y=Fish_Site_RDA2, colour=fish.rda.sitscr$Site, shape=fish.rda.sitscr$Site), size=6)
fish.rda.plot<- fish.rda.plot + scale_colour_manual(values= c("CD" = "#1b9e77", "CV" = "#d95f02", "HE" = "#7570b3", "HS" = "#e7298a", "LN" = "#66a61e"))  
fish.rda.plot<- fish.rda.plot + labs(x= "RDA1 = 19% variation explained", y= "RDA2 = 6% variation explained")+ theme_bw()  
fish.rda.plot<- fish.rda.plot + theme(axis.text.x = element_text(colour="black", size=16))
fish.rda.plot<- fish.rda.plot + theme(axis.text.y = element_text(colour="black", size=16))
fish.rda.plot<- fish.rda.plot + theme(axis.title.x = element_text(size = rel(2), angle=00))
fish.rda.plot<- fish.rda.plot + theme(axis.title.y = element_text(size = rel(2), angle=90)) 
fish.rda.plot<- fish.rda.plot + geom_text(data=fish.rda.spscr, aes(x=Fish_Sp_RDA1, y=Fish_Sp_RDA2, label=rownames(fish.rda.spscr)), size=4, colour="black")


#Univariate regression tree
goby<- as.data.frame(cbind(env.raw, fish.raw$RG))
colnames(goby)[10]<- 'RG'
goby<- as.data.frame(goby[,3:10])

goby.fit<- rpart(RG~TEMP + TN + TP + COND + CHL + pH + TURB, method="anova", data=goby)
printcp(goby.fit) #Variables actually used in construction
plot(goby.fit)
summary(goby.fit)
goby.fit.prune<-  prune(goby.fit, cp=goby.fit$cptable[which.min(goby.fit$cptable[,"xerror"]),"CP"])
summary(goby.fit.prune)

plot(as.party(goby.fit), tp_args = list(id = FALSE))
##################################################################################################





