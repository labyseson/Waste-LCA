library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(rgdal)
library(scales)
library(choroplethr)
library(choroplethrMaps)
library(psych)
library(RColorBrewer)
library(compare)


#setwd("C:/Users/person/Dropbox/R related/GC")

#import datasets
waste_county_raw <- read.csv("./data/billionton_county_all/billionton_county_wastes.csv", stringsAsFactors=FALSE)
ag_county_raw <- read.csv("./data/billionton_county_all/billionton_county_agriculture_basecase.csv", stringsAsFactors=FALSE)
coefficients <- read.csv("./data/Coefficients_new_bioCO2.csv", stringsAsFactors=FALSE)
coefficients1 <- read.csv("./data/Coefficients_new_bioCO2_25.csv", stringsAsFactors=FALSE)
coefficients2 <- read.csv("./data/Coefficients_new_bioCO2_50.csv", stringsAsFactors=FALSE)
coefficients3 <- read.csv("./data/Coefficients_new_bioCO2_100.csv", stringsAsFactors=FALSE)
tech <- read.csv("./data/tech_feasi.csv")
current <- read.csv("./data/current_per.csv")
current1 <- read.csv("./data/current_per1.csv")
power_LCA <- read.csv("./data/NREL_LCA.csv")
state_GHG_ef <- read.csv("./data/state_GHG_ef.csv")
state_GHG_ef1 <- read.csv("./data/state_GHG_ef_1.csv")
state_GHG_ef2 <- read.csv("./data/state_GHG_ef_2.csv")
state_name <- read.csv("./data/state_name_mapping.csv")

waste_county <- subset(waste_county_raw, Year == 2015 & Biomass.Price == 100)
ag_residue_county <- subset(ag_county_raw, Feedstock %in% c("Barley straw", "Corn stover", "Oats straw", "Wheat straw", 
                                                            "Sorghum stubble") & Year == 2015 & Biomass.Price == 100)
waste_all <- rbind(waste_county, ag_residue_county)

#unit conversion, 1 BDT = 0.907 metric tons
waste_all$Prod <- as.numeric(waste_all$Production) * 0.907
waste_all$Feedstock[waste_all$Feedstock == "Other"] <- "Other MSW"
waste_all$Crop.Type[waste_all$Crop.Type == "Manure"] <- "Animal Manure"
waste_all$Crop.Type[waste_all$Crop.Type == "Food Waste"] <- "MSW"

#Resource mapping
## set output directory
County_FOLDER <- "figures_05212019"
dir.create(County_FOLDER)

#Adding categories for dry weight
colnames(waste_all)[23] <- "region"

waste_county_total <- aggregate(Prod ~ State + County + region, waste_all, FUN = sum)
waste_county_total$value[waste_county_total$Prod < 5000] <- "< 5 000"
waste_county_total$value[waste_county_total$Prod >= 5000 & waste_county_total$Prod  < 10000] <- "5 000 ~ 10 000"
waste_county_total$value[waste_county_total$Prod >= 10000 & waste_county_total$Prod  < 20000] <- "10 000 ~ 20 000"
waste_county_total$value[waste_county_total$Prod >= 20000 & waste_county_total$Prod  < 50000] <- "20 000 ~ 50 000"
waste_county_total$value[waste_county_total$Prod >= 50000 & waste_county_total$Prod  < 100000] <- "50 000 ~ 100 000"
waste_county_total$value[waste_county_total$Prod >= 100000 & waste_county_total$Prod  < 500000] <- "100 000 ~ 500 000"
waste_county_total$value[waste_county_total$Prod >= 500000 & waste_county_total$Prod  < 1000000] <- "500 000 ~ 1 000 000"
waste_county_total$value[waste_county_total$Prod >= 1000000 & waste_county_total$Prod  < 2000000] <- "1 000 000 ~ 2 000 000"
waste_county_total$value[waste_county_total$Prod >= 2000000] <- "> 2 000 000"

waste_county_total$value <- factor(waste_county_total$value, 
                                   levels = c("< 5 000", "5 000 ~ 10 000", "10 000 ~ 20 000", "20 000 ~ 50 000", "50 000 ~ 100 000", "100 000 ~ 500 000", 
                                              "500 000 ~ 1 000 000", "1 000 000 ~ 2 000 000", "> 2 000 000"))

waste_county_type <- aggregate(Prod ~ State + County + region + Crop.Type, waste_all, FUN = sum)

waste_county_ag <- subset(waste_county_type, Crop.Type == "Ag Residues")
waste_county_ag$value[waste_county_ag$Prod < 500] <- "< 500"
waste_county_ag$value[waste_county_ag$Prod >= 500 & waste_county_ag$Prod  < 1000] <- "500 ~ 1 000"
waste_county_ag$value[waste_county_ag$Prod >= 1000 & waste_county_ag$Prod  < 5000] <- "1 000 ~ 5 000"
waste_county_ag$value[waste_county_ag$Prod >= 5000 & waste_county_ag$Prod  < 10000] <- "5 000 ~ 10 000"
waste_county_ag$value[waste_county_ag$Prod >= 10000 & waste_county_ag$Prod  < 50000] <- "10 000 ~ 50 000"
waste_county_ag$value[waste_county_ag$Prod >= 50000 & waste_county_ag$Prod  < 100000] <- "50 000 ~ 100 000"
waste_county_ag$value[waste_county_ag$Prod >= 100000 & waste_county_ag$Prod  < 500000] <- "100 000 ~ 500 000"
waste_county_ag$value[waste_county_ag$Prod >= 500000 & waste_county_ag$Prod  < 1000000] <- "500 000 ~ 1 000 000"
waste_county_ag$value[waste_county_ag$Prod >= 1000000] <- "> 1 000 000"
waste_county_ag$value <- factor(waste_county_ag$value, 
                                levels = c("< 500", "500 ~ 1 000", "1 000 ~ 5 000", "5 000 ~ 10 000", "10 000 ~ 50 000", 
                                           "50 000 ~ 100 000", "100 000 ~ 500 000", "500 000 ~ 1 000 000", "> 1 000 000"))

waste_county_mn <- subset(waste_county_type, Crop.Type == "Animal Manure")
waste_county_mn$value[waste_county_mn$Prod < 500] <- "< 500"
waste_county_mn$value[waste_county_mn$Prod >= 500 & waste_county_mn$Prod  < 1000] <- "500 ~ 1 000"
waste_county_mn$value[waste_county_mn$Prod >= 1000 & waste_county_mn$Prod  < 5000] <- "1 000 ~ 5 000"
waste_county_mn$value[waste_county_mn$Prod >= 5000 & waste_county_mn$Prod  < 10000] <- "5 000 ~ 10 000"
waste_county_mn$value[waste_county_mn$Prod >= 10000 & waste_county_mn$Prod  < 50000] <- "10 000 ~ 50 000"
waste_county_mn$value[waste_county_mn$Prod >= 50000 & waste_county_mn$Prod  < 100000] <- "50 000 ~ 100 000"
waste_county_mn$value[waste_county_mn$Prod >= 100000 & waste_county_mn$Prod  < 200000] <- "100 000 ~ 200 000"
waste_county_mn$value[waste_county_mn$Prod >= 200000 & waste_county_mn$Prod  < 500000] <- "200 000 ~ 500 000"
waste_county_mn$value[waste_county_mn$Prod >= 500000] <- "> 500 000"
waste_county_mn$value <- factor(waste_county_mn$value, 
                                levels = c("< 500", "500 ~ 1 000", "1 000 ~ 5 000", "5 000 ~ 10 000", "10 000 ~ 50 000", 
                                           "50 000 ~ 100 000", "100 000 ~ 200 000", "200 000 ~ 500 000", "> 500 000"))

waste_county_fr <- subset(waste_county_type, Crop.Type == "Forest Residues")
waste_county_fr$value[waste_county_fr$Prod < 500] <- "< 500"
waste_county_fr$value[waste_county_fr$Prod >= 500 & waste_county_fr$Prod  < 1000] <- "500 ~ 1 000"
waste_county_fr$value[waste_county_fr$Prod >= 1000 & waste_county_fr$Prod  < 5000] <- "1 000 ~ 5 000"
waste_county_fr$value[waste_county_fr$Prod >= 5000 & waste_county_fr$Prod  < 10000] <- "5 000 ~ 10 000"
waste_county_fr$value[waste_county_fr$Prod >= 10000 & waste_county_fr$Prod  < 20000] <- "10 000 ~ 20 000"
waste_county_fr$value[waste_county_fr$Prod >= 20000 & waste_county_fr$Prod  < 50000] <- "20 000 ~ 50 000"
waste_county_fr$value[waste_county_fr$Prod >= 50000 & waste_county_fr$Prod  < 100000] <- "50 000 ~ 100 000"
waste_county_fr$value[waste_county_fr$Prod >= 100000 & waste_county_fr$Prod  < 200000] <- "100 000 ~ 200 000"
waste_county_fr$value[waste_county_fr$Prod >= 200000] <- "> 200 000"
waste_county_fr$value <- factor(waste_county_fr$value, 
                                levels = c("< 500", "500 ~ 1 000", "1 000 ~ 5 000", "5 000 ~ 10 000", "10 000 ~ 20 000",
                                           "20 000 ~ 50 000", "50 000 ~ 100 000", "100 000 ~ 200 000", "> 200 000"))

waste_county_msw <- subset(waste_county_type, Crop.Type == "MSW")
waste_county_msw$value[waste_county_msw$Prod < 1000] <- "< 1 000"
waste_county_msw$value[waste_county_msw$Prod >= 1000 & waste_county_msw$Prod  < 5000] <- "1 000 ~ 5 000"
waste_county_msw$value[waste_county_msw$Prod >= 5000 & waste_county_msw$Prod  < 10000] <- "5 000 ~ 10 000"
waste_county_msw$value[waste_county_msw$Prod >= 10000 & waste_county_msw$Prod  < 50000] <- "10 000 ~ 50 000"
waste_county_msw$value[waste_county_msw$Prod >= 50000 & waste_county_msw$Prod  < 100000] <- "50 000 ~ 100 000"
waste_county_msw$value[waste_county_msw$Prod >= 100000 & waste_county_msw$Prod  < 500000] <- "100 000 ~ 500 000"
waste_county_msw$value[waste_county_msw$Prod >= 500000 & waste_county_msw$Prod  < 1000000] <- "500 000 ~ 1 000 000"
waste_county_msw$value[waste_county_msw$Prod >= 1000000 & waste_county_msw$Prod  < 2000000] <- "1 000 000 ~ 2 000 000"
waste_county_msw$value[waste_county_msw$Prod >= 2000000] <- "> 2 000 000"
waste_county_msw$value <- factor(waste_county_msw$value, 
                                 levels = c("< 1 000", "1 000 ~ 5 000", "5 000 ~ 10 000", "10 000 ~ 50 000", "50 000 ~ 100 000", 
                                            "100 000 ~ 500 000", "500 000 ~ 1 000 000", "1 000 000 ~ 2 000 000", "> 2 000 000"))


#Mapping by resource type

## US total
data(continental_us_states)
p <- county_choropleth(waste_county_total, state_zoom =  continental_us_states) +
  scale_fill_brewer(palette = 1) +
  guides(fill = guide_legend(title = "Mg, dry weight", title.theme = element_text(size = 18, face = "bold", angle = 0),
                             label.theme = element_text(size = 16, angle = 0))) +
  ggtitle("(a) Technically available total waste biomass resources, 2015") +
  theme(plot.title = element_text(face = "bold", size = 20, vjust = -3))
ggsave(paste(County_FOLDER, "/County_total_2015.png", sep=""), plot=p, width=10,height=4,units="in",dpi=300)


## by waste type - with legend
p <- county_choropleth(waste_county_ag, state_zoom = continental_us_states) +
  scale_fill_brewer(palette = "YlOrBr", na.value="white") +
  guides(fill = guide_legend(title = "Mg, dry weight", title.theme = element_text(size = 18, face = "bold", angle = 0),
                             label.theme = element_text(size = 16, angle = 0))) +
  ggtitle("(a) Technically available agricultural residues, 2015") +
  theme(plot.title = element_text(face = "bold", size = 20, vjust = -3))
ggsave(paste(County_FOLDER, "/County_ag_2015.png", sep=""), plot=p, width=10,height=4,units="in",dpi=300)

p <- county_choropleth(waste_county_mn, state_zoom = continental_us_states) +
  scale_fill_brewer(palette = "PuRd", na.value="white") +
  guides(fill = guide_legend(title = "Mg, dry weight", title.theme = element_text(size = 18, face = "bold", angle = 0),
                             label.theme = element_text(size = 16, angle = 0))) +
  ggtitle("(b) Technically available animal manure, 2015") +
  theme(plot.title = element_text(face = "bold", size = 20, vjust = -3))
ggsave(paste(County_FOLDER, "/County_mn_2015.png", sep=""), plot=p, width=10,height=4,units="in",dpi=300)

p <- county_choropleth(waste_county_fr, state_zoom = continental_us_states) +
  scale_fill_brewer(palette = 2, na.value="white") +
  guides(fill = guide_legend(title = "Mg, dry weight", title.theme = element_text(size = 18, face = "bold", angle = 0),
                             label.theme = element_text(size = 16, angle = 0))) +
  ggtitle("(c) Technically available forest residues, 2015") +
  theme(plot.title = element_text(face = "bold", size = 20, vjust = -3))
ggsave(paste(County_FOLDER, "/County_fr_2015.png", sep=""), plot=p, width=10,height=4,units="in",dpi=300)

p <- county_choropleth(waste_county_msw, state_zoom = continental_us_states) +
  scale_fill_brewer(palette = "Reds", na.value="white") +
  guides(fill = guide_legend(title = "Mg, dry weight", title.theme = element_text(size = 18, face = "bold", angle = 0),
                             label.theme = element_text(size = 16, angle = 0))) +
  ggtitle("(d) Technically available municipal solid waste, 2015") +
  theme(plot.title = element_text(face = "bold", size = 20, vjust = -3))
ggsave(paste(County_FOLDER, "/County_msw_2015.png", sep=""), plot=p, width=10,height=4,units="in",dpi=300)


## by waste type - without legend
p <- county_choropleth(waste_county_total, state_zoom =  continental_us_states) +
  scale_fill_brewer(palette = 1) +
  guides(fill = FALSE) +
  ggtitle("(a) Technically available total waste biomass resources") +
  theme(plot.title = element_text(face = "bold", size = 20, vjust = -3))
ggsave(paste(County_FOLDER, "/County_total_2015_1.png", sep=""), plot=p, width=8,height=4,units="in",dpi=300)

p <- county_choropleth(waste_county_ag, state_zoom = continental_us_states) +
  scale_fill_brewer(palette = "YlOrBr", na.value="white") +
  guides(fill = FALSE) +
  ggtitle("(a) Technically available agricultural residues") +
  theme(plot.title = element_text(face = "bold", size = 20, vjust = -3))
ggsave(paste(County_FOLDER, "/County_ag_2015_1.png", sep=""), plot=p, width=8,height=4,units="in",dpi=300)

p <- county_choropleth(waste_county_mn, state_zoom = continental_us_states) +
  scale_fill_brewer(palette = "PuRd", na.value="white") +
  guides(fill = FALSE) +
  ggtitle("(b) Technically available animal manure") +
  theme(plot.title = element_text(face = "bold", size = 20, vjust = -3))
ggsave(paste(County_FOLDER, "/County_mn_2015_1.png", sep=""), plot=p, width=8,height=4,units="in",dpi=300)

p <- county_choropleth(waste_county_fr, state_zoom = continental_us_states) +
  scale_fill_brewer(palette = 2, na.value="white") +
  guides(fill = FALSE) +
  ggtitle("(c) Technically available forest residues") +
  theme(plot.title = element_text(face = "bold", size = 20, vjust = -3))
ggsave(paste(County_FOLDER, "/County_fr_2015_1.png", sep=""), plot=p, width=8,height=4,units="in",dpi=300)

p <- county_choropleth(waste_county_msw, state_zoom = continental_us_states) +
  scale_fill_brewer(palette = "Reds", na.value="white") +
  guides(fill = FALSE) +
  ggtitle("(d) Technically available municipal solid waste") +
  theme(plot.title = element_text(face = "bold", size = 20, vjust = -3))
ggsave(paste(County_FOLDER, "/County_msw_2015_1.png", sep=""), plot=p, width=8,height=4,units="in",dpi=300)


#total resource curve 

waste_county_total_1 <- waste_county_total[order(-waste_county_total$Prod),]
waste_county_total_1$accsum <- cumsum(waste_county_total_1$Prod)
waste_county_total_1$count <- 1
waste_county_total_1$top <- cumsum(waste_county_total_1$count)
waste_county_total_1$top1 <-  waste_county_total_1$top / 3109 *100
waste_county_total_1$percent <- waste_county_total_1$accsum / sum(waste_county_total_1$Prod) * 100


p <- ggplot(waste_county_total_1, aes(x = top1, y = percent)) + 
  geom_line(linetype = 1) +
  theme_bw() +
  theme(text = element_text(size=20)) +
  scale_y_continuous(name="Percent (%) of total waste resources") +
  scale_x_continuous(name="Percent (%) of total number of counties") +
  ggtitle("(b) Distribution") +
  theme(plot.title = element_text(face = "bold", size = 20))
ggsave(paste(County_FOLDER, "/Resources_county_curve.png", sep=""), plot=p, width=6,height=6,units="in",dpi=300)

#pick a county for dataset 
waste_all$county_full <- paste(waste_all$County, waste_all$State, sep = ", ")
c_occur <- data.frame(table(waste_all$county_full))
waste_Tulare <- subset(waste_all, county_full == "Tulare County, California")
waste_SJC <- subset(waste_all, county_full == "San Joaquin County, California")
waste_Stanislaus <- subset(waste_all, county_full == "Stanislaus County, California")

write.csv(waste_Tulare, file = "./Results/waste_Tulare.csv")