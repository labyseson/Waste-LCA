#Author: Bo Liu, UCLA

#reshape dataset 
waste_scenario <- subset(waste_2, select = c(State, County, region, Waste_type, Feedstock, Prod_ww,
                                             E1_energy,E2_energy,E3_energy,E4_energy,M1_energy,M2_energy,Eth1_energy, Rd1_energy, 
                                             Rd2_energy,Bj1_energy,Bj2_energy,Bj3_energy,Bj4_energy,Bj5_energy,Bj6_energy,
                                             E1_netenergy,E2_netenergy,E3_netenergy,E4_netenergy,M1_netenergy,M2_netenergy,Eth1_netenergy, Rd1_netenergy, 
                                             Rd2_netenergy,Bj1_netenergy,Bj2_netenergy,Bj3_netenergy,Bj4_netenergy,Bj5_netenergy,Bj6_netenergy,
                                             E1_netemiss,E2_netemiss,E3_netemiss,E4_netemiss,M1_netemiss,M2_netemiss,Eth1_netemiss, Rd1_netemiss, 
                                             Rd2_netemiss,Bj1_netemiss,Bj2_netemiss,Bj3_netemiss,Bj4_netemiss,Bj5_netemiss,Bj6_netemiss))

waste_scenario_1 <- gather(waste_scenario, temp, value, E1_energy:Bj6_netemiss)

waste_scenario_2 <- waste_scenario_1 %>% 
                               separate(temp, c("Tech", "Category"), "_")

waste_scenario_3 <-  spread(waste_scenario_2, key = Category, value = value)

waste_results <-  waste_scenario_3[which(waste_scenario_3$energy != 0),]


#MEP map
MEP <- aggregate(energy ~ State + County + region + Waste_type + Feedstock, waste_results, FUN=max)
MEP <- merge(MEP, waste_results)
MEP <- subset(MEP, Tech != "Bj6")

sum(MEP$energy)
sum(MEP$netenergy)
sum(MEP$netemiss)

MEP_energy <- aggregate(energy ~ State + County + region, MEP, FUN=sum)
MEP_energy$TJ <- MEP_energy$energy / 1000
MEP_energy$value[MEP_energy$TJ < 100] <- "< 100"
MEP_energy$value[MEP_energy$TJ >= 100 & MEP_energy$TJ  < 500] <- "100 ~ 500"
MEP_energy$value[MEP_energy$TJ >= 500 & MEP_energy$TJ  < 1000] <- "500 ~ 1,000"
MEP_energy$value[MEP_energy$TJ >= 1000 & MEP_energy$TJ  < 2000] <- "1,000 ~ 2,000"
MEP_energy$value[MEP_energy$TJ >= 2000 & MEP_energy$TJ  < 5000] <- "2,000 ~ 5,000"
MEP_energy$value[MEP_energy$TJ >= 5000 & MEP_energy$TJ  < 10000] <- "5,000 ~ 10,000"
MEP_energy$value[MEP_energy$TJ >= 10000 & MEP_energy$TJ  < 20000] <- "10,000 ~ 20,000"
MEP_energy$value[MEP_energy$TJ >= 20000 & MEP_energy$TJ  < 40000] <- "20,000 ~ 40,000"
MEP_energy$value[MEP_energy$TJ >= 40000] <- "> 40,000"
MEP_energy$value <- factor(MEP_energy$value, 
                           levels = c("< 100", "100 ~ 500", "500 ~ 1,000", "1,000 ~ 2,000", 
                                      "2,000 ~ 5,000", "5,000 ~ 10,000","10,000 ~ 20,000", "20,000 ~ 40,000", "> 40,000"))
data(continental_us_states)
p <- county_choropleth(MEP_energy, state_zoom = continental_us_states) +
  scale_fill_brewer(palette = "Purples") +
  guides(fill = guide_legend(title = "Terajoule (TJ)", title.theme = element_text(size = 18, face = "bold", angle = 0), 
                             label.theme = element_text(size = 16, angle = 0))) +
  ggtitle("(a) Renewable Energy production - MEP Scenario") +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0))
ggsave(paste(County_FOLDER, "/MEP_energy.png", sep=""), plot=p, width=8,height=4,units="in",dpi=300)

p <- county_choropleth(MEP_energy, state_zoom = continental_us_states) +
  scale_fill_brewer(palette = "Purples") +
  guides(fill = FALSE) +
  ggtitle("(a) Renewable Energy production - MEP Scenario") +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0))
ggsave(paste(County_FOLDER, "/MEP_energy1.png", sep=""), plot=p, width=6.5,height=4,units="in",dpi=300)


MEP_netenergy <- aggregate(netenergy ~ State + County + region, MEP, FUN=sum)
MEP_netenergy$TJ <- MEP_netenergy$netenergy / 1000
MEP_netenergy$value[MEP_netenergy$TJ < -1000] <- "< -1,000"
MEP_netenergy$value[MEP_netenergy$TJ >= -1000 & MEP_netenergy$TJ  < 0] <- "-1,000 ~ 0"
MEP_netenergy$value[MEP_netenergy$TJ >= 0 & MEP_netenergy$TJ  < 500] <- "0 ~ 500"
MEP_netenergy$value[MEP_netenergy$TJ >= 500 & MEP_netenergy$TJ  < 1000] <- "500 ~ 1,000"
MEP_netenergy$value[MEP_netenergy$TJ >= 1000 & MEP_netenergy$TJ  < 2000] <- "1,000 ~ 2,000"
MEP_netenergy$value[MEP_netenergy$TJ >= 2000 & MEP_netenergy$TJ  < 5000] <- "2,000 ~ 5,000"
MEP_netenergy$value[MEP_netenergy$TJ >= 5000 & MEP_netenergy$TJ  < 10000] <- "5,000 ~ 10,000"
MEP_netenergy$value[MEP_netenergy$TJ >= 10000 & MEP_netenergy$TJ  < 20000] <- "10,000 ~ 20,000"
MEP_netenergy$value[MEP_netenergy$TJ >= 20000 ] <- "> 20,000"

MEP_netenergy$value <- factor(MEP_netenergy$value, 
                              levels = c("< -1,000", "-1,000 ~ 0","0 ~ 500", "500 ~ 1,000", "1,000 ~ 2,000",
                                         "2,000 ~ 5,000", "5,000 ~ 10,000","10,000 ~ 20,000", "> 20,000"))

colors_1 <- c("< -1,000" = "#D73027", 
              "-1,000 ~ 0" = "#FEE090", 
              "0 ~ 500" = "#DEEBF7", 
              "500 ~ 1,000" = "#C6DBEF", 
              "1,000 ~ 2,000" = "#9ECAE1",
              "2,000 ~ 5,000" = "#6BAED6", 
              "5,000 ~ 10,000" = "#4292C6",
              "10,000 ~ 20,000" = "#2171B5", 
              "> 20,000" = "#08519C")
p <- county_choropleth(MEP_netenergy, state_zoom = continental_us_states) +
  scale_fill_manual(values=colors_1) +
  guides(fill = guide_legend(title = "Terajoule (TJ)", title.theme = element_text(size = 18, face = "bold", angle = 0), 
                             label.theme = element_text(size = 16, angle = 0))) +
  ggtitle("(b) Net energy - MEP Scenario") +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0))
ggsave(paste(County_FOLDER, "/MEP_netenergy.png", sep=""), plot=p, width=8,height=4,units="in",dpi=300)

p <- county_choropleth(MEP_netenergy, state_zoom = continental_us_states) +
  scale_fill_manual(values=colors_1) +
  guides(fill = FALSE) +
  ggtitle("(b) Net energy - MEP Scenario") +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0))
ggsave(paste(County_FOLDER, "/MEP_netenergy1.png", sep=""), plot=p, width=6.5,height=4,units="in",dpi=300)


MEP_netemiss <- aggregate(netemiss ~ State + County + region, MEP, FUN=sum)
MEP_netemiss$TMT <- MEP_netemiss$netemiss / 10^6
MEP_netemiss$value[MEP_netemiss$TMT  < -1000] <- "< -1,000"
MEP_netemiss$value[MEP_netemiss$TMT >= -1000 & MEP_netemiss$TMT  < -500] <- "-1,000 ~ -500"
MEP_netemiss$value[MEP_netemiss$TMT >= -500 & MEP_netemiss$TMT  < -200] <- "-500 ~ -200"
MEP_netemiss$value[MEP_netemiss$TMT >= -200 & MEP_netemiss$TMT  < -100] <- "-200 ~ -100"
MEP_netemiss$value[MEP_netemiss$TMT >= -100 & MEP_netemiss$TMT  < -50] <- "-100 ~ -50"
MEP_netemiss$value[MEP_netemiss$TMT >= -50 & MEP_netemiss$TMT  < 0] <- "-50 ~ 0"
MEP_netemiss$value[MEP_netemiss$TMT >= 0 & MEP_netemiss$TMT  < 50] <- "0 ~ 50"
MEP_netemiss$value[MEP_netemiss$TMT >= 50 & MEP_netemiss$TMT  < 200] <- "50 ~ 200"
MEP_netemiss$value[MEP_netemiss$TMT >= 200] <- "> 200"
MEP_netemiss$value <- factor(MEP_netemiss$value, 
                             levels = c("< -1,000", "-1,000 ~ -500","-500 ~ -200", "-200 ~ -100", "-100 ~ -50", 
                                        "-50 ~ 0", "0 ~ 50", "50 ~ 200", "> 200" ))
colors_2 <- c("< -1,000" = "#00441B", 
              "-1,000 ~ -500" = "#006D2C", 
              "-500 ~ -200" = "#238B45", 
              "-200 ~ -100" = "#41AB5D",
              "-100 ~ -50" = "#74C476", 
              "-50 ~ 0" = "#A1D99B",
              "0 ~ 50" = "#9970AB",
              "50 ~ 200" = "#762A83",
              "> 200" = "#40004B")
p <- county_choropleth(MEP_netemiss, state_zoom = continental_us_states) +
  scale_fill_manual(values=colors_2) +
  guides(fill = guide_legend(title = "Thousand metric \n ton CO2e", title.theme = element_text(size = 18, face = "bold", angle = 0), 
                             label.theme = element_text(size = 16, angle = 0))) +
  ggtitle("(c) GWP - MEP Scenario") + 
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0))
ggsave(paste(County_FOLDER, "/MEP_netemiss.png", sep=""), plot=p, width=8,height=4,units="in",dpi=300)

p <- county_choropleth(MEP_netemiss, state_zoom = continental_us_states) +
  scale_fill_manual(values=colors_2) +
  guides(fill = FALSE) +
  ggtitle("(c) GWP (w. biogenic CO2) - MEP Scenario") + 
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0))
ggsave(paste(County_FOLDER, "/MEP_netemiss1.png", sep=""), plot=p, width=6.5,height=4,units="in",dpi=300)

#caculate the number of counties 
MEP_netenergy_1 <- subset(MEP_netenergy, MEP_netenergy$netenergy < 0)
MEP_netemiss_1 <- subset(MEP_netemiss, MEP_netemiss$netemiss > 0)

#MNE map
MNE <- aggregate(netenergy ~ State + County + region + Waste_type +Feedstock, waste_results, FUN=max)
MNE <- merge(MNE, waste_results)
sum(MNE$energy)
sum(MNE$netenergy)
sum(MNE$netemiss)

MNE_energy <- aggregate(energy ~ State + County + region, MNE, FUN=sum)
MNE_energy$TJ <- MNE_energy$energy / 1000
MNE_energy$value[MNE_energy$TJ < 100] <- "< 100"
MNE_energy$value[MNE_energy$TJ >= 100 & MNE_energy$TJ  < 500] <- "100 ~ 500"
MNE_energy$value[MNE_energy$TJ >= 500 & MNE_energy$TJ  < 1000] <- "500 ~ 1,000"
MNE_energy$value[MNE_energy$TJ >= 1000 & MNE_energy$TJ  < 2000] <- "1,000 ~ 2,000"
MNE_energy$value[MNE_energy$TJ >= 2000 & MNE_energy$TJ  < 5000] <- "2,000 ~ 5,000"
MNE_energy$value[MNE_energy$TJ >= 5000 & MNE_energy$TJ  < 10000] <- "5,000 ~ 10,000"
MNE_energy$value[MNE_energy$TJ >= 10000 & MNE_energy$TJ  < 20000] <- "10,000 ~ 20,000"
MNE_energy$value[MNE_energy$TJ >= 20000 & MNE_energy$TJ  < 40000] <- "20,000 ~ 40,000"
MNE_energy$value[MNE_energy$TJ >= 40000] <- "> 40,000"
MNE_energy$value <- factor(MNE_energy$value, 
                           levels = c("< 100", "100 ~ 500", "500 ~ 1,000", "1,000 ~ 2,000", 
                                      "2,000 ~ 5,000", "5,000 ~ 10,000","10,000 ~ 20,000", "20,000 ~ 40,000", "> 40,000"))
data(continental_us_states)
p <- county_choropleth(MNE_energy, state_zoom = continental_us_states) +
  scale_fill_brewer(palette = "Purples") +
  guides(fill = guide_legend(title = "TJ", title.theme = element_text(size = 18, face = "bold", angle = 0), 
                             label.theme = element_text(size = 16, angle = 0))) +
  ggtitle("(d) Renewable energy production - MNE Scenario") +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0))
ggsave(paste(County_FOLDER, "/MNE_energy.png", sep=""), plot=p, width=8,height=4,units="in",dpi=300)

p <- county_choropleth(MNE_energy, state_zoom = continental_us_states) +
  scale_fill_brewer(palette = "Purples") +
  guides(fill = FALSE) +
  ggtitle("(d) Renewable energy production - MNE Scenario") +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0))
ggsave(paste(County_FOLDER, "/MNE_energy1.png", sep=""), plot=p, width=6.5,height=4,units="in",dpi=300)



MNE_netenergy <- aggregate(netenergy ~ State + County + region, MNE, FUN=sum)
MNE_netenergy$TJ <- MNE_netenergy$netenergy / 1000
MNE_netenergy$value[MNE_netenergy$TJ < -1000] <- "< -1,000"
MNE_netenergy$value[MNE_netenergy$TJ >= -1000 & MNE_netenergy$TJ  < 0] <- "-1,000 ~ 0"
MNE_netenergy$value[MNE_netenergy$TJ >= 0 & MNE_netenergy$TJ  < 500] <- "0 ~ 500"
MNE_netenergy$value[MNE_netenergy$TJ >= 500 & MNE_netenergy$TJ  < 1000] <- "500 ~ 1,000"
MNE_netenergy$value[MNE_netenergy$TJ >= 1000 & MNE_netenergy$TJ  < 2000] <- "1,000 ~ 2,000"
MNE_netenergy$value[MNE_netenergy$TJ >= 2000 & MNE_netenergy$TJ  < 5000] <- "2,000 ~ 5,000"
MNE_netenergy$value[MNE_netenergy$TJ >= 5000 & MNE_netenergy$TJ  < 10000] <- "5,000 ~ 10,000"
MNE_netenergy$value[MNE_netenergy$TJ >= 10000 & MNE_netenergy$TJ  < 20000] <- "10,000 ~ 20,000"
MNE_netenergy$value[MNE_netenergy$TJ >= 20000 ] <- "> 20,000"

MNE_netenergy$value <- factor(MNE_netenergy$value, 
                              levels = c("< -1,000", "-1,000 ~ 0","0 ~ 500", "500 ~ 1,000", "1,000 ~ 2,000",
                                         "2,000 ~ 5,000", "5,000 ~ 10,000","10,000 ~ 20,000", "> 20,000"))

p <- county_choropleth(MNE_netenergy, state_zoom = continental_us_states) +
  scale_fill_manual(values=colors_1) +
  guides(fill = guide_legend(title = "TJ", title.theme = element_text(size = 18, face = "bold", angle = 0), 
                             label.theme = element_text(size = 16, angle = 0))) +
  ggtitle("(e) Net energy - MNE Scenario") +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0))
ggsave(paste(County_FOLDER, "/MNE_netenergy.png", sep=""), plot=p, width=8,height=4,units="in",dpi=300)

p <- county_choropleth(MNE_netenergy, state_zoom = continental_us_states) +
  scale_fill_manual(values=colors_1) +
  guides(fill = FALSE) +
  ggtitle("(e) Net energy - MNE Scenario") +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0))
ggsave(paste(County_FOLDER, "/MNE_netenergy1.png", sep=""), plot=p, width=6.5,height=4,units="in",dpi=300)



MNE_netemiss <- aggregate(netemiss ~ State + County + region, MNE, FUN=sum)
MNE_netemiss$TMT <- MNE_netemiss$netemiss / 10^6
MNE_netemiss$value[MNE_netemiss$TMT  < -1000] <- "< -1,000"
MNE_netemiss$value[MNE_netemiss$TMT >= -1000 & MNE_netemiss$TMT  < -500] <- "-1,000 ~ -500"
MNE_netemiss$value[MNE_netemiss$TMT >= -500 & MNE_netemiss$TMT  < -200] <- "-500 ~ -200"
MNE_netemiss$value[MNE_netemiss$TMT >= -200 & MNE_netemiss$TMT  < -100] <- "-200 ~ -100"
MNE_netemiss$value[MNE_netemiss$TMT >= -100 & MNE_netemiss$TMT  < -50] <- "-100 ~ -50"
MNE_netemiss$value[MNE_netemiss$TMT >= -50 & MNE_netemiss$TMT  < 0] <- "-50 ~ 0"
MNE_netemiss$value[MNE_netemiss$TMT >= 0 & MNE_netemiss$TMT  < 50] <- "0 ~ 50"
MNE_netemiss$value[MNE_netemiss$TMT >= 50 & MNE_netemiss$TMT  < 200] <- "50 ~ 200"
MNE_netemiss$value[MNE_netemiss$TMT >= 200] <- "> 200"
MNE_netemiss$value <- factor(MNE_netemiss$value, 
                             levels = c("< -1,000", "-1,000 ~ -500","-500 ~ -200", "-200 ~ -100", "-100 ~ -50", 
                                        "-50 ~ 0", "0 ~ 50", "50 ~ 200", "> 200" ))

p <- county_choropleth(MNE_netemiss, state_zoom = continental_us_states) +
  scale_fill_manual(values=colors_2) +
  guides(fill = guide_legend(title = "Thousand MT", title.theme = element_text(size = 18, face = "bold", angle = 0), 
                             label.theme = element_text(size = 16, angle = 0))) +
  ggtitle("(f) GWP - MNE Scenario") + 
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0))
ggsave(paste(County_FOLDER, "/MNE_netemiss.png", sep=""), plot=p, width=8,height=4,units="in",dpi=300)

p <- county_choropleth(MNE_netemiss, state_zoom = continental_us_states) +
  scale_fill_manual(values=colors_2) +
  guides(fill = FALSE) +
  ggtitle("(f)  GWP (w. biogenic CO2) - MNE Scenario") + 
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0))
ggsave(paste(County_FOLDER, "/MNE_netemiss1.png", sep=""), plot=p, width=6.5,height=4,units="in",dpi=300)



#MER map
MER <- aggregate(netemiss ~ State + County + region +Feedstock + Waste_type, waste_results, FUN=min)
MER <- merge(MER, waste_results)
sum(MER$energy)
sum(MER$netenergy)
sum(MER$netemiss)

MER_energy <- aggregate(energy ~ State + County + region, MER, FUN=sum)
MER_energy$TJ <- MER_energy$energy / 1000
MER_energy$value[MER_energy$TJ < 100] <- "< 100"
MER_energy$value[MER_energy$TJ >= 100 & MER_energy$TJ  < 500] <- "100 ~ 500"
MER_energy$value[MER_energy$TJ >= 500 & MER_energy$TJ  < 1000] <- "500 ~ 1,000"
MER_energy$value[MER_energy$TJ >= 1000 & MER_energy$TJ  < 2000] <- "1,000 ~ 2,000"
MER_energy$value[MER_energy$TJ >= 2000 & MER_energy$TJ  < 5000] <- "2,000 ~ 5,000"
MER_energy$value[MER_energy$TJ >= 5000 & MER_energy$TJ  < 10000] <- "5,000 ~ 10,000"
MER_energy$value[MER_energy$TJ >= 10000 & MER_energy$TJ  < 20000] <- "10,000 ~ 20,000"
MER_energy$value[MER_energy$TJ >= 20000 & MER_energy$TJ  < 40000] <- "20,000 ~ 40,000"
MER_energy$value[MER_energy$TJ >= 40000] <- "> 40,000"
MER_energy$value <- factor(MER_energy$value, 
                           levels = c("< 100", "100 ~ 500", "500 ~ 1,000", "1,000 ~ 2,000", 
                                      "2,000 ~ 5,000", "5,000 ~ 10,000","10,000 ~ 20,000", "20,000 ~ 40,000", "> 40,000"))
data(continental_us_states)
p <- county_choropleth(MER_energy, state_zoom = continental_us_states) +
  scale_fill_brewer(palette = "Purples") +
  guides(fill = guide_legend(title = "TJ", title.theme = element_text(size = 18, face = "bold", angle = 0), 
                             label.theme = element_text(size = 16, angle = 0))) +
  ggtitle("(g) Renewable energy production - MER Scenario") +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0))
ggsave(paste(County_FOLDER, "/MER_energy.png", sep=""), plot=p, width=8,height=4,units="in",dpi=300)

p <- county_choropleth(MER_energy, state_zoom = continental_us_states) +
  scale_fill_brewer(palette = "Purples") +
  guides(fill = FALSE) +
  ggtitle("(g) Renewable energy production - MER Scenario") +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0))
ggsave(paste(County_FOLDER, "/MER_energy1.png", sep=""), plot=p, width=6.5,height=4,units="in",dpi=300)



MER_netenergy <- aggregate(netenergy ~ State + County + region, MER, FUN=sum)
MER_netenergy$TJ <- MER_netenergy$netenergy / 1000
MER_netenergy$value[MER_netenergy$TJ < -1000] <- "< -1,000"
MER_netenergy$value[MER_netenergy$TJ >= -1000 & MER_netenergy$TJ  < 0] <- "-1,000 ~ 0"
MER_netenergy$value[MER_netenergy$TJ >= 0 & MER_netenergy$TJ  < 500] <- "0 ~ 500"
MER_netenergy$value[MER_netenergy$TJ >= 500 & MER_netenergy$TJ  < 1000] <- "500 ~ 1,000"
MER_netenergy$value[MER_netenergy$TJ >= 1000 & MER_netenergy$TJ  < 2000] <- "1,000 ~ 2,000"
MER_netenergy$value[MER_netenergy$TJ >= 2000 & MER_netenergy$TJ  < 5000] <- "2,000 ~ 5,000"
MER_netenergy$value[MER_netenergy$TJ >= 5000 & MER_netenergy$TJ  < 10000] <- "5,000 ~ 10,000"
MER_netenergy$value[MER_netenergy$TJ >= 10000 & MER_netenergy$TJ  < 20000] <- "10,000 ~ 20,000"
MER_netenergy$value[MER_netenergy$TJ >= 20000 ] <- "> 20,000"

MER_netenergy$value <- factor(MER_netenergy$value, 
                              levels = c("< -1,000", "-1,000 ~ 0","0 ~ 500", "500 ~ 1,000", "1,000 ~ 2,000",
                                         "2,000 ~ 5,000", "5,000 ~ 10,000","10,000 ~ 20,000", "> 20,000"))

p <- county_choropleth(MER_netenergy, state_zoom = continental_us_states) +
  scale_fill_manual(values=colors_1) +
  guides(fill = guide_legend(title = "TJ", title.theme = element_text(size = 18, face = "bold", angle = 0), 
                             label.theme = element_text(size = 16, angle = 0))) +
  ggtitle("(h) Net energy - MER Scenario") +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0))
ggsave(paste(County_FOLDER, "/MER_netenergy.png", sep=""), plot=p, width=8,height=4,units="in",dpi=300)

p <- county_choropleth(MER_netenergy, state_zoom = continental_us_states) +
  scale_fill_manual(values=colors_1) +
  guides(fill = FALSE) +
  ggtitle("(h) Net energy - MER Scenario") +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0))
ggsave(paste(County_FOLDER, "/MER_netenergy1.png", sep=""), plot=p, width=6.5,height=4,units="in",dpi=300)


MER_netemiss <- aggregate(netemiss ~ State + County + region, MER, FUN=sum)
MER_netemiss$TMT <- MER_netemiss$netemiss / 10^6
MER_netemiss$value[MER_netemiss$TMT  < -1000] <- "< -1,000"
MER_netemiss$value[MER_netemiss$TMT >= -1000 & MER_netemiss$TMT  < -500] <- "-1,000 ~ -500"
MER_netemiss$value[MER_netemiss$TMT >= -500 & MER_netemiss$TMT  < -200] <- "-500 ~ -200"
MER_netemiss$value[MER_netemiss$TMT >= -200 & MER_netemiss$TMT  < -100] <- "-200 ~ -100"
MER_netemiss$value[MER_netemiss$TMT >= -100 & MER_netemiss$TMT  < -50] <- "-100 ~ -50"
MER_netemiss$value[MER_netemiss$TMT >= -50 & MER_netemiss$TMT  < 0] <- "-50 ~ 0"
MER_netemiss$value[MER_netemiss$TMT >= 0 & MER_netemiss$TMT  < 50] <- "0 ~ 50"
MER_netemiss$value[MER_netemiss$TMT >= 50 & MER_netemiss$TMT  < 200] <- "50 ~ 200"
MER_netemiss$value[MER_netemiss$TMT >= 200] <- "> 200"
MER_netemiss$value <- factor(MER_netemiss$value, 
                             levels = c("< -1,000", "-1,000 ~ -500","-500 ~ -200", "-200 ~ -100", "-100 ~ -50", 
                                        "-50 ~ 0", "0 ~ 50", "50 ~ 200", "> 200" ))

p <- county_choropleth(MER_netemiss, state_zoom = continental_us_states) +
  scale_fill_manual(values=colors_2) +
  guides(fill = guide_legend(title = "Thousand MT", title.theme = element_text(size = 18, face = "bold", angle = 0), 
                             label.theme = element_text(size = 16, angle = 0))) +
  ggtitle("(i)  GWP - MER Scenario") + 
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0))
ggsave(paste(County_FOLDER, "/MER_netemiss.png", sep=""), plot=p, width=8,height=4,units="in",dpi=300)

p <- county_choropleth(MER_netemiss, state_zoom = continental_us_states) +
  scale_fill_manual(values=colors_2) +
  guides(fill = FALSE) +
  ggtitle("(i) GWP (w. biogenic CO2) - MER Scenario") + 
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0))
ggsave(paste(County_FOLDER, "/MER_netemiss1.png", sep=""), plot=p, width=6.5,height=4,units="in",dpi=300)

##analysis of the tradeoffs
#tradeoffs - percent county with policy goals aligned
colnames(MEP)[8] <- "Tech_MEP"
colnames(MNE)[8] <- "Tech_MNE"
colnames(MER)[8] <- "Tech_MER"

Tech_mep <- MEP[c(1,2,3,4,5,8)]
Tech_mne <- MNE[c(1,2,3,4,5,8)]
Tech_mer <- MER[c(1,2,3,4,5,8)]

Tech_all <- Reduce(function(x, y) merge(x, y, all=TRUE), list(Tech_mep, Tech_mne, Tech_mer))

Tech_all$TO_all <- ifelse(Tech_all$Tech_MEP == Tech_all$Tech_MNE & Tech_all$Tech_MNE == Tech_all$Tech_MER, 1, 0)
Tech_all$TO_epne <- ifelse(Tech_all$Tech_MEP == Tech_all$Tech_MNE & Tech_all$Tech_MNE != Tech_all$Tech_MER, 1, 0)
Tech_all$TO_eper <- ifelse(Tech_all$Tech_MEP == Tech_all$Tech_MER & Tech_all$Tech_MNE != Tech_all$Tech_MER, 1, 0)
Tech_all$TO_neer <- ifelse(Tech_all$Tech_MER == Tech_all$Tech_MNE & Tech_all$Tech_MNE != Tech_all$Tech_MEP, 1, 0)
Tech_all$TO_no <- ifelse(Tech_all$Tech_MEP != Tech_all$Tech_MNE &
                           Tech_all$Tech_MNE != Tech_all$Tech_MER & Tech_all$Tech_MEP != Tech_all$Tech_MER, 1, 0)
Tech_all$count <- 1
Tech_all_1 <- Tech_all[c(4,5,9:14)]
##county-level results
Tech_all_2 <- aggregate(.~Feedstock + Waste_type, Tech_all_1, FUN=sum)
Tech_all_2$TO_all_per <- Tech_all_2$TO_all/Tech_all_2$count * 100
Tech_all_2$TO_epne_per <- Tech_all_2$TO_epne/Tech_all_2$count * 100
Tech_all_2$TO_eper_per <- Tech_all_2$TO_eper/Tech_all_2$count * 100
Tech_all_2$TO_neer_per <- Tech_all_2$TO_neer/Tech_all_2$count * 100
Tech_all_2$TO_no_per <- Tech_all_2$TO_no/Tech_all_2$count * 100
write.csv(Tech_all_2, file = "./Results/Tech_scenario_tradeoffs1_bioco2.csv")

Tech_all_3 <- subset(Tech_all, Tech_all$TO_all == 1)
Tech_all_3_1 <- aggregate(TO_all~Feedstock + Waste_type + Tech_MEP + Tech_MNE + Tech_MER, Tech_all_3, FUN=sum)
Tech_all_4 <- subset(Tech_all, Tech_all$TO_epne == 1)
Tech_all_4_1 <- aggregate(TO_epne~Feedstock + Waste_type + Tech_MEP + Tech_MNE + Tech_MER, Tech_all_4, FUN=sum)
Tech_all_5 <- subset(Tech_all, Tech_all$TO_eper == 1)
Tech_all_5_1 <- aggregate(TO_eper~Feedstock + Waste_type + Tech_MEP + Tech_MNE + Tech_MER, Tech_all_5, FUN=sum)
Tech_all_6 <- subset(Tech_all, Tech_all$TO_neer == 1)
Tech_all_6_1 <- aggregate(TO_neer~Feedstock + Waste_type + Tech_MEP + Tech_MNE + Tech_MER, Tech_all_6, FUN=sum)
Tech_all_7 <- subset(Tech_all, Tech_all$TO_no == 1)
Tech_all_7_1 <- aggregate(TO_no~Feedstock + Waste_type + Tech_MEP + Tech_MNE + Tech_MER, Tech_all_7, FUN=sum)

##state-level results
Tech_mep_s <- Tech_mep %>% count(State, Feedstock, Waste_type, Tech_MEP)
Tech_mne_s <- Tech_mne %>% count(State, Feedstock, Waste_type, Tech_MNE)
Tech_mer_s <- Tech_mer %>% count(State, Feedstock, Waste_type, Tech_MER)

Tech_state <- Reduce(function(x, y) merge(x, y, all=TRUE), list(Tech_mep_s[c(1,2,3,4)], Tech_mne_s[c(1,2,3,4)], Tech_mer_s[c(1,2,3,4)]))
Tech_state <- merge(Tech_state, state_name)

Tech_state$TO_all <- ifelse(Tech_state$Tech_MEP == Tech_state$Tech_MNE & Tech_state$Tech_MNE == Tech_state$Tech_MER, 1, 0)
Tech_state$TO_epne <- ifelse(Tech_state$Tech_MEP == Tech_state$Tech_MNE & Tech_state$Tech_MNE != Tech_state$Tech_MER, 1, 0)
Tech_state$TO_eper <- ifelse(Tech_state$Tech_MEP == Tech_state$Tech_MER & Tech_state$Tech_MNE != Tech_state$Tech_MER, 1, 0)
Tech_state$TO_neer <- ifelse(Tech_state$Tech_MER == Tech_state$Tech_MNE & Tech_state$Tech_MNE != Tech_state$Tech_MEP, 1, 0)
Tech_state$TO_no <- ifelse(Tech_state$Tech_MEP != Tech_state$Tech_MNE &
                             Tech_state$Tech_MNE != Tech_state$Tech_MER & Tech_state$Tech_MEP != Tech_state$Tech_MER, 1, 0)
Tech_state$count <- 1
Tech_state_1 <- Tech_state[c(2:3,8:13)]

##state-level results
Tech_state_2 <- aggregate(.~Feedstock + Waste_type, Tech_state_1, FUN=sum)
Tech_state_2$TO_all_per <- Tech_state_2$TO_all/Tech_state_2$count * 100
Tech_state_2$TO_epne_per <- Tech_state_2$TO_epne/Tech_state_2$count * 100
Tech_state_2$TO_eper_per <- Tech_state_2$TO_eper/Tech_state_2$count * 100
Tech_state_2$TO_neer_per <- Tech_state_2$TO_neer/Tech_state_2$count * 100
Tech_state_2$TO_no_per <- Tech_state_2$TO_no/Tech_state_2$count * 100


Tech_state_3 <- subset(Tech_state, Tech_state$TO_all == 1)
Tech_state_3_1 <- aggregate(Abb ~ Feedstock + Waste_type, data = Tech_state_3, paste, collapse = ", ")
colnames(Tech_state_3_1)[3] <- "TO_all_state"
Tech_state_3_2 <- aggregate(TO_all~Feedstock + Waste_type + Tech_MEP + Tech_MNE + Tech_MER, Tech_state_3, FUN=sum)

Tech_state_4 <- subset(Tech_state, Tech_state$TO_epne == 1)
Tech_state_4_1 <- aggregate(Abb ~ Feedstock + Waste_type, data = Tech_state_4, paste, collapse = ", ")
colnames(Tech_state_4_1)[3] <- "TO_epne_state"
Tech_state_4_2 <- aggregate(TO_epne~Feedstock + Waste_type + Tech_MEP + Tech_MNE + Tech_MER, Tech_state_4, FUN=sum)

#Tech_state_5 <- subset(Tech_state, Tech_state$TO_eper == 1)
#Tech_state_5_1 <- aggregate(Abb ~ Feedstock + Waste_type, data = Tech_state_5, paste, collapse = ", ")
#colnames(Tech_state_5_1)[3] <- "TO_eper_state"
#Tech_state_5_2 <- aggregate(TO_eper~Feedstock + Waste_type + Tech_MEP + Tech_MNE + Tech_MER, Tech_state_5, FUN=sum)

Tech_state_6 <- subset(Tech_state, Tech_state$TO_neer == 1)
Tech_state_6_1 <- aggregate(Abb ~ Feedstock + Waste_type, data = Tech_state_6, paste, collapse = ", ")
colnames(Tech_state_6_1)[3] <- "TO_neer_state"
Tech_state_6_2 <- aggregate(TO_neer~Feedstock + Waste_type + Tech_MEP + Tech_MNE + Tech_MER, Tech_state_6, FUN=sum)

Tech_state_7 <- subset(Tech_state, Tech_state$TO_no == 1)
Tech_state_7_1 <- aggregate(Abb ~ Feedstock + Waste_type, data = Tech_state_7, paste, collapse = ", ")
colnames(Tech_state_7_1)[3] <- "TO_no_state"

TO_state <- Reduce(function(x, y) merge(x, y, all=TRUE), list(Tech_state_2, Tech_state_3_1, 
                                                              Tech_state_4_1, 
                                                              Tech_state_6_1, Tech_state_7_1))
write.csv(TO_state, file = "./Results/Tech_tradeoffs_state_bioco2.csv")


#Scenario results by feedstock

#MEP - by feedstock
MEP_1 <- aggregate(energy ~ Feedstock, MEP, FUN=sum)
MEP_1$Tech <- MEP$Tech[match(MEP_1$Feedstock, MEP$Feedstock)]
MEP_1$Waste_type <- MEP$Waste_type[match(MEP_1$Feedstock, MEP$Feedstock)]
MEP_1$PJ <- MEP_1$energy / 10^6

#MNE - by feedstock
MNE_1 <- aggregate(netenergy ~ Feedstock, MNE, FUN=sum)
MNE_1$Tech <- MNE$Tech[match(MNE_1$Feedstock, MNE$Feedstock)]
MNE_1$Waste_type <- MNE$Waste_type[match(MNE_1$Feedstock, MNE$Feedstock)]
MNE_1$PJ <- format(MNE_1$netenergy / 10^6, scientific = FALSE)
MNE_1$PJ <- as.numeric(MNE_1$PJ)

#MER - by feedstock
MER_1 <- aggregate(netemiss ~ Feedstock, MER, FUN=sum)
MER_1$Tech <- MER$Tech[match(MER_1$Feedstock, MER$Feedstock)]
MER_1$Waste_type <- MER$Waste_type[match(MER_1$Feedstock, MER$Feedstock)]
MER_1$MMT <- format(MER_1$netemiss / 10^9, scientific = FALSE)
MER_1$MMT <- as.numeric(MER_1$MMT)






