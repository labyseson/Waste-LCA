#Author: Bo Liu, UCLA

#County-level calculation
waste <- waste_all[c(4,5,6,15,23,24)]
colnames(waste)[4] <- "Waste_type"
coefficients <- merge(coefficients, tech)
waste_1 <- merge(waste, coefficients)
waste_2 <- merge(waste_1, state_GHG_ef)

#Collection & Transportation_1 (energy - GJ, emiss - kg CO2e)
waste_2$Prod_ww <- waste_2$Prod/(1-waste_2$MC/100)
waste_2$collection_en <- waste_2$Prod_ww * waste_2$Collection_Diesel
waste_2$collection_emiss <- waste_2$collection_en * waste_2$Diesel_GHG 
waste_2$transport1_en <- waste_2$Prod_ww * waste_2$Transport_km_1 * waste_2$Transport_Diesel
waste_2$transport1_emiss <- waste_2$transport1_en  * waste_2$Diesel_GHG 


### Accounting method 1: excluding biogenic CO2

#E1
#electricity T&D loss - 6.5%, heat loss - 20%
waste_2$E1_elec <- waste_2$Prod_ww * waste_2$E1.e.out * (1-0.065)
waste_2$E1_heat <- waste_2$Prod_ww * waste_2$E1.h.out * (1-0.2)
waste_2$E1_energy <- waste_2$E1_elec + waste_2$E1_heat
waste_2$E1_energymain <- waste_2$E1_elec
waste_2$E1_energyco <- waste_2$E1_heat
waste_2$E1_netenergy <- (waste_2$E1_energy - waste_2$Prod_ww * (waste_2$E1.e.in + waste_2$E1.h.in + waste_2$E1.d.in) - waste_2$collection_en -  waste_2$transport1_en) * waste_2$E1_tech
waste_2$E1_collectionemiss <- waste_2$collection_emiss * waste_2$E1_tech
waste_2$E1_transport1emiss <- waste_2$transport1_emiss * waste_2$E1_tech
waste_2$E1_processemiss <- waste_2$Prod_ww * (waste_2$E1.e.in * waste_2$Powergen_GHG 
                                              + waste_2$E1.h.in * waste_2$Heatgen_GHG 
                                              + waste_2$E1.d.in * waste_2$Diesel_GHG
                                              + waste_2$Nonbio_emiss1) * waste_2$E1_tech
waste_2$E1_enduseemiss <- 0
waste_2$E1_dispemiss <- 0 - waste_2$E1_elec* waste_2$Powergen_GHG  - waste_2$E1_heat* waste_2$Heatgen_GHG 
waste_2$E1_dispemissmain <- 0 - waste_2$E1_elec* waste_2$Powergen_GHG
waste_2$E1_dispemissco <- 0 - waste_2$E1_heat* waste_2$Heatgen_GHG
waste_2$E1_netemiss <- waste_2$E1_collectionemiss + waste_2$E1_transport1emiss + waste_2$E1_processemiss + waste_2$E1_enduseemiss + waste_2$E1_dispemiss

#E2
#electricity T&D loss - 6.5%, heat loss - 20%
waste_2$E2_elec <- waste_2$Prod_ww * waste_2$E2.e.out * (1-0.065)
waste_2$E2_heat <- waste_2$Prod_ww * waste_2$E2.h.out * (1-0.2)
waste_2$E2_energy <- waste_2$E2_elec + waste_2$E2_heat
waste_2$E2_energymain <- waste_2$E2_elec
waste_2$E2_energyco <- waste_2$E2_heat
waste_2$E2_netenergy <- (waste_2$E2_energy - waste_2$Prod_ww * (waste_2$E2.e.in + waste_2$E2.h.in + waste_2$E2.d.in) - waste_2$collection_en - waste_2$transport1_en) * waste_2$E2_tech
waste_2$E2_collectionemiss <- waste_2$collection_emiss * waste_2$E2_tech
waste_2$E2_transport1emiss <- waste_2$transport1_emiss * waste_2$E2_tech
waste_2$E2_processemiss <- waste_2$Prod_ww * (waste_2$E2.e.in * waste_2$Powergen_GHG 
                                              + waste_2$E2.h.in * waste_2$Heatgen_GHG
                                              + waste_2$E2.d.in * waste_2$Diesel_GHG
                                              + waste_2$Nonbio_emiss1)  * waste_2$E2_tech 
waste_2$E2_enduseemiss <- 0
waste_2$E2_dispemiss <- 0 - waste_2$E2_elec* waste_2$Powergen_GHG - waste_2$E2_heat* waste_2$Heatgen_GHG
waste_2$E2_dispemissmain <- 0 - waste_2$E2_elec* waste_2$Powergen_GHG
waste_2$E2_dispemissco <- 0 - waste_2$E2_heat* waste_2$Heatgen_GHG
waste_2$E2_netemiss <- waste_2$E2_collectionemiss + waste_2$E2_transport1emiss + waste_2$E2_processemiss + waste_2$E2_enduseemiss + waste_2$E2_dispemiss

#E3
#electricity T&D loss - 6.5%
waste_2$E3_energy <- waste_2$Prod_ww * waste_2$E3.e.out * (1-0.065)
waste_2$E3_energymain <- waste_2$E3_energy
waste_2$E3_netenergy <- (waste_2$E3_energy - waste_2$Prod_ww * (waste_2$E3.e.in + waste_2$E3.h.in + waste_2$E3.d.in) - waste_2$collection_en -  waste_2$transport1_en) * waste_2$E3_tech
waste_2$E3_collectionemiss <- waste_2$collection_emiss * waste_2$E3_tech
waste_2$E3_transport1emiss <- waste_2$transport1_emiss * waste_2$E3_tech
waste_2$E3_processemiss <- waste_2$Prod_ww * (waste_2$E3.e.in * waste_2$Powergen_GHG 
                                              + waste_2$E3.h.in * waste_2$Heatgen_GHG
                                              + waste_2$E3.d.in * waste_2$Diesel_GHG
                                              + waste_2$Nonbio_emiss1)  * waste_2$E3_tech
waste_2$E3_enduseemiss <- 0
waste_2$E3_dispemiss <- 0 - waste_2$E3_energy* waste_2$Powergen_GHG
waste_2$E3_dispemissmain <- 0 - waste_2$E3_energy* waste_2$Powergen_GHG
waste_2$E3_netemiss <- waste_2$E3_collectionemiss + waste_2$E3_transport1emiss + waste_2$E3_processemiss + waste_2$E3_enduseemiss + waste_2$E3_dispemiss

#E4
#electricity T&D loss - 6.5%, heat loss - 20%
waste_2$E4_elec <- waste_2$Prod_ww * waste_2$E4.e.out * (1-0.065)
waste_2$E4_heat <- waste_2$Prod_ww * waste_2$E4.h.out * (1-0.2)
waste_2$E4_energy <- waste_2$E4_elec + waste_2$E4_heat
waste_2$E4_energymain <- waste_2$E4_elec
waste_2$E4_energyco <- waste_2$E4_heat
waste_2$E4_netenergy <- (waste_2$E4_energy - waste_2$Prod_ww * (waste_2$E4.ng.in + waste_2$E4.d.in) - waste_2$collection_en - waste_2$transport1_en) * waste_2$E4_tech
waste_2$E4_collectionemiss <- waste_2$collection_emiss * waste_2$E4_tech
waste_2$E4_transport1emiss <- waste_2$transport1_emiss * waste_2$E4_tech
waste_2$E4_processemiss <- waste_2$Prod_ww * (waste_2$E4.ng.in * waste_2$NG_GHG 
                                              + waste_2$E4.d.in * waste_2$Diesel_GHG
                                              + waste_2$Nonbio_emiss1) * waste_2$E4_tech 
waste_2$E4_enduseemiss <- 0
waste_2$E4_dispemiss <- 0 - waste_2$E4_elec* waste_2$Powergen_GHG - waste_2$E4_heat* waste_2$Heatgen_GHG
waste_2$E4_dispemissmain <- 0 - waste_2$E4_elec* waste_2$Powergen_GHG
waste_2$E4_dispemissco <- 0 - waste_2$E4_heat* waste_2$Heatgen_GHG
waste_2$E4_netemiss <- waste_2$E4_collectionemiss + waste_2$E4_transport1emiss + waste_2$E4_processemiss + waste_2$E4_enduseemiss + waste_2$E4_dispemiss

#M1
#methane leakage - 2%
waste_2$M1_energy <- waste_2$Prod_ww * waste_2$M1.m.out * (1-0.02)
waste_2$M1_energymain <- waste_2$M1_energy
waste_2$M1_netenergy <- (waste_2$M1_energy - waste_2$Prod_ww * (waste_2$M1.e.in + waste_2$M1.h.in) - waste_2$collection_en - waste_2$transport1_en) * waste_2$M1_tech
waste_2$M1_collectionemiss <- waste_2$collection_emiss * waste_2$M1_tech
waste_2$M1_transport1emiss <- waste_2$transport1_emiss * waste_2$M1_tech
waste_2$M1_processemiss <- waste_2$Prod_ww * (waste_2$M1.e.in * waste_2$Powergen_GHG + waste_2$M1.h.in * waste_2$Heatgen_GHG)* waste_2$M1_tech
waste_2$M1_transport2emiss <- waste_2$Prod_ww * waste_2$M1.m.out *0.02 /50 *28 * waste_2$M1_tech
waste_2$M1_enduseemiss <- waste_2$Prod_ww * waste_2$Nonbio_emiss2  * waste_2$M1_tech
waste_2$M1_dispemiss <- 0 - waste_2$M1_energy* waste_2$NG_GHG
waste_2$M1_dispemissmain <- waste_2$M1_dispemiss
waste_2$M1_netemiss <- waste_2$M1_collectionemiss + waste_2$M1_transport1emiss + waste_2$M1_processemiss + waste_2$M1_transport2emiss + waste_2$M1_enduseemiss + waste_2$M1_dispemiss

#M2
#methane leakage - 2%
waste_2$M2_energy <- waste_2$Prod_ww * waste_2$M2.m.out * (1-0.02)
waste_2$M2_energymain <- waste_2$M2_energy
waste_2$M2_netenergy <- (waste_2$M2_energy - waste_2$Prod_ww * (waste_2$M2.e.in + waste_2$M2.h.in + waste_2$M2.d.in) - waste_2$collection_en -  waste_2$transport1_en) * waste_2$M2_tech
waste_2$M2_collectionemiss <- waste_2$collection_emiss * waste_2$M2_tech
waste_2$M2_transport1emiss <- waste_2$transport1_emiss * waste_2$M2_tech
waste_2$M2_processemiss <- waste_2$Prod_ww * (waste_2$M2.e.in * waste_2$Powergen_GHG + 
                                                waste_2$M2.h.in * waste_2$Heatgen_GHG +
                                                waste_2$M2.d.in * waste_2$Diesel_GHG) * waste_2$M2_tech
waste_2$M2_transport2emiss <- waste_2$Prod_ww * waste_2$M2.m.out * 0.02 /50 *28 * waste_2$M1_tech
waste_2$M2_enduseemiss <- waste_2$Prod_ww * waste_2$Nonbio_emiss2 * waste_2$M2_tech
waste_2$M2_dispemiss <- 0 - waste_2$M2_energy* waste_2$NG_GHG
waste_2$M2_dispemissmain <- waste_2$M2_dispemiss
waste_2$M2_netemiss <- waste_2$M2_collectionemiss + waste_2$M2_transport1emiss + waste_2$M2_processemiss + waste_2$M2_transport2emiss + waste_2$M2_enduseemiss + waste_2$M2_dispemiss

#Eth1
#energy intensity of ethanol - 26.95 MJ/kg
waste_2$Eth1_elec <- waste_2$Prod_ww * waste_2$Eth1.e.out * (1-0.065)
waste_2$Eth1_eth <- waste_2$Prod_ww * waste_2$Eth1.eth.out
waste_2$Eth1_energy <- waste_2$Eth1_elec + waste_2$Eth1_eth
waste_2$Eth1_energymain <- waste_2$Eth1_eth
waste_2$Eth1_energyco <- waste_2$Eth1_elec
waste_2$Eth1_netenergy <- (waste_2$Eth1_energy - waste_2$Prod_ww * (waste_2$Eth1.ng.in  + waste_2$Eth1.d.in) -
                             (waste_2$collection_en +  waste_2$transport1_en) * waste_2$M1_tech -
                             waste_2$Eth1_eth / 26.95 * waste_2$Transport_km_2 * waste_2$Transport_Diesel) * waste_2$Eth1_tech
waste_2$Eth1_collectionemiss <- waste_2$collection_emiss * waste_2$Eth1_tech
waste_2$Eth1_transport1emiss <- waste_2$transport1_emiss * waste_2$Eth1_tech
waste_2$Eth1_processemiss <- waste_2$Prod_ww * (waste_2$Eth1.ng.in * waste_2$Heatgen_GHG + waste_2$Eth1.d.in * waste_2$Diesel_GHG) * waste_2$Eth1_tech 
waste_2$Eth1_transport2emiss <- waste_2$Eth1_eth / 26.95 * waste_2$Transport_km_2 * waste_2$Transport_Diesel * waste_2$Diesel_GHG 
waste_2$Eth1_enduseemiss <- waste_2$Prod_ww * waste_2$Nonbio_emiss2 * waste_2$Eth1_tech
waste_2$Eth1_dispemiss <- 0 - waste_2$Eth1_eth* waste_2$Gasoline_GHG  - waste_2$Eth1_elec * waste_2$Powergen_GHG
waste_2$Eth1_dispemissmain <- 0 - waste_2$Eth1_eth* waste_2$Gasoline_GHG
waste_2$Eth1_dispemissco <- 0 - waste_2$Eth1_elec * waste_2$Powergen_GHG
waste_2$Eth1_netemiss <- waste_2$Eth1_collectionemiss + waste_2$Eth1_transport1emiss + waste_2$Eth1_processemiss + waste_2$Eth1_transport2emiss + waste_2$Eth1_enduseemiss + waste_2$Eth1_dispemiss

#Rd1
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
#electricity T&D loss - 6.5%, methane leakage - 2%
waste_2$Rd1_d <- waste_2$Prod_ww * waste_2$Rd1.d.out
waste_2$Rd1_g <- waste_2$Prod_ww * waste_2$Rd1.g.out
waste_2$Rd1_j <- waste_2$Prod_ww * waste_2$Rd1.j.out
waste_2$Rd1_m <- waste_2$Prod_ww * waste_2$Rd1.m.out * (1-0.02)
waste_2$Rd1_elec <- waste_2$Prod_ww * waste_2$Rd1.e.out * (1-0.065)
waste_2$Rd1_energy <- waste_2$Rd1_d + waste_2$Rd1_g + waste_2$Rd1_j + waste_2$Rd1_m + waste_2$Rd1_elec
waste_2$Rd1_energymain <- waste_2$Rd1_d
waste_2$Rd1_energyco <- waste_2$Rd1_g + waste_2$Rd1_j + waste_2$Rd1_m + waste_2$Rd1_elec
waste_2$Rd1_netenergy <-(waste_2$Rd1_energy - waste_2$Prod_ww * waste_2$Rd1.e.in -
                           (waste_2$collection_en +  waste_2$transport1_en)  -
                           waste_2$Rd1_d / 42.79 * waste_2$Transport_km_2 * waste_2$Transport_Diesel -
                           waste_2$Rd1_g / 41.74 * waste_2$Transport_km_2 * waste_2$Transport_Diesel -
                           waste_2$Rd1_j / 43.10 * waste_2$Transport_km_2 * waste_2$Transport_Diesel ) * waste_2$Rd1_tech
waste_2$Rd1_collectionemiss <- waste_2$collection_emiss * waste_2$Rd1_tech
waste_2$Rd1_transport1emiss <- waste_2$transport1_emiss * waste_2$Rd1_tech
waste_2$Rd1_processemiss <- waste_2$Prod_ww * waste_2$Rd1.e.in * waste_2$Powergen_GHG * waste_2$Rd1_tech
waste_2$Rd1_transport2emiss <- (waste_2$Rd1_d / 42.79 + waste_2$Rd1_g / 41.74 + waste_2$Rd1_j / 43.10) * waste_2$Transport_km_2 * waste_2$Transport_Diesel * waste_2$Diesel_GHG  
waste_2$Rd1_enduseemiss <- waste_2$Prod_ww * waste_2$Nonbio_emiss2 * waste_2$Rd1_tech
waste_2$Rd1_dispemiss <- 0 - waste_2$Rd1_d *waste_2$Diesel_GHG - waste_2$Rd1_g * waste_2$Gasoline_GHG -  waste_2$Rd1_j * waste_2$Jet_GHG -
  waste_2$Rd1_m * waste_2$NG_GHG - waste_2$Rd1_elec * waste_2$Powergen_GHG 
waste_2$Rd1_dispemissmain <- 0 - waste_2$Rd1_d *waste_2$Diesel_GHG
waste_2$Rd1_dispemissco <- 0 - waste_2$Rd1_g * waste_2$Gasoline_GHG -  waste_2$Rd1_j * waste_2$Jet_GHG -
  waste_2$Rd1_m * waste_2$NG_GHG - waste_2$Rd1_elec * waste_2$Powergen_GHG 
waste_2$Rd1_netemiss <- waste_2$Rd1_collectionemiss + waste_2$Rd1_transport1emiss + waste_2$Rd1_processemiss + waste_2$Rd1_enduseemiss + waste_2$Rd1_transport2emiss + waste_2$Rd1_dispemiss


#Rd2
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74
waste_2$Rd2_d <- waste_2$Prod_ww * waste_2$Rd2.d.out
waste_2$Rd2_g <- waste_2$Prod_ww * waste_2$Rd2.g.out
waste_2$Rd2_energy <- waste_2$Rd2_d + waste_2$Rd2_g
waste_2$Rd2_energymain <- waste_2$Rd2_d
waste_2$Rd2_energyco <- waste_2$Rd2_g
waste_2$Rd2_netenergy <-(waste_2$Rd2_energy - waste_2$Prod_ww * (waste_2$Rd2.e.in + waste_2$Rd2.ng.in) -
                           (waste_2$collection_en +  waste_2$transport1_en)  -
                           waste_2$Rd2_d / 42.79 * waste_2$Transport_km_2 * waste_2$Transport_Diesel -
                           waste_2$Rd2_g / 41.74 * waste_2$Transport_km_2 * waste_2$Transport_Diesel ) * waste_2$Rd2_tech
waste_2$Rd2_collectionemiss <- waste_2$collection_emiss * waste_2$Rd2_tech
waste_2$Rd2_transport1emiss <- waste_2$transport1_emiss * waste_2$Rd2_tech
waste_2$Rd2_processemiss  <- waste_2$Prod_ww * (waste_2$Rd2.e.in * waste_2$Powergen_GHG + waste_2$Rd2.ng.in * waste_2$H2_GHG) * waste_2$Rd2_tech
waste_2$Rd2_transport2emiss <- (waste_2$Rd2_d / 42.79  + waste_2$Rd2_g / 41.74) * waste_2$Transport_km_2 * waste_2$Transport_Diesel * waste_2$Diesel_GHG 
waste_2$Rd2_enduseemiss <- waste_2$Prod_ww * waste_2$Nonbio_emiss2 * waste_2$Rd2_tech
waste_2$Rd2_dispemiss <- 0 - waste_2$Rd2_d *waste_2$Diesel_GHG - waste_2$Rd2_g* waste_2$Gasoline_GHG 
waste_2$Rd2_dispemissmain <- 0 - waste_2$Rd2_d *waste_2$Diesel_GHG 
waste_2$Rd2_dispemissco <- 0 - waste_2$Rd2_g* waste_2$Gasoline_GHG 
waste_2$Rd2_netemiss <- waste_2$Rd2_collectionemiss + waste_2$Rd2_transport1emiss + waste_2$Rd2_processemiss + waste_2$Rd2_transport2emiss + waste_2$Rd2_enduseemiss + waste_2$Rd2_dispemiss

#Bj1 
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2$Bj1_d <- waste_2$Prod_ww * waste_2$Bj1.d.out
waste_2$Bj1_g <- waste_2$Prod_ww * waste_2$Bj1.g.out
waste_2$Bj1_j <- waste_2$Prod_ww * waste_2$Bj1.j.out
waste_2$Bj1_energy <- waste_2$Bj1_d + waste_2$Bj1_g + waste_2$Bj1_j 
waste_2$Bj1_energymain <- waste_2$Bj1_d
waste_2$Bj1_energyco <- waste_2$Bj1_g + waste_2$Bj1_j 
waste_2$Bj1_netenergy <-(waste_2$Bj1_energy - waste_2$Prod_ww * waste_2$Bj1.h2.in -
                           (waste_2$collection_en +  waste_2$transport1_en)  -
                           (waste_2$Bj1_d / 42.79 + waste_2$Bj1_g / 41.74 +waste_2$Bj1_j / 43.10) * waste_2$Transport_km_2 * waste_2$Transport_Diesel) * waste_2$Bj1_tech
waste_2$Bj1_collectionemiss <- waste_2$collection_emiss * waste_2$Bj1_tech
waste_2$Bj1_transport1emiss <- waste_2$transport1_emiss * waste_2$Bj1_tech
waste_2$Bj1_processemiss <- waste_2$Prod_ww * waste_2$Bj1.h2.in * waste_2$H2_GHG * waste_2$Bj1_tech
waste_2$Bj1_transport2emiss <- (waste_2$Bj1_d / 42.79 + waste_2$Bj1_g / 41.74 + waste_2$Bj1_j / 43.10) * waste_2$Transport_km_2 * waste_2$Transport_Diesel * waste_2$Diesel_GHG 
waste_2$Bj1_enduseemiss <- waste_2$Prod_ww * waste_2$Nonbio_emiss2 * waste_2$Bj1_tech
waste_2$Bj1_dispemiss <- 0 - waste_2$Bj1_d *waste_2$Diesel_GHG - waste_2$Bj1_g* waste_2$Gasoline_GHG -  waste_2$Bj1_j * waste_2$Jet_GHG 
waste_2$Bj1_dispemissmain <- 0 - waste_2$Bj1_d *waste_2$Diesel_GHG
waste_2$Bj1_dispemissco <- 0 - waste_2$Bj1_g* waste_2$Gasoline_GHG -  waste_2$Bj1_j * waste_2$Jet_GHG 
waste_2$Bj1_netemiss <- waste_2$Bj1_collectionemiss + waste_2$Bj1_transport1emiss + waste_2$Bj1_processemiss + waste_2$Bj1_transport2emiss + waste_2$Bj1_enduseemiss + waste_2$Bj1_dispemiss

#Bj2 
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2$Bj2_energy <- waste_2$Prod_ww * waste_2$Bj2.j.out
waste_2$Bj2_energymain <- waste_2$Bj1_energy
waste_2$Bj2_netenergy <-(waste_2$Bj2_energy - waste_2$Prod_ww * waste_2$Bj2.h2.in -
                           (waste_2$collection_en +  waste_2$transport1_en)  -
                           waste_2$Bj2_energy / 43.10 * waste_2$Transport_km_2 * waste_2$Transport_Diesel ) * waste_2$Bj2_tech
waste_2$Bj2_collectionemiss <- waste_2$collection_emiss * waste_2$Bj2_tech
waste_2$Bj2_transport1emiss <- waste_2$transport1_emiss * waste_2$Bj2_tech
waste_2$Bj2_processemiss <- waste_2$Prod_ww * waste_2$Bj2.h2.in * waste_2$H2_GHG 
waste_2$Bj2_transport2emiss <- waste_2$Bj2_energy / 43.10 * waste_2$Transport_km_2 * waste_2$Transport_Diesel * waste_2$Diesel_GHG 
waste_2$Bj2_enduseemiss <- waste_2$Prod_ww * waste_2$Nonbio_emiss2 * waste_2$Bj2_tech
waste_2$Bj2_dispemiss <- 0 - waste_2$Bj2_energy * waste_2$Jet_GHG 
waste_2$Bj2_dispemissmain <- waste_2$Bj2_dispemiss
waste_2$Bj2_netemiss <- waste_2$Bj2_collectionemiss + waste_2$Bj2_transport1emiss + waste_2$Bj2_processemiss + waste_2$Bj2_transport2emiss + waste_2$Bj2_enduseemiss + waste_2$Bj2_dispemiss

#Bj3
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2$Bj3_d <- waste_2$Prod_ww * waste_2$Bj3.d.out
waste_2$Bj3_g <- waste_2$Prod_ww * waste_2$Bj3.g.out
waste_2$Bj3_j <- waste_2$Prod_ww * waste_2$Bj3.j.out
waste_2$Bj3_energy <- waste_2$Bj3_d + waste_2$Bj3_g + waste_2$Bj3_j 
waste_2$Bj3_energymain <- waste_2$Bj3_j
waste_2$Bj3_energyco <- waste_2$Bj3_d + waste_2$Bj3_g 
waste_2$Bj3_netenergy <-(waste_2$Bj3_energy - waste_2$Prod_ww * waste_2$Bj3.e.in -
                           (waste_2$collection_en +  waste_2$transport1_en)  -
                           (waste_2$Bj3_d / 42.79 + waste_2$Bj3_g / 41.74 +waste_2$Bj3_j / 43.10) * waste_2$Transport_km_2 * waste_2$Transport_Diesel) * waste_2$Bj3_tech
waste_2$Bj3_collectionemiss <- waste_2$collection_emiss * waste_2$Bj3_tech
waste_2$Bj3_transport1emiss <- waste_2$transport1_emiss * waste_2$Bj3_tech
waste_2$Bj3_processemiss <- waste_2$Prod_ww * waste_2$Bj3.e.in * waste_2$Powergen_GHG * waste_2$Bj3_tech
waste_2$Bj3_transport2emiss <- (waste_2$Bj3_d / 42.79 + waste_2$Bj3_g / 41.74 + waste_2$Bj3_j / 43.10) * waste_2$Transport_km_2 * waste_2$Transport_Diesel * waste_2$Diesel_GHG
waste_2$Bj3_enduseemiss <- waste_2$Prod_ww * waste_2$Nonbio_emiss2 * waste_2$Bj3_tech
waste_2$Bj3_dispemiss <- 0 - waste_2$Bj3_d *waste_2$Diesel_GHG - waste_2$Bj3_g* waste_2$Gasoline_GHG -  waste_2$Bj3_j * waste_2$Jet_GHG 
waste_2$Bj3_dispemissmain <- 0 - waste_2$Bj3_j * waste_2$Jet_GHG
waste_2$Bj3_dispemissco <- 0 - waste_2$Bj3_d * waste_2$Diesel_GHG - waste_2$Bj3_g * waste_2$Gasoline_GHG
waste_2$Bj3_netemiss <- waste_2$Bj3_collectionemiss + waste_2$Bj3_transport1emiss + waste_2$Bj3_processemiss + waste_2$Bj3_transport2emiss + waste_2$Bj3_enduseemiss + waste_2$Bj3_dispemiss

#Bj4
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2$Bj4_d <- waste_2$Prod_ww * waste_2$Bj4.d.out
waste_2$Bj4_g <- waste_2$Prod_ww * waste_2$Bj4.g.out
waste_2$Bj4_j <- waste_2$Prod_ww * waste_2$Bj4.j.out
waste_2$Bj4_energy <- waste_2$Bj4_d + waste_2$Bj4_g + waste_2$Bj4_j 
waste_2$Bj4_energymain <- waste_2$Bj4_j
waste_2$Bj4_energyco <- waste_2$Bj4_d + waste_2$Bj4_g 
waste_2$Bj4_netenergy <-(waste_2$Bj4_energy - waste_2$Prod_ww * waste_2$Bj4.h2.in -
                           (waste_2$collection_en +  waste_2$transport1_en)  -
                           (waste_2$Bj4_d / 42.79 + waste_2$Bj4_g / 41.74 +waste_2$Bj4_j / 43.10) * waste_2$Transport_km_2 * waste_2$Transport_Diesel) * waste_2$Bj4_tech
waste_2$Bj4_collectionemiss <- waste_2$collection_emiss * waste_2$Bj4_tech
waste_2$Bj4_transport1emiss <- waste_2$transport1_emiss * waste_2$Bj4_tech
waste_2$Bj4_processemiss <- waste_2$Prod_ww * waste_2$Bj4.h2.in * waste_2$H2_GHG * waste_2$Bj4_tech
waste_2$Bj4_transport2emiss <- (waste_2$Bj4_d / 42.79 + waste_2$Bj4_g / 41.74 + waste_2$Bj4_j / 43.10) * waste_2$Transport_km_2 * waste_2$Transport_Diesel * waste_2$Diesel_GHG 
waste_2$Bj4_enduseemiss <- waste_2$Prod_ww * waste_2$Nonbio_emiss2 * waste_2$Bj4_tech
waste_2$Bj4_dispemiss <- 0 - waste_2$Bj4_d *waste_2$Diesel_GHG - waste_2$Bj4_g* waste_2$Gasoline_GHG -  waste_2$Bj4_j * waste_2$Jet_GHG 
waste_2$Bj4_dispemissmain <- 0 - waste_2$Bj4_j * waste_2$Jet_GHG
waste_2$Bj4_dispemissco <- 0 - waste_2$Bj4_d * waste_2$Diesel_GHG - waste_2$Bj4_g * waste_2$Gasoline_GHG
waste_2$Bj4_netemiss <- waste_2$Bj4_collectionemiss + waste_2$Bj4_transport1emiss + waste_2$Bj4_processemiss + waste_2$Bj4_transport2emiss+ waste_2$Bj4_enduseemiss + waste_2$Bj4_dispemiss

#Bj5
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2$Bj5_d <- waste_2$Prod_ww * waste_2$Bj5.d.out
waste_2$Bj5_g <- waste_2$Prod_ww * waste_2$Bj5.g.out
waste_2$Bj5_j <- waste_2$Prod_ww * waste_2$Bj5.j.out
waste_2$Bj5_energy <- waste_2$Bj5_d + waste_2$Bj5_g + waste_2$Bj5_j 
waste_2$Bj5_energymain <- waste_2$Bj5_j
waste_2$Bj5_energyco <- waste_2$Bj5_d + waste_2$Bj5_g 
waste_2$Bj5_netenergy <-(waste_2$Bj5_energy - waste_2$Prod_ww * waste_2$Bj5.e.in -
                           (waste_2$collection_en +  waste_2$transport1_en)  -
                           (waste_2$Bj5_d / 42.79 + waste_2$Bj5_g / 41.74 +waste_2$Bj5_j / 43.10) * waste_2$Transport_km_2 * waste_2$Transport_Diesel) * waste_2$Bj5_tech
waste_2$Bj5_collectionemiss <- waste_2$collection_emiss * waste_2$Bj5_tech
waste_2$Bj5_transport1emiss <- waste_2$transport1_emiss * waste_2$Bj5_tech
waste_2$Bj5_processemiss <- waste_2$Prod_ww * waste_2$Bj5.e.in * waste_2$Powergen_GHG * waste_2$Bj5_tech
waste_2$Bj5_transport2emiss <-  (waste_2$Bj5_d / 42.79 + waste_2$Bj5_g / 41.74 + waste_2$Bj5_j / 43.10) * waste_2$Transport_km_2 * waste_2$Transport_Diesel * waste_2$Diesel_GHG 
waste_2$Bj5_enduseemiss <- waste_2$Prod_ww * waste_2$Nonbio_emiss2 * waste_2$Bj5_tech
waste_2$Bj5_dispemiss <- 0 - waste_2$Bj5_d *waste_2$Diesel_GHG - waste_2$Bj5_g* waste_2$Gasoline_GHG -  waste_2$Bj5_j * waste_2$Jet_GHG 
waste_2$Bj5_dispemissmain <- 0 - waste_2$Bj5_j * waste_2$Jet_GHG
waste_2$Bj5_dispemissco <- 0 - waste_2$Bj5_d * waste_2$Diesel_GHG - waste_2$Bj5_g * waste_2$Gasoline_GHG
waste_2$Bj5_netemiss <- waste_2$Bj5_collectionemiss + waste_2$Bj5_transport1emiss + waste_2$Bj5_processemiss + waste_2$Bj5_transport2emiss + waste_2$Bj5_enduseemiss + waste_2$Bj5_dispemiss

#Bj6
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2$Bj6_d <- waste_2$Prod_ww * waste_2$Bj6.d.out
waste_2$Bj6_g <- waste_2$Prod_ww * waste_2$Bj6.g.out
waste_2$Bj6_j <- waste_2$Prod_ww * waste_2$Bj6.j.out
waste_2$Bj6_energy <- waste_2$Bj6_d + waste_2$Bj6_g + waste_2$Bj6_j 
waste_2$Bj6_energymain <- waste_2$Bj6_j
waste_2$Bj6_energyco <- waste_2$Bj6_d + waste_2$Bj6_g 
waste_2$Bj6_netenergy <-(waste_2$Bj6_energy - waste_2$Prod_ww * waste_2$Bj6.h2.in - waste_2$Prod_ww * waste_2$Bj6.e.in -
                           (waste_2$collection_en +  waste_2$transport1_en)  -
                           (waste_2$Bj6_d / 42.79 + waste_2$Bj6_g / 41.74 +waste_2$Bj6_j / 43.10) * waste_2$Transport_km_2 * waste_2$Transport_Diesel) * waste_2$Bj6_tech
waste_2$Bj6_collectionemiss <- waste_2$collection_emiss * waste_2$Bj6_tech
waste_2$Bj6_transport1emiss <- waste_2$transport1_emiss * waste_2$Bj6_tech
waste_2$Bj6_processemiss <- waste_2$Prod_ww * (waste_2$Bj6.h2.in * waste_2$H2_GHG + waste_2$Bj6.e.in * waste_2$Powergen_GHG) * waste_2$Bj6_tech
waste_2$Bj6_transport2emiss <-  (waste_2$Bj6_d / 42.79 + waste_2$Bj6_g / 41.74 + waste_2$Bj6_j / 43.10) * waste_2$Transport_km_2 * waste_2$Transport_Diesel * waste_2$Diesel_GHG
waste_2$Bj6_enduseemiss <- waste_2$Prod_ww * waste_2$Nonbio_emiss2 * waste_2$Bj6_tech
waste_2$Bj6_dispemiss <- 0 - waste_2$Bj6_d *waste_2$Diesel_GHG - waste_2$Bj6_g* waste_2$Gasoline_GHG -  waste_2$Bj6_j * waste_2$Jet_GHG 
waste_2$Bj6_dispemissmain <- 0 - waste_2$Bj6_j * waste_2$Jet_GHG
waste_2$Bj6_dispemissco <- 0 - waste_2$Bj6_d * waste_2$Diesel_GHG - waste_2$Bj6_g * waste_2$Gasoline_GHG
waste_2$Bj6_netemiss <- waste_2$Bj6_collectionemiss + waste_2$Bj6_transport1emiss + waste_2$Bj6_processemiss + waste_2$Bj6_transport2emiss + waste_2$Bj6_enduseemiss + waste_2$Bj6_dispemiss

#US results by waste type
#energy main & co-products
waste_energy_US <- subset(waste_2, select = c( Waste_type, Prod_ww, 
                                               E1_energymain, E1_energyco,
                                               E2_energymain, E2_energyco,
                                               E3_energymain, 
                                               E4_energymain, E4_energyco, 
                                               M1_energymain, 
                                               M2_energymain, 
                                               Eth1_energymain, Eth1_energyco, 
                                               Rd1_energymain, Rd1_energyco, 
                                               Rd2_energymain, Rd2_energyco, 
                                               Bj1_energymain, Bj1_energyco, 
                                               Bj2_energymain,  
                                               Bj3_energymain, Bj3_energyco, 
                                               Bj4_energymain, Bj4_energyco,
                                               Bj5_energymain, Bj5_energyco, 
                                               Bj6_energymain, Bj6_energyco))
waste_energy_US  <- aggregate(.~Waste_type, waste_energy_US, sum)
waste_energy_US_1 <- gather(waste_energy_US, category, Energy_GJ, E1_energymain:Bj6_energyco)
waste_energy_US_1$Energy_GJ_per <- waste_energy_US_1$Energy_GJ / waste_energy_US_1$Prod_ww
waste_energy_US_1 <- separate(data = waste_energy_US_1, col = category, into = c("Tech", "Category"), sep = "\\_")
waste_energy_US_1$Tech <- factor(waste_energy_US_1$Tech, 
                                 levels = c("E1", "E2", "E3", "E4", "M1", "M2", "Eth1", "Rd1", "Rd2",
                                            "Bj1", "Bj2", "Bj3", "Bj4", "Bj5", "Bj6"))
#netenergy
waste_netenergy_US <- subset(waste_2, select = c(Waste_type, Prod_ww, 
                                                 E1_netenergy,
                                                 E2_netenergy,
                                                 E3_netenergy, 
                                                 E4_netenergy, 
                                                 M1_netenergy, 
                                                 M2_netenergy, 
                                                 Eth1_netenergy, 
                                                 Rd1_netenergy, 
                                                 Rd2_netenergy, 
                                                 Bj1_netenergy, 
                                                 Bj2_netenergy,  
                                                 Bj3_netenergy, 
                                                 Bj4_netenergy,
                                                 Bj5_netenergy, 
                                                 Bj6_netenergy))

waste_netenergy_US  <- aggregate(.~Waste_type, waste_netenergy_US, sum)
waste_netenergy_US_1 <- gather(waste_netenergy_US, category, Netenergy_GJ, E1_netenergy:Bj6_netenergy)
waste_netenergy_US_1$Netenergy_GJ_per <- waste_netenergy_US_1$Netenergy_GJ / waste_netenergy_US_1$Prod_ww
waste_netenergy_US_1 <- separate(data = waste_netenergy_US_1, col = category, into = c("Tech", "Category"), sep = "\\_")
waste_netenergy_US_1$Tech <- factor(waste_netenergy_US_1$Tech, 
                                    levels = c("E1", "E2", "E3", "E4", "M1", "M2", "Eth1", "Rd1", "Rd2",
                                               "Bj1", "Bj2", "Bj3", "Bj4", "Bj5", "Bj6"))

#EROI
waste_energy_tot <- subset(waste_2, select = c(Waste_type, Prod_ww, 
                                               E1_energy,
                                               E2_energy,
                                               E3_energy, 
                                               E4_energy, 
                                               M1_energy, 
                                               M2_energy, 
                                               Eth1_energy, 
                                               Rd1_energy, 
                                               Rd2_energy, 
                                               Bj1_energy, 
                                               Bj2_energy,  
                                               Bj3_energy, 
                                               Bj4_energy,
                                               Bj5_energy, 
                                               Bj6_energy))
waste_energy_tot  <- aggregate(.~Waste_type, waste_energy_tot, sum)
waste_energy_tot_1 <- gather(waste_energy_tot, category, Energy_GJ, E1_energy:Bj6_energy)
waste_energy_tot_1$Energy_GJ_per <- waste_energy_tot_1$Energy_GJ / waste_energy_tot_1$Prod_ww
waste_energy_tot_1 <- separate(data = waste_energy_tot_1, col = category, into = c("Tech", "Category"), sep = "\\_")
waste_energy_tot_1$Tech <- factor(waste_energy_tot_1$Tech, 
                                  levels = c("E1", "E2", "E3", "E4", "M1", "M2", "Eth1", "Rd1", "Rd2",
                                             "Bj1", "Bj2", "Bj3", "Bj4", "Bj5", "Bj6"))

waste_eroi <- merge(subset(waste_energy_tot_1, select = -c(Category)), subset(waste_netenergy_US_1, select = -c(Category)), by=c("Waste_type", "Tech", "Prod_ww"))
waste_eroi_1 <- waste_eroi[which(waste_eroi$Energy_GJ_per>0),]
waste_eroi_1$EROI <- waste_eroi_1$Energy_GJ_per / (waste_eroi_1$Energy_GJ_per - waste_eroi_1$Netenergy_GJ_per)

#emissions by stage
waste_emiss_US <- subset(waste_2, select = c(Waste_type, Prod_ww, 
                                             E1_collectionemiss, E1_transport1emiss, E1_processemiss, E1_dispemissmain, E1_dispemissco, E1_netemiss, E1_enduseemiss, 
                                             E2_collectionemiss, E2_transport1emiss, E2_processemiss, E2_dispemissmain, E2_dispemissco, E2_netemiss, E2_enduseemiss,
                                             E3_collectionemiss, E3_transport1emiss, E3_processemiss, E3_dispemissmain, E3_netemiss, E3_enduseemiss,
                                             E4_collectionemiss, E4_transport1emiss, E4_processemiss, E4_dispemissmain, E4_dispemissco, E4_netemiss, E4_enduseemiss,
                                             M1_collectionemiss, M1_transport1emiss, M1_processemiss, M1_dispemissmain, M1_netemiss, M1_enduseemiss,
                                             M2_collectionemiss, M2_transport1emiss, M2_processemiss, M2_dispemissmain, M2_netemiss, M2_enduseemiss,
                                             Eth1_collectionemiss, Eth1_transport1emiss, Eth1_processemiss, Eth1_transport2emiss, Eth1_dispemissmain, Eth1_dispemissco, Eth1_netemiss, Eth1_enduseemiss,
                                             Rd1_collectionemiss, Rd1_transport1emiss, Rd1_processemiss, Rd1_transport2emiss, Rd1_dispemissmain, Rd1_dispemissco, Rd1_netemiss, Rd1_enduseemiss,
                                             Rd2_collectionemiss, Rd2_transport1emiss, Rd2_processemiss, Rd2_transport2emiss, Rd2_dispemissmain, Rd2_dispemissco, Rd2_netemiss, Rd2_enduseemiss,
                                             Bj1_collectionemiss, Bj1_transport1emiss, Bj1_processemiss, Bj1_transport2emiss, Bj1_dispemissmain, Bj1_dispemissco, Bj1_netemiss, Bj1_enduseemiss,
                                             Bj2_collectionemiss, Bj2_transport1emiss, Bj2_processemiss, Bj2_transport2emiss, Bj2_dispemissmain, Bj2_netemiss, Bj2_enduseemiss,
                                             Bj3_collectionemiss, Bj3_transport1emiss, Bj3_processemiss, Bj3_transport2emiss, Bj3_dispemissmain, Bj3_dispemissco, Bj3_netemiss, Bj3_enduseemiss,
                                             Bj4_collectionemiss, Bj4_transport1emiss, Bj4_processemiss, Bj4_transport2emiss, Bj4_dispemissmain, Bj4_dispemissco, Bj4_netemiss, Bj4_enduseemiss,
                                             Bj5_collectionemiss, Bj5_transport1emiss, Bj5_processemiss, Bj5_transport2emiss, Bj5_dispemissmain, Bj5_dispemissco, Bj5_netemiss, Bj5_enduseemiss,
                                             Bj6_collectionemiss, Bj6_transport1emiss, Bj6_processemiss, Bj6_transport2emiss, Bj6_dispemissmain, Bj6_dispemissco, Bj6_netemiss, Bj6_enduseemiss))

waste_emiss_US <- aggregate(.~Waste_type, waste_emiss_US, sum)
waste_emiss_US_1 <- gather(waste_emiss_US, category, emiss_kg, E1_collectionemiss:Bj6_enduseemiss)
waste_emiss_US_1$category <- gsub("emiss", "", waste_emiss_US_1$category)
waste_emiss_US_1 <- separate(data = waste_emiss_US_1, col = category, into = c("Tech", "Stage"), sep = "\\_")
waste_emiss_US_1$emiss_MT_per <- waste_emiss_US_1$emiss_kg / waste_emiss_US_1$Prod_ww / 1000
waste_emiss_US_1$Tech <- factor(waste_emiss_US_1$Tech, 
                                levels = c("E1", "E2","E3","E4", "M1", "M2", "Eth1", "Rd1", "Rd2",
                                           "Bj1", "Bj2", "Bj3", "Bj4", "Bj5", "Bj6", "Current"))
waste_emiss_US_1$Stage <- factor(waste_emiss_US_1$Stage, 
                                 levels = c("collection", "transport1","process","transport2", "dispmain", "dispco", "net", "enduse"))
waste_emiss_US_2 <- subset(waste_emiss_US_1, waste_emiss_US_1$Stage != "net")
waste_emiss_US_3 <- rbind(waste_emiss_US_2, current)
waste_emiss_US_4 <- subset(waste_emiss_US_1, waste_emiss_US_1$Stage == "net")


#US results by feedstock
#energy main & co-products
waste_energy_fs <- subset(waste_2, select = c(Feedstock, Prod_ww, 
                                              E1_energymain, E1_energyco,
                                              E2_energymain, E2_energyco,
                                              E3_energymain, 
                                              E4_energymain, E4_energyco, 
                                              M1_energymain, 
                                              M2_energymain, 
                                              Eth1_energymain, Eth1_energyco, 
                                              Rd1_energymain, Rd1_energyco, 
                                              Rd2_energymain, Rd2_energyco, 
                                              Bj1_energymain, Bj1_energyco, 
                                              Bj2_energymain,  
                                              Bj3_energymain, Bj3_energyco, 
                                              Bj4_energymain, Bj4_energyco,
                                              Bj5_energymain, Bj5_energyco, 
                                              Bj6_energymain, Bj6_energyco))
waste_energy_fs <- aggregate(.~Feedstock, waste_energy_fs, sum)
waste_energy_fs_1 <- gather(waste_energy_fs, category, Energy_GJ, E1_energymain:Bj6_energyco)
waste_energy_fs_1$Energy_GJ_per <- waste_energy_fs_1$Energy_GJ / waste_energy_fs_1$Prod_ww
waste_energy_fs_1 <- separate(data = waste_energy_fs_1, col = category, into = c("Tech", "Category"), sep = "\\_")
waste_energy_fs_1$Tech <- factor(waste_energy_fs_1$Tech, 
                                 levels = c("E1", "E2", "E3", "E4", "M1", "M2", "Eth1", "Rd1", "Rd2",
                                            "Bj1", "Bj2", "Bj3", "Bj4", "Bj5", "Bj6"))
waste_energy_fs_1$Waste_type <- tech$Waste_type[match(waste_energy_fs_1$Feedstock, tech$Feedstock)] 
waste_energy_fs_1$Feedstock <- factor(waste_energy_fs_1$Feedstock, 
                                      levels = c("Barley straw", "Citrus residues", "Corn stover", "Cotton gin trash", "Cotton residue", "Noncitrus residues", "Oats straw", "Rice hulls", 
                                                 "Rice straw", "Sorghum stubble", "Sugarcane bagasse", "Sugarcane trash", "Tree nut residues", "Wheat straw", "Hogs, 1000+ head", 
                                                 "Milk cows, 500+ head", "Food waste", "Primary mill residue", "Secondary mill residue", "Other forest residue", "Other forest thinnings", 
                                                 "CD waste", "MSW wood", "Other MSW", "Paper and paperboard", "Plastics", "Rubber and leather", "Textiles", "Yard trimmings"))

#netenergy
waste_netenergy_fs <- subset(waste_2, select = c(Feedstock, Prod_ww, 
                                                 E1_netenergy,
                                                 E2_netenergy,
                                                 E3_netenergy, 
                                                 E4_netenergy, 
                                                 M1_netenergy, 
                                                 M2_netenergy, 
                                                 Eth1_netenergy, 
                                                 Rd1_netenergy, 
                                                 Rd2_netenergy, 
                                                 Bj1_netenergy, 
                                                 Bj2_netenergy,  
                                                 Bj3_netenergy, 
                                                 Bj4_netenergy,
                                                 Bj5_netenergy, 
                                                 Bj6_netenergy))
waste_netenergy_fs <- aggregate(.~Feedstock, waste_netenergy_fs, sum)
waste_netenergy_fs_1 <- gather(waste_netenergy_fs, category, Netenergy_GJ, E1_netenergy:Bj6_netenergy)
waste_netenergy_fs_1$Netenergy_GJ_per <- waste_netenergy_fs_1$Netenergy_GJ / waste_netenergy_fs_1$Prod_ww
waste_netenergy_fs_1 <- separate(data = waste_netenergy_fs_1, col = category, into = c("Tech", "Category"), sep = "\\_")
waste_netenergy_fs_1$Tech <- factor(waste_netenergy_fs_1$Tech, 
                                    levels = c("E1", "E2", "E3", "E4", "M1", "M2", "Eth1", "Rd1", "Rd2",
                                               "Bj1", "Bj2", "Bj3", "Bj4", "Bj5", "Bj6"))
waste_netenergy_fs_1$Waste_type <- tech$Waste_type[match(waste_netenergy_fs_1$Feedstock, tech$Feedstock)] 
waste_netenergy_fs_1$Feedstock <- factor(waste_netenergy_fs_1$Feedstock, 
                                         levels = c("Barley straw", "Citrus residues", "Corn stover", "Cotton gin trash", "Cotton residue", "Noncitrus residues", "Oats straw", "Rice hulls", 
                                                    "Rice straw", "Sorghum stubble", "Sugarcane bagasse", "Sugarcane trash", "Tree nut residues", "Wheat straw", "Hogs, 1000+ head", 
                                                    "Milk cows, 500+ head", "Food waste", "Primary mill residue", "Secondary mill residue", "Other forest residue", "Other forest thinnings", 
                                                    "CD waste", "MSW wood", "Other MSW", "Paper and paperboard", "Plastics", "Rubber and leather", "Textiles", "Yard trimmings"))


#EROI
waste_energy_tot_fs <- subset(waste_2, select = c(Feedstock, Prod_ww, 
                                                  E1_energy,
                                                  E2_energy,
                                                  E3_energy, 
                                                  E4_energy, 
                                                  M1_energy, 
                                                  M2_energy, 
                                                  Eth1_energy, 
                                                  Rd1_energy, 
                                                  Rd2_energy, 
                                                  Bj1_energy, 
                                                  Bj2_energy,  
                                                  Bj3_energy, 
                                                  Bj4_energy,
                                                  Bj5_energy, 
                                                  Bj6_energy))
waste_energy_tot_fs  <- aggregate(.~Feedstock, waste_energy_tot_fs, sum)
waste_energy_tot_fs_1 <- gather(waste_energy_tot_fs, category, Energy_GJ, E1_energy:Bj6_energy)
waste_energy_tot_fs_1$Energy_GJ_per <- waste_energy_tot_fs_1$Energy_GJ / waste_energy_tot_fs_1$Prod_ww
waste_energy_tot_fs_1 <- separate(data = waste_energy_tot_fs_1, col = category, into = c("Tech", "Category"), sep = "\\_")
waste_energy_tot_fs_1$Tech <- factor(waste_energy_tot_fs_1$Tech, 
                                     levels = c("E1", "E2", "E3", "E4", "M1", "M2", "Eth1", "Rd1", "Rd2",
                                                "Bj1", "Bj2", "Bj3", "Bj4", "Bj5", "Bj6"))
waste_eroi_fs <- merge(subset(waste_energy_tot_fs_1, select = -c(Category)), subset(waste_netenergy_fs_1, select = -c(Category)), by=c("Feedstock", "Tech", "Prod_ww"))
waste_eroi_fs_1 <- waste_eroi_fs[which(waste_eroi_fs$Energy_GJ_per>0),]
waste_eroi_fs_1$EROI <- waste_eroi_fs_1$Energy_GJ_per / (waste_eroi_fs_1$Energy_GJ_per - waste_eroi_fs_1$Netenergy_GJ_per)
waste_eroi_fs_1$Feedstock <- factor(waste_eroi_fs_1$Feedstock, 
                                    levels = c("Barley straw", "Citrus residues", "Corn stover", "Cotton gin trash", "Cotton residue", "Noncitrus residues", "Oats straw", "Rice hulls", 
                                               "Rice straw", "Sorghum stubble", "Sugarcane bagasse", "Sugarcane trash", "Tree nut residues", "Wheat straw", "Hogs, 1000+ head", 
                                               "Milk cows, 500+ head", "Food waste", "Primary mill residue", "Secondary mill residue", "Other forest residue", "Other forest thinnings", 
                                               "CD waste", "MSW wood", "Other MSW", "Paper and paperboard", "Plastics", "Rubber and leather", "Textiles", "Yard trimmings"))


#emissions by stage
waste_emiss_fs <- subset(waste_2, select = c(Feedstock, Prod_ww, 
                                             E1_collectionemiss, E1_transport1emiss, E1_processemiss, E1_dispemissmain, E1_dispemissco, E1_netemiss, E1_enduseemiss, 
                                             E2_collectionemiss, E2_transport1emiss, E2_processemiss, E2_dispemissmain, E2_dispemissco, E2_netemiss, E2_enduseemiss,
                                             E3_collectionemiss, E3_transport1emiss, E3_processemiss, E3_dispemissmain, E3_netemiss, E3_enduseemiss,
                                             E4_collectionemiss, E4_transport1emiss, E4_processemiss, E4_dispemissmain, E4_dispemissco, E4_netemiss, E4_enduseemiss,
                                             M1_collectionemiss, M1_transport1emiss, M1_processemiss, M1_dispemissmain, M1_netemiss, M1_enduseemiss,
                                             M2_collectionemiss, M2_transport1emiss, M2_processemiss, M2_dispemissmain, M2_netemiss, M2_enduseemiss,
                                             Eth1_collectionemiss, Eth1_transport1emiss, Eth1_processemiss, Eth1_transport2emiss, Eth1_dispemissmain, Eth1_dispemissco, Eth1_netemiss, Eth1_enduseemiss,
                                             Rd1_collectionemiss, Rd1_transport1emiss, Rd1_processemiss, Rd1_transport2emiss, Rd1_dispemissmain, Rd1_dispemissco, Rd1_netemiss, Rd1_enduseemiss,
                                             Rd2_collectionemiss, Rd2_transport1emiss, Rd2_processemiss, Rd2_transport2emiss, Rd2_dispemissmain, Rd2_dispemissco, Rd2_netemiss, Rd2_enduseemiss,
                                             Bj1_collectionemiss, Bj1_transport1emiss, Bj1_processemiss, Bj1_transport2emiss, Bj1_dispemissmain, Bj1_dispemissco, Bj1_netemiss, Bj1_enduseemiss,
                                             Bj2_collectionemiss, Bj2_transport1emiss, Bj2_processemiss, Bj2_transport2emiss, Bj2_dispemissmain, Bj2_netemiss, Bj2_enduseemiss,
                                             Bj3_collectionemiss, Bj3_transport1emiss, Bj3_processemiss, Bj3_transport2emiss, Bj3_dispemissmain, Bj3_dispemissco, Bj3_netemiss, Bj3_enduseemiss,
                                             Bj4_collectionemiss, Bj4_transport1emiss, Bj4_processemiss, Bj4_transport2emiss, Bj4_dispemissmain, Bj4_dispemissco, Bj4_netemiss, Bj4_enduseemiss,
                                             Bj5_collectionemiss, Bj5_transport1emiss, Bj5_processemiss, Bj5_transport2emiss, Bj5_dispemissmain, Bj5_dispemissco, Bj5_netemiss, Bj5_enduseemiss,
                                             Bj6_collectionemiss, Bj6_transport1emiss, Bj6_processemiss, Bj6_transport2emiss, Bj6_dispemissmain, Bj6_dispemissco, Bj6_netemiss, Bj6_enduseemiss))
waste_emiss_fs <- aggregate(.~Feedstock, waste_emiss_fs, sum)
waste_emiss_fs_1 <- gather(waste_emiss_fs, category, emiss_kg, E1_collectionemiss:Bj6_enduseemiss)
waste_emiss_fs_1$category <- gsub("emiss", "", waste_emiss_fs_1$category)
waste_emiss_fs_1 <- separate(data = waste_emiss_fs_1, col = category, into = c("Tech", "Stage"), sep = "\\_")
waste_emiss_fs_1$emiss_MT_per <- waste_emiss_fs_1$emiss_kg / waste_emiss_fs_1$Prod_ww / 1000
waste_emiss_fs_1$Tech <- factor(waste_emiss_fs_1$Tech, 
                                levels = c("E1", "E2","E3","E4", "M1", "M2", "Eth1", "Rd1", "Rd2",
                                           "Bj1", "Bj2", "Bj3", "Bj4", "Bj5", "Bj6"))
waste_emiss_fs_1$Stage <- factor(waste_emiss_fs_1$Stage, 
                                 levels = c("collection", "transport1","process","transport2", "dispmain", "dispco", "net", "enduse"))
waste_emiss_fs_1$Feedstock <- factor(waste_emiss_fs_1$Feedstock, 
                                     levels = c("Barley straw", "Citrus residues", "Corn stover", "Cotton gin trash", "Cotton residue", "Noncitrus residues", "Oats straw", "Rice hulls", 
                                                "Rice straw", "Sorghum stubble", "Sugarcane bagasse", "Sugarcane trash", "Tree nut residues", "Wheat straw", "Hogs, 1000+ head", 
                                                "Milk cows, 500+ head", "Food waste", "Primary mill residue", "Secondary mill residue", "Other forest residue", "Other forest thinnings", 
                                                "CD waste", "MSW wood", "Other MSW", "Paper and paperboard", "Plastics", "Rubber and leather", "Textiles", "Yard trimmings"))
waste_emiss_fs_2 <- subset(waste_emiss_fs_1, waste_emiss_fs_1$Stage != "net")
waste_emiss_fs_3 <- subset(waste_emiss_fs_1, waste_emiss_fs_1$Stage == "net")


##Charts

#By waste type
#figure 2a - energy
colors_energy <- c("energymain" = "#2171B5", 
                   "energyco" = "#4292C6",
                   "netenergy" = "#08306B")
p <- ggplot()+ 
  geom_bar(data = waste_energy_US_1[which(waste_energy_US_1$Energy_GJ_per>0),],
           aes(x=Tech, y=Energy_GJ_per, fill=Category), stat="identity", position = "stack") +
  geom_bar(data = waste_netenergy_US_1[which(waste_netenergy_US_1$Netenergy_GJ_per != 0),], 
           aes(x=Tech, y=Netenergy_GJ_per, fill=Category), stat="identity", width=0.5)+
  geom_hline(yintercept=0, size=0.05)+
  theme_bw() +
  theme(text = element_text(size=20)) +
  scale_fill_manual(values=colors_energy, breaks=c("energymain", "energyco","netenergy"),
                    labels = c("Energy production/main product", "Energy production/co-product(s)", "Net energy")) +
  guides(fill = guide_legend(title = "", label.theme = element_text(size = 20, angle = 0))) +
  scale_y_continuous(name="GJ/Mg ww", limits =  c(-5,15)) +
  labs(x = '') +
  theme(legend.position="top",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_grid(~Waste_type, scales = "free_x", space = "free_x")+
  theme(strip.background =element_rect(fill="white"), strip.text = element_text(size = 20, face = "bold")) +
  ggtitle("(a) Energy") +
  theme(plot.title = element_text(face = "bold", size = 24, hjust = 0))
ggsave(paste(County_FOLDER, "/energy_nobioCO2.png", sep=""), plot=p, width=18.5,height=4.5,units="in",dpi=300)


#figure 2b - EROI
p <- ggplot()+ 
  geom_bar(data = waste_eroi_1, aes(x=Tech, y=EROI), stat="identity", fill="#F16913") +
  geom_hline(yintercept=0, size=0.05)+
  geom_hline(yintercept=1, size=0.2)+
  theme_bw() +
  theme(text = element_text(size=20)) +
  guides(fill = FALSE) +
  scale_y_continuous(name="", limits =  c(0,20)) +
  labs(x = '') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_grid(~Waste_type, scales = "free_x", space = "free_x")+
  theme(strip.background =element_rect(fill="white"), strip.text = element_text(size = 20, face = "bold")) +
  ggtitle("(b) Energy return on investment") +
  theme(plot.title = element_text(face = "bold", size = 24, hjust = 0))
ggsave(paste(County_FOLDER, "/eroi_nobioCO2.png", sep=""), plot=p, width=18.5,height=4.5,units="in",dpi=300)


#figure 2c - emiss by stage (3 column legend)
colors_stage <- c("dispmain" = "#33A02C",
                  "dispco" = "#66C2A5",
                  "collection" = "#FC8D62",
                  "transport1" = "#A6CEE3",
                  "process" = "#1F78B4",
                  "transport2" = "#FDBF6F",
                  "enduse" = "#FB9A99",
                  "net" = "#762A83",
                  "all" = "#B3B3B3")
p <- ggplot()+ 
  geom_bar(data = waste_emiss_US_3[which(waste_emiss_US_3$emiss_MT_per != 0),],
           aes(x=Tech, y=emiss_MT_per, fill=Stage), stat="identity", position = "stack") +
  geom_bar(data = waste_emiss_US_4[which(waste_emiss_US_4$emiss_MT_per != 0),], 
           aes(x=Tech, y=emiss_MT_per, fill=Stage), stat="identity", width=0.5)+
  geom_hline(yintercept=0, size=0.05)+
  theme_bw() +
  theme(text = element_text(size=20)) +
  scale_fill_manual(values=colors_stage, breaks=c("collection", "transport1","process","transport2", "enduse", "dispmain", "dispco", "net", "all"),
                    labels = c("Collection", "Transport to facility", "Processing", 
                               "Transmission & Distribution", "Enduse", "Displacement/main product", "Displacement/co-product(s)","Net GWP", "Current management practices")) +
  guides(fill = guide_legend(title = "", label.theme = element_text(size = 20, angle = 0), ncol = 5)) +
  scale_y_continuous(name="Metric ton CO2e/Mg ww", limits =  c(-1.5,2)) +
  labs(x = '') +
  theme(legend.position="top",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_grid(~Waste_type, scales = "free_x", space = "free_x")+
  theme(strip.background =element_rect(fill="white"), strip.text = element_text(size = 20, face = "bold"))+
  ggtitle("(c) GWP - excluding biogenic CO2") +
  theme(plot.title = element_text(face = "bold", size = 24, hjust = 0))
ggsave(paste(County_FOLDER, "/emiss_by_stage_nobioCO2.png", sep=""), plot=p, width=20.5,height=6,units="in",dpi=300)
#ggsave(paste(County_FOLDER, "/emiss_by_stage_nobioCO2.pdf", sep=""), plot=p, width=20.5,height=6,units="in")

p <- ggplot()+ 
  geom_bar(data = waste_emiss_US_3[which(waste_emiss_US_3$emiss_MT_per != 0),],
           aes(x=Tech, y=emiss_MT_per, fill=Stage), stat="identity", position = "stack") +
  geom_bar(data = waste_emiss_US_4[which(waste_emiss_US_4$emiss_MT_per != 0),], 
           aes(x=Tech, y=emiss_MT_per, fill=Stage), stat="identity", width=0.5)+
  geom_hline(yintercept=0, size=0.05)+
  theme_bw() +
  theme(text = element_text(size=20)) +
  scale_fill_manual(values=colors_stage, breaks=c("collection", "transport1","process","transport2", "enduse", "dispmain", "dispco", "net", "all"),
                    labels = c("Collection", "Transport to facility", "Processing", 
                               "Transmission & Distribution", "Enduse", "Displacement/main product", "Displacement/co-product(s)","Net GWP", "Current management practices")) +
  guides(fill = guide_legend(title = "", label.theme = element_text(size = 20, angle = 0), ncol = 5)) +
  scale_y_continuous(name="Metric ton CO2e/Mg ww", limits =  c(-1.5,2)) +
  labs(x = '') +
  theme(legend.position="top",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_grid(~Waste_type, scales = "free_x", space = "free_x")+
  theme(strip.background =element_rect(fill="white"), strip.text = element_text(size = 20, face = "bold"))+
  ggtitle("(d) GWP - excluding biogenic CO2") +
  theme(plot.title = element_text(face = "bold", size = 24, hjust = 0))
ggsave(paste(County_FOLDER, "/emiss_by_stage_nobioCO2_1.png", sep=""), plot=p, width=20.5,height=6,units="in",dpi=300)
#ggsave(paste(County_FOLDER, "/emiss_by_stage_nobioCO2_1.pdf", sep=""), plot=p, width=20.5,height=6,units="in")



#by feedstok
##energy

p <- ggplot()+ 
  geom_bar(data = waste_energy_fs_1[which(waste_energy_fs_1$Energy_GJ_per>0),],
           aes(x=Tech, y=Energy_GJ_per, fill=Category), stat="identity", position = "stack") +
  geom_bar(data = waste_netenergy_fs_1[which(waste_netenergy_fs_1$Netenergy_GJ_per != 0),], 
           aes(x=Tech, y=Netenergy_GJ_per, fill=Category), stat="identity", width=0.5)+
  geom_hline(yintercept=0, size=0.05)+
  theme_bw() +
  theme(text = element_text(size=32)) +
  scale_fill_manual(values=colors_energy, breaks=c("energymain", "energyco","netenergy"),
                    labels = c("Energy production/main product", "Energy production/co-product(s)", "Net energy")) +
  guides(fill = guide_legend(title = "", label.theme = element_text(size = 44, angle = 0))) +
  scale_y_continuous(name="GJ/Mg ww", limits =  c(-5,30)) +
  labs(x = '') +
  theme(legend.position=c(0.8,0.1),
        legend.key = element_rect(size = 32),
        legend.key.height = unit(1, "in"),
        legend.key.width = unit(1, "in"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3),
        axis.title.y =  element_text(size=40, face = "bold"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_wrap(~Feedstock, scales = "free", ncol = 8)+
  theme(strip.background =element_rect(fill="white"), strip.text = element_text(size = 32, face = "bold")) 
#ggtitle("(a) Energy") +
#theme(plot.title = element_text(face = "bold", size = 24, hjust = 0))
ggsave(paste(County_FOLDER, "/energy_fs_nobioCO2.png", sep=""), plot=p, width=60,height=40,units="in",dpi=300, limitsize = FALSE)


##eroi
p <- ggplot()+ 
  geom_bar(data = waste_eroi_fs_1, aes(x=Tech, y=EROI), stat="identity", fill="#F16913") +
  geom_hline(yintercept=0, size=0.05)+
  geom_hline(yintercept=1, size=0.2)+
  theme_bw() +
  theme(text = element_text(size=32)) +
  guides(fill = FALSE) +
  scale_y_continuous(name="", limits =  c(0,30)) +
  labs(x = '') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3),
        axis.title.y =  element_text(size=40, face = "bold"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_wrap(~Feedstock, scales = "free", ncol = 8)+
  theme(strip.background =element_rect(fill="white"), strip.text = element_text(size = 32, face = "bold")) 
#ggtitle("(b) Energy return on investment") +
#theme(plot.title = element_text(face = "bold", size = 24, hjust = 0))
ggsave(paste(County_FOLDER, "/eroi_fs_nobioCO2.png", sep=""), plot=p, width=60,height=40,units="in",dpi=300, limitsize = FALSE)

##emissions
p <- ggplot()+ 
  geom_bar(data = waste_emiss_fs_2[which(waste_emiss_fs_2$emiss_MT_per != 0),],
           aes(x=Tech, y=emiss_MT_per, fill=Stage), stat="identity", position = "stack") +
  geom_bar(data = waste_emiss_fs_3[which(waste_emiss_fs_3$emiss_MT_per != 0),], 
           aes(x=Tech, y=emiss_MT_per, fill=Stage), stat="identity", width=0.5)+
  geom_hline(yintercept=0, size=0.05)+
  theme_bw() +
  theme(text = element_text(size=32)) +
  scale_fill_manual(values=colors_stage, breaks=c("collection", "transport1","process","transport2", "enduse", "dispmain", "dispco", "net"),
                    labels = c("Collection", "Transport to facility", "Processing", 
                               "Transmission & Distribution", "Enduse", "Displacement/main product", "Displacement/co-product(s)","Net GWP")) +
  guides(fill = guide_legend(title = "", label.theme = element_text(size = 40, angle = 0))) +
  scale_y_continuous(name="Metric ton CO2e/Mg ww", limits =  c(-4,4)) +
  labs(x = '') +
  theme(legend.position=c(0.8,0.1),
        legend.key = element_rect(size = 32),
        legend.key.height = unit(1, "in"),
        legend.key.width = unit(1, "in"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3),
        axis.title.y =  element_text(size=40, face = "bold"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_wrap(~Feedstock, scales = "free", ncol = 8)+
  theme(strip.background =element_rect(fill="white"), strip.text = element_text(size = 32, face = "bold")) 
ggsave(paste(County_FOLDER, "/emiss_by_stage_fs_nobioCO2.png", sep=""), plot=p, width=60,height=40,units="in",dpi=300, limitsize = FALSE)
###sensitivity analysis on power generation

#clean power (-50%)
waste_2_1 <- merge(waste_1, state_GHG_ef1)

#Collection & Transportation_1 (energy - GJ, emiss - kg CO1e)
waste_2_1$Prod_ww <- waste_2_1$Prod/(1-waste_2_1$MC/100)
waste_2_1$collection_en <- waste_2_1$Prod_ww * waste_2_1$Collection_Diesel
waste_2_1$collection_emiss <- waste_2_1$collection_en * waste_2_1$Diesel_GHG 
waste_2_1$transport1_en <- waste_2_1$Prod_ww * waste_2_1$Transport_km_1 * waste_2_1$Transport_Diesel
waste_2_1$transport1_emiss <- waste_2_1$transport1_en  * waste_2_1$Diesel_GHG 

#E1
#electricity T&D loss - 6.5%, heat loss - 20%
waste_2_1$E1_elec <- waste_2_1$Prod_ww * waste_2_1$E1.e.out * (1-0.065)
waste_2_1$E1_heat <- waste_2_1$Prod_ww * waste_2_1$E1.h.out * (1-0.2)
waste_2_1$E1_energy <- waste_2_1$E1_elec + waste_2_1$E1_heat
waste_2_1$E1_energymain <- waste_2_1$E1_elec
waste_2_1$E1_energyco <- waste_2_1$E1_heat
waste_2_1$E1_netenergy <- (waste_2_1$E1_energy - waste_2_1$Prod_ww * (waste_2_1$E1.e.in + waste_2_1$E1.h.in + waste_2_1$E1.d.in) - waste_2_1$collection_en -  waste_2_1$transport1_en) * waste_2_1$E1_tech
waste_2_1$E1_collectionemiss <- waste_2_1$collection_emiss * waste_2_1$E1_tech
waste_2_1$E1_transport1emiss <- waste_2_1$transport1_emiss * waste_2_1$E1_tech
waste_2_1$E1_processemiss <- waste_2_1$Prod_ww * (waste_2_1$E1.e.in * waste_2_1$Powergen_GHG 
                                                  + waste_2_1$E1.h.in * waste_2_1$Heatgen_GHG 
                                                  + waste_2_1$E1.d.in * waste_2_1$Diesel_GHG
                                                  + waste_2_1$Nonbio_emiss1) * waste_2_1$E1_tech
waste_2_1$E1_enduseemiss <- 0
waste_2_1$E1_dispemiss <- 0 - waste_2_1$E1_elec* waste_2_1$Powergen_GHG  - waste_2_1$E1_heat* waste_2_1$Heatgen_GHG 
waste_2_1$E1_dispemissmain <- 0 - waste_2_1$E1_elec* waste_2_1$Powergen_GHG
waste_2_1$E1_dispemissco <- 0 - waste_2_1$E1_heat* waste_2_1$Heatgen_GHG
waste_2_1$E1_netemiss <- waste_2_1$E1_collectionemiss + waste_2_1$E1_transport1emiss + waste_2_1$E1_processemiss + waste_2_1$E1_enduseemiss + waste_2_1$E1_dispemiss

#E2
#electricity T&D loss - 6.5%, heat loss - 20%
waste_2_1$E2_elec <- waste_2_1$Prod_ww * waste_2_1$E2.e.out * (1-0.065)
waste_2_1$E2_heat <- waste_2_1$Prod_ww * waste_2_1$E2.h.out * (1-0.2)
waste_2_1$E2_energy <- waste_2_1$E2_elec + waste_2_1$E2_heat
waste_2_1$E2_energymain <- waste_2_1$E2_elec
waste_2_1$E2_energyco <- waste_2_1$E2_heat
waste_2_1$E2_netenergy <- (waste_2_1$E2_energy - waste_2_1$Prod_ww * (waste_2_1$E2.e.in + waste_2_1$E2.h.in + waste_2_1$E2.d.in) - waste_2_1$collection_en - waste_2_1$transport1_en) * waste_2_1$E2_tech
waste_2_1$E2_collectionemiss <- waste_2_1$collection_emiss * waste_2_1$E2_tech
waste_2_1$E2_transport1emiss <- waste_2_1$transport1_emiss * waste_2_1$E2_tech
waste_2_1$E2_processemiss <- waste_2_1$Prod_ww * (waste_2_1$E2.e.in * waste_2_1$Powergen_GHG 
                                                  + waste_2_1$E2.h.in * waste_2_1$Heatgen_GHG
                                                  + waste_2_1$E2.d.in * waste_2_1$Diesel_GHG
                                                  + waste_2_1$Nonbio_emiss1)  * waste_2_1$E2_tech 
waste_2_1$E2_enduseemiss <- 0
waste_2_1$E2_dispemiss <- 0 - waste_2_1$E2_elec* waste_2_1$Powergen_GHG - waste_2_1$E2_heat* waste_2_1$Heatgen_GHG
waste_2_1$E2_dispemissmain <- 0 - waste_2_1$E2_elec* waste_2_1$Powergen_GHG
waste_2_1$E2_dispemissco <- 0 - waste_2_1$E2_heat* waste_2_1$Heatgen_GHG
waste_2_1$E2_netemiss <- waste_2_1$E2_collectionemiss + waste_2_1$E2_transport1emiss + waste_2_1$E2_processemiss + waste_2_1$E2_enduseemiss + waste_2_1$E2_dispemiss

#E3
#electricity T&D loss - 6.5%
waste_2_1$E3_energy <- waste_2_1$Prod_ww * waste_2_1$E3.e.out * (1-0.065)
waste_2_1$E3_energymain <- waste_2_1$E3_energy
waste_2_1$E3_netenergy <- (waste_2_1$E3_energy - waste_2_1$Prod_ww * (waste_2_1$E3.e.in + waste_2_1$E3.h.in + waste_2_1$E3.d.in) - waste_2_1$collection_en -  waste_2_1$transport1_en) * waste_2_1$E3_tech
waste_2_1$E3_collectionemiss <- waste_2_1$collection_emiss * waste_2_1$E3_tech
waste_2_1$E3_transport1emiss <- waste_2_1$transport1_emiss * waste_2_1$E3_tech
waste_2_1$E3_processemiss <- waste_2_1$Prod_ww * (waste_2_1$E3.e.in * waste_2_1$Powergen_GHG 
                                                  + waste_2_1$E3.h.in * waste_2_1$Heatgen_GHG
                                                  + waste_2_1$E3.d.in * waste_2_1$Diesel_GHG
                                                  + waste_2_1$Nonbio_emiss1)  * waste_2_1$E3_tech
waste_2_1$E3_enduseemiss <- 0
waste_2_1$E3_dispemiss <- 0 - waste_2_1$E3_energy* waste_2_1$Powergen_GHG
waste_2_1$E3_dispemissmain <- 0 - waste_2_1$E3_energy* waste_2_1$Powergen_GHG
waste_2_1$E3_netemiss <- waste_2_1$E3_collectionemiss + waste_2_1$E3_transport1emiss + waste_2_1$E3_processemiss + waste_2_1$E3_enduseemiss + waste_2_1$E3_dispemiss

#E4
#electricity T&D loss - 6.5%, heat loss - 20%
waste_2_1$E4_elec <- waste_2_1$Prod_ww * waste_2_1$E4.e.out * (1-0.065)
waste_2_1$E4_heat <- waste_2_1$Prod_ww * waste_2_1$E4.h.out * (1-0.2)
waste_2_1$E4_energy <- waste_2_1$E4_elec + waste_2_1$E4_heat
waste_2_1$E4_energymain <- waste_2_1$E4_elec
waste_2_1$E4_energyco <- waste_2_1$E4_heat
waste_2_1$E4_netenergy <- (waste_2_1$E4_energy - waste_2_1$Prod_ww * (waste_2_1$E4.ng.in + waste_2_1$E4.d.in) - waste_2_1$collection_en - waste_2_1$transport1_en) * waste_2_1$E4_tech
waste_2_1$E4_collectionemiss <- waste_2_1$collection_emiss * waste_2_1$E4_tech
waste_2_1$E4_transport1emiss <- waste_2_1$transport1_emiss * waste_2_1$E4_tech
waste_2_1$E4_processemiss <- waste_2_1$Prod_ww * (waste_2_1$E4.ng.in * waste_2_1$NG_GHG 
                                                  + waste_2_1$E4.d.in * waste_2_1$Diesel_GHG
                                                  + waste_2_1$Nonbio_emiss1) * waste_2_1$E4_tech 
waste_2_1$E4_enduseemiss <- 0
waste_2_1$E4_dispemiss <- 0 - waste_2_1$E4_elec* waste_2_1$Powergen_GHG - waste_2_1$E4_heat* waste_2_1$Heatgen_GHG
waste_2_1$E4_dispemissmain <- 0 - waste_2_1$E4_elec* waste_2_1$Powergen_GHG
waste_2_1$E4_dispemissco <- 0 - waste_2_1$E4_heat* waste_2_1$Heatgen_GHG
waste_2_1$E4_netemiss <- waste_2_1$E4_collectionemiss + waste_2_1$E4_transport1emiss + waste_2_1$E4_processemiss + waste_2_1$E4_enduseemiss  + waste_2_1$E4_dispemiss

#M1
#methane leakage - 2%
waste_2_1$M1_energy <- waste_2_1$Prod_ww * waste_2_1$M1.m.out * (1-0.02)
waste_2_1$M1_energymain <- waste_2_1$M1_energy
waste_2_1$M1_netenergy <- (waste_2_1$M1_energy - waste_2_1$Prod_ww * (waste_2_1$M1.e.in + waste_2_1$M1.h.in) - waste_2_1$collection_en - waste_2_1$transport1_en) * waste_2_1$M1_tech
waste_2_1$M1_collectionemiss <- waste_2_1$collection_emiss * waste_2_1$M1_tech
waste_2_1$M1_transport1emiss <- waste_2_1$transport1_emiss * waste_2_1$M1_tech
waste_2_1$M1_processemiss <- waste_2_1$Prod_ww * (waste_2_1$M1.e.in * waste_2_1$Powergen_GHG + waste_2_1$M1.h.in * waste_2_1$Heatgen_GHG)* waste_2_1$M1_tech
waste_2_1$M1_transport2emiss <- waste_2_1$Prod_ww * waste_2_1$M1.m.out *0.02 /50 *28 * waste_2_1$M1_tech
waste_2_1$M1_enduseemiss <- waste_2_1$Prod_ww * waste_2_1$Nonbio_emiss2  * waste_2_1$M1_tech
waste_2_1$M1_dispemiss <- 0 - waste_2_1$M1_energy* waste_2_1$NG_GHG
waste_2_1$M1_dispemissmain <- waste_2_1$M1_dispemiss
waste_2_1$M1_netemiss <- waste_2_1$M1_collectionemiss + waste_2_1$M1_transport1emiss + waste_2_1$M1_processemiss + waste_2_1$M1_transport2emiss + waste_2_1$M1_enduseemiss + waste_2_1$M1_dispemiss

#M2
#methane leakage - 2%
waste_2_1$M2_energy <- waste_2_1$Prod_ww * waste_2_1$M2.m.out * (1-0.02)
waste_2_1$M2_energymain <- waste_2_1$M2_energy
waste_2_1$M2_netenergy <- (waste_2_1$M2_energy - waste_2_1$Prod_ww * (waste_2_1$M2.e.in + waste_2_1$M2.h.in + waste_2_1$M2.d.in) - waste_2_1$collection_en -  waste_2_1$transport1_en) * waste_2_1$M2_tech
waste_2_1$M2_collectionemiss <- waste_2_1$collection_emiss * waste_2_1$M2_tech
waste_2_1$M2_transport1emiss <- waste_2_1$transport1_emiss * waste_2_1$M2_tech
waste_2_1$M2_processemiss <- waste_2_1$Prod_ww * (waste_2_1$M2.e.in * waste_2_1$Powergen_GHG + 
                                                    waste_2_1$M2.h.in * waste_2_1$Heatgen_GHG +
                                                    waste_2_1$M2.d.in * waste_2_1$Diesel_GHG) * waste_2_1$M2_tech
waste_2_1$M2_transport2emiss <- waste_2_1$Prod_ww * waste_2_1$M2.m.out *0.02 /50 *28 * waste_2_1$M1_tech
waste_2_1$M2_enduseemiss <- waste_2_1$Prod_ww * waste_2_1$Nonbio_emiss2 * waste_2_1$M2_tech
waste_2_1$M2_dispemiss <- 0 - waste_2_1$M2_energy* waste_2_1$NG_GHG
waste_2_1$M2_dispemissmain <- waste_2_1$M2_dispemiss
waste_2_1$M2_netemiss <- waste_2_1$M2_collectionemiss + waste_2_1$M2_transport1emiss + waste_2_1$M2_processemiss + waste_2_1$M2_transport2emiss + waste_2_1$M2_enduseemiss + waste_2_1$M2_dispemiss

#Eth1
#energy intensity of ethanol - 26.95 MJ/kg
waste_2_1$Eth1_elec <- waste_2_1$Prod_ww * waste_2_1$Eth1.e.out * (1-0.065)
waste_2_1$Eth1_eth <- waste_2_1$Prod_ww * waste_2_1$Eth1.eth.out
waste_2_1$Eth1_energy <- waste_2_1$Eth1_elec + waste_2_1$Eth1_eth
waste_2_1$Eth1_energymain <- waste_2_1$Eth1_eth
waste_2_1$Eth1_energyco <- waste_2_1$Eth1_elec
waste_2_1$Eth1_netenergy <- (waste_2_1$Eth1_energy - waste_2_1$Prod_ww * (waste_2_1$Eth1.ng.in  + waste_2_1$Eth1.d.in) -
                               (waste_2_1$collection_en +  waste_2_1$transport1_en) * waste_2_1$M1_tech -
                               waste_2_1$Eth1_eth / 26.95 * waste_2_1$Transport_km_2 * waste_2_1$Transport_Diesel) * waste_2_1$Eth1_tech
waste_2_1$Eth1_collectionemiss <- waste_2_1$collection_emiss * waste_2_1$Eth1_tech
waste_2_1$Eth1_transport1emiss <- waste_2_1$transport1_emiss * waste_2_1$Eth1_tech
waste_2_1$Eth1_processemiss <- waste_2_1$Prod_ww * (waste_2_1$Eth1.ng.in * waste_2_1$Heatgen_GHG + waste_2_1$Eth1.d.in * waste_2_1$Diesel_GHG) * waste_2_1$Eth1_tech 
waste_2_1$Eth1_transport2emiss <- waste_2_1$Eth1_eth / 26.95 * waste_2_1$Transport_km_2 * waste_2_1$Transport_Diesel * waste_2_1$Diesel_GHG 
waste_2_1$Eth1_enduseemiss <- waste_2_1$Prod_ww * waste_2_1$Nonbio_emiss2 * waste_2_1$Eth1_tech
waste_2_1$Eth1_dispemiss <- 0 - waste_2_1$Eth1_eth* waste_2_1$Gasoline_GHG  - waste_2_1$Eth1_elec * waste_2_1$Powergen_GHG
waste_2_1$Eth1_dispemissmain <- 0 - waste_2_1$Eth1_eth* waste_2_1$Gasoline_GHG
waste_2_1$Eth1_dispemissco <- 0 - waste_2_1$Eth1_elec * waste_2_1$Powergen_GHG
waste_2_1$Eth1_netemiss <- waste_2_1$Eth1_collectionemiss + waste_2_1$Eth1_transport1emiss + waste_2_1$Eth1_processemiss + waste_2_1$Eth1_transport2emiss + waste_2_1$Eth1_enduseemiss + waste_2_1$Eth1_dispemiss

#Rd1
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
#electricity T&D loss - 6.5%, methane leakage - 2%
waste_2_1$Rd1_d <- waste_2_1$Prod_ww * waste_2_1$Rd1.d.out
waste_2_1$Rd1_g <- waste_2_1$Prod_ww * waste_2_1$Rd1.g.out
waste_2_1$Rd1_j <- waste_2_1$Prod_ww * waste_2_1$Rd1.j.out
waste_2_1$Rd1_m <- waste_2_1$Prod_ww * waste_2_1$Rd1.m.out * (1-0.02)
waste_2_1$Rd1_elec <- waste_2_1$Prod_ww * waste_2_1$Rd1.e.out * (1-0.065)
waste_2_1$Rd1_energy <- waste_2_1$Rd1_d + waste_2_1$Rd1_g + waste_2_1$Rd1_j + waste_2_1$Rd1_m + waste_2_1$Rd1_elec
waste_2_1$Rd1_energymain <- waste_2_1$Rd1_d
waste_2_1$Rd1_energyco <- waste_2_1$Rd1_g + waste_2_1$Rd1_j + waste_2_1$Rd1_m + waste_2_1$Rd1_elec
waste_2_1$Rd1_netenergy <-(waste_2_1$Rd1_energy - waste_2_1$Prod_ww * waste_2_1$Rd1.e.in -
                             (waste_2_1$collection_en +  waste_2_1$transport1_en)  -
                             waste_2_1$Rd1_d / 42.79 * waste_2_1$Transport_km_2 * waste_2_1$Transport_Diesel -
                             waste_2_1$Rd1_g / 41.74 * waste_2_1$Transport_km_2 * waste_2_1$Transport_Diesel -
                             waste_2_1$Rd1_j / 43.10 * waste_2_1$Transport_km_2 * waste_2_1$Transport_Diesel ) * waste_2_1$Rd1_tech
waste_2_1$Rd1_collectionemiss <- waste_2_1$collection_emiss * waste_2_1$Rd1_tech
waste_2_1$Rd1_transport1emiss <- waste_2_1$transport1_emiss * waste_2_1$Rd1_tech
waste_2_1$Rd1_processemiss <- waste_2_1$Prod_ww * waste_2_1$Rd1.e.in * waste_2_1$Powergen_GHG * waste_2_1$Rd1_tech
waste_2_1$Rd1_transport2emiss <- (waste_2_1$Rd1_d / 42.79 + waste_2_1$Rd1_g / 41.74 + waste_2_1$Rd1_j / 43.10) * waste_2_1$Transport_km_2 * waste_2_1$Transport_Diesel * waste_2_1$Diesel_GHG  
waste_2_1$Rd1_enduseemiss <- waste_2_1$Prod_ww * waste_2_1$Nonbio_emiss2 * waste_2_1$Rd1_tech
waste_2_1$Rd1_dispemiss <- 0 - waste_2_1$Rd1_d *waste_2_1$Diesel_GHG - waste_2_1$Rd1_g * waste_2_1$Gasoline_GHG -  waste_2_1$Rd1_j * waste_2_1$Jet_GHG -
  waste_2_1$Rd1_m * waste_2_1$NG_GHG - waste_2_1$Rd1_elec * waste_2_1$Powergen_GHG 
waste_2_1$Rd1_dispemissmain <- 0 - waste_2_1$Rd1_d *waste_2_1$Diesel_GHG
waste_2_1$Rd1_dispemissco <- 0 - waste_2_1$Rd1_g * waste_2_1$Gasoline_GHG -  waste_2_1$Rd1_j * waste_2_1$Jet_GHG -
  waste_2_1$Rd1_m * waste_2_1$NG_GHG - waste_2_1$Rd1_elec * waste_2_1$Powergen_GHG 
waste_2_1$Rd1_netemiss <- waste_2_1$Rd1_collectionemiss + waste_2_1$Rd1_transport1emiss + waste_2_1$Rd1_processemiss + waste_2_1$Rd1_enduseemiss + waste_2_1$Rd1_transport2emiss + waste_2_1$Rd1_dispemiss


#Rd2
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74
waste_2_1$Rd2_d <- waste_2_1$Prod_ww * waste_2_1$Rd2.d.out
waste_2_1$Rd2_g <- waste_2_1$Prod_ww * waste_2_1$Rd2.g.out
waste_2_1$Rd2_energy <- waste_2_1$Rd2_d + waste_2_1$Rd2_g
waste_2_1$Rd2_energymain <- waste_2_1$Rd2_d
waste_2_1$Rd2_energyco <- waste_2_1$Rd2_g
waste_2_1$Rd2_netenergy <-(waste_2_1$Rd2_energy - waste_2_1$Prod_ww * (waste_2_1$Rd2.e.in + waste_2_1$Rd2.ng.in) -
                             (waste_2_1$collection_en +  waste_2_1$transport1_en)  -
                             waste_2_1$Rd2_d / 42.79 * waste_2_1$Transport_km_2 * waste_2_1$Transport_Diesel -
                             waste_2_1$Rd2_g / 41.74 * waste_2_1$Transport_km_2 * waste_2_1$Transport_Diesel ) * waste_2_1$Rd2_tech
waste_2_1$Rd2_collectionemiss <- waste_2_1$collection_emiss * waste_2_1$Rd2_tech
waste_2_1$Rd2_transport1emiss <- waste_2_1$transport1_emiss * waste_2_1$Rd2_tech
waste_2_1$Rd2_processemiss  <- waste_2_1$Prod_ww * (waste_2_1$Rd2.e.in * waste_2_1$Powergen_GHG + waste_2_1$Rd2.ng.in * waste_2_1$H2_GHG) * waste_2_1$Rd2_tech
waste_2_1$Rd2_transport2emiss <- (waste_2_1$Rd2_d / 42.79  + waste_2_1$Rd2_g / 41.74) * waste_2_1$Transport_km_2 * waste_2_1$Transport_Diesel * waste_2_1$Diesel_GHG 
waste_2_1$Rd2_enduseemiss <- waste_2_1$Prod_ww * waste_2_1$Nonbio_emiss2 * waste_2_1$Rd2_tech
waste_2_1$Rd2_dispemiss <- 0 - waste_2_1$Rd2_d *waste_2_1$Diesel_GHG - waste_2_1$Rd2_g* waste_2_1$Gasoline_GHG 
waste_2_1$Rd2_dispemissmain <- 0 - waste_2_1$Rd2_d *waste_2_1$Diesel_GHG 
waste_2_1$Rd2_dispemissco <- 0 - waste_2_1$Rd2_g* waste_2_1$Gasoline_GHG 
waste_2_1$Rd2_netemiss <- waste_2_1$Rd2_collectionemiss + waste_2_1$Rd2_transport1emiss + waste_2_1$Rd2_processemiss + waste_2_1$Rd2_transport2emiss + waste_2_1$Rd2_enduseemiss + waste_2_1$Rd2_dispemiss

#Bj1 
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_1$Bj1_d <- waste_2_1$Prod_ww * waste_2_1$Bj1.d.out
waste_2_1$Bj1_g <- waste_2_1$Prod_ww * waste_2_1$Bj1.g.out
waste_2_1$Bj1_j <- waste_2_1$Prod_ww * waste_2_1$Bj1.j.out
waste_2_1$Bj1_energy <- waste_2_1$Bj1_d + waste_2_1$Bj1_g + waste_2_1$Bj1_j 
waste_2_1$Bj1_energymain <- waste_2_1$Bj1_d
waste_2_1$Bj1_energyco <- waste_2_1$Bj1_g + waste_2_1$Bj1_j 
waste_2_1$Bj1_netenergy <-(waste_2_1$Bj1_energy - waste_2_1$Prod_ww * waste_2_1$Bj1.h2.in -
                             (waste_2_1$collection_en +  waste_2_1$transport1_en)  -
                             (waste_2_1$Bj1_d / 42.79 + waste_2_1$Bj1_g / 41.74 +waste_2_1$Bj1_j / 43.10) * waste_2_1$Transport_km_2 * waste_2_1$Transport_Diesel) * waste_2_1$Bj1_tech
waste_2_1$Bj1_collectionemiss <- waste_2_1$collection_emiss * waste_2_1$Bj1_tech
waste_2_1$Bj1_transport1emiss <- waste_2_1$transport1_emiss * waste_2_1$Bj1_tech
waste_2_1$Bj1_processemiss <- waste_2_1$Prod_ww * waste_2_1$Bj1.h2.in * waste_2_1$H2_GHG * waste_2_1$Bj1_tech
waste_2_1$Bj1_transport2emiss <- (waste_2_1$Bj1_d / 42.79 + waste_2_1$Bj1_g / 41.74 + waste_2_1$Bj1_j / 43.10) * waste_2_1$Transport_km_2 * waste_2_1$Transport_Diesel * waste_2_1$Diesel_GHG 
waste_2_1$Bj1_enduseemiss <- waste_2_1$Prod_ww * waste_2_1$Nonbio_emiss2 * waste_2_1$Bj1_tech
waste_2_1$Bj1_dispemiss <- 0 - waste_2_1$Bj1_d *waste_2_1$Diesel_GHG - waste_2_1$Bj1_g* waste_2_1$Gasoline_GHG -  waste_2_1$Bj1_j * waste_2_1$Jet_GHG 
waste_2_1$Bj1_dispemissmain <- 0 - waste_2_1$Bj1_d *waste_2_1$Diesel_GHG
waste_2_1$Bj1_dispemissco <- 0 - waste_2_1$Bj1_g* waste_2_1$Gasoline_GHG -  waste_2_1$Bj1_j * waste_2_1$Jet_GHG 
waste_2_1$Bj1_netemiss <- waste_2_1$Bj1_collectionemiss + waste_2_1$Bj1_transport1emiss + waste_2_1$Bj1_processemiss + waste_2_1$Bj1_transport2emiss + waste_2_1$Bj1_enduseemiss + waste_2_1$Bj1_dispemiss

#Bj2 
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_1$Bj2_energy <- waste_2_1$Prod_ww * waste_2_1$Bj2.j.out
waste_2_1$Bj2_energymain <- waste_2_1$Bj1_energy
waste_2_1$Bj2_netenergy <-(waste_2_1$Bj2_energy - waste_2_1$Prod_ww * waste_2_1$Bj2.h2.in -
                             (waste_2_1$collection_en +  waste_2_1$transport1_en)  -
                             waste_2_1$Bj2_energy / 43.10 * waste_2_1$Transport_km_2 * waste_2_1$Transport_Diesel ) * waste_2_1$Bj2_tech
waste_2_1$Bj2_collectionemiss <- waste_2_1$collection_emiss * waste_2_1$Bj2_tech
waste_2_1$Bj2_transport1emiss <- waste_2_1$transport1_emiss * waste_2_1$Bj2_tech
waste_2_1$Bj2_processemiss <- waste_2_1$Prod_ww * waste_2_1$Bj2.h2.in * waste_2_1$H2_GHG 
waste_2_1$Bj2_transport2emiss <- waste_2_1$Bj2_energy / 43.10 * waste_2_1$Transport_km_2 * waste_2_1$Transport_Diesel * waste_2_1$Diesel_GHG 
waste_2_1$Bj2_enduseemiss <- waste_2_1$Prod_ww * waste_2_1$Nonbio_emiss2 * waste_2_1$Bj2_tech
waste_2_1$Bj2_dispemiss <- 0 - waste_2_1$Bj2_energy * waste_2_1$Jet_GHG 
waste_2_1$Bj2_dispemissmain <- waste_2_1$Bj2_dispemiss
waste_2_1$Bj2_netemiss <- waste_2_1$Bj2_collectionemiss + waste_2_1$Bj2_transport1emiss + waste_2_1$Bj2_processemiss + waste_2_1$Bj2_transport2emiss + waste_2_1$Bj2_enduseemiss + waste_2_1$Bj2_dispemiss

#Bj3
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_1$Bj3_d <- waste_2_1$Prod_ww * waste_2_1$Bj3.d.out
waste_2_1$Bj3_g <- waste_2_1$Prod_ww * waste_2_1$Bj3.g.out
waste_2_1$Bj3_j <- waste_2_1$Prod_ww * waste_2_1$Bj3.j.out
waste_2_1$Bj3_energy <- waste_2_1$Bj3_d + waste_2_1$Bj3_g + waste_2_1$Bj3_j 
waste_2_1$Bj3_energymain <- waste_2_1$Bj3_j
waste_2_1$Bj3_energyco <- waste_2_1$Bj3_d + waste_2_1$Bj3_g 
waste_2_1$Bj3_netenergy <-(waste_2_1$Bj3_energy - waste_2_1$Prod_ww * waste_2_1$Bj3.e.in -
                             (waste_2_1$collection_en +  waste_2_1$transport1_en)  -
                             (waste_2_1$Bj3_d / 42.79 + waste_2_1$Bj3_g / 41.74 +waste_2_1$Bj3_j / 43.10) * waste_2_1$Transport_km_2 * waste_2_1$Transport_Diesel) * waste_2_1$Bj3_tech
waste_2_1$Bj3_collectionemiss <- waste_2_1$collection_emiss * waste_2_1$Bj3_tech
waste_2_1$Bj3_transport1emiss <- waste_2_1$transport1_emiss * waste_2_1$Bj3_tech
waste_2_1$Bj3_processemiss <- waste_2_1$Prod_ww * waste_2_1$Bj3.e.in * waste_2_1$Powergen_GHG * waste_2_1$Bj3_tech
waste_2_1$Bj3_transport2emiss <- (waste_2_1$Bj3_d / 42.79 + waste_2_1$Bj3_g / 41.74 + waste_2_1$Bj3_j / 43.10) * waste_2_1$Transport_km_2 * waste_2_1$Transport_Diesel * waste_2_1$Diesel_GHG
waste_2_1$Bj3_enduseemiss <- waste_2_1$Prod_ww * waste_2_1$Nonbio_emiss2 * waste_2_1$Bj3_tech
waste_2_1$Bj3_dispemiss <- 0 - waste_2_1$Bj3_d *waste_2_1$Diesel_GHG - waste_2_1$Bj3_g* waste_2_1$Gasoline_GHG -  waste_2_1$Bj3_j * waste_2_1$Jet_GHG 
waste_2_1$Bj3_dispemissmain <- 0 - waste_2_1$Bj3_j * waste_2_1$Jet_GHG
waste_2_1$Bj3_dispemissco <- 0 - waste_2_1$Bj3_d * waste_2_1$Diesel_GHG - waste_2_1$Bj3_g * waste_2_1$Gasoline_GHG
waste_2_1$Bj3_netemiss <- waste_2_1$Bj3_collectionemiss + waste_2_1$Bj3_transport1emiss + waste_2_1$Bj3_processemiss + waste_2_1$Bj3_transport2emiss + waste_2_1$Bj3_enduseemiss + waste_2_1$Bj3_dispemiss

#Bj4
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_1$Bj4_d <- waste_2_1$Prod_ww * waste_2_1$Bj4.d.out
waste_2_1$Bj4_g <- waste_2_1$Prod_ww * waste_2_1$Bj4.g.out
waste_2_1$Bj4_j <- waste_2_1$Prod_ww * waste_2_1$Bj4.j.out
waste_2_1$Bj4_energy <- waste_2_1$Bj4_d + waste_2_1$Bj4_g + waste_2_1$Bj4_j 
waste_2_1$Bj4_energymain <- waste_2_1$Bj4_j
waste_2_1$Bj4_energyco <- waste_2_1$Bj4_d + waste_2_1$Bj4_g 
waste_2_1$Bj4_netenergy <-(waste_2_1$Bj4_energy - waste_2_1$Prod_ww * waste_2_1$Bj4.h2.in -
                             (waste_2_1$collection_en +  waste_2_1$transport1_en)  -
                             (waste_2_1$Bj4_d / 42.79 + waste_2_1$Bj4_g / 41.74 +waste_2_1$Bj4_j / 43.10) * waste_2_1$Transport_km_2 * waste_2_1$Transport_Diesel) * waste_2_1$Bj4_tech
waste_2_1$Bj4_collectionemiss <- waste_2_1$collection_emiss * waste_2_1$Bj4_tech
waste_2_1$Bj4_transport1emiss <- waste_2_1$transport1_emiss * waste_2_1$Bj4_tech
waste_2_1$Bj4_processemiss <- waste_2_1$Prod_ww * waste_2_1$Bj4.h2.in * waste_2_1$H2_GHG * waste_2_1$Bj4_tech
waste_2_1$Bj4_transport2emiss <- (waste_2_1$Bj4_d / 42.79 + waste_2_1$Bj4_g / 41.74 + waste_2_1$Bj4_j / 43.10) * waste_2_1$Transport_km_2 * waste_2_1$Transport_Diesel * waste_2_1$Diesel_GHG 
waste_2_1$Bj4_enduseemiss <- waste_2_1$Prod_ww * waste_2_1$Nonbio_emiss2 * waste_2_1$Bj4_tech
waste_2_1$Bj4_dispemiss <- 0 - waste_2_1$Bj4_d *waste_2_1$Diesel_GHG - waste_2_1$Bj4_g* waste_2_1$Gasoline_GHG -  waste_2_1$Bj4_j * waste_2_1$Jet_GHG 
waste_2_1$Bj4_dispemissmain <- 0 - waste_2_1$Bj4_j * waste_2_1$Jet_GHG
waste_2_1$Bj4_dispemissco <- 0 - waste_2_1$Bj4_d * waste_2_1$Diesel_GHG - waste_2_1$Bj4_g * waste_2_1$Gasoline_GHG
waste_2_1$Bj4_netemiss <- waste_2_1$Bj4_collectionemiss + waste_2_1$Bj4_transport1emiss + waste_2_1$Bj4_processemiss + waste_2_1$Bj4_transport2emiss+ waste_2_1$Bj4_enduseemiss + waste_2_1$Bj4_dispemiss

#Bj5
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_1$Bj5_d <- waste_2_1$Prod_ww * waste_2_1$Bj5.d.out
waste_2_1$Bj5_g <- waste_2_1$Prod_ww * waste_2_1$Bj5.g.out
waste_2_1$Bj5_j <- waste_2_1$Prod_ww * waste_2_1$Bj5.j.out
waste_2_1$Bj5_energy <- waste_2_1$Bj5_d + waste_2_1$Bj5_g + waste_2_1$Bj5_j 
waste_2_1$Bj5_energymain <- waste_2_1$Bj5_j
waste_2_1$Bj5_energyco <- waste_2_1$Bj5_d + waste_2_1$Bj5_g 
waste_2_1$Bj5_netenergy <-(waste_2_1$Bj5_energy - waste_2_1$Prod_ww * waste_2_1$Bj5.e.in -
                             (waste_2_1$collection_en +  waste_2_1$transport1_en)  -
                             (waste_2_1$Bj5_d / 42.79 + waste_2_1$Bj5_g / 41.74 +waste_2_1$Bj5_j / 43.10) * waste_2_1$Transport_km_2 * waste_2_1$Transport_Diesel) * waste_2_1$Bj5_tech
waste_2_1$Bj5_collectionemiss <- waste_2_1$collection_emiss * waste_2_1$Bj5_tech
waste_2_1$Bj5_transport1emiss <- waste_2_1$transport1_emiss * waste_2_1$Bj5_tech
waste_2_1$Bj5_processemiss <- waste_2_1$Prod_ww * waste_2_1$Bj5.e.in * waste_2_1$Powergen_GHG * waste_2_1$Bj5_tech
waste_2_1$Bj5_transport2emiss <-  (waste_2_1$Bj5_d / 42.79 + waste_2_1$Bj5_g / 41.74 + waste_2_1$Bj5_j / 43.10) * waste_2_1$Transport_km_2 * waste_2_1$Transport_Diesel * waste_2_1$Diesel_GHG 
waste_2_1$Bj5_enduseemiss <- waste_2_1$Prod_ww * waste_2_1$Nonbio_emiss2 * waste_2_1$Bj5_tech
waste_2_1$Bj5_dispemiss <- 0 - waste_2_1$Bj5_d *waste_2_1$Diesel_GHG - waste_2_1$Bj5_g* waste_2_1$Gasoline_GHG -  waste_2_1$Bj5_j * waste_2_1$Jet_GHG 
waste_2_1$Bj5_dispemissmain <- 0 - waste_2_1$Bj5_j * waste_2_1$Jet_GHG
waste_2_1$Bj5_dispemissco <- 0 - waste_2_1$Bj5_d * waste_2_1$Diesel_GHG - waste_2_1$Bj5_g * waste_2_1$Gasoline_GHG
waste_2_1$Bj5_netemiss <- waste_2_1$Bj5_collectionemiss + waste_2_1$Bj5_transport1emiss + waste_2_1$Bj5_processemiss + waste_2_1$Bj5_transport2emiss + waste_2_1$Bj5_enduseemiss + waste_2_1$Bj5_dispemiss

#Bj6
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_1$Bj6_d <- waste_2_1$Prod_ww * waste_2_1$Bj6.d.out
waste_2_1$Bj6_g <- waste_2_1$Prod_ww * waste_2_1$Bj6.g.out
waste_2_1$Bj6_j <- waste_2_1$Prod_ww * waste_2_1$Bj6.j.out
waste_2_1$Bj6_energy <- waste_2_1$Bj6_d + waste_2_1$Bj6_g + waste_2_1$Bj6_j 
waste_2_1$Bj6_energymain <- waste_2_1$Bj6_j
waste_2_1$Bj6_energyco <- waste_2_1$Bj6_d + waste_2_1$Bj6_g 
waste_2_1$Bj6_netenergy <-(waste_2_1$Bj6_energy - waste_2_1$Prod_ww * waste_2_1$Bj6.h2.in - waste_2_1$Prod_ww * waste_2_1$Bj6.e.in -
                             (waste_2_1$collection_en +  waste_2_1$transport1_en)  -
                             (waste_2_1$Bj6_d / 42.79 + waste_2_1$Bj6_g / 41.74 +waste_2_1$Bj6_j / 43.10) * waste_2_1$Transport_km_2 * waste_2_1$Transport_Diesel) * waste_2_1$Bj6_tech
waste_2_1$Bj6_collectionemiss <- waste_2_1$collection_emiss * waste_2_1$Bj6_tech
waste_2_1$Bj6_transport1emiss <- waste_2_1$transport1_emiss * waste_2_1$Bj6_tech
waste_2_1$Bj6_processemiss <- waste_2_1$Prod_ww * (waste_2_1$Bj6.h2.in * waste_2_1$H2_GHG + waste_2_1$Bj6.e.in * waste_2_1$Powergen_GHG) * waste_2_1$Bj6_tech
waste_2_1$Bj6_transport2emiss <-  (waste_2_1$Bj6_d / 42.79 + waste_2_1$Bj6_g / 41.74 + waste_2_1$Bj6_j / 43.10) * waste_2_1$Transport_km_2 * waste_2_1$Transport_Diesel * waste_2_1$Diesel_GHG
waste_2_1$Bj6_enduseemiss <- waste_2_1$Prod_ww * waste_2_1$Nonbio_emiss2 * waste_2_1$Bj6_tech
waste_2_1$Bj6_dispemiss <- 0 - waste_2_1$Bj6_d *waste_2_1$Diesel_GHG - waste_2_1$Bj6_g* waste_2_1$Gasoline_GHG -  waste_2_1$Bj6_j * waste_2_1$Jet_GHG 
waste_2_1$Bj6_dispemissmain <- 0 - waste_2_1$Bj6_j * waste_2_1$Jet_GHG
waste_2_1$Bj6_dispemissco <- 0 - waste_2_1$Bj6_d * waste_2_1$Diesel_GHG - waste_2_1$Bj6_g * waste_2_1$Gasoline_GHG
waste_2_1$Bj6_netemiss <- waste_2_1$Bj6_collectionemiss + waste_2_1$Bj6_transport1emiss + waste_2_1$Bj6_processemiss + waste_2_1$Bj6_transport2emiss + waste_2_1$Bj6_enduseemiss + waste_2_1$Bj6_dispemiss

#fossil roll back (+50%)
waste_2_2 <- merge(waste_1, state_GHG_ef2)

#Collection & Transportation_1 (energy - GJ, emiss - kg CO1e)
waste_2_2$Prod_ww <- waste_2_2$Prod/(1-waste_2_2$MC/100)
waste_2_2$collection_en <- waste_2_2$Prod_ww * waste_2_2$Collection_Diesel
waste_2_2$collection_emiss <- waste_2_2$collection_en * waste_2_2$Diesel_GHG 
waste_2_2$transport1_en <- waste_2_2$Prod_ww * waste_2_2$Transport_km_1 * waste_2_2$Transport_Diesel
waste_2_2$transport1_emiss <- waste_2_2$transport1_en  * waste_2_2$Diesel_GHG 

#E1
#electricity T&D loss - 6.5%, heat loss - 20%
waste_2_2$E1_elec <- waste_2_2$Prod_ww * waste_2_2$E1.e.out * (1-0.065)
waste_2_2$E1_heat <- waste_2_2$Prod_ww * waste_2_2$E1.h.out * (1-0.2)
waste_2_2$E1_energy <- waste_2_2$E1_elec + waste_2_2$E1_heat
waste_2_2$E1_energymain <- waste_2_2$E1_elec
waste_2_2$E1_energyco <- waste_2_2$E1_heat
waste_2_2$E1_netenergy <- (waste_2_2$E1_energy - waste_2_2$Prod_ww * (waste_2_2$E1.e.in + waste_2_2$E1.h.in + waste_2_2$E1.d.in) - waste_2_2$collection_en -  waste_2_2$transport1_en) * waste_2_2$E1_tech
waste_2_2$E1_collectionemiss <- waste_2_2$collection_emiss * waste_2_2$E1_tech
waste_2_2$E1_transport1emiss <- waste_2_2$transport1_emiss * waste_2_2$E1_tech
waste_2_2$E1_processemiss <- waste_2_2$Prod_ww * (waste_2_2$E1.e.in * waste_2_2$Powergen_GHG 
                                                  + waste_2_2$E1.h.in * waste_2_2$Heatgen_GHG 
                                                  + waste_2_2$E1.d.in * waste_2_2$Diesel_GHG
                                                  + waste_2_2$Nonbio_emiss1) * waste_2_2$E1_tech
waste_2_2$E1_enduseemiss <- 0
waste_2_2$E1_dispemiss <- 0 - waste_2_2$E1_elec* waste_2_2$Powergen_GHG  - waste_2_2$E1_heat* waste_2_2$Heatgen_GHG 
waste_2_2$E1_dispemissmain <- 0 - waste_2_2$E1_elec* waste_2_2$Powergen_GHG
waste_2_2$E1_dispemissco <- 0 - waste_2_2$E1_heat* waste_2_2$Heatgen_GHG
waste_2_2$E1_netemiss <- waste_2_2$E1_collectionemiss + waste_2_2$E1_transport1emiss + waste_2_2$E1_processemiss + waste_2_2$E1_enduseemiss + waste_2_2$E1_dispemiss

#E2
#electricity T&D loss - 6.5%, heat loss - 20%
waste_2_2$E2_elec <- waste_2_2$Prod_ww * waste_2_2$E2.e.out * (1-0.065)
waste_2_2$E2_heat <- waste_2_2$Prod_ww * waste_2_2$E2.h.out * (1-0.2)
waste_2_2$E2_energy <- waste_2_2$E2_elec + waste_2_2$E2_heat
waste_2_2$E2_energymain <- waste_2_2$E2_elec
waste_2_2$E2_energyco <- waste_2_2$E2_heat
waste_2_2$E2_netenergy <- (waste_2_2$E2_energy - waste_2_2$Prod_ww * (waste_2_2$E2.e.in + waste_2_2$E2.h.in + waste_2_2$E2.d.in) - waste_2_2$collection_en - waste_2_2$transport1_en) * waste_2_2$E2_tech
waste_2_2$E2_collectionemiss <- waste_2_2$collection_emiss * waste_2_2$E2_tech
waste_2_2$E2_transport1emiss <- waste_2_2$transport1_emiss * waste_2_2$E2_tech
waste_2_2$E2_processemiss <- waste_2_2$Prod_ww * (waste_2_2$E2.e.in * waste_2_2$Powergen_GHG 
                                                  + waste_2_2$E2.h.in * waste_2_2$Heatgen_GHG
                                                  + waste_2_2$E2.d.in * waste_2_2$Diesel_GHG
                                                  + waste_2_2$Nonbio_emiss1)  * waste_2_2$E2_tech 
waste_2_2$E2_enduseemiss <- 0
waste_2_2$E2_dispemiss <- 0 - waste_2_2$E2_elec* waste_2_2$Powergen_GHG - waste_2_2$E2_heat* waste_2_2$Heatgen_GHG
waste_2_2$E2_dispemissmain <- 0 - waste_2_2$E2_elec* waste_2_2$Powergen_GHG
waste_2_2$E2_dispemissco <- 0 - waste_2_2$E2_heat* waste_2_2$Heatgen_GHG
waste_2_2$E2_netemiss <- waste_2_2$E2_collectionemiss + waste_2_2$E2_transport1emiss + waste_2_2$E2_processemiss + waste_2_2$E2_enduseemiss + waste_2_2$E2_dispemiss

#E3
#electricity T&D loss - 6.5%
waste_2_2$E3_energy <- waste_2_2$Prod_ww * waste_2_2$E3.e.out * (1-0.065)
waste_2_2$E3_energymain <- waste_2_2$E3_energy
waste_2_2$E3_netenergy <- (waste_2_2$E3_energy - waste_2_2$Prod_ww * (waste_2_2$E3.e.in + waste_2_2$E3.h.in + waste_2_2$E3.d.in) - waste_2_2$collection_en -  waste_2_2$transport1_en) * waste_2_2$E3_tech
waste_2_2$E3_collectionemiss <- waste_2_2$collection_emiss * waste_2_2$E3_tech
waste_2_2$E3_transport1emiss <- waste_2_2$transport1_emiss * waste_2_2$E3_tech
waste_2_2$E3_processemiss <- waste_2_2$Prod_ww * (waste_2_2$E3.e.in * waste_2_2$Powergen_GHG 
                                                  + waste_2_2$E3.h.in * waste_2_2$Heatgen_GHG
                                                  + waste_2_2$E3.d.in * waste_2_2$Diesel_GHG
                                                  + waste_2_2$Nonbio_emiss1)  * waste_2_2$E3_tech
waste_2_2$E3_enduseemiss <- 0
waste_2_2$E3_dispemiss <- 0 - waste_2_2$E3_energy* waste_2_2$Powergen_GHG
waste_2_2$E3_dispemissmain <- 0 - waste_2_2$E3_energy* waste_2_2$Powergen_GHG
waste_2_2$E3_netemiss <- waste_2_2$E3_collectionemiss + waste_2_2$E3_transport1emiss + waste_2_2$E3_processemiss + waste_2_2$E3_enduseemiss + waste_2_2$E3_dispemiss

#E4
#electricity T&D loss - 6.5%, heat loss - 20%
waste_2_2$E4_elec <- waste_2_2$Prod_ww * waste_2_2$E4.e.out * (1-0.065)
waste_2_2$E4_heat <- waste_2_2$Prod_ww * waste_2_2$E4.h.out * (1-0.2)
waste_2_2$E4_energy <- waste_2_2$E4_elec + waste_2_2$E4_heat
waste_2_2$E4_energymain <- waste_2_2$E4_elec
waste_2_2$E4_energyco <- waste_2_2$E4_heat
waste_2_2$E4_netenergy <- (waste_2_2$E4_energy - waste_2_2$Prod_ww * (waste_2_2$E4.ng.in + waste_2_2$E4.d.in) - waste_2_2$collection_en - waste_2_2$transport1_en) * waste_2_2$E4_tech
waste_2_2$E4_collectionemiss <- waste_2_2$collection_emiss * waste_2_2$E4_tech
waste_2_2$E4_transport1emiss <- waste_2_2$transport1_emiss * waste_2_2$E4_tech
waste_2_2$E4_processemiss <- waste_2_2$Prod_ww * (waste_2_2$E4.ng.in * waste_2_2$NG_GHG 
                                                  + waste_2_2$E4.d.in * waste_2_2$Diesel_GHG
                                                  + waste_2_2$Nonbio_emiss1) * waste_2_2$E4_tech 
waste_2_2$E4_enduseemiss <- 0
waste_2_2$E4_dispemiss <- 0 - waste_2_2$E4_elec* waste_2_2$Powergen_GHG - waste_2_2$E4_heat* waste_2_2$Heatgen_GHG
waste_2_2$E4_dispemissmain <- 0 - waste_2_2$E4_elec* waste_2_2$Powergen_GHG
waste_2_2$E4_dispemissco <- 0 - waste_2_2$E4_heat* waste_2_2$Heatgen_GHG
waste_2_2$E4_netemiss <- waste_2_2$E4_collectionemiss + waste_2_2$E4_transport1emiss + waste_2_2$E4_processemiss + waste_2_2$E4_enduseemiss + waste_2_2$E4_dispemiss

#M1
#methane leakage - 2%
waste_2_2$M1_energy <- waste_2_2$Prod_ww * waste_2_2$M1.m.out * (1-0.02)
waste_2_2$M1_energymain <- waste_2_2$M1_energy
waste_2_2$M1_netenergy <- (waste_2_2$M1_energy - waste_2_2$Prod_ww * (waste_2_2$M1.e.in + waste_2_2$M1.h.in) - waste_2_2$collection_en - waste_2_2$transport1_en) * waste_2_2$M1_tech
waste_2_2$M1_collectionemiss <- waste_2_2$collection_emiss * waste_2_2$M1_tech
waste_2_2$M1_transport1emiss <- waste_2_2$transport1_emiss * waste_2_2$M1_tech
waste_2_2$M1_processemiss <- waste_2_2$Prod_ww * (waste_2_2$M1.e.in * waste_2_2$Powergen_GHG + waste_2_2$M1.h.in * waste_2_2$Heatgen_GHG)* waste_2_2$M1_tech
waste_2_2$M1_transport2emiss <- waste_2_2$Prod_ww * waste_2_2$M1.m.out *0.02 /50 *28 * waste_2_2$M1_tech
waste_2_2$M1_enduseemiss <- waste_2_2$Prod_ww * waste_2_2$Nonbio_emiss2  * waste_2_2$M1_tech
waste_2_2$M1_dispemiss <- 0 - waste_2_2$M1_energy* waste_2_2$NG_GHG
waste_2_2$M1_dispemissmain <- waste_2_2$M1_dispemiss
waste_2_2$M1_netemiss <- waste_2_2$M1_collectionemiss + waste_2_2$M1_transport1emiss + waste_2_2$M1_processemiss + waste_2_2$M1_transport2emiss + waste_2_2$M1_enduseemiss + waste_2_2$M1_dispemiss

#M2
#methane leakage - 2%
waste_2_2$M2_energy <- waste_2_2$Prod_ww * waste_2_2$M2.m.out * (1-0.02)
waste_2_2$M2_energymain <- waste_2_2$M2_energy
waste_2_2$M2_netenergy <- (waste_2_2$M2_energy - waste_2_2$Prod_ww * (waste_2_2$M2.e.in + waste_2_2$M2.h.in + waste_2_2$M2.d.in) - waste_2_2$collection_en -  waste_2_2$transport1_en) * waste_2_2$M2_tech
waste_2_2$M2_collectionemiss <- waste_2_2$collection_emiss * waste_2_2$M2_tech
waste_2_2$M2_transport1emiss <- waste_2_2$transport1_emiss * waste_2_2$M2_tech
waste_2_2$M2_processemiss <- waste_2_2$Prod_ww * (waste_2_2$M2.e.in * waste_2_2$Powergen_GHG + 
                                                    waste_2_2$M2.h.in * waste_2_2$Heatgen_GHG +
                                                    waste_2_2$M2.d.in * waste_2_2$Diesel_GHG) * waste_2_2$M2_tech
waste_2_2$M2_transport2emiss <- waste_2_2$Prod_ww * waste_2_2$M2.m.out *0.02 /50 *28 * waste_2_2$M1_tech
waste_2_2$M2_enduseemiss <- waste_2_2$Prod_ww * waste_2_2$Nonbio_emiss2 * waste_2_2$M2_tech
waste_2_2$M2_dispemiss <- 0 - waste_2_2$M2_energy* waste_2_2$NG_GHG
waste_2_2$M2_dispemissmain <- waste_2_2$M2_dispemiss
waste_2_2$M2_netemiss <- waste_2_2$M2_collectionemiss + waste_2_2$M2_transport1emiss + waste_2_2$M2_processemiss + waste_2_2$M2_transport2emiss + waste_2_2$M2_enduseemiss + waste_2_2$M2_dispemiss

#Eth1
#energy intensity of ethanol - 26.95 MJ/kg
waste_2_2$Eth1_elec <- waste_2_2$Prod_ww * waste_2_2$Eth1.e.out * (1-0.065)
waste_2_2$Eth1_eth <- waste_2_2$Prod_ww * waste_2_2$Eth1.eth.out
waste_2_2$Eth1_energy <- waste_2_2$Eth1_elec + waste_2_2$Eth1_eth
waste_2_2$Eth1_energymain <- waste_2_2$Eth1_eth
waste_2_2$Eth1_energyco <- waste_2_2$Eth1_elec
waste_2_2$Eth1_netenergy <- (waste_2_2$Eth1_energy - waste_2_2$Prod_ww * (waste_2_2$Eth1.ng.in  + waste_2_2$Eth1.d.in) -
                               (waste_2_2$collection_en +  waste_2_2$transport1_en) * waste_2_2$M1_tech -
                               waste_2_2$Eth1_eth / 26.95 * waste_2_2$Transport_km_2 * waste_2_2$Transport_Diesel) * waste_2_2$Eth1_tech
waste_2_2$Eth1_collectionemiss <- waste_2_2$collection_emiss * waste_2_2$Eth1_tech
waste_2_2$Eth1_transport1emiss <- waste_2_2$transport1_emiss * waste_2_2$Eth1_tech
waste_2_2$Eth1_processemiss <- waste_2_2$Prod_ww * (waste_2_2$Eth1.ng.in * waste_2_2$Heatgen_GHG + waste_2_2$Eth1.d.in * waste_2_2$Diesel_GHG) * waste_2_2$Eth1_tech 
waste_2_2$Eth1_transport2emiss <- waste_2_2$Eth1_eth / 26.95 * waste_2_2$Transport_km_2 * waste_2_2$Transport_Diesel * waste_2_2$Diesel_GHG 
waste_2_2$Eth1_enduseemiss <- waste_2_2$Prod_ww * waste_2_2$Nonbio_emiss2 * waste_2_2$Eth1_tech
waste_2_2$Eth1_dispemiss <- 0 - waste_2_2$Eth1_eth* waste_2_2$Gasoline_GHG  - waste_2_2$Eth1_elec * waste_2_2$Powergen_GHG
waste_2_2$Eth1_dispemissmain <- 0 - waste_2_2$Eth1_eth* waste_2_2$Gasoline_GHG
waste_2_2$Eth1_dispemissco <- 0 - waste_2_2$Eth1_elec * waste_2_2$Powergen_GHG
waste_2_2$Eth1_netemiss <- waste_2_2$Eth1_collectionemiss + waste_2_2$Eth1_transport1emiss + waste_2_2$Eth1_processemiss + waste_2_2$Eth1_transport2emiss + waste_2_2$Eth1_enduseemiss + waste_2_2$Eth1_dispemiss

#Rd1
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
#electricity T&D loss - 6.5%, methane leakage - 2%
waste_2_2$Rd1_d <- waste_2_2$Prod_ww * waste_2_2$Rd1.d.out
waste_2_2$Rd1_g <- waste_2_2$Prod_ww * waste_2_2$Rd1.g.out
waste_2_2$Rd1_j <- waste_2_2$Prod_ww * waste_2_2$Rd1.j.out
waste_2_2$Rd1_m <- waste_2_2$Prod_ww * waste_2_2$Rd1.m.out * (1-0.02)
waste_2_2$Rd1_elec <- waste_2_2$Prod_ww * waste_2_2$Rd1.e.out * (1-0.065)
waste_2_2$Rd1_energy <- waste_2_2$Rd1_d + waste_2_2$Rd1_g + waste_2_2$Rd1_j + waste_2_2$Rd1_m + waste_2_2$Rd1_elec
waste_2_2$Rd1_energymain <- waste_2_2$Rd1_d
waste_2_2$Rd1_energyco <- waste_2_2$Rd1_g + waste_2_2$Rd1_j + waste_2_2$Rd1_m + waste_2_2$Rd1_elec
waste_2_2$Rd1_netenergy <-(waste_2_2$Rd1_energy - waste_2_2$Prod_ww * waste_2_2$Rd1.e.in -
                             (waste_2_2$collection_en +  waste_2_2$transport1_en)  -
                             waste_2_2$Rd1_d / 42.79 * waste_2_2$Transport_km_2 * waste_2_2$Transport_Diesel -
                             waste_2_2$Rd1_g / 41.74 * waste_2_2$Transport_km_2 * waste_2_2$Transport_Diesel -
                             waste_2_2$Rd1_j / 43.10 * waste_2_2$Transport_km_2 * waste_2_2$Transport_Diesel ) * waste_2_2$Rd1_tech
waste_2_2$Rd1_collectionemiss <- waste_2_2$collection_emiss * waste_2_2$Rd1_tech
waste_2_2$Rd1_transport1emiss <- waste_2_2$transport1_emiss * waste_2_2$Rd1_tech
waste_2_2$Rd1_processemiss <- waste_2_2$Prod_ww * waste_2_2$Rd1.e.in * waste_2_2$Powergen_GHG * waste_2_2$Rd1_tech
waste_2_2$Rd1_transport2emiss <- (waste_2_2$Rd1_d / 42.79 + waste_2_2$Rd1_g / 41.74 + waste_2_2$Rd1_j / 43.10) * waste_2_2$Transport_km_2 * waste_2_2$Transport_Diesel * waste_2_2$Diesel_GHG  
waste_2_2$Rd1_enduseemiss <- waste_2_2$Prod_ww * waste_2_2$Nonbio_emiss2 * waste_2_2$Rd1_tech
waste_2_2$Rd1_dispemiss <- 0 - waste_2_2$Rd1_d *waste_2_2$Diesel_GHG - waste_2_2$Rd1_g * waste_2_2$Gasoline_GHG -  waste_2_2$Rd1_j * waste_2_2$Jet_GHG -
  waste_2_2$Rd1_m * waste_2_2$NG_GHG - waste_2_2$Rd1_elec * waste_2_2$Powergen_GHG 
waste_2_2$Rd1_dispemissmain <- 0 - waste_2_2$Rd1_d *waste_2_2$Diesel_GHG
waste_2_2$Rd1_dispemissco <- 0 - waste_2_2$Rd1_g * waste_2_2$Gasoline_GHG -  waste_2_2$Rd1_j * waste_2_2$Jet_GHG -
  waste_2_2$Rd1_m * waste_2_2$NG_GHG - waste_2_2$Rd1_elec * waste_2_2$Powergen_GHG 
waste_2_2$Rd1_netemiss <- waste_2_2$Rd1_collectionemiss + waste_2_2$Rd1_transport1emiss + waste_2_2$Rd1_processemiss + waste_2_2$Rd1_enduseemiss + waste_2_2$Rd1_transport2emiss + waste_2_2$Rd1_dispemiss


#Rd2
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74
waste_2_2$Rd2_d <- waste_2_2$Prod_ww * waste_2_2$Rd2.d.out
waste_2_2$Rd2_g <- waste_2_2$Prod_ww * waste_2_2$Rd2.g.out
waste_2_2$Rd2_energy <- waste_2_2$Rd2_d + waste_2_2$Rd2_g
waste_2_2$Rd2_energymain <- waste_2_2$Rd2_d
waste_2_2$Rd2_energyco <- waste_2_2$Rd2_g
waste_2_2$Rd2_netenergy <-(waste_2_2$Rd2_energy - waste_2_2$Prod_ww * (waste_2_2$Rd2.e.in + waste_2_2$Rd2.ng.in) -
                             (waste_2_2$collection_en +  waste_2_2$transport1_en)  -
                             waste_2_2$Rd2_d / 42.79 * waste_2_2$Transport_km_2 * waste_2_2$Transport_Diesel -
                             waste_2_2$Rd2_g / 41.74 * waste_2_2$Transport_km_2 * waste_2_2$Transport_Diesel ) * waste_2_2$Rd2_tech
waste_2_2$Rd2_collectionemiss <- waste_2_2$collection_emiss * waste_2_2$Rd2_tech
waste_2_2$Rd2_transport1emiss <- waste_2_2$transport1_emiss * waste_2_2$Rd2_tech
waste_2_2$Rd2_processemiss  <- waste_2_2$Prod_ww * (waste_2_2$Rd2.e.in * waste_2_2$Powergen_GHG + waste_2_2$Rd2.ng.in * waste_2_2$H2_GHG) * waste_2_2$Rd2_tech
waste_2_2$Rd2_transport2emiss <- (waste_2_2$Rd2_d / 42.79  + waste_2_2$Rd2_g / 41.74) * waste_2_2$Transport_km_2 * waste_2_2$Transport_Diesel * waste_2_2$Diesel_GHG 
waste_2_2$Rd2_enduseemiss <- waste_2_2$Prod_ww * waste_2_2$Nonbio_emiss2 * waste_2_2$Rd2_tech
waste_2_2$Rd2_dispemiss <- 0 - waste_2_2$Rd2_d *waste_2_2$Diesel_GHG - waste_2_2$Rd2_g* waste_2_2$Gasoline_GHG 
waste_2_2$Rd2_dispemissmain <- 0 - waste_2_2$Rd2_d *waste_2_2$Diesel_GHG 
waste_2_2$Rd2_dispemissco <- 0 - waste_2_2$Rd2_g* waste_2_2$Gasoline_GHG 
waste_2_2$Rd2_netemiss <- waste_2_2$Rd2_collectionemiss + waste_2_2$Rd2_transport1emiss + waste_2_2$Rd2_processemiss + waste_2_2$Rd2_transport2emiss + waste_2_2$Rd2_enduseemiss + waste_2_2$Rd2_dispemiss

#Bj1 
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_2$Bj1_d <- waste_2_2$Prod_ww * waste_2_2$Bj1.d.out
waste_2_2$Bj1_g <- waste_2_2$Prod_ww * waste_2_2$Bj1.g.out
waste_2_2$Bj1_j <- waste_2_2$Prod_ww * waste_2_2$Bj1.j.out
waste_2_2$Bj1_energy <- waste_2_2$Bj1_d + waste_2_2$Bj1_g + waste_2_2$Bj1_j 
waste_2_2$Bj1_energymain <- waste_2_2$Bj1_d
waste_2_2$Bj1_energyco <- waste_2_2$Bj1_g + waste_2_2$Bj1_j 
waste_2_2$Bj1_netenergy <-(waste_2_2$Bj1_energy - waste_2_2$Prod_ww * waste_2_2$Bj1.h2.in -
                             (waste_2_2$collection_en +  waste_2_2$transport1_en)  -
                             (waste_2_2$Bj1_d / 42.79 + waste_2_2$Bj1_g / 41.74 +waste_2_2$Bj1_j / 43.10) * waste_2_2$Transport_km_2 * waste_2_2$Transport_Diesel) * waste_2_2$Bj1_tech
waste_2_2$Bj1_collectionemiss <- waste_2_2$collection_emiss * waste_2_2$Bj1_tech
waste_2_2$Bj1_transport1emiss <- waste_2_2$transport1_emiss * waste_2_2$Bj1_tech
waste_2_2$Bj1_processemiss <- waste_2_2$Prod_ww * waste_2_2$Bj1.h2.in * waste_2_2$H2_GHG * waste_2_2$Bj1_tech
waste_2_2$Bj1_transport2emiss <- (waste_2_2$Bj1_d / 42.79 + waste_2_2$Bj1_g / 41.74 + waste_2_2$Bj1_j / 43.10) * waste_2_2$Transport_km_2 * waste_2_2$Transport_Diesel * waste_2_2$Diesel_GHG 
waste_2_2$Bj1_enduseemiss <- waste_2_2$Prod_ww * waste_2_2$Nonbio_emiss2 * waste_2_2$Bj1_tech
waste_2_2$Bj1_dispemiss <- 0 - waste_2_2$Bj1_d *waste_2_2$Diesel_GHG - waste_2_2$Bj1_g* waste_2_2$Gasoline_GHG -  waste_2_2$Bj1_j * waste_2_2$Jet_GHG 
waste_2_2$Bj1_dispemissmain <- 0 - waste_2_2$Bj1_d *waste_2_2$Diesel_GHG
waste_2_2$Bj1_dispemissco <- 0 - waste_2_2$Bj1_g* waste_2_2$Gasoline_GHG -  waste_2_2$Bj1_j * waste_2_2$Jet_GHG 
waste_2_2$Bj1_netemiss <- waste_2_2$Bj1_collectionemiss + waste_2_2$Bj1_transport1emiss + waste_2_2$Bj1_processemiss + waste_2_2$Bj1_transport2emiss + waste_2_2$Bj1_enduseemiss + waste_2_2$Bj1_dispemiss

#Bj2 
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_2$Bj2_energy <- waste_2_2$Prod_ww * waste_2_2$Bj2.j.out
waste_2_2$Bj2_energymain <- waste_2_2$Bj1_energy
waste_2_2$Bj2_netenergy <-(waste_2_2$Bj2_energy - waste_2_2$Prod_ww * waste_2_2$Bj2.h2.in -
                             (waste_2_2$collection_en +  waste_2_2$transport1_en)  -
                             waste_2_2$Bj2_energy / 43.10 * waste_2_2$Transport_km_2 * waste_2_2$Transport_Diesel ) * waste_2_2$Bj2_tech
waste_2_2$Bj2_collectionemiss <- waste_2_2$collection_emiss * waste_2_2$Bj2_tech
waste_2_2$Bj2_transport1emiss <- waste_2_2$transport1_emiss * waste_2_2$Bj2_tech
waste_2_2$Bj2_processemiss <- waste_2_2$Prod_ww * waste_2_2$Bj2.h2.in * waste_2_2$H2_GHG 
waste_2_2$Bj2_transport2emiss <- waste_2_2$Bj2_energy / 43.10 * waste_2_2$Transport_km_2 * waste_2_2$Transport_Diesel * waste_2_2$Diesel_GHG 
waste_2_2$Bj2_enduseemiss <- waste_2_2$Prod_ww * waste_2_2$Nonbio_emiss2 * waste_2_2$Bj2_tech
waste_2_2$Bj2_dispemiss <- 0 - waste_2_2$Bj2_energy * waste_2_2$Jet_GHG 
waste_2_2$Bj2_dispemissmain <- waste_2_2$Bj2_dispemiss
waste_2_2$Bj2_netemiss <- waste_2_2$Bj2_collectionemiss + waste_2_2$Bj2_transport1emiss + waste_2_2$Bj2_processemiss + waste_2_2$Bj2_transport2emiss + waste_2_2$Bj2_enduseemiss + waste_2_2$Bj2_dispemiss

#Bj3
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_2$Bj3_d <- waste_2_2$Prod_ww * waste_2_2$Bj3.d.out
waste_2_2$Bj3_g <- waste_2_2$Prod_ww * waste_2_2$Bj3.g.out
waste_2_2$Bj3_j <- waste_2_2$Prod_ww * waste_2_2$Bj3.j.out
waste_2_2$Bj3_energy <- waste_2_2$Bj3_d + waste_2_2$Bj3_g + waste_2_2$Bj3_j 
waste_2_2$Bj3_energymain <- waste_2_2$Bj3_j
waste_2_2$Bj3_energyco <- waste_2_2$Bj3_d + waste_2_2$Bj3_g 
waste_2_2$Bj3_netenergy <-(waste_2_2$Bj3_energy - waste_2_2$Prod_ww * waste_2_2$Bj3.e.in -
                             (waste_2_2$collection_en +  waste_2_2$transport1_en)  -
                             (waste_2_2$Bj3_d / 42.79 + waste_2_2$Bj3_g / 41.74 +waste_2_2$Bj3_j / 43.10) * waste_2_2$Transport_km_2 * waste_2_2$Transport_Diesel) * waste_2_2$Bj3_tech
waste_2_2$Bj3_collectionemiss <- waste_2_2$collection_emiss * waste_2_2$Bj3_tech
waste_2_2$Bj3_transport1emiss <- waste_2_2$transport1_emiss * waste_2_2$Bj3_tech
waste_2_2$Bj3_processemiss <- waste_2_2$Prod_ww * waste_2_2$Bj3.e.in * waste_2_2$Powergen_GHG * waste_2_2$Bj3_tech
waste_2_2$Bj3_transport2emiss <- (waste_2_2$Bj3_d / 42.79 + waste_2_2$Bj3_g / 41.74 + waste_2_2$Bj3_j / 43.10) * waste_2_2$Transport_km_2 * waste_2_2$Transport_Diesel * waste_2_2$Diesel_GHG
waste_2_2$Bj3_enduseemiss <- waste_2_2$Prod_ww * waste_2_2$Nonbio_emiss2 * waste_2_2$Bj3_tech
waste_2_2$Bj3_dispemiss <- 0 - waste_2_2$Bj3_d *waste_2_2$Diesel_GHG - waste_2_2$Bj3_g* waste_2_2$Gasoline_GHG -  waste_2_2$Bj3_j * waste_2_2$Jet_GHG 
waste_2_2$Bj3_dispemissmain <- 0 - waste_2_2$Bj3_j * waste_2_2$Jet_GHG
waste_2_2$Bj3_dispemissco <- 0 - waste_2_2$Bj3_d * waste_2_2$Diesel_GHG - waste_2_2$Bj3_g * waste_2_2$Gasoline_GHG
waste_2_2$Bj3_netemiss <- waste_2_2$Bj3_collectionemiss + waste_2_2$Bj3_transport1emiss + waste_2_2$Bj3_processemiss + waste_2_2$Bj3_transport2emiss + waste_2_2$Bj3_enduseemiss + waste_2_2$Bj3_dispemiss

#Bj4
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_2$Bj4_d <- waste_2_2$Prod_ww * waste_2_2$Bj4.d.out
waste_2_2$Bj4_g <- waste_2_2$Prod_ww * waste_2_2$Bj4.g.out
waste_2_2$Bj4_j <- waste_2_2$Prod_ww * waste_2_2$Bj4.j.out
waste_2_2$Bj4_energy <- waste_2_2$Bj4_d + waste_2_2$Bj4_g + waste_2_2$Bj4_j 
waste_2_2$Bj4_energymain <- waste_2_2$Bj4_j
waste_2_2$Bj4_energyco <- waste_2_2$Bj4_d + waste_2_2$Bj4_g 
waste_2_2$Bj4_netenergy <-(waste_2_2$Bj4_energy - waste_2_2$Prod_ww * waste_2_2$Bj4.h2.in -
                             (waste_2_2$collection_en +  waste_2_2$transport1_en)  -
                             (waste_2_2$Bj4_d / 42.79 + waste_2_2$Bj4_g / 41.74 +waste_2_2$Bj4_j / 43.10) * waste_2_2$Transport_km_2 * waste_2_2$Transport_Diesel) * waste_2_2$Bj4_tech
waste_2_2$Bj4_collectionemiss <- waste_2_2$collection_emiss * waste_2_2$Bj4_tech
waste_2_2$Bj4_transport1emiss <- waste_2_2$transport1_emiss * waste_2_2$Bj4_tech
waste_2_2$Bj4_processemiss <- waste_2_2$Prod_ww * waste_2_2$Bj4.h2.in * waste_2_2$H2_GHG * waste_2_2$Bj4_tech
waste_2_2$Bj4_transport2emiss <- (waste_2_2$Bj4_d / 42.79 + waste_2_2$Bj4_g / 41.74 + waste_2_2$Bj4_j / 43.10) * waste_2_2$Transport_km_2 * waste_2_2$Transport_Diesel * waste_2_2$Diesel_GHG 
waste_2_2$Bj4_enduseemiss <- waste_2_2$Prod_ww * waste_2_2$Nonbio_emiss2 * waste_2_2$Bj4_tech
waste_2_2$Bj4_dispemiss <- 0 - waste_2_2$Bj4_d *waste_2_2$Diesel_GHG - waste_2_2$Bj4_g* waste_2_2$Gasoline_GHG -  waste_2_2$Bj4_j * waste_2_2$Jet_GHG 
waste_2_2$Bj4_dispemissmain <- 0 - waste_2_2$Bj4_j * waste_2_2$Jet_GHG
waste_2_2$Bj4_dispemissco <- 0 - waste_2_2$Bj4_d * waste_2_2$Diesel_GHG - waste_2_2$Bj4_g * waste_2_2$Gasoline_GHG
waste_2_2$Bj4_netemiss <- waste_2_2$Bj4_collectionemiss + waste_2_2$Bj4_transport1emiss + waste_2_2$Bj4_processemiss + waste_2_2$Bj4_transport2emiss+ waste_2_2$Bj4_enduseemiss + waste_2_2$Bj4_dispemiss

#Bj5
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_2$Bj5_d <- waste_2_2$Prod_ww * waste_2_2$Bj5.d.out
waste_2_2$Bj5_g <- waste_2_2$Prod_ww * waste_2_2$Bj5.g.out
waste_2_2$Bj5_j <- waste_2_2$Prod_ww * waste_2_2$Bj5.j.out
waste_2_2$Bj5_energy <- waste_2_2$Bj5_d + waste_2_2$Bj5_g + waste_2_2$Bj5_j 
waste_2_2$Bj5_energymain <- waste_2_2$Bj5_j
waste_2_2$Bj5_energyco <- waste_2_2$Bj5_d + waste_2_2$Bj5_g 
waste_2_2$Bj5_netenergy <-(waste_2_2$Bj5_energy - waste_2_2$Prod_ww * waste_2_2$Bj5.e.in -
                             (waste_2_2$collection_en +  waste_2_2$transport1_en)  -
                             (waste_2_2$Bj5_d / 42.79 + waste_2_2$Bj5_g / 41.74 +waste_2_2$Bj5_j / 43.10) * waste_2_2$Transport_km_2 * waste_2_2$Transport_Diesel) * waste_2_2$Bj5_tech
waste_2_2$Bj5_collectionemiss <- waste_2_2$collection_emiss * waste_2_2$Bj5_tech
waste_2_2$Bj5_transport1emiss <- waste_2_2$transport1_emiss * waste_2_2$Bj5_tech
waste_2_2$Bj5_processemiss <- waste_2_2$Prod_ww * waste_2_2$Bj5.e.in * waste_2_2$Powergen_GHG * waste_2_2$Bj5_tech
waste_2_2$Bj5_transport2emiss <-  (waste_2_2$Bj5_d / 42.79 + waste_2_2$Bj5_g / 41.74 + waste_2_2$Bj5_j / 43.10) * waste_2_2$Transport_km_2 * waste_2_2$Transport_Diesel * waste_2_2$Diesel_GHG 
waste_2_2$Bj5_enduseemiss <- waste_2_2$Prod_ww * waste_2_2$Nonbio_emiss2 * waste_2_2$Bj5_tech
waste_2_2$Bj5_dispemiss <- 0 - waste_2_2$Bj5_d *waste_2_2$Diesel_GHG - waste_2_2$Bj5_g* waste_2_2$Gasoline_GHG -  waste_2_2$Bj5_j * waste_2_2$Jet_GHG 
waste_2_2$Bj5_dispemissmain <- 0 - waste_2_2$Bj5_j * waste_2_2$Jet_GHG
waste_2_2$Bj5_dispemissco <- 0 - waste_2_2$Bj5_d * waste_2_2$Diesel_GHG - waste_2_2$Bj5_g * waste_2_2$Gasoline_GHG
waste_2_2$Bj5_netemiss <- waste_2_2$Bj5_collectionemiss + waste_2_2$Bj5_transport1emiss + waste_2_2$Bj5_processemiss + waste_2_2$Bj5_transport2emiss + waste_2_2$Bj5_enduseemiss + waste_2_2$Bj5_dispemiss

#Bj6
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_2$Bj6_d <- waste_2_2$Prod_ww * waste_2_2$Bj6.d.out
waste_2_2$Bj6_g <- waste_2_2$Prod_ww * waste_2_2$Bj6.g.out
waste_2_2$Bj6_j <- waste_2_2$Prod_ww * waste_2_2$Bj6.j.out
waste_2_2$Bj6_energy <- waste_2_2$Bj6_d + waste_2_2$Bj6_g + waste_2_2$Bj6_j 
waste_2_2$Bj6_energymain <- waste_2_2$Bj6_j
waste_2_2$Bj6_energyco <- waste_2_2$Bj6_d + waste_2_2$Bj6_g 
waste_2_2$Bj6_netenergy <-(waste_2_2$Bj6_energy - waste_2_2$Prod_ww * waste_2_2$Bj6.h2.in - waste_2_2$Prod_ww * waste_2_2$Bj6.e.in -
                             (waste_2_2$collection_en +  waste_2_2$transport1_en)  -
                             (waste_2_2$Bj6_d / 42.79 + waste_2_2$Bj6_g / 41.74 +waste_2_2$Bj6_j / 43.10) * waste_2_2$Transport_km_2 * waste_2_2$Transport_Diesel) * waste_2_2$Bj6_tech
waste_2_2$Bj6_collectionemiss <- waste_2_2$collection_emiss * waste_2_2$Bj6_tech
waste_2_2$Bj6_transport1emiss <- waste_2_2$transport1_emiss * waste_2_2$Bj6_tech
waste_2_2$Bj6_processemiss <- waste_2_2$Prod_ww * (waste_2_2$Bj6.h2.in * waste_2_2$H2_GHG + waste_2_2$Bj6.e.in * waste_2_2$Powergen_GHG) * waste_2_2$Bj6_tech
waste_2_2$Bj6_transport2emiss <-  (waste_2_2$Bj6_d / 42.79 + waste_2_2$Bj6_g / 41.74 + waste_2_2$Bj6_j / 43.10) * waste_2_2$Transport_km_2 * waste_2_2$Transport_Diesel * waste_2_2$Diesel_GHG
waste_2_2$Bj6_enduseemiss <- waste_2_2$Prod_ww * waste_2_2$Nonbio_emiss2 * waste_2_2$Bj6_tech
waste_2_2$Bj6_dispemiss <- 0 - waste_2_2$Bj6_d *waste_2_2$Diesel_GHG - waste_2_2$Bj6_g* waste_2_2$Gasoline_GHG -  waste_2_2$Bj6_j * waste_2_2$Jet_GHG 
waste_2_2$Bj6_dispemissmain <- 0 - waste_2_2$Bj6_j * waste_2_2$Jet_GHG
waste_2_2$Bj6_dispemissco <- 0 - waste_2_2$Bj6_d * waste_2_2$Diesel_GHG - waste_2_2$Bj6_g * waste_2_2$Gasoline_GHG
waste_2_2$Bj6_netemiss <- waste_2_2$Bj6_collectionemiss + waste_2_2$Bj6_transport1emiss + waste_2_2$Bj6_processemiss + waste_2_2$Bj6_transport2emiss + waste_2_2$Bj6_enduseemiss + waste_2_2$Bj6_dispemiss

##extracting net emissions
waste_sen1 <- subset(waste_2_1, select = c(Waste_type, Prod_ww, E1_netemiss,E2_netemiss,E3_netemiss,E4_netemiss,M1_netemiss, 
                                           M2_netemiss,Eth1_netemiss, Rd1_netemiss,Rd2_netemiss,Bj1_netemiss,Bj2_netemiss,
                                           Bj3_netemiss,Bj4_netemiss,Bj5_netemiss, Bj6_netemiss))
waste_sen1_1 <- aggregate(.~Waste_type, waste_sen1, sum)
waste_sen1_1 <- gather(waste_sen1_1, category, emiss_kg, E1_netemiss:Bj6_netemiss)
waste_sen1_1$category <- gsub("emiss", "", waste_sen1_1$category)
waste_sen1_1 <- separate(data = waste_sen1_1, col = category, into = c("Tech", "Stage"), sep = "\\_")
waste_sen1_1$emiss_MT_per <- waste_sen1_1$emiss_kg / waste_sen1_1$Prod_ww / 1000
waste_sen1_1$Tech <- factor(waste_sen1_1$Tech, 
                            levels = c("E1", "E2","E3","E4", "M1", "M2", "Eth1", "Rd1", "Rd2",
                                       "Bj1", "Bj2", "Bj3", "Bj4", "Bj5", "Bj6"))
waste_sen1_1$Power_gen <- "Cleaner power (-50% carbon intensity)"

waste_sen2 <- subset(waste_2_2, select = c(Waste_type, Prod_ww, E1_netemiss,E2_netemiss,E3_netemiss,E4_netemiss,M1_netemiss, 
                                           M2_netemiss,Eth1_netemiss, Rd1_netemiss,Rd2_netemiss,Bj1_netemiss,Bj2_netemiss,
                                           Bj3_netemiss,Bj4_netemiss,Bj5_netemiss, Bj6_netemiss))
waste_sen2_1 <- aggregate(.~Waste_type, waste_sen2, sum)
waste_sen2_1 <- gather(waste_sen2_1, category, emiss_kg, E1_netemiss:Bj6_netemiss)
waste_sen2_1$category <- gsub("emiss", "", waste_sen2_1$category)
waste_sen2_1 <- separate(data = waste_sen2_1, col = category, into = c("Tech", "Stage"), sep = "\\_")
waste_sen2_1$emiss_MT_per <- waste_sen2_1$emiss_kg / waste_sen2_1$Prod_ww / 1000
waste_sen2_1$Tech <- factor(waste_sen2_1$Tech, 
                            levels = c("E1", "E2","E3","E4", "M1", "M2", "Eth1", "Rd1", "Rd2",
                                       "Bj1", "Bj2", "Bj3", "Bj4", "Bj5", "Bj6"))
waste_sen2_1$Power_gen <- "Fossil rollback (+50% carbon intensity)"


waste_sen3 <- subset(waste_2, select = c(Waste_type, Prod_ww, E1_netemiss,E2_netemiss,E3_netemiss,E4_netemiss,M1_netemiss, 
                                         M2_netemiss,Eth1_netemiss, Rd1_netemiss,Rd2_netemiss,Bj1_netemiss,Bj2_netemiss,
                                         Bj3_netemiss,Bj4_netemiss,Bj5_netemiss, Bj6_netemiss))
waste_sen3_1 <- aggregate(.~Waste_type, waste_sen3, sum)
waste_sen3_1 <- gather(waste_sen3_1, category, emiss_kg, E1_netemiss:Bj6_netemiss)
waste_sen3_1$category <- gsub("emiss", "", waste_sen3_1$category)
waste_sen3_1 <- separate(data = waste_sen3_1, col = category, into = c("Tech", "Stage"), sep = "\\_")
waste_sen3_1$emiss_MT_per <- waste_sen3_1$emiss_kg / waste_sen3_1$Prod_ww / 1000
waste_sen3_1$Tech <- factor(waste_sen3_1$Tech, 
                            levels = c("E1", "E2","E3","E4", "M1", "M2", "Eth1", "Rd1", "Rd2",
                                       "Bj1", "Bj2", "Bj3", "Bj4", "Bj5", "Bj6"))
waste_sen3_1$Power_gen <- "Current state power grids"



waste_sen <- rbind(waste_sen1_1[ , -which(names(waste_sen1_1) == "Stage")], waste_sen2_1[ , -which(names(waste_sen2_1) == "Stage")],
                   waste_sen3_1[ , -which(names(waste_sen3_1) == "Stage")])
waste_sen$Power_gen <- factor(waste_sen$Power_gen, 
                              levels = c("Cleaner power (-50% carbon intensity)", "Current state power grids","Fossil rollback (+50% carbon intensity)"))
#sensitivity chart
colors_power <- c("Cleaner power (-50% carbon intensity)" = "#7FC97F",
                  "Current state power grids" = "#386CB0",
                  "Fossil rollback (+50% carbon intensity)" = "grey50")
p <- ggplot()+ 
  geom_bar(data = waste_sen[which(waste_sen$emiss_MT_per != 0),],
           aes(x=Tech, y=emiss_MT_per, fill=Power_gen), stat="identity", position = "dodge") +
  geom_hline(yintercept=0, size=0.05)+
  theme_bw() +
  theme(text = element_text(size=20)) +
  scale_fill_manual(values=colors_power) +
  guides(fill = guide_legend(title = "", label.theme = element_text(size = 20, angle = 0))) +
  scale_y_continuous(name="Metric ton CO2e/Mg ww", limits =  c(-1.5, 1)) +
  labs(x = '') +
  theme(legend.position="top",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_grid(~Waste_type, scales = "free_x", space = "free_x")+
  theme(strip.background =element_rect(fill="white"), strip.text = element_text(size = 20, face = "bold"))
#ggtitle("Net GHG emissions from waste biomass utilization - sensitivity analysis") +
#theme(plot.title = element_text(face = "bold", size = 14, hjust = 0))
ggsave(paste(County_FOLDER, "/US_netemiss_SA.png", sep=""), plot=p, width=20,height=6,units="in",dpi=300)




##sensitivity analysis on transport distance
# TD = 25km
coefficients1 <- merge(coefficients1, tech)
waste_1_1 <- merge(waste, coefficients1)
waste_2_3 <- merge(waste_1_1, state_GHG_ef)

#Collection & Transportation_1 (energy - GJ, emiss - kg CO1e)
waste_2_3$Prod_ww <- waste_2_3$Prod/(1-waste_2_3$MC/100)
waste_2_3$collection_en <- waste_2_3$Prod_ww * waste_2_3$Collection_Diesel
waste_2_3$collection_emiss <- waste_2_3$collection_en * waste_2_3$Diesel_GHG 
waste_2_3$transport1_en <- waste_2_3$Prod_ww * waste_2_3$Transport_km_1 * waste_2_3$Transport_Diesel
waste_2_3$transport1_emiss <- waste_2_3$transport1_en  * waste_2_3$Diesel_GHG 

#E1
#electricity T&D loss - 6.5%, heat loss - 20%
waste_2_3$E1_elec <- waste_2_3$Prod_ww * waste_2_3$E1.e.out * (1-0.065)
waste_2_3$E1_heat <- waste_2_3$Prod_ww * waste_2_3$E1.h.out * (1-0.2)
waste_2_3$E1_energy <- waste_2_3$E1_elec + waste_2_3$E1_heat
waste_2_3$E1_energymain <- waste_2_3$E1_elec
waste_2_3$E1_energyco <- waste_2_3$E1_heat
waste_2_3$E1_netenergy <- (waste_2_3$E1_energy - waste_2_3$Prod_ww * (waste_2_3$E1.e.in + waste_2_3$E1.h.in + waste_2_3$E1.d.in) - waste_2_3$collection_en -  waste_2_3$transport1_en) * waste_2_3$E1_tech
waste_2_3$E1_collectionemiss <- waste_2_3$collection_emiss * waste_2_3$E1_tech
waste_2_3$E1_transport1emiss <- waste_2_3$transport1_emiss * waste_2_3$E1_tech
waste_2_3$E1_processemiss <- waste_2_3$Prod_ww * (waste_2_3$E1.e.in * waste_2_3$Powergen_GHG 
                                                  + waste_2_3$E1.h.in * waste_2_3$Heatgen_GHG 
                                                  + waste_2_3$E1.d.in * waste_2_3$Diesel_GHG
                                                  + waste_2_3$Nonbio_emiss1) * waste_2_3$E1_tech
waste_2_3$E1_enduseemiss <- 0
waste_2_3$E1_dispemiss <- 0 - waste_2_3$E1_elec* waste_2_3$Powergen_GHG  - waste_2_3$E1_heat* waste_2_3$Heatgen_GHG 
waste_2_3$E1_dispemissmain <- 0 - waste_2_3$E1_elec* waste_2_3$Powergen_GHG
waste_2_3$E1_dispemissco <- 0 - waste_2_3$E1_heat* waste_2_3$Heatgen_GHG
waste_2_3$E1_netemiss <- waste_2_3$E1_collectionemiss + waste_2_3$E1_transport1emiss + waste_2_3$E1_processemiss + waste_2_3$E1_enduseemiss + waste_2_3$E1_dispemiss

#E2
#electricity T&D loss - 6.5%, heat loss - 20%
waste_2_3$E2_elec <- waste_2_3$Prod_ww * waste_2_3$E2.e.out * (1-0.065)
waste_2_3$E2_heat <- waste_2_3$Prod_ww * waste_2_3$E2.h.out * (1-0.2)
waste_2_3$E2_energy <- waste_2_3$E2_elec + waste_2_3$E2_heat
waste_2_3$E2_energymain <- waste_2_3$E2_elec
waste_2_3$E2_energyco <- waste_2_3$E2_heat
waste_2_3$E2_netenergy <- (waste_2_3$E2_energy - waste_2_3$Prod_ww * (waste_2_3$E2.e.in + waste_2_3$E2.h.in + waste_2_3$E2.d.in) - waste_2_3$collection_en - waste_2_3$transport1_en) * waste_2_3$E2_tech
waste_2_3$E2_collectionemiss <- waste_2_3$collection_emiss * waste_2_3$E2_tech
waste_2_3$E2_transport1emiss <- waste_2_3$transport1_emiss * waste_2_3$E2_tech
waste_2_3$E2_processemiss <- waste_2_3$Prod_ww * (waste_2_3$E2.e.in * waste_2_3$Powergen_GHG 
                                                  + waste_2_3$E2.h.in * waste_2_3$Heatgen_GHG
                                                  + waste_2_3$E2.d.in * waste_2_3$Diesel_GHG
                                                  + waste_2_3$Nonbio_emiss1)  * waste_2_3$E2_tech 
waste_2_3$E2_enduseemiss <- 0
waste_2_3$E2_dispemiss <- 0 - waste_2_3$E2_elec* waste_2_3$Powergen_GHG - waste_2_3$E2_heat* waste_2_3$Heatgen_GHG
waste_2_3$E2_dispemissmain <- 0 - waste_2_3$E2_elec* waste_2_3$Powergen_GHG
waste_2_3$E2_dispemissco <- 0 - waste_2_3$E2_heat* waste_2_3$Heatgen_GHG
waste_2_3$E2_netemiss <- waste_2_3$E2_collectionemiss + waste_2_3$E2_transport1emiss + waste_2_3$E2_processemiss + waste_2_3$E2_enduseemiss + waste_2_3$E2_dispemiss

#E3
#electricity T&D loss - 6.5%
waste_2_3$E3_energy <- waste_2_3$Prod_ww * waste_2_3$E3.e.out * (1-0.065)
waste_2_3$E3_energymain <- waste_2_3$E3_energy
waste_2_3$E3_netenergy <- (waste_2_3$E3_energy - waste_2_3$Prod_ww * (waste_2_3$E3.e.in + waste_2_3$E3.h.in + waste_2_3$E3.d.in) - waste_2_3$collection_en -  waste_2_3$transport1_en) * waste_2_3$E3_tech
waste_2_3$E3_collectionemiss <- waste_2_3$collection_emiss * waste_2_3$E3_tech
waste_2_3$E3_transport1emiss <- waste_2_3$transport1_emiss * waste_2_3$E3_tech
waste_2_3$E3_processemiss <- waste_2_3$Prod_ww * (waste_2_3$E3.e.in * waste_2_3$Powergen_GHG 
                                                  + waste_2_3$E3.h.in * waste_2_3$Heatgen_GHG
                                                  + waste_2_3$E3.d.in * waste_2_3$Diesel_GHG
                                                  + waste_2_3$Nonbio_emiss1)  * waste_2_3$E3_tech
waste_2_3$E3_enduseemiss <- 0
waste_2_3$E3_dispemiss <- 0 - waste_2_3$E3_energy* waste_2_3$Powergen_GHG
waste_2_3$E3_dispemissmain <- 0 - waste_2_3$E3_energy* waste_2_3$Powergen_GHG
waste_2_3$E3_netemiss <- waste_2_3$E3_collectionemiss + waste_2_3$E3_transport1emiss + waste_2_3$E3_processemiss + waste_2_3$E3_enduseemiss + waste_2_3$E3_dispemiss

#E4
#electricity T&D loss - 6.5%, heat loss - 20%
waste_2_3$E4_elec <- waste_2_3$Prod_ww * waste_2_3$E4.e.out * (1-0.065)
waste_2_3$E4_heat <- waste_2_3$Prod_ww * waste_2_3$E4.h.out * (1-0.2)
waste_2_3$E4_energy <- waste_2_3$E4_elec + waste_2_3$E4_heat
waste_2_3$E4_energymain <- waste_2_3$E4_elec
waste_2_3$E4_energyco <- waste_2_3$E4_heat
waste_2_3$E4_netenergy <- (waste_2_3$E4_energy - waste_2_3$Prod_ww * (waste_2_3$E4.ng.in + waste_2_3$E4.d.in) - waste_2_3$collection_en - waste_2_3$transport1_en) * waste_2_3$E4_tech
waste_2_3$E4_collectionemiss <- waste_2_3$collection_emiss * waste_2_3$E4_tech
waste_2_3$E4_transport1emiss <- waste_2_3$transport1_emiss * waste_2_3$E4_tech
waste_2_3$E4_processemiss <- waste_2_3$Prod_ww * (waste_2_3$E4.ng.in * waste_2_3$NG_GHG 
                                                  + waste_2_3$E4.d.in * waste_2_3$Diesel_GHG
                                                  + waste_2_3$Nonbio_emiss1) * waste_2_3$E4_tech 
waste_2_3$E4_enduseemiss <- 0
waste_2_3$E4_dispemiss <- 0 - waste_2_3$E4_elec* waste_2_3$Powergen_GHG - waste_2_3$E4_heat* waste_2_3$Heatgen_GHG
waste_2_3$E4_dispemissmain <- 0 - waste_2_3$E4_elec* waste_2_3$Powergen_GHG
waste_2_3$E4_dispemissco <- 0 - waste_2_3$E4_heat* waste_2_3$Heatgen_GHG
waste_2_3$E4_netemiss <- waste_2_3$E4_collectionemiss + waste_2_3$E4_transport1emiss + waste_2_3$E4_processemiss + waste_2_3$E4_enduseemiss + waste_2_3$E4_dispemiss

#M1
#methane leakage - 2%
waste_2_3$M1_energy <- waste_2_3$Prod_ww * waste_2_3$M1.m.out * (1-0.02)
waste_2_3$M1_energymain <- waste_2_3$M1_energy
waste_2_3$M1_netenergy <- (waste_2_3$M1_energy - waste_2_3$Prod_ww * (waste_2_3$M1.e.in + waste_2_3$M1.h.in) - waste_2_3$collection_en - waste_2_3$transport1_en) * waste_2_3$M1_tech
waste_2_3$M1_collectionemiss <- waste_2_3$collection_emiss * waste_2_3$M1_tech
waste_2_3$M1_transport1emiss <- waste_2_3$transport1_emiss * waste_2_3$M1_tech
waste_2_3$M1_processemiss <- waste_2_3$Prod_ww * (waste_2_3$M1.e.in * waste_2_3$Powergen_GHG + waste_2_3$M1.h.in * waste_2_3$Heatgen_GHG)* waste_2_3$M1_tech
waste_2_3$M1_transport2emiss <- waste_2_3$Prod_ww * waste_2_3$M1.m.out *0.02 /50 *28 * waste_2_3$M1_tech
waste_2_3$M1_enduseemiss <- waste_2_3$Prod_ww * waste_2_3$Nonbio_emiss2  * waste_2_3$M1_tech
waste_2_3$M1_dispemiss <- 0 - waste_2_3$M1_energy* waste_2_3$NG_GHG
waste_2_3$M1_dispemissmain <- waste_2_3$M1_dispemiss
waste_2_3$M1_netemiss <- waste_2_3$M1_collectionemiss + waste_2_3$M1_transport1emiss + waste_2_3$M1_processemiss + waste_2_3$M1_transport2emiss + waste_2_3$M1_enduseemiss + waste_2_3$M1_dispemiss

#M2
#methane leakage - 2%
waste_2_3$M2_energy <- waste_2_3$Prod_ww * waste_2_3$M2.m.out * (1-0.02)
waste_2_3$M2_energymain <- waste_2_3$M2_energy
waste_2_3$M2_netenergy <- (waste_2_3$M2_energy - waste_2_3$Prod_ww * (waste_2_3$M2.e.in + waste_2_3$M2.h.in + waste_2_3$M2.d.in) - waste_2_3$collection_en -  waste_2_3$transport1_en) * waste_2_3$M2_tech
waste_2_3$M2_collectionemiss <- waste_2_3$collection_emiss * waste_2_3$M2_tech
waste_2_3$M2_transport1emiss <- waste_2_3$transport1_emiss * waste_2_3$M2_tech
waste_2_3$M2_processemiss <- waste_2_3$Prod_ww * (waste_2_3$M2.e.in * waste_2_3$Powergen_GHG + 
                                                    waste_2_3$M2.h.in * waste_2_3$Heatgen_GHG +
                                                    waste_2_3$M2.d.in * waste_2_3$Diesel_GHG) * waste_2_3$M2_tech
waste_2_3$M2_transport2emiss <- waste_2_3$Prod_ww * waste_2_3$M2.m.out *0.02 /50 *28 * waste_2_3$M1_tech
waste_2_3$M2_enduseemiss <- waste_2_3$Prod_ww * waste_2_3$Nonbio_emiss2 * waste_2_3$M2_tech
waste_2_3$M2_dispemiss <- 0 - waste_2_3$M2_energy* waste_2_3$NG_GHG
waste_2_3$M2_dispemissmain <- waste_2_3$M2_dispemiss
waste_2_3$M2_netemiss <- waste_2_3$M2_collectionemiss + waste_2_3$M2_transport1emiss + waste_2_3$M2_processemiss + waste_2_3$M2_transport2emiss + waste_2_3$M2_enduseemiss + waste_2_3$M2_dispemiss

#Eth1
#energy intensity of ethanol - 26.95 MJ/kg
waste_2_3$Eth1_elec <- waste_2_3$Prod_ww * waste_2_3$Eth1.e.out * (1-0.065)
waste_2_3$Eth1_eth <- waste_2_3$Prod_ww * waste_2_3$Eth1.eth.out
waste_2_3$Eth1_energy <- waste_2_3$Eth1_elec + waste_2_3$Eth1_eth
waste_2_3$Eth1_energymain <- waste_2_3$Eth1_eth
waste_2_3$Eth1_energyco <- waste_2_3$Eth1_elec
waste_2_3$Eth1_netenergy <- (waste_2_3$Eth1_energy - waste_2_3$Prod_ww * (waste_2_3$Eth1.ng.in  + waste_2_3$Eth1.d.in) -
                               (waste_2_3$collection_en +  waste_2_3$transport1_en) * waste_2_3$M1_tech -
                               waste_2_3$Eth1_eth / 26.95 * waste_2_3$Transport_km_2 * waste_2_3$Transport_Diesel) * waste_2_3$Eth1_tech
waste_2_3$Eth1_collectionemiss <- waste_2_3$collection_emiss * waste_2_3$Eth1_tech
waste_2_3$Eth1_transport1emiss <- waste_2_3$transport1_emiss * waste_2_3$Eth1_tech
waste_2_3$Eth1_processemiss <- waste_2_3$Prod_ww * (waste_2_3$Eth1.ng.in * waste_2_3$Heatgen_GHG + waste_2_3$Eth1.d.in * waste_2_3$Diesel_GHG) * waste_2_3$Eth1_tech 
waste_2_3$Eth1_transport2emiss <- waste_2_3$Eth1_eth / 26.95 * waste_2_3$Transport_km_2 * waste_2_3$Transport_Diesel * waste_2_3$Diesel_GHG 
waste_2_3$Eth1_enduseemiss <- waste_2_3$Prod_ww * waste_2_3$Nonbio_emiss2 * waste_2_3$Eth1_tech
waste_2_3$Eth1_dispemiss <- 0 - waste_2_3$Eth1_eth* waste_2_3$Gasoline_GHG  - waste_2_3$Eth1_elec * waste_2_3$Powergen_GHG
waste_2_3$Eth1_dispemissmain <- 0 - waste_2_3$Eth1_eth* waste_2_3$Gasoline_GHG
waste_2_3$Eth1_dispemissco <- 0 - waste_2_3$Eth1_elec * waste_2_3$Powergen_GHG
waste_2_3$Eth1_netemiss <- waste_2_3$Eth1_collectionemiss + waste_2_3$Eth1_transport1emiss + waste_2_3$Eth1_processemiss + waste_2_3$Eth1_transport2emiss + waste_2_3$Eth1_enduseemiss + waste_2_3$Eth1_dispemiss

#Rd1
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
#electricity T&D loss - 6.5%, methane leakage - 2%
waste_2_3$Rd1_d <- waste_2_3$Prod_ww * waste_2_3$Rd1.d.out
waste_2_3$Rd1_g <- waste_2_3$Prod_ww * waste_2_3$Rd1.g.out
waste_2_3$Rd1_j <- waste_2_3$Prod_ww * waste_2_3$Rd1.j.out
waste_2_3$Rd1_m <- waste_2_3$Prod_ww * waste_2_3$Rd1.m.out * (1-0.02)
waste_2_3$Rd1_elec <- waste_2_3$Prod_ww * waste_2_3$Rd1.e.out * (1-0.065)
waste_2_3$Rd1_energy <- waste_2_3$Rd1_d + waste_2_3$Rd1_g + waste_2_3$Rd1_j + waste_2_3$Rd1_m + waste_2_3$Rd1_elec
waste_2_3$Rd1_energymain <- waste_2_3$Rd1_d
waste_2_3$Rd1_energyco <- waste_2_3$Rd1_g + waste_2_3$Rd1_j + waste_2_3$Rd1_m + waste_2_3$Rd1_elec
waste_2_3$Rd1_netenergy <-(waste_2_3$Rd1_energy - waste_2_3$Prod_ww * waste_2_3$Rd1.e.in -
                             (waste_2_3$collection_en +  waste_2_3$transport1_en)  -
                             waste_2_3$Rd1_d / 42.79 * waste_2_3$Transport_km_2 * waste_2_3$Transport_Diesel -
                             waste_2_3$Rd1_g / 41.74 * waste_2_3$Transport_km_2 * waste_2_3$Transport_Diesel -
                             waste_2_3$Rd1_j / 43.10 * waste_2_3$Transport_km_2 * waste_2_3$Transport_Diesel ) * waste_2_3$Rd1_tech
waste_2_3$Rd1_collectionemiss <- waste_2_3$collection_emiss * waste_2_3$Rd1_tech
waste_2_3$Rd1_transport1emiss <- waste_2_3$transport1_emiss * waste_2_3$Rd1_tech
waste_2_3$Rd1_processemiss <- waste_2_3$Prod_ww * waste_2_3$Rd1.e.in * waste_2_3$Powergen_GHG * waste_2_3$Rd1_tech
waste_2_3$Rd1_transport2emiss <- (waste_2_3$Rd1_d / 42.79 + waste_2_3$Rd1_g / 41.74 + waste_2_3$Rd1_j / 43.10) * waste_2_3$Transport_km_2 * waste_2_3$Transport_Diesel * waste_2_3$Diesel_GHG  
waste_2_3$Rd1_enduseemiss <- waste_2_3$Prod_ww * waste_2_3$Nonbio_emiss2 * waste_2_3$Rd1_tech
waste_2_3$Rd1_dispemiss <- 0 - waste_2_3$Rd1_d *waste_2_3$Diesel_GHG - waste_2_3$Rd1_g * waste_2_3$Gasoline_GHG -  waste_2_3$Rd1_j * waste_2_3$Jet_GHG -
  waste_2_3$Rd1_m * waste_2_3$NG_GHG - waste_2_3$Rd1_elec * waste_2_3$Powergen_GHG 
waste_2_3$Rd1_dispemissmain <- 0 - waste_2_3$Rd1_d *waste_2_3$Diesel_GHG
waste_2_3$Rd1_dispemissco <- 0 - waste_2_3$Rd1_g * waste_2_3$Gasoline_GHG -  waste_2_3$Rd1_j * waste_2_3$Jet_GHG -
  waste_2_3$Rd1_m * waste_2_3$NG_GHG - waste_2_3$Rd1_elec * waste_2_3$Powergen_GHG 
waste_2_3$Rd1_netemiss <- waste_2_3$Rd1_collectionemiss + waste_2_3$Rd1_transport1emiss + waste_2_3$Rd1_processemiss + waste_2_3$Rd1_enduseemiss + waste_2_3$Rd1_transport2emiss + waste_2_3$Rd1_dispemiss


#Rd2
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74
waste_2_3$Rd2_d <- waste_2_3$Prod_ww * waste_2_3$Rd2.d.out
waste_2_3$Rd2_g <- waste_2_3$Prod_ww * waste_2_3$Rd2.g.out
waste_2_3$Rd2_energy <- waste_2_3$Rd2_d + waste_2_3$Rd2_g
waste_2_3$Rd2_energymain <- waste_2_3$Rd2_d
waste_2_3$Rd2_energyco <- waste_2_3$Rd2_g
waste_2_3$Rd2_netenergy <-(waste_2_3$Rd2_energy - waste_2_3$Prod_ww * (waste_2_3$Rd2.e.in + waste_2_3$Rd2.ng.in) -
                             (waste_2_3$collection_en +  waste_2_3$transport1_en)  -
                             waste_2_3$Rd2_d / 42.79 * waste_2_3$Transport_km_2 * waste_2_3$Transport_Diesel -
                             waste_2_3$Rd2_g / 41.74 * waste_2_3$Transport_km_2 * waste_2_3$Transport_Diesel ) * waste_2_3$Rd2_tech
waste_2_3$Rd2_collectionemiss <- waste_2_3$collection_emiss * waste_2_3$Rd2_tech
waste_2_3$Rd2_transport1emiss <- waste_2_3$transport1_emiss * waste_2_3$Rd2_tech
waste_2_3$Rd2_processemiss  <- waste_2_3$Prod_ww * (waste_2_3$Rd2.e.in * waste_2_3$Powergen_GHG + waste_2_3$Rd2.ng.in * waste_2_3$H2_GHG) * waste_2_3$Rd2_tech
waste_2_3$Rd2_transport2emiss <- (waste_2_3$Rd2_d / 42.79  + waste_2_3$Rd2_g / 41.74) * waste_2_3$Transport_km_2 * waste_2_3$Transport_Diesel * waste_2_3$Diesel_GHG 
waste_2_3$Rd2_enduseemiss <- waste_2_3$Prod_ww * waste_2_3$Nonbio_emiss2 * waste_2_3$Rd2_tech
waste_2_3$Rd2_dispemiss <- 0 - waste_2_3$Rd2_d *waste_2_3$Diesel_GHG - waste_2_3$Rd2_g* waste_2_3$Gasoline_GHG 
waste_2_3$Rd2_dispemissmain <- 0 - waste_2_3$Rd2_d *waste_2_3$Diesel_GHG 
waste_2_3$Rd2_dispemissco <- 0 - waste_2_3$Rd2_g* waste_2_3$Gasoline_GHG 
waste_2_3$Rd2_netemiss <- waste_2_3$Rd2_collectionemiss + waste_2_3$Rd2_transport1emiss + waste_2_3$Rd2_processemiss + waste_2_3$Rd2_transport2emiss + waste_2_3$Rd2_enduseemiss + waste_2_3$Rd2_dispemiss

#Bj1 
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_3$Bj1_d <- waste_2_3$Prod_ww * waste_2_3$Bj1.d.out
waste_2_3$Bj1_g <- waste_2_3$Prod_ww * waste_2_3$Bj1.g.out
waste_2_3$Bj1_j <- waste_2_3$Prod_ww * waste_2_3$Bj1.j.out
waste_2_3$Bj1_energy <- waste_2_3$Bj1_d + waste_2_3$Bj1_g + waste_2_3$Bj1_j 
waste_2_3$Bj1_energymain <- waste_2_3$Bj1_d
waste_2_3$Bj1_energyco <- waste_2_3$Bj1_g + waste_2_3$Bj1_j 
waste_2_3$Bj1_netenergy <-(waste_2_3$Bj1_energy - waste_2_3$Prod_ww * waste_2_3$Bj1.h2.in -
                             (waste_2_3$collection_en +  waste_2_3$transport1_en)  -
                             (waste_2_3$Bj1_d / 42.79 + waste_2_3$Bj1_g / 41.74 +waste_2_3$Bj1_j / 43.10) * waste_2_3$Transport_km_2 * waste_2_3$Transport_Diesel) * waste_2_3$Bj1_tech
waste_2_3$Bj1_collectionemiss <- waste_2_3$collection_emiss * waste_2_3$Bj1_tech
waste_2_3$Bj1_transport1emiss <- waste_2_3$transport1_emiss * waste_2_3$Bj1_tech
waste_2_3$Bj1_processemiss <- waste_2_3$Prod_ww * waste_2_3$Bj1.h2.in * waste_2_3$H2_GHG * waste_2_3$Bj1_tech
waste_2_3$Bj1_transport2emiss <- (waste_2_3$Bj1_d / 42.79 + waste_2_3$Bj1_g / 41.74 + waste_2_3$Bj1_j / 43.10) * waste_2_3$Transport_km_2 * waste_2_3$Transport_Diesel * waste_2_3$Diesel_GHG 
waste_2_3$Bj1_enduseemiss <- waste_2_3$Prod_ww * waste_2_3$Nonbio_emiss2 * waste_2_3$Bj1_tech
waste_2_3$Bj1_dispemiss <- 0 - waste_2_3$Bj1_d *waste_2_3$Diesel_GHG - waste_2_3$Bj1_g* waste_2_3$Gasoline_GHG -  waste_2_3$Bj1_j * waste_2_3$Jet_GHG 
waste_2_3$Bj1_dispemissmain <- 0 - waste_2_3$Bj1_d *waste_2_3$Diesel_GHG
waste_2_3$Bj1_dispemissco <- 0 - waste_2_3$Bj1_g* waste_2_3$Gasoline_GHG -  waste_2_3$Bj1_j * waste_2_3$Jet_GHG 
waste_2_3$Bj1_netemiss <- waste_2_3$Bj1_collectionemiss + waste_2_3$Bj1_transport1emiss + waste_2_3$Bj1_processemiss + waste_2_3$Bj1_transport2emiss + waste_2_3$Bj1_enduseemiss + waste_2_3$Bj1_dispemiss

#Bj2 
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_3$Bj2_energy <- waste_2_3$Prod_ww * waste_2_3$Bj2.j.out
waste_2_3$Bj2_energymain <- waste_2_3$Bj1_energy
waste_2_3$Bj2_netenergy <-(waste_2_3$Bj2_energy - waste_2_3$Prod_ww * waste_2_3$Bj2.h2.in -
                             (waste_2_3$collection_en +  waste_2_3$transport1_en)  -
                             waste_2_3$Bj2_energy / 43.10 * waste_2_3$Transport_km_2 * waste_2_3$Transport_Diesel ) * waste_2_3$Bj2_tech
waste_2_3$Bj2_collectionemiss <- waste_2_3$collection_emiss * waste_2_3$Bj2_tech
waste_2_3$Bj2_transport1emiss <- waste_2_3$transport1_emiss * waste_2_3$Bj2_tech
waste_2_3$Bj2_processemiss <- waste_2_3$Prod_ww * waste_2_3$Bj2.h2.in * waste_2_3$H2_GHG 
waste_2_3$Bj2_transport2emiss <- waste_2_3$Bj2_energy / 43.10 * waste_2_3$Transport_km_2 * waste_2_3$Transport_Diesel * waste_2_3$Diesel_GHG 
waste_2_3$Bj2_enduseemiss <- waste_2_3$Prod_ww * waste_2_3$Nonbio_emiss2 * waste_2_3$Bj2_tech
waste_2_3$Bj2_dispemiss <- 0 - waste_2_3$Bj2_energy * waste_2_3$Jet_GHG 
waste_2_3$Bj2_dispemissmain <- waste_2_3$Bj2_dispemiss
waste_2_3$Bj2_netemiss <- waste_2_3$Bj2_collectionemiss + waste_2_3$Bj2_transport1emiss + waste_2_3$Bj2_processemiss + waste_2_3$Bj2_transport2emiss + waste_2_3$Bj2_enduseemiss + waste_2_3$Bj2_dispemiss

#Bj3
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_3$Bj3_d <- waste_2_3$Prod_ww * waste_2_3$Bj3.d.out
waste_2_3$Bj3_g <- waste_2_3$Prod_ww * waste_2_3$Bj3.g.out
waste_2_3$Bj3_j <- waste_2_3$Prod_ww * waste_2_3$Bj3.j.out
waste_2_3$Bj3_energy <- waste_2_3$Bj3_d + waste_2_3$Bj3_g + waste_2_3$Bj3_j 
waste_2_3$Bj3_energymain <- waste_2_3$Bj3_j
waste_2_3$Bj3_energyco <- waste_2_3$Bj3_d + waste_2_3$Bj3_g 
waste_2_3$Bj3_netenergy <-(waste_2_3$Bj3_energy - waste_2_3$Prod_ww * waste_2_3$Bj3.e.in -
                             (waste_2_3$collection_en +  waste_2_3$transport1_en)  -
                             (waste_2_3$Bj3_d / 42.79 + waste_2_3$Bj3_g / 41.74 +waste_2_3$Bj3_j / 43.10) * waste_2_3$Transport_km_2 * waste_2_3$Transport_Diesel) * waste_2_3$Bj3_tech
waste_2_3$Bj3_collectionemiss <- waste_2_3$collection_emiss * waste_2_3$Bj3_tech
waste_2_3$Bj3_transport1emiss <- waste_2_3$transport1_emiss * waste_2_3$Bj3_tech
waste_2_3$Bj3_processemiss <- waste_2_3$Prod_ww * waste_2_3$Bj3.e.in * waste_2_3$Powergen_GHG * waste_2_3$Bj3_tech
waste_2_3$Bj3_transport2emiss <- (waste_2_3$Bj3_d / 42.79 + waste_2_3$Bj3_g / 41.74 + waste_2_3$Bj3_j / 43.10) * waste_2_3$Transport_km_2 * waste_2_3$Transport_Diesel * waste_2_3$Diesel_GHG
waste_2_3$Bj3_enduseemiss <- waste_2_3$Prod_ww * waste_2_3$Nonbio_emiss2 * waste_2_3$Bj3_tech
waste_2_3$Bj3_dispemiss <- 0 - waste_2_3$Bj3_d *waste_2_3$Diesel_GHG - waste_2_3$Bj3_g* waste_2_3$Gasoline_GHG -  waste_2_3$Bj3_j * waste_2_3$Jet_GHG 
waste_2_3$Bj3_dispemissmain <- 0 - waste_2_3$Bj3_j * waste_2_3$Jet_GHG
waste_2_3$Bj3_dispemissco <- 0 - waste_2_3$Bj3_d * waste_2_3$Diesel_GHG - waste_2_3$Bj3_g * waste_2_3$Gasoline_GHG
waste_2_3$Bj3_netemiss <- waste_2_3$Bj3_collectionemiss + waste_2_3$Bj3_transport1emiss + waste_2_3$Bj3_processemiss + waste_2_3$Bj3_transport2emiss + waste_2_3$Bj3_enduseemiss + waste_2_3$Bj3_dispemiss

#Bj4
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_3$Bj4_d <- waste_2_3$Prod_ww * waste_2_3$Bj4.d.out
waste_2_3$Bj4_g <- waste_2_3$Prod_ww * waste_2_3$Bj4.g.out
waste_2_3$Bj4_j <- waste_2_3$Prod_ww * waste_2_3$Bj4.j.out
waste_2_3$Bj4_energy <- waste_2_3$Bj4_d + waste_2_3$Bj4_g + waste_2_3$Bj4_j 
waste_2_3$Bj4_energymain <- waste_2_3$Bj4_j
waste_2_3$Bj4_energyco <- waste_2_3$Bj4_d + waste_2_3$Bj4_g 
waste_2_3$Bj4_netenergy <-(waste_2_3$Bj4_energy - waste_2_3$Prod_ww * waste_2_3$Bj4.h2.in -
                             (waste_2_3$collection_en +  waste_2_3$transport1_en)  -
                             (waste_2_3$Bj4_d / 42.79 + waste_2_3$Bj4_g / 41.74 +waste_2_3$Bj4_j / 43.10) * waste_2_3$Transport_km_2 * waste_2_3$Transport_Diesel) * waste_2_3$Bj4_tech
waste_2_3$Bj4_collectionemiss <- waste_2_3$collection_emiss * waste_2_3$Bj4_tech
waste_2_3$Bj4_transport1emiss <- waste_2_3$transport1_emiss * waste_2_3$Bj4_tech
waste_2_3$Bj4_processemiss <- waste_2_3$Prod_ww * waste_2_3$Bj4.h2.in * waste_2_3$H2_GHG * waste_2_3$Bj4_tech
waste_2_3$Bj4_transport2emiss <- (waste_2_3$Bj4_d / 42.79 + waste_2_3$Bj4_g / 41.74 + waste_2_3$Bj4_j / 43.10) * waste_2_3$Transport_km_2 * waste_2_3$Transport_Diesel * waste_2_3$Diesel_GHG 
waste_2_3$Bj4_enduseemiss <- waste_2_3$Prod_ww * waste_2_3$Nonbio_emiss2 * waste_2_3$Bj4_tech
waste_2_3$Bj4_dispemiss <- 0 - waste_2_3$Bj4_d *waste_2_3$Diesel_GHG - waste_2_3$Bj4_g* waste_2_3$Gasoline_GHG -  waste_2_3$Bj4_j * waste_2_3$Jet_GHG 
waste_2_3$Bj4_dispemissmain <- 0 - waste_2_3$Bj4_j * waste_2_3$Jet_GHG
waste_2_3$Bj4_dispemissco <- 0 - waste_2_3$Bj4_d * waste_2_3$Diesel_GHG - waste_2_3$Bj4_g * waste_2_3$Gasoline_GHG
waste_2_3$Bj4_netemiss <- waste_2_3$Bj4_collectionemiss + waste_2_3$Bj4_transport1emiss + waste_2_3$Bj4_processemiss + waste_2_3$Bj4_transport2emiss+ waste_2_3$Bj4_enduseemiss + waste_2_3$Bj4_dispemiss

#Bj5
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_3$Bj5_d <- waste_2_3$Prod_ww * waste_2_3$Bj5.d.out
waste_2_3$Bj5_g <- waste_2_3$Prod_ww * waste_2_3$Bj5.g.out
waste_2_3$Bj5_j <- waste_2_3$Prod_ww * waste_2_3$Bj5.j.out
waste_2_3$Bj5_energy <- waste_2_3$Bj5_d + waste_2_3$Bj5_g + waste_2_3$Bj5_j 
waste_2_3$Bj5_energymain <- waste_2_3$Bj5_j
waste_2_3$Bj5_energyco <- waste_2_3$Bj5_d + waste_2_3$Bj5_g 
waste_2_3$Bj5_netenergy <-(waste_2_3$Bj5_energy - waste_2_3$Prod_ww * waste_2_3$Bj5.e.in -
                             (waste_2_3$collection_en +  waste_2_3$transport1_en)  -
                             (waste_2_3$Bj5_d / 42.79 + waste_2_3$Bj5_g / 41.74 +waste_2_3$Bj5_j / 43.10) * waste_2_3$Transport_km_2 * waste_2_3$Transport_Diesel) * waste_2_3$Bj5_tech
waste_2_3$Bj5_collectionemiss <- waste_2_3$collection_emiss * waste_2_3$Bj5_tech
waste_2_3$Bj5_transport1emiss <- waste_2_3$transport1_emiss * waste_2_3$Bj5_tech
waste_2_3$Bj5_processemiss <- waste_2_3$Prod_ww * waste_2_3$Bj5.e.in * waste_2_3$Powergen_GHG * waste_2_3$Bj5_tech
waste_2_3$Bj5_transport2emiss <-  (waste_2_3$Bj5_d / 42.79 + waste_2_3$Bj5_g / 41.74 + waste_2_3$Bj5_j / 43.10) * waste_2_3$Transport_km_2 * waste_2_3$Transport_Diesel * waste_2_3$Diesel_GHG 
waste_2_3$Bj5_enduseemiss <- waste_2_3$Prod_ww * waste_2_3$Nonbio_emiss2 * waste_2_3$Bj5_tech
waste_2_3$Bj5_dispemiss <- 0 - waste_2_3$Bj5_d *waste_2_3$Diesel_GHG - waste_2_3$Bj5_g* waste_2_3$Gasoline_GHG -  waste_2_3$Bj5_j * waste_2_3$Jet_GHG 
waste_2_3$Bj5_dispemissmain <- 0 - waste_2_3$Bj5_j * waste_2_3$Jet_GHG
waste_2_3$Bj5_dispemissco <- 0 - waste_2_3$Bj5_d * waste_2_3$Diesel_GHG - waste_2_3$Bj5_g * waste_2_3$Gasoline_GHG
waste_2_3$Bj5_netemiss <- waste_2_3$Bj5_collectionemiss + waste_2_3$Bj5_transport1emiss + waste_2_3$Bj5_processemiss + waste_2_3$Bj5_transport2emiss + waste_2_3$Bj5_enduseemiss + waste_2_3$Bj5_dispemiss

#Bj6
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_3$Bj6_d <- waste_2_3$Prod_ww * waste_2_3$Bj6.d.out
waste_2_3$Bj6_g <- waste_2_3$Prod_ww * waste_2_3$Bj6.g.out
waste_2_3$Bj6_j <- waste_2_3$Prod_ww * waste_2_3$Bj6.j.out
waste_2_3$Bj6_energy <- waste_2_3$Bj6_d + waste_2_3$Bj6_g + waste_2_3$Bj6_j 
waste_2_3$Bj6_energymain <- waste_2_3$Bj6_j
waste_2_3$Bj6_energyco <- waste_2_3$Bj6_d + waste_2_3$Bj6_g 
waste_2_3$Bj6_netenergy <-(waste_2_3$Bj6_energy - waste_2_3$Prod_ww * waste_2_3$Bj6.h2.in - waste_2_3$Prod_ww * waste_2_3$Bj6.e.in -
                             (waste_2_3$collection_en +  waste_2_3$transport1_en)  -
                             (waste_2_3$Bj6_d / 42.79 + waste_2_3$Bj6_g / 41.74 +waste_2_3$Bj6_j / 43.10) * waste_2_3$Transport_km_2 * waste_2_3$Transport_Diesel) * waste_2_3$Bj6_tech
waste_2_3$Bj6_collectionemiss <- waste_2_3$collection_emiss * waste_2_3$Bj6_tech
waste_2_3$Bj6_transport1emiss <- waste_2_3$transport1_emiss * waste_2_3$Bj6_tech
waste_2_3$Bj6_processemiss <- waste_2_3$Prod_ww * (waste_2_3$Bj6.h2.in * waste_2_3$H2_GHG + waste_2_3$Bj6.e.in * waste_2_3$Powergen_GHG) * waste_2_3$Bj6_tech
waste_2_3$Bj6_transport2emiss <-  (waste_2_3$Bj6_d / 42.79 + waste_2_3$Bj6_g / 41.74 + waste_2_3$Bj6_j / 43.10) * waste_2_3$Transport_km_2 * waste_2_3$Transport_Diesel * waste_2_3$Diesel_GHG
waste_2_3$Bj6_enduseemiss <- waste_2_3$Prod_ww * waste_2_3$Nonbio_emiss2 * waste_2_3$Bj6_tech
waste_2_3$Bj6_dispemiss <- 0 - waste_2_3$Bj6_d *waste_2_3$Diesel_GHG - waste_2_3$Bj6_g* waste_2_3$Gasoline_GHG -  waste_2_3$Bj6_j * waste_2_3$Jet_GHG 
waste_2_3$Bj6_dispemissmain <- 0 - waste_2_3$Bj6_j * waste_2_3$Jet_GHG
waste_2_3$Bj6_dispemissco <- 0 - waste_2_3$Bj6_d * waste_2_3$Diesel_GHG - waste_2_3$Bj6_g * waste_2_3$Gasoline_GHG
waste_2_3$Bj6_netemiss <- waste_2_3$Bj6_collectionemiss + waste_2_3$Bj6_transport1emiss + waste_2_3$Bj6_processemiss + waste_2_3$Bj6_transport2emiss + waste_2_3$Bj6_enduseemiss + waste_2_3$Bj6_dispemiss



# TD = 50km
coefficients2 <- merge(coefficients2, tech)
waste_1_2 <- merge(waste, coefficients2)
waste_2_4 <- merge(waste_1_2, state_GHG_ef)

#Collection & Transportation_1 (energy - GJ, emiss - kg CO1e)
waste_2_4$Prod_ww <- waste_2_4$Prod/(1-waste_2_4$MC/100)
waste_2_4$collection_en <- waste_2_4$Prod_ww * waste_2_4$Collection_Diesel
waste_2_4$collection_emiss <- waste_2_4$collection_en * waste_2_4$Diesel_GHG 
waste_2_4$transport1_en <- waste_2_4$Prod_ww * waste_2_4$Transport_km_1 * waste_2_4$Transport_Diesel
waste_2_4$transport1_emiss <- waste_2_4$transport1_en  * waste_2_4$Diesel_GHG 

#E1
#electricity T&D loss - 6.5%, heat loss - 20%
waste_2_4$E1_elec <- waste_2_4$Prod_ww * waste_2_4$E1.e.out * (1-0.065)
waste_2_4$E1_heat <- waste_2_4$Prod_ww * waste_2_4$E1.h.out * (1-0.2)
waste_2_4$E1_energy <- waste_2_4$E1_elec + waste_2_4$E1_heat
waste_2_4$E1_energymain <- waste_2_4$E1_elec
waste_2_4$E1_energyco <- waste_2_4$E1_heat
waste_2_4$E1_netenergy <- (waste_2_4$E1_energy - waste_2_4$Prod_ww * (waste_2_4$E1.e.in + waste_2_4$E1.h.in + waste_2_4$E1.d.in) - waste_2_4$collection_en -  waste_2_4$transport1_en) * waste_2_4$E1_tech
waste_2_4$E1_collectionemiss <- waste_2_4$collection_emiss * waste_2_4$E1_tech
waste_2_4$E1_transport1emiss <- waste_2_4$transport1_emiss * waste_2_4$E1_tech
waste_2_4$E1_processemiss <- waste_2_4$Prod_ww * (waste_2_4$E1.e.in * waste_2_4$Powergen_GHG 
                                                  + waste_2_4$E1.h.in * waste_2_4$Heatgen_GHG 
                                                  + waste_2_4$E1.d.in * waste_2_4$Diesel_GHG
                                                  + waste_2_4$Nonbio_emiss1) * waste_2_4$E1_tech
waste_2_4$E1_enduseemiss <- 0
waste_2_4$E1_dispemiss <- 0 - waste_2_4$E1_elec* waste_2_4$Powergen_GHG  - waste_2_4$E1_heat* waste_2_4$Heatgen_GHG 
waste_2_4$E1_dispemissmain <- 0 - waste_2_4$E1_elec* waste_2_4$Powergen_GHG
waste_2_4$E1_dispemissco <- 0 - waste_2_4$E1_heat* waste_2_4$Heatgen_GHG
waste_2_4$E1_netemiss <- waste_2_4$E1_collectionemiss + waste_2_4$E1_transport1emiss + waste_2_4$E1_processemiss + waste_2_4$E1_enduseemiss + waste_2_4$E1_dispemiss

#E2
#electricity T&D loss - 6.5%, heat loss - 20%
waste_2_4$E2_elec <- waste_2_4$Prod_ww * waste_2_4$E2.e.out * (1-0.065)
waste_2_4$E2_heat <- waste_2_4$Prod_ww * waste_2_4$E2.h.out * (1-0.2)
waste_2_4$E2_energy <- waste_2_4$E2_elec + waste_2_4$E2_heat
waste_2_4$E2_energymain <- waste_2_4$E2_elec
waste_2_4$E2_energyco <- waste_2_4$E2_heat
waste_2_4$E2_netenergy <- (waste_2_4$E2_energy - waste_2_4$Prod_ww * (waste_2_4$E2.e.in + waste_2_4$E2.h.in + waste_2_4$E2.d.in) - waste_2_4$collection_en - waste_2_4$transport1_en) * waste_2_4$E2_tech
waste_2_4$E2_collectionemiss <- waste_2_4$collection_emiss * waste_2_4$E2_tech
waste_2_4$E2_transport1emiss <- waste_2_4$transport1_emiss * waste_2_4$E2_tech
waste_2_4$E2_processemiss <- waste_2_4$Prod_ww * (waste_2_4$E2.e.in * waste_2_4$Powergen_GHG 
                                                  + waste_2_4$E2.h.in * waste_2_4$Heatgen_GHG
                                                  + waste_2_4$E2.d.in * waste_2_4$Diesel_GHG
                                                  + waste_2_4$Nonbio_emiss1)  * waste_2_4$E2_tech 
waste_2_4$E2_enduseemiss <- 0
waste_2_4$E2_dispemiss <- 0 - waste_2_4$E2_elec* waste_2_4$Powergen_GHG - waste_2_4$E2_heat* waste_2_4$Heatgen_GHG
waste_2_4$E2_dispemissmain <- 0 - waste_2_4$E2_elec* waste_2_4$Powergen_GHG
waste_2_4$E2_dispemissco <- 0 - waste_2_4$E2_heat* waste_2_4$Heatgen_GHG
waste_2_4$E2_netemiss <- waste_2_4$E2_collectionemiss + waste_2_4$E2_transport1emiss + waste_2_4$E2_processemiss + waste_2_4$E2_enduseemiss + waste_2_4$E2_dispemiss

#E3
#electricity T&D loss - 6.5%
waste_2_4$E3_energy <- waste_2_4$Prod_ww * waste_2_4$E3.e.out * (1-0.065)
waste_2_4$E3_energymain <- waste_2_4$E3_energy
waste_2_4$E3_netenergy <- (waste_2_4$E3_energy - waste_2_4$Prod_ww * (waste_2_4$E3.e.in + waste_2_4$E3.h.in + waste_2_4$E3.d.in) - waste_2_4$collection_en -  waste_2_4$transport1_en) * waste_2_4$E3_tech
waste_2_4$E3_collectionemiss <- waste_2_4$collection_emiss * waste_2_4$E3_tech
waste_2_4$E3_transport1emiss <- waste_2_4$transport1_emiss * waste_2_4$E3_tech
waste_2_4$E3_processemiss <- waste_2_4$Prod_ww * (waste_2_4$E3.e.in * waste_2_4$Powergen_GHG 
                                                  + waste_2_4$E3.h.in * waste_2_4$Heatgen_GHG
                                                  + waste_2_4$E3.d.in * waste_2_4$Diesel_GHG
                                                  + waste_2_4$Nonbio_emiss1)  * waste_2_4$E3_tech
waste_2_4$E3_enduseemiss <- 0
waste_2_4$E3_dispemiss <- 0 - waste_2_4$E3_energy* waste_2_4$Powergen_GHG
waste_2_4$E3_dispemissmain <- 0 - waste_2_4$E3_energy* waste_2_4$Powergen_GHG
waste_2_4$E3_netemiss <- waste_2_4$E3_collectionemiss + waste_2_4$E3_transport1emiss + waste_2_4$E3_processemiss + waste_2_4$E3_enduseemiss + waste_2_4$E3_dispemiss

#E4
#electricity T&D loss - 6.5%, heat loss - 20%
waste_2_4$E4_elec <- waste_2_4$Prod_ww * waste_2_4$E4.e.out * (1-0.065)
waste_2_4$E4_heat <- waste_2_4$Prod_ww * waste_2_4$E4.h.out * (1-0.2)
waste_2_4$E4_energy <- waste_2_4$E4_elec + waste_2_4$E4_heat
waste_2_4$E4_energymain <- waste_2_4$E4_elec
waste_2_4$E4_energyco <- waste_2_4$E4_heat
waste_2_4$E4_netenergy <- (waste_2_4$E4_energy - waste_2_4$Prod_ww * (waste_2_4$E4.ng.in + waste_2_4$E4.d.in) - waste_2_4$collection_en - waste_2_4$transport1_en) * waste_2_4$E4_tech
waste_2_4$E4_collectionemiss <- waste_2_4$collection_emiss * waste_2_4$E4_tech
waste_2_4$E4_transport1emiss <- waste_2_4$transport1_emiss * waste_2_4$E4_tech
waste_2_4$E4_processemiss <- waste_2_4$Prod_ww * (waste_2_4$E4.ng.in * waste_2_4$NG_GHG 
                                                  + waste_2_4$E4.d.in * waste_2_4$Diesel_GHG
                                                  + waste_2_4$Nonbio_emiss1) * waste_2_4$E4_tech 
waste_2_4$E4_enduseemiss <- 0
waste_2_4$E4_dispemiss <- 0 - waste_2_4$E4_elec* waste_2_4$Powergen_GHG - waste_2_4$E4_heat* waste_2_4$Heatgen_GHG
waste_2_4$E4_dispemissmain <- 0 - waste_2_4$E4_elec* waste_2_4$Powergen_GHG
waste_2_4$E4_dispemissco <- 0 - waste_2_4$E4_heat* waste_2_4$Heatgen_GHG
waste_2_4$E4_netemiss <- waste_2_4$E4_collectionemiss + waste_2_4$E4_transport1emiss + waste_2_4$E4_processemiss + waste_2_4$E4_enduseemiss + waste_2_4$E4_dispemiss

#M1
#methane leakage - 2%
waste_2_4$M1_energy <- waste_2_4$Prod_ww * waste_2_4$M1.m.out * (1-0.02)
waste_2_4$M1_energymain <- waste_2_4$M1_energy
waste_2_4$M1_netenergy <- (waste_2_4$M1_energy - waste_2_4$Prod_ww * (waste_2_4$M1.e.in + waste_2_4$M1.h.in) - waste_2_4$collection_en - waste_2_4$transport1_en) * waste_2_4$M1_tech
waste_2_4$M1_collectionemiss <- waste_2_4$collection_emiss * waste_2_4$M1_tech
waste_2_4$M1_transport1emiss <- waste_2_4$transport1_emiss * waste_2_4$M1_tech
waste_2_4$M1_processemiss <- waste_2_4$Prod_ww * (waste_2_4$M1.e.in * waste_2_4$Powergen_GHG + waste_2_4$M1.h.in * waste_2_4$Heatgen_GHG)* waste_2_4$M1_tech
waste_2_4$M1_transport2emiss <- waste_2_4$Prod_ww * waste_2_4$M1.m.out *0.02 /50 *28 * waste_2_4$M1_tech
waste_2_4$M1_enduseemiss <- waste_2_4$Prod_ww * waste_2_4$Nonbio_emiss2  * waste_2_4$M1_tech
waste_2_4$M1_dispemiss <- 0 - waste_2_4$M1_energy* waste_2_4$NG_GHG
waste_2_4$M1_dispemissmain <- waste_2_4$M1_dispemiss
waste_2_4$M1_netemiss <- waste_2_4$M1_collectionemiss + waste_2_4$M1_transport1emiss + waste_2_4$M1_processemiss + waste_2_4$M1_transport2emiss + waste_2_4$M1_enduseemiss + waste_2_4$M1_dispemiss

#M2
#methane leakage - 2%
waste_2_4$M2_energy <- waste_2_4$Prod_ww * waste_2_4$M2.m.out * (1-0.02)
waste_2_4$M2_energymain <- waste_2_4$M2_energy
waste_2_4$M2_netenergy <- (waste_2_4$M2_energy - waste_2_4$Prod_ww * (waste_2_4$M2.e.in + waste_2_4$M2.h.in + waste_2_4$M2.d.in) - waste_2_4$collection_en -  waste_2_4$transport1_en) * waste_2_4$M2_tech
waste_2_4$M2_collectionemiss <- waste_2_4$collection_emiss * waste_2_4$M2_tech
waste_2_4$M2_transport1emiss <- waste_2_4$transport1_emiss * waste_2_4$M2_tech
waste_2_4$M2_processemiss <- waste_2_4$Prod_ww * (waste_2_4$M2.e.in * waste_2_4$Powergen_GHG + 
                                                    waste_2_4$M2.h.in * waste_2_4$Heatgen_GHG +
                                                    waste_2_4$M2.d.in * waste_2_4$Diesel_GHG) * waste_2_4$M2_tech
waste_2_4$M2_transport2emiss <- waste_2_4$Prod_ww * waste_2_4$M2.m.out *0.02 /50 *28 * waste_2_4$M1_tech
waste_2_4$M2_enduseemiss <- waste_2_4$Prod_ww * waste_2_4$Nonbio_emiss2 * waste_2_4$M2_tech
waste_2_4$M2_dispemiss <- 0 - waste_2_4$M2_energy* waste_2_4$NG_GHG
waste_2_4$M2_dispemissmain <- waste_2_4$M2_dispemiss
waste_2_4$M2_netemiss <- waste_2_4$M2_collectionemiss + waste_2_4$M2_transport1emiss + waste_2_4$M2_processemiss + waste_2_4$M2_transport2emiss + waste_2_4$M2_enduseemiss + waste_2_4$M2_dispemiss

#Eth1
#energy intensity of ethanol - 26.95 MJ/kg
waste_2_4$Eth1_elec <- waste_2_4$Prod_ww * waste_2_4$Eth1.e.out * (1-0.065)
waste_2_4$Eth1_eth <- waste_2_4$Prod_ww * waste_2_4$Eth1.eth.out
waste_2_4$Eth1_energy <- waste_2_4$Eth1_elec + waste_2_4$Eth1_eth
waste_2_4$Eth1_energymain <- waste_2_4$Eth1_eth
waste_2_4$Eth1_energyco <- waste_2_4$Eth1_elec
waste_2_4$Eth1_netenergy <- (waste_2_4$Eth1_energy - waste_2_4$Prod_ww * (waste_2_4$Eth1.ng.in  + waste_2_4$Eth1.d.in) -
                               (waste_2_4$collection_en +  waste_2_4$transport1_en) * waste_2_4$M1_tech -
                               waste_2_4$Eth1_eth / 26.95 * waste_2_4$Transport_km_2 * waste_2_4$Transport_Diesel) * waste_2_4$Eth1_tech
waste_2_4$Eth1_collectionemiss <- waste_2_4$collection_emiss * waste_2_4$Eth1_tech
waste_2_4$Eth1_transport1emiss <- waste_2_4$transport1_emiss * waste_2_4$Eth1_tech
waste_2_4$Eth1_processemiss <- waste_2_4$Prod_ww * (waste_2_4$Eth1.ng.in * waste_2_4$Heatgen_GHG + waste_2_4$Eth1.d.in * waste_2_4$Diesel_GHG) * waste_2_4$Eth1_tech 
waste_2_4$Eth1_transport2emiss <- waste_2_4$Eth1_eth / 26.95 * waste_2_4$Transport_km_2 * waste_2_4$Transport_Diesel * waste_2_4$Diesel_GHG 
waste_2_4$Eth1_enduseemiss <- waste_2_4$Prod_ww * waste_2_4$Nonbio_emiss2 * waste_2_4$Eth1_tech
waste_2_4$Eth1_dispemiss <- 0 - waste_2_4$Eth1_eth* waste_2_4$Gasoline_GHG  - waste_2_4$Eth1_elec * waste_2_4$Powergen_GHG
waste_2_4$Eth1_dispemissmain <- 0 - waste_2_4$Eth1_eth* waste_2_4$Gasoline_GHG
waste_2_4$Eth1_dispemissco <- 0 - waste_2_4$Eth1_elec * waste_2_4$Powergen_GHG
waste_2_4$Eth1_netemiss <- waste_2_4$Eth1_collectionemiss + waste_2_4$Eth1_transport1emiss + waste_2_4$Eth1_processemiss + waste_2_4$Eth1_transport2emiss + waste_2_4$Eth1_enduseemiss + waste_2_4$Eth1_dispemiss

#Rd1
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
#electricity T&D loss - 6.5%, methane leakage - 2%
waste_2_4$Rd1_d <- waste_2_4$Prod_ww * waste_2_4$Rd1.d.out
waste_2_4$Rd1_g <- waste_2_4$Prod_ww * waste_2_4$Rd1.g.out
waste_2_4$Rd1_j <- waste_2_4$Prod_ww * waste_2_4$Rd1.j.out
waste_2_4$Rd1_m <- waste_2_4$Prod_ww * waste_2_4$Rd1.m.out * (1-0.02)
waste_2_4$Rd1_elec <- waste_2_4$Prod_ww * waste_2_4$Rd1.e.out * (1-0.065)
waste_2_4$Rd1_energy <- waste_2_4$Rd1_d + waste_2_4$Rd1_g + waste_2_4$Rd1_j + waste_2_4$Rd1_m + waste_2_4$Rd1_elec
waste_2_4$Rd1_energymain <- waste_2_4$Rd1_d
waste_2_4$Rd1_energyco <- waste_2_4$Rd1_g + waste_2_4$Rd1_j + waste_2_4$Rd1_m + waste_2_4$Rd1_elec
waste_2_4$Rd1_netenergy <-(waste_2_4$Rd1_energy - waste_2_4$Prod_ww * waste_2_4$Rd1.e.in -
                             (waste_2_4$collection_en +  waste_2_4$transport1_en)  -
                             waste_2_4$Rd1_d / 42.79 * waste_2_4$Transport_km_2 * waste_2_4$Transport_Diesel -
                             waste_2_4$Rd1_g / 41.74 * waste_2_4$Transport_km_2 * waste_2_4$Transport_Diesel -
                             waste_2_4$Rd1_j / 43.10 * waste_2_4$Transport_km_2 * waste_2_4$Transport_Diesel ) * waste_2_4$Rd1_tech
waste_2_4$Rd1_collectionemiss <- waste_2_4$collection_emiss * waste_2_4$Rd1_tech
waste_2_4$Rd1_transport1emiss <- waste_2_4$transport1_emiss * waste_2_4$Rd1_tech
waste_2_4$Rd1_processemiss <- waste_2_4$Prod_ww * waste_2_4$Rd1.e.in * waste_2_4$Powergen_GHG * waste_2_4$Rd1_tech
waste_2_4$Rd1_transport2emiss <- (waste_2_4$Rd1_d / 42.79 + waste_2_4$Rd1_g / 41.74 + waste_2_4$Rd1_j / 43.10) * waste_2_4$Transport_km_2 * waste_2_4$Transport_Diesel * waste_2_4$Diesel_GHG  
waste_2_4$Rd1_enduseemiss <- waste_2_4$Prod_ww * waste_2_4$Nonbio_emiss2 * waste_2_4$Rd1_tech
waste_2_4$Rd1_dispemiss <- 0 - waste_2_4$Rd1_d *waste_2_4$Diesel_GHG - waste_2_4$Rd1_g * waste_2_4$Gasoline_GHG -  waste_2_4$Rd1_j * waste_2_4$Jet_GHG -
  waste_2_4$Rd1_m * waste_2_4$NG_GHG - waste_2_4$Rd1_elec * waste_2_4$Powergen_GHG 
waste_2_4$Rd1_dispemissmain <- 0 - waste_2_4$Rd1_d *waste_2_4$Diesel_GHG
waste_2_4$Rd1_dispemissco <- 0 - waste_2_4$Rd1_g * waste_2_4$Gasoline_GHG -  waste_2_4$Rd1_j * waste_2_4$Jet_GHG -
  waste_2_4$Rd1_m * waste_2_4$NG_GHG - waste_2_4$Rd1_elec * waste_2_4$Powergen_GHG 
waste_2_4$Rd1_netemiss <- waste_2_4$Rd1_collectionemiss + waste_2_4$Rd1_transport1emiss + waste_2_4$Rd1_processemiss + waste_2_4$Rd1_enduseemiss + waste_2_4$Rd1_transport2emiss + waste_2_4$Rd1_dispemiss


#Rd2
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74
waste_2_4$Rd2_d <- waste_2_4$Prod_ww * waste_2_4$Rd2.d.out
waste_2_4$Rd2_g <- waste_2_4$Prod_ww * waste_2_4$Rd2.g.out
waste_2_4$Rd2_energy <- waste_2_4$Rd2_d + waste_2_4$Rd2_g
waste_2_4$Rd2_energymain <- waste_2_4$Rd2_d
waste_2_4$Rd2_energyco <- waste_2_4$Rd2_g
waste_2_4$Rd2_netenergy <-(waste_2_4$Rd2_energy - waste_2_4$Prod_ww * (waste_2_4$Rd2.e.in + waste_2_4$Rd2.ng.in) -
                             (waste_2_4$collection_en +  waste_2_4$transport1_en)  -
                             waste_2_4$Rd2_d / 42.79 * waste_2_4$Transport_km_2 * waste_2_4$Transport_Diesel -
                             waste_2_4$Rd2_g / 41.74 * waste_2_4$Transport_km_2 * waste_2_4$Transport_Diesel ) * waste_2_4$Rd2_tech
waste_2_4$Rd2_collectionemiss <- waste_2_4$collection_emiss * waste_2_4$Rd2_tech
waste_2_4$Rd2_transport1emiss <- waste_2_4$transport1_emiss * waste_2_4$Rd2_tech
waste_2_4$Rd2_processemiss  <- waste_2_4$Prod_ww * (waste_2_4$Rd2.e.in * waste_2_4$Powergen_GHG + waste_2_4$Rd2.ng.in * waste_2_4$H2_GHG) * waste_2_4$Rd2_tech
waste_2_4$Rd2_transport2emiss <- (waste_2_4$Rd2_d / 42.79  + waste_2_4$Rd2_g / 41.74) * waste_2_4$Transport_km_2 * waste_2_4$Transport_Diesel * waste_2_4$Diesel_GHG 
waste_2_4$Rd2_enduseemiss <- waste_2_4$Prod_ww * waste_2_4$Nonbio_emiss2 * waste_2_4$Rd2_tech
waste_2_4$Rd2_dispemiss <- 0 - waste_2_4$Rd2_d *waste_2_4$Diesel_GHG - waste_2_4$Rd2_g* waste_2_4$Gasoline_GHG 
waste_2_4$Rd2_dispemissmain <- 0 - waste_2_4$Rd2_d *waste_2_4$Diesel_GHG 
waste_2_4$Rd2_dispemissco <- 0 - waste_2_4$Rd2_g* waste_2_4$Gasoline_GHG 
waste_2_4$Rd2_netemiss <- waste_2_4$Rd2_collectionemiss + waste_2_4$Rd2_transport1emiss + waste_2_4$Rd2_processemiss + waste_2_4$Rd2_transport2emiss + waste_2_4$Rd2_enduseemiss + waste_2_4$Rd2_dispemiss

#Bj1 
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_4$Bj1_d <- waste_2_4$Prod_ww * waste_2_4$Bj1.d.out
waste_2_4$Bj1_g <- waste_2_4$Prod_ww * waste_2_4$Bj1.g.out
waste_2_4$Bj1_j <- waste_2_4$Prod_ww * waste_2_4$Bj1.j.out
waste_2_4$Bj1_energy <- waste_2_4$Bj1_d + waste_2_4$Bj1_g + waste_2_4$Bj1_j 
waste_2_4$Bj1_energymain <- waste_2_4$Bj1_d
waste_2_4$Bj1_energyco <- waste_2_4$Bj1_g + waste_2_4$Bj1_j 
waste_2_4$Bj1_netenergy <-(waste_2_4$Bj1_energy - waste_2_4$Prod_ww * waste_2_4$Bj1.h2.in -
                             (waste_2_4$collection_en +  waste_2_4$transport1_en)  -
                             (waste_2_4$Bj1_d / 42.79 + waste_2_4$Bj1_g / 41.74 +waste_2_4$Bj1_j / 43.10) * waste_2_4$Transport_km_2 * waste_2_4$Transport_Diesel) * waste_2_4$Bj1_tech
waste_2_4$Bj1_collectionemiss <- waste_2_4$collection_emiss * waste_2_4$Bj1_tech
waste_2_4$Bj1_transport1emiss <- waste_2_4$transport1_emiss * waste_2_4$Bj1_tech
waste_2_4$Bj1_processemiss <- waste_2_4$Prod_ww * waste_2_4$Bj1.h2.in * waste_2_4$H2_GHG * waste_2_4$Bj1_tech
waste_2_4$Bj1_transport2emiss <- (waste_2_4$Bj1_d / 42.79 + waste_2_4$Bj1_g / 41.74 + waste_2_4$Bj1_j / 43.10) * waste_2_4$Transport_km_2 * waste_2_4$Transport_Diesel * waste_2_4$Diesel_GHG 
waste_2_4$Bj1_enduseemiss <- waste_2_4$Prod_ww * waste_2_4$Nonbio_emiss2 * waste_2_4$Bj1_tech
waste_2_4$Bj1_dispemiss <- 0 - waste_2_4$Bj1_d *waste_2_4$Diesel_GHG - waste_2_4$Bj1_g* waste_2_4$Gasoline_GHG -  waste_2_4$Bj1_j * waste_2_4$Jet_GHG 
waste_2_4$Bj1_dispemissmain <- 0 - waste_2_4$Bj1_d *waste_2_4$Diesel_GHG
waste_2_4$Bj1_dispemissco <- 0 - waste_2_4$Bj1_g* waste_2_4$Gasoline_GHG -  waste_2_4$Bj1_j * waste_2_4$Jet_GHG 
waste_2_4$Bj1_netemiss <- waste_2_4$Bj1_collectionemiss + waste_2_4$Bj1_transport1emiss + waste_2_4$Bj1_processemiss + waste_2_4$Bj1_transport2emiss + waste_2_4$Bj1_enduseemiss + waste_2_4$Bj1_dispemiss

#Bj2 
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_4$Bj2_energy <- waste_2_4$Prod_ww * waste_2_4$Bj2.j.out
waste_2_4$Bj2_energymain <- waste_2_4$Bj1_energy
waste_2_4$Bj2_netenergy <-(waste_2_4$Bj2_energy - waste_2_4$Prod_ww * waste_2_4$Bj2.h2.in -
                             (waste_2_4$collection_en +  waste_2_4$transport1_en)  -
                             waste_2_4$Bj2_energy / 43.10 * waste_2_4$Transport_km_2 * waste_2_4$Transport_Diesel ) * waste_2_4$Bj2_tech
waste_2_4$Bj2_collectionemiss <- waste_2_4$collection_emiss * waste_2_4$Bj2_tech
waste_2_4$Bj2_transport1emiss <- waste_2_4$transport1_emiss * waste_2_4$Bj2_tech
waste_2_4$Bj2_processemiss <- waste_2_4$Prod_ww * waste_2_4$Bj2.h2.in * waste_2_4$H2_GHG 
waste_2_4$Bj2_transport2emiss <- waste_2_4$Bj2_energy / 43.10 * waste_2_4$Transport_km_2 * waste_2_4$Transport_Diesel * waste_2_4$Diesel_GHG 
waste_2_4$Bj2_enduseemiss <- waste_2_4$Prod_ww * waste_2_4$Nonbio_emiss2 * waste_2_4$Bj2_tech
waste_2_4$Bj2_dispemiss <- 0 - waste_2_4$Bj2_energy * waste_2_4$Jet_GHG 
waste_2_4$Bj2_dispemissmain <- waste_2_4$Bj2_dispemiss
waste_2_4$Bj2_netemiss <- waste_2_4$Bj2_collectionemiss + waste_2_4$Bj2_transport1emiss + waste_2_4$Bj2_processemiss + waste_2_4$Bj2_transport2emiss + waste_2_4$Bj2_enduseemiss + waste_2_4$Bj2_dispemiss

#Bj3
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_4$Bj3_d <- waste_2_4$Prod_ww * waste_2_4$Bj3.d.out
waste_2_4$Bj3_g <- waste_2_4$Prod_ww * waste_2_4$Bj3.g.out
waste_2_4$Bj3_j <- waste_2_4$Prod_ww * waste_2_4$Bj3.j.out
waste_2_4$Bj3_energy <- waste_2_4$Bj3_d + waste_2_4$Bj3_g + waste_2_4$Bj3_j 
waste_2_4$Bj3_energymain <- waste_2_4$Bj3_j
waste_2_4$Bj3_energyco <- waste_2_4$Bj3_d + waste_2_4$Bj3_g 
waste_2_4$Bj3_netenergy <-(waste_2_4$Bj3_energy - waste_2_4$Prod_ww * waste_2_4$Bj3.e.in -
                             (waste_2_4$collection_en +  waste_2_4$transport1_en)  -
                             (waste_2_4$Bj3_d / 42.79 + waste_2_4$Bj3_g / 41.74 +waste_2_4$Bj3_j / 43.10) * waste_2_4$Transport_km_2 * waste_2_4$Transport_Diesel) * waste_2_4$Bj3_tech
waste_2_4$Bj3_collectionemiss <- waste_2_4$collection_emiss * waste_2_4$Bj3_tech
waste_2_4$Bj3_transport1emiss <- waste_2_4$transport1_emiss * waste_2_4$Bj3_tech
waste_2_4$Bj3_processemiss <- waste_2_4$Prod_ww * waste_2_4$Bj3.e.in * waste_2_4$Powergen_GHG * waste_2_4$Bj3_tech
waste_2_4$Bj3_transport2emiss <- (waste_2_4$Bj3_d / 42.79 + waste_2_4$Bj3_g / 41.74 + waste_2_4$Bj3_j / 43.10) * waste_2_4$Transport_km_2 * waste_2_4$Transport_Diesel * waste_2_4$Diesel_GHG
waste_2_4$Bj3_enduseemiss <- waste_2_4$Prod_ww * waste_2_4$Nonbio_emiss2 * waste_2_4$Bj3_tech
waste_2_4$Bj3_dispemiss <- 0 - waste_2_4$Bj3_d *waste_2_4$Diesel_GHG - waste_2_4$Bj3_g* waste_2_4$Gasoline_GHG -  waste_2_4$Bj3_j * waste_2_4$Jet_GHG 
waste_2_4$Bj3_dispemissmain <- 0 - waste_2_4$Bj3_j * waste_2_4$Jet_GHG
waste_2_4$Bj3_dispemissco <- 0 - waste_2_4$Bj3_d * waste_2_4$Diesel_GHG - waste_2_4$Bj3_g * waste_2_4$Gasoline_GHG
waste_2_4$Bj3_netemiss <- waste_2_4$Bj3_collectionemiss + waste_2_4$Bj3_transport1emiss + waste_2_4$Bj3_processemiss + waste_2_4$Bj3_transport2emiss + waste_2_4$Bj3_enduseemiss + waste_2_4$Bj3_dispemiss

#Bj4
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_4$Bj4_d <- waste_2_4$Prod_ww * waste_2_4$Bj4.d.out
waste_2_4$Bj4_g <- waste_2_4$Prod_ww * waste_2_4$Bj4.g.out
waste_2_4$Bj4_j <- waste_2_4$Prod_ww * waste_2_4$Bj4.j.out
waste_2_4$Bj4_energy <- waste_2_4$Bj4_d + waste_2_4$Bj4_g + waste_2_4$Bj4_j 
waste_2_4$Bj4_energymain <- waste_2_4$Bj4_j
waste_2_4$Bj4_energyco <- waste_2_4$Bj4_d + waste_2_4$Bj4_g 
waste_2_4$Bj4_netenergy <-(waste_2_4$Bj4_energy - waste_2_4$Prod_ww * waste_2_4$Bj4.h2.in -
                             (waste_2_4$collection_en +  waste_2_4$transport1_en)  -
                             (waste_2_4$Bj4_d / 42.79 + waste_2_4$Bj4_g / 41.74 +waste_2_4$Bj4_j / 43.10) * waste_2_4$Transport_km_2 * waste_2_4$Transport_Diesel) * waste_2_4$Bj4_tech
waste_2_4$Bj4_collectionemiss <- waste_2_4$collection_emiss * waste_2_4$Bj4_tech
waste_2_4$Bj4_transport1emiss <- waste_2_4$transport1_emiss * waste_2_4$Bj4_tech
waste_2_4$Bj4_processemiss <- waste_2_4$Prod_ww * waste_2_4$Bj4.h2.in * waste_2_4$H2_GHG * waste_2_4$Bj4_tech
waste_2_4$Bj4_transport2emiss <- (waste_2_4$Bj4_d / 42.79 + waste_2_4$Bj4_g / 41.74 + waste_2_4$Bj4_j / 43.10) * waste_2_4$Transport_km_2 * waste_2_4$Transport_Diesel * waste_2_4$Diesel_GHG 
waste_2_4$Bj4_enduseemiss <- waste_2_4$Prod_ww * waste_2_4$Nonbio_emiss2 * waste_2_4$Bj4_tech
waste_2_4$Bj4_dispemiss <- 0 - waste_2_4$Bj4_d *waste_2_4$Diesel_GHG - waste_2_4$Bj4_g* waste_2_4$Gasoline_GHG -  waste_2_4$Bj4_j * waste_2_4$Jet_GHG 
waste_2_4$Bj4_dispemissmain <- 0 - waste_2_4$Bj4_j * waste_2_4$Jet_GHG
waste_2_4$Bj4_dispemissco <- 0 - waste_2_4$Bj4_d * waste_2_4$Diesel_GHG - waste_2_4$Bj4_g * waste_2_4$Gasoline_GHG
waste_2_4$Bj4_netemiss <- waste_2_4$Bj4_collectionemiss + waste_2_4$Bj4_transport1emiss + waste_2_4$Bj4_processemiss + waste_2_4$Bj4_transport2emiss+ waste_2_4$Bj4_enduseemiss + waste_2_4$Bj4_dispemiss

#Bj5
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_4$Bj5_d <- waste_2_4$Prod_ww * waste_2_4$Bj5.d.out
waste_2_4$Bj5_g <- waste_2_4$Prod_ww * waste_2_4$Bj5.g.out
waste_2_4$Bj5_j <- waste_2_4$Prod_ww * waste_2_4$Bj5.j.out
waste_2_4$Bj5_energy <- waste_2_4$Bj5_d + waste_2_4$Bj5_g + waste_2_4$Bj5_j 
waste_2_4$Bj5_energymain <- waste_2_4$Bj5_j
waste_2_4$Bj5_energyco <- waste_2_4$Bj5_d + waste_2_4$Bj5_g 
waste_2_4$Bj5_netenergy <-(waste_2_4$Bj5_energy - waste_2_4$Prod_ww * waste_2_4$Bj5.e.in -
                             (waste_2_4$collection_en +  waste_2_4$transport1_en)  -
                             (waste_2_4$Bj5_d / 42.79 + waste_2_4$Bj5_g / 41.74 +waste_2_4$Bj5_j / 43.10) * waste_2_4$Transport_km_2 * waste_2_4$Transport_Diesel) * waste_2_4$Bj5_tech
waste_2_4$Bj5_collectionemiss <- waste_2_4$collection_emiss * waste_2_4$Bj5_tech
waste_2_4$Bj5_transport1emiss <- waste_2_4$transport1_emiss * waste_2_4$Bj5_tech
waste_2_4$Bj5_processemiss <- waste_2_4$Prod_ww * waste_2_4$Bj5.e.in * waste_2_4$Powergen_GHG * waste_2_4$Bj5_tech
waste_2_4$Bj5_transport2emiss <-  (waste_2_4$Bj5_d / 42.79 + waste_2_4$Bj5_g / 41.74 + waste_2_4$Bj5_j / 43.10) * waste_2_4$Transport_km_2 * waste_2_4$Transport_Diesel * waste_2_4$Diesel_GHG 
waste_2_4$Bj5_enduseemiss <- waste_2_4$Prod_ww * waste_2_4$Nonbio_emiss2 * waste_2_4$Bj5_tech
waste_2_4$Bj5_dispemiss <- 0 - waste_2_4$Bj5_d *waste_2_4$Diesel_GHG - waste_2_4$Bj5_g* waste_2_4$Gasoline_GHG -  waste_2_4$Bj5_j * waste_2_4$Jet_GHG 
waste_2_4$Bj5_dispemissmain <- 0 - waste_2_4$Bj5_j * waste_2_4$Jet_GHG
waste_2_4$Bj5_dispemissco <- 0 - waste_2_4$Bj5_d * waste_2_4$Diesel_GHG - waste_2_4$Bj5_g * waste_2_4$Gasoline_GHG
waste_2_4$Bj5_netemiss <- waste_2_4$Bj5_collectionemiss + waste_2_4$Bj5_transport1emiss + waste_2_4$Bj5_processemiss + waste_2_4$Bj5_transport2emiss + waste_2_4$Bj5_enduseemiss + waste_2_4$Bj5_dispemiss

#Bj6
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_4$Bj6_d <- waste_2_4$Prod_ww * waste_2_4$Bj6.d.out
waste_2_4$Bj6_g <- waste_2_4$Prod_ww * waste_2_4$Bj6.g.out
waste_2_4$Bj6_j <- waste_2_4$Prod_ww * waste_2_4$Bj6.j.out
waste_2_4$Bj6_energy <- waste_2_4$Bj6_d + waste_2_4$Bj6_g + waste_2_4$Bj6_j 
waste_2_4$Bj6_energymain <- waste_2_4$Bj6_j
waste_2_4$Bj6_energyco <- waste_2_4$Bj6_d + waste_2_4$Bj6_g 
waste_2_4$Bj6_netenergy <-(waste_2_4$Bj6_energy - waste_2_4$Prod_ww * waste_2_4$Bj6.h2.in - waste_2_4$Prod_ww * waste_2_4$Bj6.e.in -
                             (waste_2_4$collection_en +  waste_2_4$transport1_en)  -
                             (waste_2_4$Bj6_d / 42.79 + waste_2_4$Bj6_g / 41.74 +waste_2_4$Bj6_j / 43.10) * waste_2_4$Transport_km_2 * waste_2_4$Transport_Diesel) * waste_2_4$Bj6_tech
waste_2_4$Bj6_collectionemiss <- waste_2_4$collection_emiss * waste_2_4$Bj6_tech
waste_2_4$Bj6_transport1emiss <- waste_2_4$transport1_emiss * waste_2_4$Bj6_tech
waste_2_4$Bj6_processemiss <- waste_2_4$Prod_ww * (waste_2_4$Bj6.h2.in * waste_2_4$H2_GHG + waste_2_4$Bj6.e.in * waste_2_4$Powergen_GHG) * waste_2_4$Bj6_tech
waste_2_4$Bj6_transport2emiss <-  (waste_2_4$Bj6_d / 42.79 + waste_2_4$Bj6_g / 41.74 + waste_2_4$Bj6_j / 43.10) * waste_2_4$Transport_km_2 * waste_2_4$Transport_Diesel * waste_2_4$Diesel_GHG
waste_2_4$Bj6_enduseemiss <- waste_2_4$Prod_ww * waste_2_4$Nonbio_emiss2 * waste_2_4$Bj6_tech
waste_2_4$Bj6_dispemiss <- 0 - waste_2_4$Bj6_d *waste_2_4$Diesel_GHG - waste_2_4$Bj6_g* waste_2_4$Gasoline_GHG -  waste_2_4$Bj6_j * waste_2_4$Jet_GHG 
waste_2_4$Bj6_dispemissmain <- 0 - waste_2_4$Bj6_j * waste_2_4$Jet_GHG
waste_2_4$Bj6_dispemissco <- 0 - waste_2_4$Bj6_d * waste_2_4$Diesel_GHG - waste_2_4$Bj6_g * waste_2_4$Gasoline_GHG
waste_2_4$Bj6_netemiss <- waste_2_4$Bj6_collectionemiss + waste_2_4$Bj6_transport1emiss + waste_2_4$Bj6_processemiss + waste_2_4$Bj6_transport2emiss + waste_2_4$Bj6_enduseemiss + waste_2_4$Bj6_dispemiss


# TD = 100km
coefficients3 <- merge(coefficients3, tech)
waste_1_3 <- merge(waste, coefficients3)
waste_2_5 <- merge(waste_1_3, state_GHG_ef)

#Collection & Transportation_3 (energy - GJ, emiss - kg CO1e)
waste_2_5$Prod_ww <- waste_2_5$Prod/(1-waste_2_5$MC/100)
waste_2_5$collection_en <- waste_2_5$Prod_ww * waste_2_5$Collection_Diesel
waste_2_5$collection_emiss <- waste_2_5$collection_en * waste_2_5$Diesel_GHG 
waste_2_5$transport1_en <- waste_2_5$Prod_ww * waste_2_5$Transport_km_1 * waste_2_5$Transport_Diesel
waste_2_5$transport1_emiss <- waste_2_5$transport1_en  * waste_2_5$Diesel_GHG 

#E1
#electricity T&D loss - 6.5%, heat loss - 20%
waste_2_5$E1_elec <- waste_2_5$Prod_ww * waste_2_5$E1.e.out * (1-0.065)
waste_2_5$E1_heat <- waste_2_5$Prod_ww * waste_2_5$E1.h.out * (1-0.2)
waste_2_5$E1_energy <- waste_2_5$E1_elec + waste_2_5$E1_heat
waste_2_5$E1_energymain <- waste_2_5$E1_elec
waste_2_5$E1_energyco <- waste_2_5$E1_heat
waste_2_5$E1_netenergy <- (waste_2_5$E1_energy - waste_2_5$Prod_ww * (waste_2_5$E1.e.in + waste_2_5$E1.h.in + waste_2_5$E1.d.in) - waste_2_5$collection_en -  waste_2_5$transport1_en) * waste_2_5$E1_tech
waste_2_5$E1_collectionemiss <- waste_2_5$collection_emiss * waste_2_5$E1_tech
waste_2_5$E1_transport1emiss <- waste_2_5$transport1_emiss * waste_2_5$E1_tech
waste_2_5$E1_processemiss <- waste_2_5$Prod_ww * (waste_2_5$E1.e.in * waste_2_5$Powergen_GHG 
                                                  + waste_2_5$E1.h.in * waste_2_5$Heatgen_GHG 
                                                  + waste_2_5$E1.d.in * waste_2_5$Diesel_GHG
                                                  + waste_2_5$Nonbio_emiss1) * waste_2_5$E1_tech
waste_2_5$E1_enduseemiss <- 0
waste_2_5$E1_dispemiss <- 0 - waste_2_5$E1_elec* waste_2_5$Powergen_GHG  - waste_2_5$E1_heat* waste_2_5$Heatgen_GHG 
waste_2_5$E1_dispemissmain <- 0 - waste_2_5$E1_elec* waste_2_5$Powergen_GHG
waste_2_5$E1_dispemissco <- 0 - waste_2_5$E1_heat* waste_2_5$Heatgen_GHG
waste_2_5$E1_netemiss <- waste_2_5$E1_collectionemiss + waste_2_5$E1_transport1emiss + waste_2_5$E1_processemiss + waste_2_5$E1_enduseemiss + waste_2_5$E1_dispemiss

#E2
#electricity T&D loss - 6.5%, heat loss - 20%
waste_2_5$E2_elec <- waste_2_5$Prod_ww * waste_2_5$E2.e.out * (1-0.065)
waste_2_5$E2_heat <- waste_2_5$Prod_ww * waste_2_5$E2.h.out * (1-0.2)
waste_2_5$E2_energy <- waste_2_5$E2_elec + waste_2_5$E2_heat
waste_2_5$E2_energymain <- waste_2_5$E2_elec
waste_2_5$E2_energyco <- waste_2_5$E2_heat
waste_2_5$E2_netenergy <- (waste_2_5$E2_energy - waste_2_5$Prod_ww * (waste_2_5$E2.e.in + waste_2_5$E2.h.in + waste_2_5$E2.d.in) - waste_2_5$collection_en - waste_2_5$transport1_en) * waste_2_5$E2_tech
waste_2_5$E2_collectionemiss <- waste_2_5$collection_emiss * waste_2_5$E2_tech
waste_2_5$E2_transport1emiss <- waste_2_5$transport1_emiss * waste_2_5$E2_tech
waste_2_5$E2_processemiss <- waste_2_5$Prod_ww * (waste_2_5$E2.e.in * waste_2_5$Powergen_GHG 
                                                  + waste_2_5$E2.h.in * waste_2_5$Heatgen_GHG
                                                  + waste_2_5$E2.d.in * waste_2_5$Diesel_GHG
                                                  + waste_2_5$Nonbio_emiss1)  * waste_2_5$E2_tech 
waste_2_5$E2_enduseemiss <- 0
waste_2_5$E2_dispemiss <- 0 - waste_2_5$E2_elec* waste_2_5$Powergen_GHG - waste_2_5$E2_heat* waste_2_5$Heatgen_GHG
waste_2_5$E2_dispemissmain <- 0 - waste_2_5$E2_elec* waste_2_5$Powergen_GHG
waste_2_5$E2_dispemissco <- 0 - waste_2_5$E2_heat* waste_2_5$Heatgen_GHG
waste_2_5$E2_netemiss <- waste_2_5$E2_collectionemiss + waste_2_5$E2_transport1emiss + waste_2_5$E2_processemiss + waste_2_5$E2_enduseemiss + waste_2_5$E2_dispemiss

#E3
#electricity T&D loss - 6.5%
waste_2_5$E3_energy <- waste_2_5$Prod_ww * waste_2_5$E3.e.out * (1-0.065)
waste_2_5$E3_energymain <- waste_2_5$E3_energy
waste_2_5$E3_netenergy <- (waste_2_5$E3_energy - waste_2_5$Prod_ww * (waste_2_5$E3.e.in + waste_2_5$E3.h.in + waste_2_5$E3.d.in) - waste_2_5$collection_en -  waste_2_5$transport1_en) * waste_2_5$E3_tech
waste_2_5$E3_collectionemiss <- waste_2_5$collection_emiss * waste_2_5$E3_tech
waste_2_5$E3_transport1emiss <- waste_2_5$transport1_emiss * waste_2_5$E3_tech
waste_2_5$E3_processemiss <- waste_2_5$Prod_ww * (waste_2_5$E3.e.in * waste_2_5$Powergen_GHG 
                                                  + waste_2_5$E3.h.in * waste_2_5$Heatgen_GHG
                                                  + waste_2_5$E3.d.in * waste_2_5$Diesel_GHG
                                                  + waste_2_5$Nonbio_emiss1)  * waste_2_5$E3_tech
waste_2_5$E3_enduseemiss <- 0
waste_2_5$E3_dispemiss <- 0 - waste_2_5$E3_energy* waste_2_5$Powergen_GHG
waste_2_5$E3_dispemissmain <- 0 - waste_2_5$E3_energy* waste_2_5$Powergen_GHG
waste_2_5$E3_netemiss <- waste_2_5$E3_collectionemiss + waste_2_5$E3_transport1emiss + waste_2_5$E3_processemiss + waste_2_5$E3_enduseemiss + waste_2_5$E3_dispemiss

#E4
#electricity T&D loss - 6.5%, heat loss - 20%
waste_2_5$E4_elec <- waste_2_5$Prod_ww * waste_2_5$E4.e.out * (1-0.065)
waste_2_5$E4_heat <- waste_2_5$Prod_ww * waste_2_5$E4.h.out * (1-0.2)
waste_2_5$E4_energy <- waste_2_5$E4_elec + waste_2_5$E4_heat
waste_2_5$E4_energymain <- waste_2_5$E4_elec
waste_2_5$E4_energyco <- waste_2_5$E4_heat
waste_2_5$E4_netenergy <- (waste_2_5$E4_energy - waste_2_5$Prod_ww * (waste_2_5$E4.ng.in + waste_2_5$E4.d.in) - waste_2_5$collection_en - waste_2_5$transport1_en) * waste_2_5$E4_tech
waste_2_5$E4_collectionemiss <- waste_2_5$collection_emiss * waste_2_5$E4_tech
waste_2_5$E4_transport1emiss <- waste_2_5$transport1_emiss * waste_2_5$E4_tech
waste_2_5$E4_processemiss <- waste_2_5$Prod_ww * (waste_2_5$E4.ng.in * waste_2_5$NG_GHG 
                                                  + waste_2_5$E4.d.in * waste_2_5$Diesel_GHG
                                                  + waste_2_5$Nonbio_emiss1) * waste_2_5$E4_tech 
waste_2_5$E4_enduseemiss <- 0
waste_2_5$E4_dispemiss <- 0 - waste_2_5$E4_elec* waste_2_5$Powergen_GHG - waste_2_5$E4_heat* waste_2_5$Heatgen_GHG
waste_2_5$E4_dispemissmain <- 0 - waste_2_5$E4_elec* waste_2_5$Powergen_GHG
waste_2_5$E4_dispemissco <- 0 - waste_2_5$E4_heat* waste_2_5$Heatgen_GHG
waste_2_5$E4_netemiss <- waste_2_5$E4_collectionemiss + waste_2_5$E4_transport1emiss + waste_2_5$E4_processemiss + waste_2_5$E4_enduseemiss + waste_2_5$E4_dispemiss

#M1
#methane leakage - 2%
waste_2_5$M1_energy <- waste_2_5$Prod_ww * waste_2_5$M1.m.out * (1-0.02)
waste_2_5$M1_energymain <- waste_2_5$M1_energy
waste_2_5$M1_netenergy <- (waste_2_5$M1_energy - waste_2_5$Prod_ww * (waste_2_5$M1.e.in + waste_2_5$M1.h.in) - waste_2_5$collection_en - waste_2_5$transport1_en) * waste_2_5$M1_tech
waste_2_5$M1_collectionemiss <- waste_2_5$collection_emiss * waste_2_5$M1_tech
waste_2_5$M1_transport1emiss <- waste_2_5$transport1_emiss * waste_2_5$M1_tech
waste_2_5$M1_processemiss <- waste_2_5$Prod_ww * (waste_2_5$M1.e.in * waste_2_5$Powergen_GHG + waste_2_5$M1.h.in * waste_2_5$Heatgen_GHG)* waste_2_5$M1_tech
waste_2_5$M1_transport2emiss <- waste_2_5$Prod_ww * waste_2_5$M1.m.out *0.02 /50 *28 * waste_2_5$M1_tech
waste_2_5$M1_enduseemiss <- waste_2_5$Prod_ww * waste_2_5$Nonbio_emiss2  * waste_2_5$M1_tech
waste_2_5$M1_dispemiss <- 0 - waste_2_5$M1_energy* waste_2_5$NG_GHG
waste_2_5$M1_dispemissmain <- waste_2_5$M1_dispemiss
waste_2_5$M1_netemiss <- waste_2_5$M1_collectionemiss + waste_2_5$M1_transport1emiss + waste_2_5$M1_processemiss + waste_2_5$M1_transport2emiss + waste_2_5$M1_enduseemiss + waste_2_5$M1_dispemiss

#M2
#methane leakage - 2%
waste_2_5$M2_energy <- waste_2_5$Prod_ww * waste_2_5$M2.m.out * (1-0.02)
waste_2_5$M2_energymain <- waste_2_5$M2_energy
waste_2_5$M2_netenergy <- (waste_2_5$M2_energy - waste_2_5$Prod_ww * (waste_2_5$M2.e.in + waste_2_5$M2.h.in + waste_2_5$M2.d.in) - waste_2_5$collection_en -  waste_2_5$transport1_en) * waste_2_5$M2_tech
waste_2_5$M2_collectionemiss <- waste_2_5$collection_emiss * waste_2_5$M2_tech
waste_2_5$M2_transport1emiss <- waste_2_5$transport1_emiss * waste_2_5$M2_tech
waste_2_5$M2_processemiss <- waste_2_5$Prod_ww * (waste_2_5$M2.e.in * waste_2_5$Powergen_GHG + 
                                                    waste_2_5$M2.h.in * waste_2_5$Heatgen_GHG +
                                                    waste_2_5$M2.d.in * waste_2_5$Diesel_GHG) * waste_2_5$M2_tech
waste_2_5$M2_transport2emiss <- waste_2_5$Prod_ww * waste_2_5$M2.m.out *0.02 /50 *28 * waste_2_5$M1_tech
waste_2_5$M2_enduseemiss <- waste_2_5$Prod_ww * waste_2_5$Nonbio_emiss2 * waste_2_5$M2_tech
waste_2_5$M2_dispemiss <- 0 - waste_2_5$M2_energy* waste_2_5$NG_GHG
waste_2_5$M2_dispemissmain <- waste_2_5$M2_dispemiss
waste_2_5$M2_netemiss <- waste_2_5$M2_collectionemiss + waste_2_5$M2_transport1emiss + waste_2_5$M2_processemiss + waste_2_5$M2_transport2emiss + waste_2_5$M2_enduseemiss + waste_2_5$M2_dispemiss

#Eth1
#energy intensity of ethanol - 26.95 MJ/kg
waste_2_5$Eth1_elec <- waste_2_5$Prod_ww * waste_2_5$Eth1.e.out * (1-0.065)
waste_2_5$Eth1_eth <- waste_2_5$Prod_ww * waste_2_5$Eth1.eth.out
waste_2_5$Eth1_energy <- waste_2_5$Eth1_elec + waste_2_5$Eth1_eth
waste_2_5$Eth1_energymain <- waste_2_5$Eth1_eth
waste_2_5$Eth1_energyco <- waste_2_5$Eth1_elec
waste_2_5$Eth1_netenergy <- (waste_2_5$Eth1_energy - waste_2_5$Prod_ww * (waste_2_5$Eth1.ng.in  + waste_2_5$Eth1.d.in) -
                               (waste_2_5$collection_en +  waste_2_5$transport1_en) * waste_2_5$M1_tech -
                               waste_2_5$Eth1_eth / 26.95 * waste_2_5$Transport_km_2 * waste_2_5$Transport_Diesel) * waste_2_5$Eth1_tech
waste_2_5$Eth1_collectionemiss <- waste_2_5$collection_emiss * waste_2_5$Eth1_tech
waste_2_5$Eth1_transport1emiss <- waste_2_5$transport1_emiss * waste_2_5$Eth1_tech
waste_2_5$Eth1_processemiss <- waste_2_5$Prod_ww * (waste_2_5$Eth1.ng.in * waste_2_5$Heatgen_GHG + waste_2_5$Eth1.d.in * waste_2_5$Diesel_GHG) * waste_2_5$Eth1_tech 
waste_2_5$Eth1_transport2emiss <- waste_2_5$Eth1_eth / 26.95 * waste_2_5$Transport_km_2 * waste_2_5$Transport_Diesel * waste_2_5$Diesel_GHG 
waste_2_5$Eth1_enduseemiss <- waste_2_5$Prod_ww * waste_2_5$Nonbio_emiss2 * waste_2_5$Eth1_tech
waste_2_5$Eth1_dispemiss <- 0 - waste_2_5$Eth1_eth* waste_2_5$Gasoline_GHG  - waste_2_5$Eth1_elec * waste_2_5$Powergen_GHG
waste_2_5$Eth1_dispemissmain <- 0 - waste_2_5$Eth1_eth* waste_2_5$Gasoline_GHG
waste_2_5$Eth1_dispemissco <- 0 - waste_2_5$Eth1_elec * waste_2_5$Powergen_GHG
waste_2_5$Eth1_netemiss <- waste_2_5$Eth1_collectionemiss + waste_2_5$Eth1_transport1emiss + waste_2_5$Eth1_processemiss + waste_2_5$Eth1_transport2emiss + waste_2_5$Eth1_enduseemiss + waste_2_5$Eth1_dispemiss

#Rd1
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
#electricity T&D loss - 6.5%, methane leakage - 2%
waste_2_5$Rd1_d <- waste_2_5$Prod_ww * waste_2_5$Rd1.d.out
waste_2_5$Rd1_g <- waste_2_5$Prod_ww * waste_2_5$Rd1.g.out
waste_2_5$Rd1_j <- waste_2_5$Prod_ww * waste_2_5$Rd1.j.out
waste_2_5$Rd1_m <- waste_2_5$Prod_ww * waste_2_5$Rd1.m.out * (1-0.02)
waste_2_5$Rd1_elec <- waste_2_5$Prod_ww * waste_2_5$Rd1.e.out * (1-0.065)
waste_2_5$Rd1_energy <- waste_2_5$Rd1_d + waste_2_5$Rd1_g + waste_2_5$Rd1_j + waste_2_5$Rd1_m + waste_2_5$Rd1_elec
waste_2_5$Rd1_energymain <- waste_2_5$Rd1_d
waste_2_5$Rd1_energyco <- waste_2_5$Rd1_g + waste_2_5$Rd1_j + waste_2_5$Rd1_m + waste_2_5$Rd1_elec
waste_2_5$Rd1_netenergy <-(waste_2_5$Rd1_energy - waste_2_5$Prod_ww * waste_2_5$Rd1.e.in -
                             (waste_2_5$collection_en +  waste_2_5$transport1_en)  -
                             waste_2_5$Rd1_d / 42.79 * waste_2_5$Transport_km_2 * waste_2_5$Transport_Diesel -
                             waste_2_5$Rd1_g / 41.74 * waste_2_5$Transport_km_2 * waste_2_5$Transport_Diesel -
                             waste_2_5$Rd1_j / 43.10 * waste_2_5$Transport_km_2 * waste_2_5$Transport_Diesel ) * waste_2_5$Rd1_tech
waste_2_5$Rd1_collectionemiss <- waste_2_5$collection_emiss * waste_2_5$Rd1_tech
waste_2_5$Rd1_transport1emiss <- waste_2_5$transport1_emiss * waste_2_5$Rd1_tech
waste_2_5$Rd1_processemiss <- waste_2_5$Prod_ww * waste_2_5$Rd1.e.in * waste_2_5$Powergen_GHG * waste_2_5$Rd1_tech
waste_2_5$Rd1_transport2emiss <- (waste_2_5$Rd1_d / 42.79 + waste_2_5$Rd1_g / 41.74 + waste_2_5$Rd1_j / 43.10) * waste_2_5$Transport_km_2 * waste_2_5$Transport_Diesel * waste_2_5$Diesel_GHG  
waste_2_5$Rd1_enduseemiss <- waste_2_5$Prod_ww * waste_2_5$Nonbio_emiss2 * waste_2_5$Rd1_tech
waste_2_5$Rd1_dispemiss <- 0 - waste_2_5$Rd1_d *waste_2_5$Diesel_GHG - waste_2_5$Rd1_g * waste_2_5$Gasoline_GHG -  waste_2_5$Rd1_j * waste_2_5$Jet_GHG -
  waste_2_5$Rd1_m * waste_2_5$NG_GHG - waste_2_5$Rd1_elec * waste_2_5$Powergen_GHG 
waste_2_5$Rd1_dispemissmain <- 0 - waste_2_5$Rd1_d *waste_2_5$Diesel_GHG
waste_2_5$Rd1_dispemissco <- 0 - waste_2_5$Rd1_g * waste_2_5$Gasoline_GHG -  waste_2_5$Rd1_j * waste_2_5$Jet_GHG -
  waste_2_5$Rd1_m * waste_2_5$NG_GHG - waste_2_5$Rd1_elec * waste_2_5$Powergen_GHG 
waste_2_5$Rd1_netemiss <- waste_2_5$Rd1_collectionemiss + waste_2_5$Rd1_transport1emiss + waste_2_5$Rd1_processemiss + waste_2_5$Rd1_enduseemiss + waste_2_5$Rd1_transport2emiss + waste_2_5$Rd1_dispemiss


#Rd2
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74
waste_2_5$Rd2_d <- waste_2_5$Prod_ww * waste_2_5$Rd2.d.out
waste_2_5$Rd2_g <- waste_2_5$Prod_ww * waste_2_5$Rd2.g.out
waste_2_5$Rd2_energy <- waste_2_5$Rd2_d + waste_2_5$Rd2_g
waste_2_5$Rd2_energymain <- waste_2_5$Rd2_d
waste_2_5$Rd2_energyco <- waste_2_5$Rd2_g
waste_2_5$Rd2_netenergy <-(waste_2_5$Rd2_energy - waste_2_5$Prod_ww * (waste_2_5$Rd2.e.in + waste_2_5$Rd2.ng.in) -
                             (waste_2_5$collection_en +  waste_2_5$transport1_en)  -
                             waste_2_5$Rd2_d / 42.79 * waste_2_5$Transport_km_2 * waste_2_5$Transport_Diesel -
                             waste_2_5$Rd2_g / 41.74 * waste_2_5$Transport_km_2 * waste_2_5$Transport_Diesel ) * waste_2_5$Rd2_tech
waste_2_5$Rd2_collectionemiss <- waste_2_5$collection_emiss * waste_2_5$Rd2_tech
waste_2_5$Rd2_transport1emiss <- waste_2_5$transport1_emiss * waste_2_5$Rd2_tech
waste_2_5$Rd2_processemiss  <- waste_2_5$Prod_ww * (waste_2_5$Rd2.e.in * waste_2_5$Powergen_GHG + waste_2_5$Rd2.ng.in * waste_2_5$H2_GHG) * waste_2_5$Rd2_tech
waste_2_5$Rd2_transport2emiss <- (waste_2_5$Rd2_d / 42.79  + waste_2_5$Rd2_g / 41.74) * waste_2_5$Transport_km_2 * waste_2_5$Transport_Diesel * waste_2_5$Diesel_GHG 
waste_2_5$Rd2_enduseemiss <- waste_2_5$Prod_ww * waste_2_5$Nonbio_emiss2 * waste_2_5$Rd2_tech
waste_2_5$Rd2_dispemiss <- 0 - waste_2_5$Rd2_d *waste_2_5$Diesel_GHG - waste_2_5$Rd2_g* waste_2_5$Gasoline_GHG 
waste_2_5$Rd2_dispemissmain <- 0 - waste_2_5$Rd2_d *waste_2_5$Diesel_GHG 
waste_2_5$Rd2_dispemissco <- 0 - waste_2_5$Rd2_g* waste_2_5$Gasoline_GHG 
waste_2_5$Rd2_netemiss <- waste_2_5$Rd2_collectionemiss + waste_2_5$Rd2_transport1emiss + waste_2_5$Rd2_processemiss + waste_2_5$Rd2_transport2emiss + waste_2_5$Rd2_enduseemiss + waste_2_5$Rd2_dispemiss

#Bj1 
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_5$Bj1_d <- waste_2_5$Prod_ww * waste_2_5$Bj1.d.out
waste_2_5$Bj1_g <- waste_2_5$Prod_ww * waste_2_5$Bj1.g.out
waste_2_5$Bj1_j <- waste_2_5$Prod_ww * waste_2_5$Bj1.j.out
waste_2_5$Bj1_energy <- waste_2_5$Bj1_d + waste_2_5$Bj1_g + waste_2_5$Bj1_j 
waste_2_5$Bj1_energymain <- waste_2_5$Bj1_d
waste_2_5$Bj1_energyco <- waste_2_5$Bj1_g + waste_2_5$Bj1_j 
waste_2_5$Bj1_netenergy <-(waste_2_5$Bj1_energy - waste_2_5$Prod_ww * waste_2_5$Bj1.h2.in -
                             (waste_2_5$collection_en +  waste_2_5$transport1_en)  -
                             (waste_2_5$Bj1_d / 42.79 + waste_2_5$Bj1_g / 41.74 +waste_2_5$Bj1_j / 43.10) * waste_2_5$Transport_km_2 * waste_2_5$Transport_Diesel) * waste_2_5$Bj1_tech
waste_2_5$Bj1_collectionemiss <- waste_2_5$collection_emiss * waste_2_5$Bj1_tech
waste_2_5$Bj1_transport1emiss <- waste_2_5$transport1_emiss * waste_2_5$Bj1_tech
waste_2_5$Bj1_processemiss <- waste_2_5$Prod_ww * waste_2_5$Bj1.h2.in * waste_2_5$H2_GHG * waste_2_5$Bj1_tech
waste_2_5$Bj1_transport2emiss <- (waste_2_5$Bj1_d / 42.79 + waste_2_5$Bj1_g / 41.74 + waste_2_5$Bj1_j / 43.10) * waste_2_5$Transport_km_2 * waste_2_5$Transport_Diesel * waste_2_5$Diesel_GHG 
waste_2_5$Bj1_enduseemiss <- waste_2_5$Prod_ww * waste_2_5$Nonbio_emiss2 * waste_2_5$Bj1_tech
waste_2_5$Bj1_dispemiss <- 0 - waste_2_5$Bj1_d *waste_2_5$Diesel_GHG - waste_2_5$Bj1_g* waste_2_5$Gasoline_GHG -  waste_2_5$Bj1_j * waste_2_5$Jet_GHG 
waste_2_5$Bj1_dispemissmain <- 0 - waste_2_5$Bj1_d *waste_2_5$Diesel_GHG
waste_2_5$Bj1_dispemissco <- 0 - waste_2_5$Bj1_g* waste_2_5$Gasoline_GHG -  waste_2_5$Bj1_j * waste_2_5$Jet_GHG 
waste_2_5$Bj1_netemiss <- waste_2_5$Bj1_collectionemiss + waste_2_5$Bj1_transport1emiss + waste_2_5$Bj1_processemiss + waste_2_5$Bj1_transport2emiss + waste_2_5$Bj1_enduseemiss + waste_2_5$Bj1_dispemiss

#Bj2 
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_5$Bj2_energy <- waste_2_5$Prod_ww * waste_2_5$Bj2.j.out
waste_2_5$Bj2_energymain <- waste_2_5$Bj1_energy
waste_2_5$Bj2_netenergy <-(waste_2_5$Bj2_energy - waste_2_5$Prod_ww * waste_2_5$Bj2.h2.in -
                             (waste_2_5$collection_en +  waste_2_5$transport1_en)  -
                             waste_2_5$Bj2_energy / 43.10 * waste_2_5$Transport_km_2 * waste_2_5$Transport_Diesel ) * waste_2_5$Bj2_tech
waste_2_5$Bj2_collectionemiss <- waste_2_5$collection_emiss * waste_2_5$Bj2_tech
waste_2_5$Bj2_transport1emiss <- waste_2_5$transport1_emiss * waste_2_5$Bj2_tech
waste_2_5$Bj2_processemiss <- waste_2_5$Prod_ww * waste_2_5$Bj2.h2.in * waste_2_5$H2_GHG 
waste_2_5$Bj2_transport2emiss <- waste_2_5$Bj2_energy / 43.10 * waste_2_5$Transport_km_2 * waste_2_5$Transport_Diesel * waste_2_5$Diesel_GHG 
waste_2_5$Bj2_enduseemiss <- waste_2_5$Prod_ww * waste_2_5$Nonbio_emiss2 * waste_2_5$Bj2_tech
waste_2_5$Bj2_dispemiss <- 0 - waste_2_5$Bj2_energy * waste_2_5$Jet_GHG 
waste_2_5$Bj2_dispemissmain <- waste_2_5$Bj2_dispemiss
waste_2_5$Bj2_netemiss <- waste_2_5$Bj2_collectionemiss + waste_2_5$Bj2_transport1emiss + waste_2_5$Bj2_processemiss + waste_2_5$Bj2_transport2emiss + waste_2_5$Bj2_enduseemiss + waste_2_5$Bj2_dispemiss

#Bj3
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_5$Bj3_d <- waste_2_5$Prod_ww * waste_2_5$Bj3.d.out
waste_2_5$Bj3_g <- waste_2_5$Prod_ww * waste_2_5$Bj3.g.out
waste_2_5$Bj3_j <- waste_2_5$Prod_ww * waste_2_5$Bj3.j.out
waste_2_5$Bj3_energy <- waste_2_5$Bj3_d + waste_2_5$Bj3_g + waste_2_5$Bj3_j 
waste_2_5$Bj3_energymain <- waste_2_5$Bj3_j
waste_2_5$Bj3_energyco <- waste_2_5$Bj3_d + waste_2_5$Bj3_g 
waste_2_5$Bj3_netenergy <-(waste_2_5$Bj3_energy - waste_2_5$Prod_ww * waste_2_5$Bj3.e.in -
                             (waste_2_5$collection_en +  waste_2_5$transport1_en)  -
                             (waste_2_5$Bj3_d / 42.79 + waste_2_5$Bj3_g / 41.74 +waste_2_5$Bj3_j / 43.10) * waste_2_5$Transport_km_2 * waste_2_5$Transport_Diesel) * waste_2_5$Bj3_tech
waste_2_5$Bj3_collectionemiss <- waste_2_5$collection_emiss * waste_2_5$Bj3_tech
waste_2_5$Bj3_transport1emiss <- waste_2_5$transport1_emiss * waste_2_5$Bj3_tech
waste_2_5$Bj3_processemiss <- waste_2_5$Prod_ww * waste_2_5$Bj3.e.in * waste_2_5$Powergen_GHG * waste_2_5$Bj3_tech
waste_2_5$Bj3_transport2emiss <- (waste_2_5$Bj3_d / 42.79 + waste_2_5$Bj3_g / 41.74 + waste_2_5$Bj3_j / 43.10) * waste_2_5$Transport_km_2 * waste_2_5$Transport_Diesel * waste_2_5$Diesel_GHG
waste_2_5$Bj3_enduseemiss <- waste_2_5$Prod_ww * waste_2_5$Nonbio_emiss2 * waste_2_5$Bj3_tech
waste_2_5$Bj3_dispemiss <- 0 - waste_2_5$Bj3_d *waste_2_5$Diesel_GHG - waste_2_5$Bj3_g* waste_2_5$Gasoline_GHG -  waste_2_5$Bj3_j * waste_2_5$Jet_GHG 
waste_2_5$Bj3_dispemissmain <- 0 - waste_2_5$Bj3_j * waste_2_5$Jet_GHG
waste_2_5$Bj3_dispemissco <- 0 - waste_2_5$Bj3_d * waste_2_5$Diesel_GHG - waste_2_5$Bj3_g * waste_2_5$Gasoline_GHG
waste_2_5$Bj3_netemiss <- waste_2_5$Bj3_collectionemiss + waste_2_5$Bj3_transport1emiss + waste_2_5$Bj3_processemiss + waste_2_5$Bj3_transport2emiss + waste_2_5$Bj3_enduseemiss + waste_2_5$Bj3_dispemiss

#Bj4
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_5$Bj4_d <- waste_2_5$Prod_ww * waste_2_5$Bj4.d.out
waste_2_5$Bj4_g <- waste_2_5$Prod_ww * waste_2_5$Bj4.g.out
waste_2_5$Bj4_j <- waste_2_5$Prod_ww * waste_2_5$Bj4.j.out
waste_2_5$Bj4_energy <- waste_2_5$Bj4_d + waste_2_5$Bj4_g + waste_2_5$Bj4_j 
waste_2_5$Bj4_energymain <- waste_2_5$Bj4_j
waste_2_5$Bj4_energyco <- waste_2_5$Bj4_d + waste_2_5$Bj4_g 
waste_2_5$Bj4_netenergy <-(waste_2_5$Bj4_energy - waste_2_5$Prod_ww * waste_2_5$Bj4.h2.in -
                             (waste_2_5$collection_en +  waste_2_5$transport1_en)  -
                             (waste_2_5$Bj4_d / 42.79 + waste_2_5$Bj4_g / 41.74 +waste_2_5$Bj4_j / 43.10) * waste_2_5$Transport_km_2 * waste_2_5$Transport_Diesel) * waste_2_5$Bj4_tech
waste_2_5$Bj4_collectionemiss <- waste_2_5$collection_emiss * waste_2_5$Bj4_tech
waste_2_5$Bj4_transport1emiss <- waste_2_5$transport1_emiss * waste_2_5$Bj4_tech
waste_2_5$Bj4_processemiss <- waste_2_5$Prod_ww * waste_2_5$Bj4.h2.in * waste_2_5$H2_GHG * waste_2_5$Bj4_tech
waste_2_5$Bj4_transport2emiss <- (waste_2_5$Bj4_d / 42.79 + waste_2_5$Bj4_g / 41.74 + waste_2_5$Bj4_j / 43.10) * waste_2_5$Transport_km_2 * waste_2_5$Transport_Diesel * waste_2_5$Diesel_GHG 
waste_2_5$Bj4_enduseemiss <- waste_2_5$Prod_ww * waste_2_5$Nonbio_emiss2 * waste_2_5$Bj4_tech
waste_2_5$Bj4_dispemiss <- 0 - waste_2_5$Bj4_d *waste_2_5$Diesel_GHG - waste_2_5$Bj4_g* waste_2_5$Gasoline_GHG -  waste_2_5$Bj4_j * waste_2_5$Jet_GHG 
waste_2_5$Bj4_dispemissmain <- 0 - waste_2_5$Bj4_j * waste_2_5$Jet_GHG
waste_2_5$Bj4_dispemissco <- 0 - waste_2_5$Bj4_d * waste_2_5$Diesel_GHG - waste_2_5$Bj4_g * waste_2_5$Gasoline_GHG
waste_2_5$Bj4_netemiss <- waste_2_5$Bj4_collectionemiss + waste_2_5$Bj4_transport1emiss + waste_2_5$Bj4_processemiss + waste_2_5$Bj4_transport2emiss+ waste_2_5$Bj4_enduseemiss + waste_2_5$Bj4_dispemiss

#Bj5
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_5$Bj5_d <- waste_2_5$Prod_ww * waste_2_5$Bj5.d.out
waste_2_5$Bj5_g <- waste_2_5$Prod_ww * waste_2_5$Bj5.g.out
waste_2_5$Bj5_j <- waste_2_5$Prod_ww * waste_2_5$Bj5.j.out
waste_2_5$Bj5_energy <- waste_2_5$Bj5_d + waste_2_5$Bj5_g + waste_2_5$Bj5_j 
waste_2_5$Bj5_energymain <- waste_2_5$Bj5_j
waste_2_5$Bj5_energyco <- waste_2_5$Bj5_d + waste_2_5$Bj5_g 
waste_2_5$Bj5_netenergy <-(waste_2_5$Bj5_energy - waste_2_5$Prod_ww * waste_2_5$Bj5.e.in -
                             (waste_2_5$collection_en +  waste_2_5$transport1_en)  -
                             (waste_2_5$Bj5_d / 42.79 + waste_2_5$Bj5_g / 41.74 +waste_2_5$Bj5_j / 43.10) * waste_2_5$Transport_km_2 * waste_2_5$Transport_Diesel) * waste_2_5$Bj5_tech
waste_2_5$Bj5_collectionemiss <- waste_2_5$collection_emiss * waste_2_5$Bj5_tech
waste_2_5$Bj5_transport1emiss <- waste_2_5$transport1_emiss * waste_2_5$Bj5_tech
waste_2_5$Bj5_processemiss <- waste_2_5$Prod_ww * waste_2_5$Bj5.e.in * waste_2_5$Powergen_GHG * waste_2_5$Bj5_tech
waste_2_5$Bj5_transport2emiss <-  (waste_2_5$Bj5_d / 42.79 + waste_2_5$Bj5_g / 41.74 + waste_2_5$Bj5_j / 43.10) * waste_2_5$Transport_km_2 * waste_2_5$Transport_Diesel * waste_2_5$Diesel_GHG 
waste_2_5$Bj5_enduseemiss <- waste_2_5$Prod_ww * waste_2_5$Nonbio_emiss2 * waste_2_5$Bj5_tech
waste_2_5$Bj5_dispemiss <- 0 - waste_2_5$Bj5_d *waste_2_5$Diesel_GHG - waste_2_5$Bj5_g* waste_2_5$Gasoline_GHG -  waste_2_5$Bj5_j * waste_2_5$Jet_GHG 
waste_2_5$Bj5_dispemissmain <- 0 - waste_2_5$Bj5_j * waste_2_5$Jet_GHG
waste_2_5$Bj5_dispemissco <- 0 - waste_2_5$Bj5_d * waste_2_5$Diesel_GHG - waste_2_5$Bj5_g * waste_2_5$Gasoline_GHG
waste_2_5$Bj5_netemiss <- waste_2_5$Bj5_collectionemiss + waste_2_5$Bj5_transport1emiss + waste_2_5$Bj5_processemiss + waste_2_5$Bj5_transport2emiss + waste_2_5$Bj5_enduseemiss + waste_2_5$Bj5_dispemiss

#Bj6
#energy intensity of diesel - 42.79 MJ/kg, gasoline - 41.74, jet - 43.10
waste_2_5$Bj6_d <- waste_2_5$Prod_ww * waste_2_5$Bj6.d.out
waste_2_5$Bj6_g <- waste_2_5$Prod_ww * waste_2_5$Bj6.g.out
waste_2_5$Bj6_j <- waste_2_5$Prod_ww * waste_2_5$Bj6.j.out
waste_2_5$Bj6_energy <- waste_2_5$Bj6_d + waste_2_5$Bj6_g + waste_2_5$Bj6_j 
waste_2_5$Bj6_energymain <- waste_2_5$Bj6_j
waste_2_5$Bj6_energyco <- waste_2_5$Bj6_d + waste_2_5$Bj6_g 
waste_2_5$Bj6_netenergy <-(waste_2_5$Bj6_energy - waste_2_5$Prod_ww * waste_2_5$Bj6.h2.in - waste_2_5$Prod_ww * waste_2_5$Bj6.e.in -
                             (waste_2_5$collection_en +  waste_2_5$transport1_en)  -
                             (waste_2_5$Bj6_d / 42.79 + waste_2_5$Bj6_g / 41.74 +waste_2_5$Bj6_j / 43.10) * waste_2_5$Transport_km_2 * waste_2_5$Transport_Diesel) * waste_2_5$Bj6_tech
waste_2_5$Bj6_collectionemiss <- waste_2_5$collection_emiss * waste_2_5$Bj6_tech
waste_2_5$Bj6_transport1emiss <- waste_2_5$transport1_emiss * waste_2_5$Bj6_tech
waste_2_5$Bj6_processemiss <- waste_2_5$Prod_ww * (waste_2_5$Bj6.h2.in * waste_2_5$H2_GHG + waste_2_5$Bj6.e.in * waste_2_5$Powergen_GHG) * waste_2_5$Bj6_tech
waste_2_5$Bj6_transport2emiss <-  (waste_2_5$Bj6_d / 42.79 + waste_2_5$Bj6_g / 41.74 + waste_2_5$Bj6_j / 43.10) * waste_2_5$Transport_km_2 * waste_2_5$Transport_Diesel * waste_2_5$Diesel_GHG
waste_2_5$Bj6_enduseemiss <- waste_2_5$Prod_ww * waste_2_5$Nonbio_emiss2 * waste_2_5$Bj6_tech
waste_2_5$Bj6_dispemiss <- 0 - waste_2_5$Bj6_d *waste_2_5$Diesel_GHG - waste_2_5$Bj6_g* waste_2_5$Gasoline_GHG -  waste_2_5$Bj6_j * waste_2_5$Jet_GHG 
waste_2_5$Bj6_dispemissmain <- 0 - waste_2_5$Bj6_j * waste_2_5$Jet_GHG
waste_2_5$Bj6_dispemissco <- 0 - waste_2_5$Bj6_d * waste_2_5$Diesel_GHG - waste_2_5$Bj6_g * waste_2_5$Gasoline_GHG
waste_2_5$Bj6_netemiss <- waste_2_5$Bj6_collectionemiss + waste_2_5$Bj6_transport1emiss + waste_2_5$Bj6_processemiss + waste_2_5$Bj6_transport2emiss + waste_2_5$Bj6_enduseemiss + waste_2_5$Bj6_dispemiss


##extracting net emissions
waste_sen4 <- subset(waste_2_3, select = c(Waste_type, Prod_ww, E1_netemiss,E2_netemiss,E3_netemiss,E4_netemiss,M1_netemiss, 
                                           M2_netemiss,Eth1_netemiss, Rd1_netemiss,Rd2_netemiss,Bj1_netemiss,Bj2_netemiss,
                                           Bj3_netemiss,Bj4_netemiss,Bj5_netemiss, Bj6_netemiss))
waste_sen4_1 <- aggregate(.~Waste_type, waste_sen4, sum)
waste_sen4_1 <- gather(waste_sen4_1, category, emiss_kg, E1_netemiss:Bj6_netemiss)
waste_sen4_1$category <- gsub("emiss", "", waste_sen4_1$category)
waste_sen4_1 <- separate(data = waste_sen4_1, col = category, into = c("Tech", "Stage"), sep = "\\_")
waste_sen4_1$emiss_MT_per <- waste_sen4_1$emiss_kg / waste_sen4_1$Prod_ww / 1000
waste_sen4_1$Tech <- factor(waste_sen4_1$Tech, 
                            levels = c("E1", "E2","E3","E4", "M1", "M2", "Eth1", "Rd1", "Rd2",
                                       "Bj1", "Bj2", "Bj3", "Bj4", "Bj5", "Bj6"))
waste_sen4_1$TD <- "25km"

waste_sen5 <- subset(waste_2_4, select = c(Waste_type, Prod_ww, E1_netemiss,E2_netemiss,E3_netemiss,E4_netemiss,M1_netemiss, 
                                           M2_netemiss,Eth1_netemiss, Rd1_netemiss,Rd2_netemiss,Bj1_netemiss,Bj2_netemiss,
                                           Bj3_netemiss,Bj4_netemiss,Bj5_netemiss, Bj6_netemiss))
waste_sen5_1 <- aggregate(.~Waste_type, waste_sen5, sum)
waste_sen5_1 <- gather(waste_sen5_1, category, emiss_kg, E1_netemiss:Bj6_netemiss)
waste_sen5_1$category <- gsub("emiss", "", waste_sen5_1$category)
waste_sen5_1 <- separate(data = waste_sen5_1, col = category, into = c("Tech", "Stage"), sep = "\\_")
waste_sen5_1$emiss_MT_per <- waste_sen5_1$emiss_kg / waste_sen5_1$Prod_ww / 1000
waste_sen5_1$Tech <- factor(waste_sen5_1$Tech, 
                            levels = c("E1", "E2","E3","E4", "M1", "M2", "Eth1", "Rd1", "Rd2",
                                       "Bj1", "Bj2", "Bj3", "Bj4", "Bj5", "Bj6"))
waste_sen5_1$TD <- "50km"

waste_sen6 <- subset(waste_2_5, select = c(Waste_type, Prod_ww, E1_netemiss,E2_netemiss,E3_netemiss,E4_netemiss,M1_netemiss, 
                                           M2_netemiss,Eth1_netemiss, Rd1_netemiss,Rd2_netemiss,Bj1_netemiss,Bj2_netemiss,
                                           Bj3_netemiss,Bj4_netemiss,Bj5_netemiss, Bj6_netemiss))
waste_sen6_1 <- aggregate(.~Waste_type, waste_sen6, sum)
waste_sen6_1 <- gather(waste_sen6_1, category, emiss_kg, E1_netemiss:Bj6_netemiss)
waste_sen6_1$category <- gsub("emiss", "", waste_sen6_1$category)
waste_sen6_1 <- separate(data = waste_sen6_1, col = category, into = c("Tech", "Stage"), sep = "\\_")
waste_sen6_1$emiss_MT_per <- waste_sen6_1$emiss_kg / waste_sen6_1$Prod_ww / 1000
waste_sen6_1$Tech <- factor(waste_sen6_1$Tech, 
                            levels = c("E1", "E2","E3","E4", "M1", "M2", "Eth1", "Rd1", "Rd2",
                                       "Bj1", "Bj2", "Bj3", "Bj4", "Bj5", "Bj6"))
waste_sen6_1$TD <- "100km"

waste_sen7 <- subset(waste_2, select = c(Waste_type, Prod_ww, E1_netemiss,E2_netemiss,E3_netemiss,E4_netemiss,M1_netemiss, 
                                         M2_netemiss,Eth1_netemiss, Rd1_netemiss,Rd2_netemiss,Bj1_netemiss,Bj2_netemiss,
                                         Bj3_netemiss,Bj4_netemiss,Bj5_netemiss, Bj6_netemiss))
waste_sen7_1 <- aggregate(.~Waste_type, waste_sen7, sum)
waste_sen7_1 <- gather(waste_sen7_1, category, emiss_kg, E1_netemiss:Bj6_netemiss)
waste_sen7_1$category <- gsub("emiss", "", waste_sen7_1$category)
waste_sen7_1 <- separate(data = waste_sen7_1, col = category, into = c("Tech", "Stage"), sep = "\\_")
waste_sen7_1$emiss_MT_per <- waste_sen7_1$emiss_kg / waste_sen7_1$Prod_ww / 1000
waste_sen7_1$Tech <- factor(waste_sen7_1$Tech, 
                            levels = c("E1", "E2","E3","E4", "M1", "M2", "Eth1", "Rd1", "Rd2",
                                       "Bj1", "Bj2", "Bj3", "Bj4", "Bj5", "Bj6"))
waste_sen7_1$TD <- "150km"


waste_sen_td <- rbind(waste_sen4_1[ , -which(names(waste_sen1_1) == "Stage")], waste_sen5_1[ , -which(names(waste_sen2_1) == "Stage")],
                   waste_sen6_1[ , -which(names(waste_sen3_1) == "Stage")], waste_sen7_1[ , -which(names(waste_sen4_1) == "Stage")])
waste_sen_td$TD <- factor(waste_sen_td$TD, 
                       levels = c("25km", "50km", "100km", "150km"))



#sensitivity chart

colors_power <- c("25km" = "#6BAED6",
                  "50km" = "#4292C6",
                  "100km" = "#2171B5",
                  "150km" = "#08519C")
p <- ggplot()+ 
  geom_bar(data = waste_sen_td[which(waste_sen_td$emiss_MT_per != 0),],
           aes(x=Tech, y=emiss_MT_per, fill=TD), stat="identity", position = "dodge") +
  geom_hline(yintercept=0, size=0.05)+
  theme_bw() +
  theme(text = element_text(size=20)) +
  scale_fill_manual(values=colors_power) +
  guides(fill = guide_legend(title = "", label.theme = element_text(size = 20, angle = 0))) +
  scale_y_continuous(name="Metric tonne CO2e/tonne ww", limits =  c(-1.5, 1)) +
  labs(x = '') +
  theme(legend.position="top",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_grid(~Waste_type, scales = "free_x", space = "free_x")+
  theme(strip.background =element_rect(fill="white"), strip.text = element_text(size = 20, face = "bold")) 
#  ggtitle("Net GHG emissions - sensitivity analysis on transportation distance") +
#  theme(plot.title = element_text(face = "bold", size = 24, hjust = 0))
ggsave(paste(County_FOLDER, "/US_netemiss_SA_TD.png", sep=""), plot=p, width=20,height=6,units="in",dpi=300)






