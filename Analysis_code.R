
### Code for analysis accompanying the paper titled: 
# A new perspective on climate feedback loop management
# Emily A Ury, Zhen Zhang, Brian Buma


library(ggplot2)

### read in global temp projection (this is change in temp in K)
dTemp <- read.csv("data/Global_temperature_projections.csv")

### read in global wetland methane projection (this is all methane from wetlands in Tg/yr)
wCH4_Z <- read.csv("data/Global_methane_projections.csv")   ### from Zhang

### using 1980 as a baseline -- how does additional methane (2020-2099) relate to global temperature anomally
#rcp 2.6
CH4.rcp26 <- wCH4_Z[60:139, 2] - wCH4_Z[20,2]  ## additional methane in Tg/yr
dT.rcp26 <- dTemp[26:105, 3]

#rcp 4.5
CH4.rcp45 <- wCH4_Z[60:139, 3] - wCH4_Z[20,3]  ## additional methane in Tg/yr
dT.rcp45 <- dTemp[26:105, 4]

#rcp 6.0
CH4.rcp60 <- wCH4_Z[60:139, 4] - wCH4_Z[20,4]  ## additional methane in Tg/yr
dT.rcp60 <- dTemp[26:105, 5]

#rcp 8.5
CH4.rcp85 <- wCH4_Z[60:139, 5] - wCH4_Z[20,5]  ## additional methane in Tg/yr
dT.rcp85 <- dTemp[26:105, 7]

## combine
all.temp <- c(dT.rcp26, dT.rcp45, dT.rcp60, dT.rcp85)
Zhang.CH4 <- c(CH4.rcp26, CH4.rcp45, CH4.rcp60, CH4.rcp85)


#### for Kleinen data
wCH4_K <- read.csv("data/Global_methane_projections_kleinen.csv")   ### from Zhang

#ssp 1.9
CH4.ssp19 <- wCH4_K[60:139, 2] - wCH4_K[20,2]  ## additional methane in Tg/yr
dT.ssp19 <- dTemp[26:105, 2]

#ssp 2.6
CH4.ssp26 <- wCH4_K[60:139, 3] - wCH4_K[20,3]  ## additional methane in Tg/yr
dT.ssp26 <- dTemp[26:105, 3]

#ssp 4.5
CH4.ssp45 <- wCH4_K[60:139, 4] - wCH4_K[20,4]  ## additional methane in Tg/yr
dT.ssp45 <- dTemp[26:105, 4]

#ssp 7.0
CH4.ssp70 <- wCH4_K[60:139, 5] - wCH4_K[20,5]  ## additional methane in Tg/yr
dT.ssp70 <- dTemp[26:105, 6]

#ssp 8.5
CH4.ssp85 <- wCH4_K[60:139, 6] - wCH4_K[20,6]  ## additional methane in Tg/yr
dT.ssp85 <- dTemp[26:105, 7]



##### plot both together

rcp <- rep(c("rcp26", "rcp45", "rcp60", "rcp85"), each = 80)

par(mar = c(5,5,1,1))
plot(all.temp, Zhang.CH4, pch = c(1, 17, 3, 6)[as.factor(rcp)], cex = 0.7, col = "red",
     xlab = "Global Temperature Anomally (K)", 
     ylab = "Additional CH4 (Tg yr^-1)", 
     ylim = c(0,360), 
     xlim = c(1,5))
legend("bottomright", legend = c("RCP 2.6", "RCP 4.5", "RCP 6.0", "RCP 8.5"), 
       pch = c(1,17, 3, 6), cex = 0.7, col = "red")
fit <- lm(Zhang.CH4 ~ all.temp)
abline(fit, col = "red")
summary(fit)
text(3.5, 30, "Zhang et al. 2017\ny = 44.0x - 26.7\n R2 = 0.98", col = "red", cex = 0.9)

Kleinen.temp <- c(dT.ssp19, dT.ssp26, dT.ssp45, dT.ssp70, dT.ssp85)
Kleinen.CH4 <- c(CH4.ssp19, CH4.ssp26, CH4.ssp45, CH4.ssp70, CH4.ssp85)
ssp <- rep(c("ssp19", "ssp26", "ssp45", "ssp70", "ssp85"), each = 80)

points(Kleinen.temp, Kleinen.CH4, pch = c(20, 1, 17, 4, 6)[as.factor(ssp)])
legend("topleft", legend = c("SSP 1.9", "SSP 2.6", "SSP 4.5", "SSP 7.0", "SSP 8.5"), 
       pch = c(20, 1, 17, 4, 6), cex = 0.7, col = "black")
fit2 <- lm(Kleinen.CH4 ~ Kleinen.temp)
abline(fit2)
summary(fit2)
text(3, 340, "Kleinen et al. 2021\ny = 79.0x - 62.9\n R2 = 0.93", col = "black", cex = 0.9)



### STORE MODEL COEFFICIENTS
Z.slope <- fit$coefficients[2]
Z.int <- fit$coefficients[1]
K.slope <-  fit2$coefficients[2]
K.int <-  fit2$coefficients[1]



### write out data
year <- rep(seq(2020,2099,1),9)
temperature <- c(all.temp, Kleinen.temp)
methane <- c(Zhang.CH4, Kleinen.CH4)
source <- c(rep("Zhang", 320), rep("Kleinen", 400))
scenario <- rep(c("rcp26", "rcp45", "rcp60", "rcp85", "ssp19", "ssp26", "ssp45", "ssp70", "ssp85"), each = 80)
plot.data <- data.frame(year, source, scenario, temperature, methane)

write.csv(plot.data, "data/source_data.csv", row.names = F)





#### add additional wetland methane to the template
additional_methane45 <- wCH4_K[c(60, 70, 80, 90, 100, 110, 120, 130, 139), 4] - wCH4_K[20,4]
additional_methane45


## adjust template for additional methane
template <- read.csv("data/MAGICC45/Templates/Baseline/rcmip_ssp245_template.csv", header = TRUE)
names <- c("model", "region", "scenario","unit", "variable", 
           "2015", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
names(template) <- names
methane <- template[2, c(7:15)]
methane_additional <- methane + additional_methane45
template[2, c(7:15)] <- methane_additional
write.csv(template, "data/MAGICC45/Templates/Feedback_methane/rcmip_ssp245_template_methane.csv", 
          row.names = FALSE)

#
###
##### RUN MAGICC 2x (external): Baseline 4.5 and baseline + additional methane, probabalistic
###
#


### read in results
baseline <- read.csv("data/MAGICC45/Outputs/Baseline/baseline_prob_45_magicc_202406080858.csv", header = TRUE)
feedback_methane <- read.csv("data/MAGICC45/Outputs/Feedback_methane/baseline_prob_45_additional_methane_magicc_202406100910.csv", header = TRUE)

## temp in 2100
# baseline
baseline[16, 118]
## 95% confidence interval, from web graphic: 2.03 - 3.74
feedback_methane[16,118]
## 95% confidence interval, from web graphic: 2.03 - 3.74


## Scenario 1

#### Climate intervention (single sector (SS))
#### reducing CO2 emissions by 7290 Mt/year beginning in 2050
#### aligns with achieving net zero across the global transportation sector

reduction <- c(0,0,0,0, 7290, 7290, 7290, 7290, 7290, 7290)

# 4.5 2015-2100 (from the Magicc input template): 
CO2_45 <-c(35635.3,	37388.1,	40594.7,	42088.6,	42961.3,	41736.4,	37446.6,	30235.7,	20641.5,	14482.9)

new_CO2_SS <- CO2_45 - reduction

## Scenario 2

#### climate intervention (multi-sector (MS))
#### reducing CO2 emissions to align with the aggressive climate mitigation scenarios (SSP 1.9 or 2.6)

# 4.5 2015-2100: 
CO2_45 <-c(35635.3,	37388.1,	40594.7,	42088.6,	42961.3,	41736.4,	37446.6,	30235.7,	20641.5,	14482.9)

# 2.6 2015-2100: 
CO2_26 <-c(35635.3,	36625.7,	34546.4,	26897,	19722.2,	13114.6,	7060.3,	-848.141,	-6300.5,	-5718.75)
CO2_45 - CO2_26

# 1.9 2015-2100: 
CO2_19 <-c(35635.3,	36518.1,	22474.9,	9091.44,	2865.45,	-35.4631,	-2644.69,	-5186.8,	-8342.06,	-11508.4)
CO2_45 - CO2_19
mean(CO2_45 - CO2_19)

### using the more aggressive scenario

new_CO2_MS <- CO2_19


### write new templates
## adjust template for SINGLE SECTOR CO2 reduction
template <- read.csv("data/MAGICC45/Templates/Feedback_methane/rcmip_ssp245_template_methane.csv", header = TRUE)
names <- c("model", "region", "scenario","unit", "variable", 
           "2015", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
names(template) <- names
template[5, c(6:15)] <- new_CO2_SS
write.csv(template, "data/MAGICC45/Templates/Interventions/Single_sector_template.csv", 
          row.names = FALSE)

template[5, c(6:15)] <- new_CO2_MS
write.csv(template, "data/MAGICC45/Templates/Interventions/Multi_sector_template.csv", 
          row.names = FALSE)


#
###
##### Run MAGICC x2 with climate intervention  (and additional methane)
##
#

### get ouputs: temperature in 2100 following intervention

multi_sector <- read.csv("data/MAGICC45/Outputs/Interventions/prob_S2_45_magicc_202406100920.csv", header = TRUE)
multi_sector[16, 118]
## 95% confidence interval, from web graphic: 1.34 - 2.44
feedback_methane[16,118] - multi_sector[16, 118]


#### climate intervention (modest)
## Achieving net zero in the global transportation sector by 2050
## subtracting 7290 Mt CO2 from emissions in 2050 onward
single_sector <- read.csv("data/MAGICC45/Outputs/Interventions/prob_S1_45_magicc_202406100918.csv", header = TRUE)
single_sector[16, 118]
## 95% confidence interval, from web graphic: 2.07 - 3.77
feedback_methane[16,118] - single_sector[16, 118]


###### Estimated reduction in wetland methane production given the reduction in global temp


dTemp_single <- single_sector[16, c(38, 48, 58, 68, 78, 88, 98, 108, 118)] - 
  feedback_methane[16, c(38, 48, 58, 68, 78, 88, 98, 108, 118)]

dTemp_multi <- multi_sector[16, c(38, 48, 58, 68, 78, 88, 98, 108, 118)] - 
  feedback_methane[16, c(38, 48, 58, 68, 78, 88, 98, 108, 118)]

## Kleinen et al. 2021
dCH4_single_K <- round(K.slope*dTemp_single, 3)
dCH4_multi_K <- round(K.slope*dTemp_multi, 3)


## Zhang
dCH4_single_Z <- round(Z.slope*dTemp_single , 3)
dCH4_multi_Z <- round(Z.slope*dTemp_multi, 3)



### feed change in methane emissions back into MAGICC to find additional 
# temperature reduction

## read in template and overwrite with reduced methane

## Single sector - Kleinen
template <- read.csv("data/MAGICC45/Templates/Interventions/Single_sector_template.csv", header = TRUE)
names <- c("model", "region", "scenario","unit", "variable", 
           "2015", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
names(template) <- names
methane <- template[2, c(7:15)]
methane_reduction <- methane + dCH4_single_K
template[2, c(7:15)] <- methane_reduction
write.csv(template, "data/MAGICC45/Templates/Final/Prob_S1_K.csv", 
          row.names = FALSE)

## Single sector - Zhang
template <- read.csv("data/MAGICC45/Templates/Interventions/Single_sector_template.csv", header = TRUE)
names <- c("model", "region", "scenario","unit", "variable", 
           "2015", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
names(template) <- names
methane <- template[2, c(7:15)]
methane_reduction <- methane + dCH4_single_Z
template[2, c(7:15)] <- methane_reduction
write.csv(template, "data/MAGICC45/Templates/Final/Prob_S1_Z.csv", 
          row.names = FALSE)

## Multi sector - Kleinen
template <- read.csv("data/MAGICC45/Templates/Interventions/Multi_sector_template.csv", header = TRUE)
names <- c("model", "region", "scenario","unit", "variable", 
           "2015", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
names(template) <- names
methane <- template[2, c(7:15)]
methane_reduction <- methane + dCH4_multi_K
template[2, c(7:15)] <- methane_reduction
write.csv(template, "data/MAGICC45/Templates/Final/Prob_S2_K.csv", 
          row.names = FALSE)

# Multi sector - Zhang
template <- read.csv("data/MAGICC45/Templates/Interventions/Multi_sector_template.csv", header = TRUE)
names <- c("model", "region", "scenario","unit", "variable", 
           "2015", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
names(template) <- names
methane <- template[2, c(7:15)]
methane_reduction <- methane + dCH4_multi_Z
template[2, c(7:15)] <- methane_reduction
write.csv(template, "data/MAGICC45/Templates/Final/Prob_S2_Z.csv", 
          row.names = FALSE)
#
###
##### Run final MAGICC (4 rounds) to get final temperatures
###
#

## read in final results and get temperature

multi_K <- read.csv("data/MAGICC45/Outputs/Final/Prob_S2_K_magicc_202406100948.csv", header = TRUE)
multi_K[16, 118]
## 95% confidence interval, from web graphic: 1.28 - 2.34
## temp change
dMK <- round(multi_sector[16,118] - multi_K[16, 118],3)
#pMK <- round(dMK/multi_sector[16,118]*100,3)
pMK <- round(-dMK/dTemp_multi[1,9]*100,3)

multi_Z <- read.csv("data/MAGICC45/Outputs/Final/Prob_S2_Z_magicc_202406100948.csv", header = TRUE)
multi_Z[16, 118]
## 95% confidence interval, from web graphic: 1.31 - 2.38
## temp change
dMZ <- round(multi_sector[16,118] - multi_Z[16, 118],3)
#pMZ <- round(dMZ/multi_sector[16,118]*100, 3)
pMZ <- round(-dMZ/dTemp_multi[1,9]*100,3)


single_K <- read.csv("data/MAGICC45/Outputs/Final/Prob_S1_K_magicc_202406100933.csv", header = TRUE)
single_K[16, 118]
## 95% confidence interval, from web graphic: 2.06 - 3.76
## temp change
dSK <- round(single_sector[16,118] - single_K[16, 118],3)
#pSK <- round(dSK/single_sector[16,118]*100,3)
pSK <- round(-dSK/dTemp_single[1,9]*100,3)


single_Z <- read.csv("data/MAGICC45/Outputs/Final/Prob_S1_Z_magicc_202406100941.csv", header = TRUE)
single_Z[16, 118]
## 95% confidence interval, from web graphic: 2.06 - 3.77
## temp change
dSZ <- round(single_sector[16,118] - single_Z[16, 118],3)
#pSZ <- round(dSZ/single_sector[16,118]*100,3)
pSZ <- round(-dSZ/dTemp_single[1,9]*100,3)

### Results table  -- temp anomaly (K) in 20100
row1 <- c("Baseline", baseline[16, 118], baseline[16, 118] )
row2 <- c("Baseline + feedback methane", feedback_methane[16,118], feedback_methane[16,118])
row3 <- c("Climate Intervention", single_sector[16,118], multi_sector[16,118])
row4 <- c("Change in temp", round(dTemp_single[1,9], 3), round(dTemp_multi[1,9],3 ))
row5 <- c("Methane reduction", paste(dCH4_single_Z[1,9], "to", dCH4_single_K[1,9]), 
          paste(dCH4_multi_Z[1,9], "to", dCH4_multi_K[1,9]))
row6 <- c("Temp reduction (K)", paste(dSZ, "to", dSK), paste(dMZ, "to", dMK))
row7 <- c("Temp reduction (%)", paste(pSZ, "to", pSK), paste(pMZ, "to", pMK))

results <- rbind(row1, row2, row3, row4, row5, row6, row7)
colnames(results) <- c("effect 2100", "single", "multi")
results






