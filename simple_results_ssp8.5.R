



####### Scenario 8.5
### high/low emissions reductions efforts


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


# plot(dT.rcp26, CH4.rcp26, pch = 16, cex = 0.7, col = "#121212dd", 
#      ylim = c(0, 200), xlim = c(1, 5))
# points(dT.rcp45, CH4.rcp45, pch = 16, cex = 0.7, col = "#ff0000dd")
# points(dT.rcp60, CH4.rcp60, pch = 16, cex = 0.7, col = "#0000ffdd")
# points(dT.rcp85, CH4.rcp85, pch = 16, cex = 0.7, col = "#00ffdd")
# 
# 
# 
all.temp <- c(dT.rcp26, dT.rcp45, dT.rcp60, dT.rcp85)
Zhang.CH4 <- c(CH4.rcp26, CH4.rcp45, CH4.rcp60, CH4.rcp85)
# col <- rep(c("#121212dd", "#ff0000dd", "#0000FFdd", "#00ffdddd"), each = 80)
# 
# 
# 
# plot(all.temp, Zhang.CH4, pch = 16, cex = 0.7, col = col,
#      xlab = "Global Temperature Anomally (K)", 
#      ylab = "Additional CH4 (Tg yr^-1)")
# legend("topleft", legend = c("RCP 2.6", "RCP 4.5", "RCP 6.0", "RCP 8.5"), 
#        pch = 16, cex = 0.7, col = c("#121212dd", "#ff0000dd", "#0000FFdd", "#00ffdddd"))
# fit <- lm(Zhang.CH4 ~ all.temp)
# abline(fit)
# summary(fit)
# text(3, 175, "y = 43.95x - 26.72\n R^2 = 0.98")



#### for Kleinen data

wCH4_K <- read.csv("data/Global_methane_projections_kleinen.csv")   ### from Zhang

#plot(wCH4_K$year, wCH4_K$CH4_Tg_yr_ssp19)

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





#### add additional wetland methane to the template
additional_methane45 <- wCH4_K[c(60, 70, 80, 90, 100, 110, 120, 130, 139), 4] - wCH4_K[20,4]
additional_methane85 <- wCH4_K[c(60, 70, 80, 90, 100, 110, 120, 130, 139), 6] - wCH4_K[20,6]

additional_methane45
additional_methane85

## adjust template for additional methane
template <- read.csv("data/MAGICC2/Templates/Baseline/rcmip_ssp585_template.csv", header = TRUE)
names <- c("model", "region", "scenario","unit", "variable", 
           "2015", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
names(template) <- names
methane <- template[2, c(7:15)]
methane_additional <- methane + additional_methane85
template[2, c(7:15)] <- methane_additional
write.csv(template, "data/MAGICC2/Templates/Feedback_methane/rcmip_ssp585_template_methane.csv", 
          row.names = FALSE)


### RUN MAGICC (external)

### read in results
baseline <- read.csv("data/MAGICC2/Outputs/Baseline/ssp585_single_magicc_202404291629.csv", header = TRUE)
feedback_methane <- read.csv("data/MAGICC2/Outputs/Feedback_methane/ssp585_single_feedback_methane_magicc_202404291658.csv", header = TRUE)

## temp in 2100
# baseline
baseline[16, 118]
feedback_methane[16,118]



#### climate intervention (drastic)
#### reducing CO2 emissions to align with the ssp 4.5 scenario

# 8.5 2015-2100: 
CO2_85 <- c(35635.3,	39762.7,	50749.7,	65340,	81245.2,	100199,	117035,	129795,	131072,	127818)
# 4.5 2015-2100: 
CO2_45 <-c(35635.3,	37388.1,	40594.7,	42088.6,	42961.3,	41736.4,	37446.6,	30235.7,	20641.5,	14482.9)
CO2_85 - CO2_45

drastic <- read.csv("data/MAGICC2/Outputs/Interventions/drastic_CO2_reductions_magicc_202405011313.csv", header = TRUE)
drastic[16, 118]

#### climate intervention (modest)
## Achieving net zero in the global transportation sector by 2050
## subtracting 7290 Mt CO2 from emissions in 2050 onward
modest <- read.csv("data/MAGICC2/Outputs/Interventions/modest_co2_reductions_magicc_202405011311.csv", header = TRUE)
modest[16, 118]



###### Estimated reduction in wetland methane production given the reduction in global temp


year <- c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)
dTemp_modest <- modest[16, c(38, 48, 58, 68, 78, 88, 98, 108, 118)] - 
  feedback_methane[16, c(38, 48, 58, 68, 78, 88, 98, 108, 118)]

dTemp_drastic <- drastic[16, c(38, 48, 58, 68, 78, 88, 98, 108, 118)] - 
  feedback_methane[16, c(38, 48, 58, 68, 78, 88, 98, 108, 118)]

## Kleinen et al. 2021
dCH4_modest_K <- round(K.slope*dTemp_modest + K.int, 3)
dCH4_drastic_K <- round(K.slope*dTemp_drastic + K.int, 3)


## Zhang
dCH4_modest_Z <- round(Z.slope*dTemp_modest + Z.int, 3)
dCH4_drastic_Z <- round(Z.slope*dTemp_drastic + Z.int, 3)



### feed change in methane emissions back into MAGICC to find additional 
# temperature reduction

## read in template and overwrite with reduced methane

## Modest - Kleinen
template <- read.csv("data/MAGICC2/Templates/Interventions/modest_CO2_reductions.csv", header = TRUE)
names <- c("model", "region", "scenario","unit", "variable", 
           "2015", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
names(template) <- names
methane <- template[2, c(7:15)]
methane_reduction <- methane + dCH4_modest_K
template[2, c(7:15)] <- methane_reduction
write.csv(template, "data/MAGICC2/Templates/Final/modest_CO2_reductions_K.csv", 
          row.names = FALSE)

## Modest - Zhang
template <- read.csv("data/MAGICC2/Templates/Interventions/modest_CO2_reductions.csv", header = TRUE)
names <- c("model", "region", "scenario","unit", "variable", 
           "2015", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
names(template) <- names
methane <- template[2, c(7:15)]
methane_reduction <- methane + dCH4_modest_Z
template[2, c(7:15)] <- methane_reduction
write.csv(template, "data/MAGICC2/Templates/Final/modest_CO2_reductions_Z.csv", 
          row.names = FALSE)

## Drastic - Kleinen
template <- read.csv("data/MAGICC2/Templates/Interventions/drastic_CO2_reductions.csv", header = TRUE)
names <- c("model", "region", "scenario","unit", "variable", 
           "2015", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
names(template) <- names
methane <- template[2, c(7:15)]
methane_reduction <- methane + dCH4_drastic_K
template[2, c(7:15)] <- methane_reduction
write.csv(template, "data/MAGICC2/Templates/Final/drastic_CO2_reductions_K.csv", 
          row.names = FALSE)

# Drastic - Zhang
template <- read.csv("data/MAGICC2/Templates/Interventions/drastic_CO2_reductions.csv", header = TRUE)
names <- c("model", "region", "scenario","unit", "variable", 
           "2015", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
names(template) <- names
methane <- template[2, c(7:15)]
methane_reduction <- methane + dCH4_drastic_Z
template[2, c(7:15)] <- methane_reduction
write.csv(template, "data/MAGICC2/Templates/Final/drastic_CO2_reductions_Z.csv", 
          row.names = FALSE)
#
###
##### Run final MAGICC (4 rounds) to get final temperatures
###
#

## read in final results and get temperature

drastic_K <- read.csv("data/MAGICC2/Outputs/Final/drastic_plus_methane_Kleinen_magicc_202405011636.csv", header = TRUE)
drastic_K[16, 118]
## temp change
dDK <- round(drastic[16,118] - drastic_K[16, 118],3)
pDK <- round(dDK/drastic[16,118]*100,3)

drastic_Z <- read.csv("data/MAGICC2/Outputs/Final/drastic_plus_methane_Zhang_magicc_202405011636.csv", header = TRUE)
drastic_Z[16, 118]
## temp change
dDZ <- round(drastic[16,118] - drastic_Z[16, 118],3)
pDZ <- round(dDZ/drastic[16,118]*100, 3)


modest_K <- read.csv("data/MAGICC2/Outputs/Final/modest_plus_methane_Kleinen_magicc_202405011636.csv", header = TRUE)
modest_K[16, 118]
## temp change
dMK <- round(modest[16,118] - modest_K[16, 118],3)
pMK <- round(dMK/modest[16,118]*100,3)

modest_Z <- read.csv("data/MAGICC2/Outputs/Final/modest_plus_methane_Zhang_magicc_202405011636.csv", header = TRUE)
modest_Z[16, 118]
## temp change
dMZ <- round(modest[16,118] - modest_Z[16, 118],3)
pMZ <- round(dMZ/modest[16,118]*100,3)


### Results table  -- temp anomaly (K) in 20100
row1 <- c("Baseline", baseline[16, 118], baseline[16, 118] )
row2 <- c("Baseline + feedback methane", feedback_methane[16,118], feedback_methane[16,118])
row3 <- c("Climate Intervention", modest[16,118], drastic[16,118])
row4 <- c("Change in temp", round(dTemp_modest[1,9], 5), dTemp_drastic[1,9] )
row5 <- c("Methane reduction", paste(dCH4_modest_Z[1,9], "to", dCH4_modest_K[1,9]), 
          paste(dCH4_drastic_Z[1,9], "to", dCH4_drastic_K[1,9]))
row6 <- c("Temp reduction (K)", paste(dMZ, "to", dMK), paste(dDZ, "to", dDK))
row7 <- c("Temp reduction (%)", paste(pMZ, "to", pMK), paste(pDZ, "to", pDK))

results <- rbind(row1, row2, row3, row4, row5, row6, row7)
colnames(results) <- c("effect 2100", "modest", "drastic")
results






