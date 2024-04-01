


#### idea generation for the bonus mitigation project

library(ggplot2)



#### Relationship between Temp and additional CH4 production
{
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


plot(dT.rcp26, CH4.rcp26, pch = 16, cex = 0.7, col = "#121212dd", 
     ylim = c(0, 200), xlim = c(1, 5))
points(dT.rcp45, CH4.rcp45, pch = 16, cex = 0.7, col = "#ff0000dd")
points(dT.rcp60, CH4.rcp60, pch = 16, cex = 0.7, col = "#0000ffdd")
points(dT.rcp85, CH4.rcp85, pch = 16, cex = 0.7, col = "#00ffdd")



all.temp <- c(dT.rcp26, dT.rcp45, dT.rcp60, dT.rcp85)
Zhang.CH4 <- c(CH4.rcp26, CH4.rcp45, CH4.rcp60, CH4.rcp85)
col <- rep(c("#121212dd", "#ff0000dd", "#0000FFdd", "#00ffdddd"), each = 80)



plot(all.temp, Zhang.CH4, pch = 16, cex = 0.7, col = col,
     xlab = "Global Temperature Anomally (K)", 
     ylab = "Additional CH4 (Tg yr^-1)")
legend("topleft", legend = c("RCP 2.6", "RCP 4.5", "RCP 6.0", "RCP 8.5"), 
       pch = 16, cex = 0.7, col = c("#121212dd", "#ff0000dd", "#0000FFdd", "#00ffdddd"))
fit <- lm(Zhang.CH4 ~ all.temp)
abline(fit)
summary(fit)
text(3, 175, "y = 43.95x - 26.72\n R^2 = 0.98")



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
text(3, 30, "Zhang\ny = 44.0x - 26.7\n R^2 = 0.98", col = "red", cex = 0.9)

Kleinen.temp <- c(dT.ssp19, dT.ssp26, dT.ssp45, dT.ssp70, dT.ssp85)
Kleinen.CH4 <- c(CH4.ssp19, CH4.ssp26, CH4.ssp45, CH4.ssp70, CH4.ssp85)
ssp <- rep(c("ssp19", "ssp26", "ssp45", "ssp70", "ssp85"), each = 80)

points(Kleinen.temp, Kleinen.CH4, pch = c(20, 1, 17, 4, 6)[as.factor(ssp)])
legend("topleft", legend = c("SSP 1.9", "SSP 2.6", "SSP 4.5", "SSP 7.0", "SSP 8.5"), 
       pch = c(20, 1, 17, 4, 6), cex = 0.7, col = "black")
fit2 <- lm(Kleinen.CH4 ~ Kleinen.temp)
abline(fit2)
summary(fit2)
text(3, 340, "Kleinen\ny = 79.0x - 62.9\n R^2 = 0.93", col = "black", cex = 0.9)



### STORE MODEL COEFFICIENTS
Z.slope <- fit$coefficients[2]
Z.int <- fit$coefficients[1]
K.slope <-  fit2$coefficients[2]
K.int <-  fit2$coefficients[1]




## version 2 - kleinen only with high and low generated from prediction interval


fit2 <- lm(Kleinen.CH4 ~ Kleinen.temp)
summary(fit2)
new.dat <- data.frame(Kleinen.temp = c(1,2,3,4,5))
predict(fit2, newdata = new.dat, interval = 'confidence') ## 95% confidence interval


y.new <- predict(fit2, new.dat)
lwr <- predict(fit2, newdata = new.dat, interval = 'prediction')[,2]
upr <- predict(fit2, newdata = new.dat, interval = 'prediction')[,3]
lwr.ci <- predict(fit2, newdata = new.dat, interval = 'confidence')[,2]
upr.ci <- predict(fit2, newdata = new.dat, interval = 'confidence')[,3]

# for plotting
x.new <- c(1:5)

plot(Kleinen.temp, Kleinen.CH4, pch = c(20, 1, 17, 4, 6)[as.factor(ssp)], 
     xlab = "Global Temperature Anomaly (K)", 
     ylab = "Wetland Methane Emissions (Tg/yr)")
legend("topleft", legend = c("SSP 1.9", "SSP 2.6", "SSP 4.5", "SSP 7.0", "SSP 8.5"), 
       pch = c(20, 1, 17, 4, 6), cex = 0.7, col = "black")
polygon(c(x.new, rev(x.new)), c(lwr.ci, rev(upr.ci)), col = '#00000055', border = NA)
points(x.new, y.new, type = 'l', col = "blue", lwd = 2)
# points(x.new, lwr.ci, type = 'l', col = "#00000055", lwd = 2)
# points(x.new, upr.ci, type = 'l', col = "#00000055", lwd = 2)
points(x.new, lwr, type = 'l', col = "red")
points(x.new, upr, type = 'l', col = "red")
text(2.5, 300, "Kleinen\ny = 79.0x - 62.9\n R^2 = 0.93", col = "black", cex = 0.9)

### prediction ranges, upper/lower

fit.upper <- lm(upr~x.new)
fit.lower <- lm(lwr~x.new)


slope.high <- fit.upper$coefficients[2]
int.high <- fit.upper$coefficients[1]
slope.low <-  fit.lower$coefficients[2]
int.low <-  fit.lower$coefficients[1]


}


#### Example 1 - US 2021 Aviation Climate Action Plan

{
# https://www.faa.gov/sites/faa.gov/files/2021-11/Aviation_Climate_Action_Plan.pdf
# Net Zero by 2050
#' "Aircraft engines produce negligible amounts of N2O and CH4, so this plan has 
#' a focus on aviation combustion CO2 emissions and well-to-tank life cycle GHG 
#' emissions (CO2, N2O, CH4).
#' 
#' 2019 Aviation emissions were 222 MT CO2 (Figure 1)
#' 
#' Projected Aviation Emissions to 2050 (Figure 3)
#' 
#' WebPlotDigitizer v4.7 (https://apps.automeris.io/wpd/)
#' Used to pull estimates of CO2 emissions for 2030, 2040, and 2050 benchmarks:
#' 2030: 207
#' 2040: 192
#' 2050: 0
#' 
#' This translates to reductions of 15, 30, and 222 MT CO2/yr, respectively for each time point. 
#' 
#' To the 4.5 MAGICC template, we subtract 15 from CO2 emissions in 2030,
#' subtract 30 from CO2 emissions in 2040,
#' subtract 222 from CO2 in 2050 and all subsequent decades.
#' 
#' 
#' MAGICC output:
output <- read.csv("data/MAGICC/Aviation_plan_intervention_rcp4.5_magicc_202403201305.csv")
S1.dT <- as.numeric(output[16,38:117])

year <- seq(2020,2099, 1)
plot(year, dT.rcp45, type = 'l')
points(year, S1.dT, type = 'l', col = "red")


(dT.rcp45 - S1.dT)[80] #2099
## for 2099, temp goes down by 0.00445 K
(dT.rcp45 - S1.dT)[31] #2050
## for 2050, temp goes down by 0.00051


## predicted additional methane in 2099
(dT.rcp45[80])*43.95-26.72 
# 88.044 Tg/yr

## predicted additional methane in 2099 with the aviation plan in place
(dT.rcp45[80]-0.00445)*43.95-26.72
# 87.849

88.044-87.849 ### reduces additional methane by 0.195 Tg




### repeat for each decade to find methane decrease to input into magic
## temp goes down by XX in 2050, 2060, 2070, 2080, 2090, 2099
temp.decrease <- (dT.rcp45 - S1.dT)[c(31, 41, 51, 61, 71, 80)]
CH4.decrease <- (dT.rcp45[c(31, 41, 51, 61, 71, 80)]-temp.decrease)*43.95-26.72
CH4.additional <- (dT.rcp45[c(31, 41, 51, 61, 71, 80)])*43.95-26.72 

CH4.additional - CH4.decrease
### take these values away from the CH4 budget for 2050-2099 in the aviation plan template


## Rerun MAGICC
output2 <- read.csv("data/MAGICC/aviation_w_additional_methane_reduction_magicc_202403201458.csv")
S1a.dT <- as.numeric(output2[16,38:117])

## 2099 temp anommaly if
## RCP 4.5 - no invervention
dT.rcp45[80]
## US aviation fuel plan implemented (CO2 reduced to zero by 2050)
S1.dT[80]
## US aviation fuel plan with additional methane mitigation factored in
S1a.dT[80]


### percent change
(dT.rcp45[80] - S1.dT[80])/dT.rcp45[80]*100

(dT.rcp45[80] - S1a.dT[80])/dT.rcp45[80]*100

(S1.dT[80] - S1a.dT[80])/S1.dT[80]*100

}



#### Example 2 -- Achieving net zero in the global transportation sector by 2050
##

### Run baseline MAGICC scenarios
### output: baseline global temperature anomaly
baseline_45 <- read.csv("data/MAGICC/Outputs/Baseline/Baseline_ssp245_magicc_202403250948.csv", header = TRUE)
baseline_45[16,118]
baseline_85 <- read.csv("data/MAGICC/Outputs/Baseline/Baseline_ssp585_magicc_202403251009.csv", header = TRUE)
baseline_85[16,118]

## 7290 Mt reduction in CO2 beginning in 2050 
## introduce this change into the MAGICC template for RCP 4.5

### add additional methane to the baseline scenario
### Zhang, RCP 4.6
#2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2099
additional_methane.Z4.5 <- wCH4_Z[c(60, 70, 80, 90, 100, 110, 120, 130, 139),3] - wCH4_Z[20,3]
additional_methane.Z8.5 <- wCH4_Z[c(60, 70, 80, 90, 100, 110, 120, 130, 139),5] - wCH4_Z[20,5]

additional_methane.K4.5 <- wCH4_K[c(60, 70, 80, 90, 100, 110, 120, 130, 139),4] - wCH4_K[20,4]
additional_methane.K8.5 <- wCH4_K[c(60, 70, 80, 90, 100, 110, 120, 130, 139),6] - wCH4_K[20,6]


template <- read.csv("data/MAGICC/Templates/Baseline/rcmip_ssp245_template.csv", header = TRUE)
names <- c("model", "region", "scenario","unit", "variable", 
           "2015", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
names(template) <- names
methane <- template[2, c(7:15)]
methane_additional <- methane + additional_methane.Z4.5
template[2, c(7:15)] <- methane_additional
write.csv(template, "data/MAGICC/Templates/Baseline_additional_wetland_methane/ssp245_zhang.csv", 
          row.names = FALSE)

methane_additional <- methane + additional_methane.K4.5
template[2, c(7:15)] <- methane_additional
write.csv(template, "data/MAGICC/Templates/Baseline_additional_wetland_methane/ssp245_kleinen.csv", 
          row.names = FALSE)


template <- read.csv("data/MAGICC/Templates/Baseline/rcmip_ssp585_template.csv", header = TRUE)
names <- c("model", "region", "scenario","unit", "variable", 
           "2015", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
names(template) <- names
methane_additional <- methane + additional_methane.Z8.5
template[2, c(7:15)] <- methane_additional
write.csv(template, "data/MAGICC/Templates/Baseline_additional_wetland_methane/ssp585_zhang.csv", 
          row.names = FALSE)

methane_additional <- methane + additional_methane.K8.5
template[2, c(7:15)] <- methane_additional
write.csv(template, "data/MAGICC/Templates/Baseline_additional_wetland_methane/ssp585_kleinen.csv", 
          row.names = FALSE)



### Output: effect on global temperature anomaly
baseline_45z <- read.csv("data/MAGICC/Outputs/Baseline_additional_wetland_methane/AWM_ssp245_zhang_magicc_202403251020.csv", header = TRUE)
baseline_45z[16,118]
baseline_45k <- read.csv("data/MAGICC/Outputs/Baseline_additional_wetland_methane/AWM_ssp245_kleinen_magicc_202403251018.csv", header = TRUE)
baseline_45k[16,118]
baseline_85z <- read.csv("data/MAGICC/Outputs/Baseline_additional_wetland_methane/AWM_ssp85_zhang_magicc_202403251030.csv", header = TRUE)
baseline_85z[16,118]
baseline_85k <- read.csv("data/MAGICC/Outputs/Baseline_additional_wetland_methane/AWM_ssp85_kleinen_magicc_202403251033.csv", header = TRUE)
baseline_85k[16,118]


#### along with the additional methane, take away the CO2 associated with 
#### the global transportation sector reaching net zero



template <- read.csv("data/MAGICC/Templates/Baseline/rcmip_ssp245_template.csv", header = TRUE)
names <- c("model", "region", "scenario","unit", "variable", 
           "2015", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
names(template) <- names
methane <- template[2, c(7:15)]
methane_additional <- methane + additional_methane.Z4.5
template[2, c(7:15)] <- methane_additional
CO2 <- template[5, c(10:15)]
CO2_modified <- CO2 - 7290
template[5, c(10:15)] <- CO2_modified
write.csv(template, "data/MAGICC/Templates/Baseline_awm_Global_transport/ssp245_zhang.csv", 
          row.names = FALSE)


methane_additional <- methane + additional_methane.K4.5
template[2, c(7:15)] <- methane_additional
CO2 <- template[5, c(10:15)]
CO2_modified <- CO2 - 7290
template[5, c(10:15)] <- CO2_modified
write.csv(template, "data/MAGICC/Templates/Baseline_awm_Global_transport/ssp245_kleinen.csv", 
          row.names = FALSE)


template <- read.csv("data/MAGICC/Templates/Baseline/rcmip_ssp585_template.csv", header = TRUE)
names <- c("model", "region", "scenario","unit", "variable", 
           "2015", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
names(template) <- names
methane_additional <- methane + additional_methane.Z8.5
template[2, c(7:15)] <- methane_additional
CO2 <- template[5, c(10:15)]
CO2_modified <- CO2 - 7290
template[5, c(10:15)] <- CO2_modified
write.csv(template, "data/MAGICC/Templates/Baseline_awm_Global_transport/ssp585_zhang.csv", 
          row.names = FALSE)

methane_additional <- methane + additional_methane.K8.5
template[2, c(7:15)] <- methane_additional
CO2 <- template[5, c(10:15)]
CO2_modified <- CO2 - 7290
template[5, c(10:15)] <- CO2_modified
write.csv(template, "data/MAGICC/Templates/Baseline_awm_Global_transport/ssp585_kleinen.csv", 
          row.names = FALSE)








#### call in the resulting output effect on global temperature

## ssp 4.5 Zhang
baseline_awm_45z <- read.csv("data/MAGICC/Outputs/Baseline_additional_wetland_methane/AWM_ssp245_zhang_magicc_202403251020.csv", header = TRUE)
## temp in 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100
baseline_awm_45z_temp <- baseline_awm_45z[16,c(38, 48, 58, 68, 78, 88, 98, 108, 118)]

output_awm_gt_45z <- read.csv("data/MAGICC/Outputs/Baseline_AWM_global_transport/awm_global_transport_ssp245_zhang_magicc_202403251103.csv", header = TRUE)
## temp in 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100
output_awm_gt_45z_temp <- output_awm_gt_45z[16,c(38, 48, 58, 68, 78, 88, 98, 108, 118)]

additional_methane <- Z.slope*baseline_awm_45z_temp + Z.int
less_methane <- Z.slope*output_awm_gt_45z_temp + Z.int
methane.difference.45Z <- additional_methane - less_methane  ##Tg/year



## ssp 4.5  Kleinen

baseline_awm_45k <- read.csv("data/MAGICC/Outputs/Baseline_additional_wetland_methane/AWM_ssp245_kleinen_magicc_202403251018.csv", header = TRUE)
## temp in 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100
baseline_awm_45k_temp <- baseline_awm_45k[16,c(38, 48, 58, 68, 78, 88, 98, 108, 118)]

output_awm_gt_45k <- read.csv("data/MAGICC/Outputs/Baseline_AWM_global_transport/awm_global_transport_ssp245_kleinen_magicc_202403251102.csv", header = TRUE)
## temp in 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100
output_awm_gt_45k_temp <- output_awm_gt_45k[16,c(38, 48, 58, 68, 78, 88, 98, 108, 118)]

additional_methane <- K.slope*baseline_awm_45k_temp + K.int
less_methane <- K.slope*output_awm_gt_45k_temp + K.int
methane.difference.45K <- additional_methane - less_methane  ##Tg



## ssp 8.5 Zhang
baseline_awm_85z <- read.csv("data/MAGICC/Outputs/Baseline_additional_wetland_methane/AWM_ssp85_zhang_magicc_202403251030.csv", header = TRUE)
## temp in 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100
baseline_awm_85z_temp <- baseline_awm_85z[16,c(38, 48, 58, 68, 78, 88, 98, 108, 118)]

output_awm_gt_85z <- read.csv("data/MAGICC/Outputs/Baseline_AWM_global_transport/awm_global_transport_ssp585_zhang_magicc_202403251102.csv", header = TRUE)
## temp in 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100
output_awm_gt_85z_temp <- output_awm_gt_85z[16,c(38, 48, 58, 68, 78, 88, 98, 108, 118)]

additional_methane <- Z.slope*baseline_awm_85z_temp + Z.int
less_methane <- Z.slope*output_awm_gt_85z_temp + Z.int
methane.difference.85Z <- additional_methane - less_methane  ##Tg/year


## ssp 4.5  Kleinen

baseline_awm_85k <- read.csv("data/MAGICC/Outputs/Baseline_additional_wetland_methane/AWM_ssp85_kleinen_magicc_202403251033.csv", header = TRUE)
## temp in 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100
baseline_awm_85k_temp <- baseline_awm_85k[16,c(38, 48, 58, 68, 78, 88, 98, 108, 118)]

output_awm_gt_85k <- read.csv("data/MAGICC/Outputs/Baseline_AWM_global_transport/awm_global_transport_ssp585_kleinen_magicc_202403251103.csv", header = TRUE)
## temp in 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100
output_awm_gt_85k_temp <- output_awm_gt_85k[16,c(38, 48, 58, 68, 78, 88, 98, 108, 118)]

additional_methane <- K.slope*baseline_awm_85k_temp + K.int
less_methane <- K.slope*output_awm_gt_85k_temp + K.int
methane.difference.85K <- additional_methane - less_methane  ##Tg


### incorporate these methane reductions into new MAGICC templates

### Zhang 4.5
template <- read.csv("data/MAGICC/Templates/Baseline_awm_Global_transport/ssp245_zhang.csv", header = TRUE)
names <- c("model", "region", "scenario","unit", "variable", 
           "2015", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
names(template) <- names
methane <- template[2, c(7:15)]
methane_reduction <- methane - methane.difference.45Z
template[2, c(7:15)] <- methane_reduction
write.csv(template, "data/MAGICC/Templates/Baseline_awm_gt_wetland_methane_reduction/ssp245_zhang.csv", 
          row.names = FALSE)


### Kleinen 4.5
template <- read.csv("data/MAGICC/Templates/Baseline_awm_Global_transport/ssp245_kleinen.csv", header = TRUE)
names <- c("model", "region", "scenario","unit", "variable", 
           "2015", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
names(template) <- names
methane <- template[2, c(7:15)]
methane_reduction <- methane - methane.difference.45K
template[2, c(7:15)] <- methane_reduction
write.csv(template, "data/MAGICC/Templates/Baseline_awm_gt_wetland_methane_reduction/ssp245_kleinen.csv", 
          row.names = FALSE)


### Zhang 8.5
template <- read.csv("data/MAGICC/Templates/Baseline_awm_Global_transport/ssp585_zhang.csv", header = TRUE)
names <- c("model", "region", "scenario","unit", "variable", 
           "2015", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
names(template) <- names
methane <- template[2, c(7:15)]
methane_reduction <- methane - methane.difference.85Z
template[2, c(7:15)] <- methane_reduction
write.csv(template, "data/MAGICC/Templates/Baseline_awm_gt_wetland_methane_reduction/ssp585_zhang.csv", 
          row.names = FALSE)


### Kleinen 8.5
template <- read.csv("data/MAGICC/Templates/Baseline_awm_Global_transport/ssp585_kleinen.csv", header = TRUE)
names <- c("model", "region", "scenario","unit", "variable", 
           "2015", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
names(template) <- names
methane <- template[2, c(7:15)]
methane_reduction <- methane - methane.difference.85K
template[2, c(7:15)] <- methane_reduction
write.csv(template, "data/MAGICC/Templates/Baseline_awm_gt_wetland_methane_reduction/ssp585_kleinen.csv", 
          row.names = FALSE)



### Final outputs

overall_45z <- read.csv("data/MAGICC/Outputs/Baseline_AWM_global_transport_AMR/final_ssp245_zhang_magicc_202403251133.csv", header = TRUE)
overall_45z[16,118]
overall_45k <- read.csv("data/MAGICC/Outputs/Baseline_AWM_global_transport_AMR/final_ssp245_kleinen_magicc_202403251131.csv", header = TRUE)
overall_45k[16,118]
overall_85z <- read.csv("data/MAGICC/Outputs/Baseline_AWM_global_transport_AMR/final_ssp585_zhang_magicc_202403251134.csv", header = TRUE)
overall_85z[16,118]
overall_85k <- read.csv("data/MAGICC/Outputs/Baseline_AWM_global_transport_AMR/final_ssp585_kleinen_magicc_202403251133.csv", header = TRUE)
overall_85k[16,118]

## percent difference
(output_awm_gt_45z[16,118] - overall_45z[16,118])/output_awm_gt_45z[16,118]*100
(output_awm_gt_45k[16,118] - overall_45k[16,118])/output_awm_gt_45k[16,118]*100

(output_awm_gt_85z[16,118] - overall_85z[16,118])/output_awm_gt_85z[16,118]*100
(output_awm_gt_85k[16,118] - overall_85k[16,118])/output_awm_gt_85k[16,118]*100
