


#### idea generation for the bonus mitigation project



### read in global temp projection (this is change in temp in K)

dTemp <- read.csv("data/Global_temperature_projections.csv")

### read in global wetland methane projection (this is all methane from wetlands in Tg/yr)

wCH4 <- read.csv("data/Global_methane_projections.csv")





### using 1980 as a baseline -- how does additional methane (2020-2099) relate to global temperature anomally

#rcp 2.6
CH4.rcp26 <- wCH4[60:139, 2] - wCH4[20,2]  ## additional methane in Tg/yr
dT.rcp26 <- dTemp[26:105, 2]

#rcp 4.5
CH4.rcp45 <- wCH4[60:139, 3] - wCH4[20,3]  ## additional methane in Tg/yr
dT.rcp45 <- dTemp[26:105, 3]

#rcp 6.0
CH4.rcp60 <- wCH4[60:139, 4] - wCH4[20,4]  ## additional methane in Tg/yr
dT.rcp60 <- dTemp[26:105, 4]

#rcp 8.5
CH4.rcp85 <- wCH4[60:139, 5] - wCH4[20,5]  ## additional methane in Tg/yr
dT.rcp85 <- dTemp[26:105, 5]


plot(dT.rcp26, CH4.rcp26, pch = 16, cex = 0.7, col = "#121212dd", 
     ylim = c(0, 200), xlim = c(1, 5))
points(dT.rcp45, CH4.rcp45, pch = 16, cex = 0.7, col = "#ff0000dd")
points(dT.rcp60, CH4.rcp60, pch = 16, cex = 0.7, col = "#0000ffdd")
points(dT.rcp85, CH4.rcp85, pch = 16, cex = 0.7, col = "#00ffdd")



all.temp <- c(dT.rcp26, dT.rcp45, dT.rcp60, dT.rcp85)
all.CH4 <- c(CH4.rcp26, CH4.rcp45, CH4.rcp60, CH4.rcp85)
col <- rep(c("#121212dd", "#ff0000dd", "#0000FFdd", "#00ffdddd"), each = 80)



plot(all.temp, all.CH4, pch = 16, cex = 0.7, col = col,
     xlab = "Global Temperature Anomally (K)", 
     ylab = "Additional CH4 (Tg yr^-1)")
legend("topleft", legend = c("RCP 2.6", "RCP 4.5", "RCP 6.0", "RCP 8.5"), 
       pch = 16, cex = 0.7, col = c("#121212dd", "#ff0000dd", "#0000FFdd", "#00ffdddd"))
fit <- lm(all.CH4 ~ all.temp)
abline(fit)
summary(fit)
text(3, 175, "y = 43.95x - 26.72\n R^2 = 0.98")





#### Example 1 - US 2021 Aviation Climate Action Plan
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
