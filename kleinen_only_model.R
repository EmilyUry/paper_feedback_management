


#### idea generation for the bonus mitigation project

library(ggplot2)



#### Relationship between Temp and additional CH4 production
{
### read in global temp projection (this is change in temp in K)
dTemp <- read.csv("data/Global_temperature_projections.csv")

### read in global wetland methane projection (this is all methane from wetlands in Tg/yr)
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


## combine all together
Kleinen.temp <- c(dT.ssp19, dT.ssp26, dT.ssp45, dT.ssp70, dT.ssp85)
Kleinen.CH4 <- c(CH4.ssp19, CH4.ssp26, CH4.ssp45, CH4.ssp70, CH4.ssp85)
ssp <- rep(c("ssp19", "ssp26", "ssp45", "ssp70", "ssp85"), each = 80)


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



#### Example 2 -- Achieving net zero in the global transportation sector by 2050
##

### Run baseline MAGICC scenarios
### output: baseline global temperature anomaly
baseline_45 <- read.csv("data/MAGICC/Outputs/Single_run/Baseline/basline_45_single_magicc_202404011512.csv", header = TRUE)
baseline_45[16,118]
baseline_85 <- read.csv("data/MAGICC/Outputs/Single_run/Baseline/baseline_85_single_magicc_202404011450.csv", header = TRUE)
baseline_85[16,118]

## 7290 Mt reduction in CO2 beginning in 2050 
## introduce this change into the MAGICC template for RCP 4.5

### add additional methane to the baseline scenario
#2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2099
additional_methane.K4.5 <- wCH4_K[c(60, 70, 80, 90, 100, 110, 120, 130, 139),4] - wCH4_K[20,4]
additional_methane.K4.5[9] #2100 
additional_methane.K8.5 <- wCH4_K[c(60, 70, 80, 90, 100, 110, 120, 130, 139),6] - wCH4_K[20,6]
additional_methane.K8.5[9] #2100 


template <- read.csv("data/MAGICC/Templates/Baseline/rcmip_ssp245_template.csv", header = TRUE)
names <- c("model", "region", "scenario","unit", "variable", 
           "2015", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
names(template) <- names
methane <- template[2, c(7:15)]
methane_additional <- methane + additional_methane.K4.5
template[2, c(7:15)] <- methane_additional
write.csv(template, "data/MAGICC/Templates/Baseline_additional_wetland_methane/ssp245_kleinen.csv", 
          row.names = FALSE)


template <- read.csv("data/MAGICC/Templates/Baseline/rcmip_ssp585_template.csv", header = TRUE)
names <- c("model", "region", "scenario","unit", "variable", 
           "2015", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
names(template) <- names
methane_additional <- methane + additional_methane.K8.5
template[2, c(7:15)] <- methane_additional
write.csv(template, "data/MAGICC/Templates/Baseline_additional_wetland_methane/ssp585_kleinen.csv", 
          row.names = FALSE)


## RUN MAGICC ON NEW TEMPLATES


### Output: effect on global temperature anomaly
baseline_45k <- read.csv("data/MAGICC/Outputs/Single_run/Additional_methane/additional_45_single_magicc_202404011519.csv", header = TRUE)
baseline_45k[16,118]
baseline_85k <- read.csv("data/MAGICC/Outputs/Single_run/Additional_methane/additional_85_sinlge_magicc_202404011519.csv", header = TRUE)
baseline_85k[16,118]


#### along with the additional methane, take away the CO2 associated with 
#### the global transportation sector reaching net zero



template <- read.csv("data/MAGICC/Templates/Baseline/rcmip_ssp245_template.csv", header = TRUE)
names <- c("model", "region", "scenario","unit", "variable", 
           "2015", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
names(template) <- names
methane <- template[2, c(7:15)]
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
methane_additional <- methane + additional_methane.K8.5
template[2, c(7:15)] <- methane_additional
CO2 <- template[5, c(10:15)]
CO2_modified <- CO2 - 7290
template[5, c(10:15)] <- CO2_modified
write.csv(template, "data/MAGICC/Templates/Baseline_awm_Global_transport/ssp585_kleinen.csv", 
          row.names = FALSE)


#### call in the resulting output effect on global temperature

## ssp 4.5  Kleinen

baseline_awm_45k <- read.csv("data/MAGICC/Outputs/Single_run/Additional_methane/additional_45_single_magicc_202404011519.csv", header = TRUE)
## temp in 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100
baseline_awm_45k_temp <- baseline_awm_45k[16,c(38, 48, 58, 68, 78, 88, 98, 108, 118)]

output_awm_gt_45k <- read.csv("data/MAGICC/Outputs/Single_run/AWM_Global_transport/awm_45_gt_magicc_202404011528.csv", header = TRUE)
## temp in 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100
output_awm_gt_45k_temp <- output_awm_gt_45k[16,c(38, 48, 58, 68, 78, 88, 98, 108, 118)]

additional_methane <- slope.low*baseline_awm_45k_temp + int.low
less_methane <- slope.low*output_awm_gt_45k_temp + int.low
methane.difference.45.low <- additional_methane - less_methane  ##Tg

additional_methane <- slope.high*baseline_awm_45k_temp + int.high
less_methane <- slope.high*output_awm_gt_45k_temp + int.high
methane.difference.45.high <- additional_methane - less_methane  ##Tg


## ssp 8.5  Kleinen

baseline_awm_85k <- read.csv("data/MAGICC/Outputs/Single_run/Additional_methane/additional_85_sinlge_magicc_202404011519.csv", header = TRUE)
## temp in 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100
baseline_awm_85k_temp <- baseline_awm_85k[16,c(38, 48, 58, 68, 78, 88, 98, 108, 118)]

output_awm_gt_85k <- read.csv("data/MAGICC/Outputs/Single_run/AWM_Global_transport/awm_85_gt_magicc_202404011528.csv", header = TRUE)
## temp in 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100
output_awm_gt_85k_temp <- output_awm_gt_85k[16,c(38, 48, 58, 68, 78, 88, 98, 108, 118)]

additional_methane <- slope.low*baseline_awm_85k_temp + int.low
less_methane <- slope.low*output_awm_gt_85k_temp + int.low
methane.difference.85.low <- additional_methane - less_methane  ##Tg

additional_methane <- slope.high*baseline_awm_85k_temp + int.high
less_methane <- slope.high*output_awm_gt_85k_temp + int.high
methane.difference.85.high <- additional_methane - less_methane  ##Tg


### incorporate these methane reductions into new MAGICC templates

### Kleinen 4.5
template <- read.csv("data/MAGICC/Templates/Baseline_awm_Global_transport/ssp245_kleinen.csv", header = TRUE)
names <- c("model", "region", "scenario","unit", "variable", 
           "2015", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
names(template) <- names
methane <- template[2, c(7:15)]
methane_reduction <- methane - methane.difference.45.low
template[2, c(7:15)] <- methane_reduction
write.csv(template, "data/MAGICC/Templates/Baseline_awm_gt_wetland_methane_reduction/ssp245_low.csv", 
          row.names = FALSE)

methane_reduction <- methane - methane.difference.45.high
template[2, c(7:15)] <- methane_reduction
write.csv(template, "data/MAGICC/Templates/Baseline_awm_gt_wetland_methane_reduction/ssp245_high.csv", 
          row.names = FALSE)


### Kleinen 8.5
template <- read.csv("data/MAGICC/Templates/Baseline_awm_Global_transport/ssp585_kleinen.csv", header = TRUE)
names <- c("model", "region", "scenario","unit", "variable", 
           "2015", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
names(template) <- names
methane <- template[2, c(7:15)]
methane_reduction <- methane - methane.difference.85.low
template[2, c(7:15)] <- methane_reduction
write.csv(template, "data/MAGICC/Templates/Baseline_awm_gt_wetland_methane_reduction/ssp585_low.csv", 
          row.names = FALSE)

methane_reduction <- methane - methane.difference.85.high
template[2, c(7:15)] <- methane_reduction
write.csv(template, "data/MAGICC/Templates/Baseline_awm_gt_wetland_methane_reduction/ssp585_high.csv", 
          row.names = FALSE)



### Final outputs

overall_45low <- read.csv("data/MAGICC/Outputs/Single_run/AWM_GT_methane_reduction/awm_45_gt_low_magicc_202404011555.csv", header = TRUE)
overall_45low[16,118]
overall_45high <- read.csv("data/MAGICC/Outputs/Single_run/AWM_GT_methane_reduction/awm_45_gt_high_magicc_202404011555.csv", header = TRUE)
overall_45high[16,118]
overall_85low <- read.csv("data/MAGICC/Outputs/Single_run/AWM_GT_methane_reduction/awm_85_gt_low_magicc_202404011555.csv", header = TRUE)
overall_85low[16,118]
overall_85high <- read.csv("data/MAGICC/Outputs/Single_run/AWM_GT_methane_reduction/awm_85_gt_high_magicc_202404011556.csv", header = TRUE)
overall_85high[16,118]

## percent difference
(output_awm_gt_45k[16,118] - overall_45low[16,118])/output_awm_gt_45k[16,118]*100
(output_awm_gt_45k[16,118] - overall_45high[16,118])/output_awm_gt_45k[16,118]*100

(output_awm_gt_85k[16,118] - overall_85low[16,118])/output_awm_gt_85k[16,118]*100
(output_awm_gt_85k[16,118] - overall_85high[16,118])/output_awm_gt_85k[16,118]*100

## percent additional temp decrease
(output_awm_gt_45k[16,118] - overall_45low[16,118])/(baseline_45k[16,118] - output_awm_gt_45k[16,118])*100
(output_awm_gt_45k[16,118] - overall_45high[16,118])/(baseline_45k[16,118] - output_awm_gt_45k[16,118])*100

(output_awm_gt_85k[16,118] - overall_85low[16,118])/(baseline_85k[16,118] - output_awm_gt_85k[16,118])*100
(output_awm_gt_85k[16,118] - overall_85high[16,118])/(baseline_85k[16,118] - output_awm_gt_85k[16,118])*100

