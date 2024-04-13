



#### Figure 2



#### Relationship between Temp and additional CH4 production
{
  ### read in global temp projection (this is change in temp in K)
  dTemp <- read.csv("data/Global_temperature_projections_magicc_single.csv")
  
  ### read in global wetland methane projection (this is all methane from wetlands in Tg/yr)
  wCH4_K <- read.csv("data/Global_methane_projections_kleinen.csv")   
  
  
  #ssp 4.5
  CH4.ssp45 <- wCH4_K[60:139, 4] - wCH4_K[20,4]  ## additional methane in Tg/yr
  dT.ssp45 <- dTemp[26:105, 2]
  
  #ssp 7.0
  CH4.ssp70 <- wCH4_K[60:139, 5] - wCH4_K[20,5]  ## additional methane in Tg/yr
  dT.ssp70 <- dTemp[26:105, 3]
  
  
  
  
  fit.ex.45 <- lm(log(CH4.ssp45) ~ dT.ssp45)
  summary(fit.ex.45)
  new.dat <- data.frame(Kleinen.temp = c(1,2,3,4,5))
  
  x.new <- seq(1,5, by = 0.1)
  new.dat <- data.frame(dT.ssp45 = seq(1,5, by = 0.1))
  
  predict(fit.ex.45, newdata = new.dat, interval = 'confidence') ## 95% confidence interval
  
  y.new <- exp(predict(fit.ex.45, new.dat))
  lwr <- exp(predict(fit.ex.45, newdata = new.dat, interval = 'prediction')[,2])
  upr <- exp(predict(fit.ex.45, newdata = new.dat, interval = 'prediction')[,3])
  lwr.ci <- exp(predict(fit.ex.45, newdata = new.dat, interval = 'confidence')[,2])
  upr.ci <- exp(predict(fit.ex.45, newdata = new.dat, interval = 'confidence')[,3])
  
  ####### ssp 70
  fit.ex.70 <- lm(log(CH4.ssp70) ~ dT.ssp70)
  summary(fit.ex.70)
  new.dat <- data.frame(Kleinen.temp = c(1,2,3,4,5))
  
  x.new <- seq(1,5, by = 0.1)
  new.dat <- data.frame(dT.ssp70 = seq(1,5, by = 0.1))
  
  predict(fit.ex.70, newdata = new.dat, interval = 'confidence') ## 95% confidence interval
  
  y.new7 <- exp(predict(fit.ex.70, new.dat))
  lwr7 <- exp(predict(fit.ex.70, newdata = new.dat, interval = 'prediction')[,2])
  upr7 <- exp(predict(fit.ex.70, newdata = new.dat, interval = 'prediction')[,3])
  lwr.ci7 <- exp(predict(fit.ex.70, newdata = new.dat, interval = 'confidence')[,2])
  upr.ci7 <- exp(predict(fit.ex.70, newdata = new.dat, interval = 'confidence')[,3])
  
  par(mfrow = c(1,1), mar = c(4,4,1,1))
  plot(dT.ssp45, CH4.ssp45, pch = 17, 
       xlab = "Global Temperature Anomaly (K)", 
       ylab = "Wetland Methane Emissions (Tg/yr)", 
       xlim = c(1,4), ylim = c(0,300))
  points(dT.ssp70, CH4.ssp70, pch =4)
  legend("topleft", legend = c( "SSP 4.5", "SSP 7.0"), 
         pch = c( 17, 4), cex = 0.7, col = "black")
  polygon(c(x.new, rev(x.new)), c(lwr.ci, rev(upr.ci)), col = '#00000055', border = NA)
  polygon(c(x.new, rev(x.new)), c(lwr.ci7, rev(upr.ci7)), col = '#00000022', border = NA)
  points(x.new, y.new, type = 'l', col = "blue", lwd = 2)
  points(x.new, y.new7, type = 'l', col = "blue", lwd = 2, lty = 2)
  points(x.new, lwr, type = 'l', col = "red")
  points(x.new, upr, type = 'l', col = "red")
  points(x.new, lwr7, type = 'l', col = "red", lty = 2)
  points(x.new, upr7, type = 'l', col = "red", lty = 2)
  text(2, 250, "SSP 4.5\ny = 8.4*3.1^x\n R^2 = 0.91", col = "black", cex = 0.9)
  text(3.5, 100, "SSP 7.0\ny = 20.1*2.0^x\n R^2 = 0.90", col = "black", cex = 0.9)
  
  
  exp(fit.ex.45$coefficients[1])
  exp(fit.ex.45$coefficients[2])
  exp(fit.ex.70$coefficients[1])
  exp(fit.ex.70$coefficients[2])
  
  
  
  
  ### prediction ranges, upper/lower
  
  fit.upper45 <- lm(log(upr)~x.new)
  fit.lower45 <- lm(log(lwr)~x.new)
  
  
  a.high.45 <- exp(fit.upper45$coefficients[1])
  b.high.45 <- exp(fit.upper45$coefficients[2])
  a.low.45 <-  exp(fit.lower45$coefficients[1])
  b.low.45 <-  exp(fit.lower45$coefficients[2])
  
  
  fit.upper70 <- lm(log(upr7)~x.new)
  fit.lower70 <- lm(log(lwr7)~x.new)
  
  
  a.high.70 <- exp(fit.upper70$coefficients[1])
  b.high.70 <- exp(fit.upper70$coefficients[2])
  a.low.70 <-  exp(fit.lower70$coefficients[1])
  b.low.70 <-  exp(fit.lower70$coefficients[2])
  
  low <- c(a.low.45, b.low.45, a.low.70, b.low.70)
  high <- c(a.high.45, b.high.45, a.high.70, b.high.70)
  coefficient.table <- data.frame(low, high)
  coefficient.table
  
  }


### build a results table
year <- c(seq(2020, 2100, by = 10))
temp45 <- dTemp[seq(26, 106, by =10), 2]
temp70 <- dTemp[seq(26, 106, by =10), 3]

additional_methane45 <- wCH4_K[c(60, 70, 80, 90, 100, 110, 120, 130, 139), 4] - wCH4_K[20,4]
additional_methane70 <- wCH4_K[c(60, 70, 80, 90, 100, 110, 120, 130, 139), 5] - wCH4_K[20,5]


results <- data.frame(year, temp45, temp70, additional_methane45, additional_methane70)

par(mfrow = c(2,2), mar = c(2,4,2,1))
plot(results$year, results$temp45, type = 'l', ylim = c(0,4), main = "SSP 4.5", 
     ylab = "Temperature anomaly (K)")
plot(results$year, results$temp70, type = 'l', ylim = c(0,4), main = "SSP 7.0", 
     ylab = " ")
plot(results$year, results$additional_methane45, type = 'l', ylim = c(0, 250), 
     ylab = "Additional CH4 (Tg)")
plot(results$year, results$additional_methane70, type = 'l', ylim = c(0, 250), 
     ylab = " ")



### read in baseline temperature (This is the same as line 119 - 120)

base_45 <- read.csv("data/MAGICC/Outputs/Single_run/Baseline/basline_45_single_magicc_202404011512.csv")
# Temp
base_45[16, c(38, 48, 58, 68, 78, 88, 98, 108, 118)]  ## 2020-2100

base_70 <- read.csv("data/MAGICC/Outputs/Single_run/Baseline/baseline_70_single_magicc_202404121148.csv")
# Temp
base_70[16, c(38, 48, 58, 68, 78, 88, 98, 108, 118)]  ## 2020-2100


#### read in baseline CO2 levels and create an adjusted CO2 scenario

template_45 <- read.csv("data/MAGICC/Templates/Baseline/rcmip_ssp245_template.csv")
# CO2 AFLO
template_45[4, c(7:15)]
# CO2 Fossil & Industrial
template_45[5, c(7:15)]
results$CO2_45 <- unlist(template_45[4, c(7:15)] + template_45[5, c(7:15)])

template_70 <- read.csv("data/MAGICC/Templates/Baseline/rcmip_ssp370_template.csv")
# CO2 AFLO
template_70[4, c(7:15)]
# CO2 Fossil & Industrial
template_70[5, c(7:15)]
results$CO2_70 <- unlist(template_70[4, c(7:15)] + template_70[5, c(7:15)])


results$CO2_adjusted_45 <- results$CO2_45 - c(0,0,0, 7290, 7290, 7290, 7290, 7290, 7290)
results$CO2_adjusted_70 <- results$CO2_70 - c(0,0,0, 7290, 7290, 7290, 7290, 7290, 7290)


### new temperature with additional wetland methane

newT_45 <- read.csv("data/MAGICC/Outputs/Single_run/Additional_methane/additional_45_single_magicc_202404011519.csv")
# Temp
results$T_adjusted_45 <- unlist(newT_45[16, c(38, 48, 58, 68, 78, 88, 98, 108, 118)])  ## 2020-2100

newT_70 <- read.csv("data/MAGICC/Outputs/Single_run/Additional_methane/additional_70_single_magicc_202404121844.csv")
# Temp
results$T_adjusted_70 <- unlist(newT_70[16, c(38, 48, 58, 68, 78, 88, 98, 108, 118)])  ## 2020-2100












par(mfrow = c(3,2), mar = c(2,4,2,1))
plot(results$year, results$CO2_45  , type = 'l', main = "SSP 4.5", 
     ylab = "CO2 Emissions (MtC)")
points(results$year, results$CO2_adjusted_45, type = 'l', lty = 2)
plot(results$year, results$CO2_70, type = 'l',  main = "SSP 7.0", 
     ylab = " ")
points(results$year, results$CO2_adjusted_70, type = 'l', lty = 2)

plot(results$year, results$additional_methane45, type = 'l', ylim = c(0, 250), 
     ylab = "Additional CH4 (Tg)")
plot(results$year, results$additional_methane70, type = 'l', ylim = c(0, 250), 
     ylab = " ")

plot(results$year, results$temp45, type = 'l', ylim = c(0,4), 
     ylab = "Temperature anomaly (K)")
points(results$year, results$T_adjusted_45, type = 'l', col = "red")
plot(results$year, results$temp70, type = 'l', ylim = c(0,4), 
     ylab = " ")
points(results$year, results$T_adjusted_70, type = 'l', col = "red")





