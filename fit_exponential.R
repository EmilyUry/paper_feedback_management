

## exponential fit exploration



### read in global temp projection (this is change in temp in K)
dTemp <- read.csv("data/Global_temperature_projections.csv")

### read in global wetland methane projection (this is all methane from wetlands in Tg/yr)
wCH4_K <- read.csv("data/Global_methane_projections_kleinen.csv")   

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




#### expontential fit 

fit.ex <- lm(log(Kleinen.CH4) ~ Kleinen.temp)
summary(fit.ex)
new.dat <- data.frame(Kleinen.temp = c(1,2,3,4,5))

x.new <- seq(1,5, by = 0.1)
new.dat <- data.frame(Kleinen.temp = seq(1,5, by = 0.1))

predict(fit.ex, newdata = new.dat, interval = 'confidence') ## 95% confidence interval

y.new <- exp(predict(fit.ex, new.dat))
lwr <- exp(predict(fit.ex, newdata = new.dat, interval = 'prediction')[,2])
upr <- exp(predict(fit.ex, newdata = new.dat, interval = 'prediction')[,3])
lwr.ci <- exp(predict(fit.ex, newdata = new.dat, interval = 'confidence')[,2])
upr.ci <- exp(predict(fit.ex, newdata = new.dat, interval = 'confidence')[,3])


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
text(2.5, 300, "Kleinen\n ln(y) = 3.1 + 0.67x\n y = 22.20*1.95^x \n R^2 = 0.83", col = "black", cex = 0.9)





######
#### expontential fit for each ssp individually



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
text(2, 250, "SSP 4.5\ny = 7.3*3.3^x\n R^2 = 0.91", col = "black", cex = 0.9)
text(3.5, 100, "SSP 7.0\ny = 20.1*2.0^x\n R^2 = 0.90", col = "black", cex = 0.9)


7.3*3.3^2


