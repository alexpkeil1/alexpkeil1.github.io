#R code to make the global temperature data plot (made in R v.3.1.2), Alex Keil
gw <- read.csv("https://www.ncdc.noaa.gov/cag/time-series/global/globe/land_ocean/12/12/1880-2016.csv", skip=3, header=TRUE)
par(bg=1, col.axis="white", col="white", col.main='white', col.lab="white")

#plot the whole range of the data with a loess fit and a piecewise linear fit
plot(gw$Year, gw$Value, pch=20, col="white", xlab="Year", ylab="Temp increase (C)")
title(main="NOAA global temperature data (annual average)")
lines(gw$Year, loess(gw$Value ~gw$Year, span=.1)$fitted, col="red", lwd=5)
j=1
for(i in 1:9){
  reg = with(gw[j:(j+ifelse(j+17>dim(gw)[1], dim(gw)[1]-j, 14)),], lm(Value ~ I(Year-min(Year)))$coef)
  segments(x0=min(gw$Year-1)+j, y0=reg[1], x1 = min(gw$Year-1)+j+14, y1=reg[1]+reg[2]*14, col="yellow", lwd=4)
  j = j+15
}
legend("topleft", col=c("yellow", "red"), c("Piecewise linear (15 yrs)", "Loess (a=0.1)"), lty=1, bty="n", lwd=4)


#just the last 15 years
plot(gw[gw$Year>=2000,]$Year, gw[gw$Year>=2000,]$Value, pch=20, xlab="Year", ylab="Temp increase from 1950 (C)")
title(main="NOAA global temperature data, 2000-2015")
lines(gw[gw$Year>=2000,]$Year, loess(gw[gw$Year>=2000,]$Value ~gw[gw$Year>=2000,]$Year, span=.6)$fitted, col="red", lwd=5)
abline(reg=lm(gw[gw$Year>=2000,]$Value ~ gw[gw$Year>=2000,]$Year), col="yellow", lwd=4)
legend("topleft", col=c("yellow", "red"), c("Piecewise linear", "Loess (a=0.6)"), lty=1, bty="n", lwd=4)



plot(gw[gw$Year>2000,]$Year, gw[gw$Year>2000,]$Value, pch=20, xlab="Year", ylab="Temp increase (C)")
title(main="NOAA global temperature data, 2001-2015")
lines(gw[gw$Year>2000,]$Year, loess(gw[gw$Year>2000,]$Value ~gw[gw$Year>2000,]$Year, span=.6)$fitted, col="red", lwd=5)
abline(reg=lm(gw[gw$Year>2000,]$Value ~ gw[gw$Year>2000,]$Year), col="yellow", lwd=4)
legend("topleft", col=c("yellow", "red"), c("Piecewise linear", "Loess (a=0.6)"), lty=1, bty="n", lwd=4)


