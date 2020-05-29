#weibull distribution, varying rho from 0.1 to 3.3 by 0.2, probability density function
	
	
dev.new()
par(mfrow=c(3,2), mar=c(2,2,2,2), pty="m")	#draws four plots l to r
	#gamma <- 1
for(gamma in seq(0.5, 3, 0.5)){
	#create a line of the function based on the current iteration of gamma, set rho to 0.5... necessary to initially draw plot to correct size and include a label for gamma value
	curve(0.5*gamma*x^(gamma-1)*exp(-0.5*x^(gamma)), from=0, to=4, ylim=c(0, 1.6), col=NULL, ylab=expression(rho), xlab="time", bty="n")
	text(y=1.55, x=3.5, labels=paste("gamma = ", gamma))

for(i in seq(0.1, 2.5, .8)){
	#iterate this function across a series of values of rho
	curve(i*gamma*x^(gamma-1)*exp(-i*x^(gamma)), from=0, to=15, add=T, n=500, col=(i*10/8+1))
	string <- as.character(i)
	text(y=i/2+0.2, x=4, labels =(paste(expression(rho), i)), col=(i*10/8+1))
	}
	}
	par( mfg=c(1,1))
	title(main=("probability density functions for the Weibull distributions"))
	
dev.off()