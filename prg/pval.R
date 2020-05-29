#p value function
#### use 95% confidence intervals

  #1st curve
	lower.cl <- 1.01
	upper.cl <- 10.01
  #2nd curve
	lower.cl2 <- 0.85
	upper.cl2 <- 2.45		
	
	alpha <- 0.05
	
########
	xlimits.mine <- c(lower.cl*.6, upper.cl*1.4)

	
	lnRR <- (log(upper.cl) + log(lower.cl))/2
	(lnse <- (log(upper.cl) - log(lower.cl))/(2*qnorm(1-alpha/2)))
	set.i <- c(NA, NA, NA, NA, NA, NA)
	RR <- exp(lnRR)
	
	par(oma=c(0,0,0,3))
	plot(x=xlimits.mine, y=c(0,1), xlim=xlimits.mine, col=NULL, log="x", xlab="Relative Risk", ylab="p value", main="P value function for hypothetical relative risks\nassuming only random error", bty="n", pty="s", las=1)
	for(i in seq(-10, 10, 0.1)){
		z <- i
		rr <- RR*exp(z*lnse)
		p <- (pnorm(abs(z), lower.tail=F))*2
		ci <-((p-1)*100)
		row.i <- cbind(z, p, rr, lnRR, lnse, ci)
		set.i <- rbind( row.i, set.i)
		}
		set.i <- data.frame(na.exclude(set.i))
	lines(x=set.i$rr, y=set.i$p, pch=20, lwd=2)
	par(new=T)
	plot(x=set.i$rr, y=set.i$ci, col=NULL, axes=F, ann=F)
	axis(4, labels=abs(seq(0, -100, -20)), at=seq(0,-100, -20), las=1)
	text(labels="Confidence level %", x=max(set.i$rr)*1.15, y=(min(set.i$ci))/2, xpd=NA, srt=270)
	box(bty="o")
	legend("topright", legend=c(paste("RR(95% CI) = ", as.character(round(RR, digits=2)), " (",as.character(round(lower.cl, digits=2)),", ", as.character(round(upper.cl, digits=2)) ,")", sep=""), ""), cex=.8, bty="n")
#set.i

#2nd curve, more precise
	lower.cl <- lower.cl2
	upper.cl <- upper.cl2
		
########	
	(lnRR <- (log(upper.cl) + log(lower.cl))/2)
	(lnse <- (log(upper.cl) - log(lower.cl))/(2*qnorm(1-alpha/2)))
	set.i <- c(NA, NA, NA, NA, NA, NA)
	RR <- exp(lnRR)
	
#	par(oma=c(0,0,0,3), new=T)
	par(new=T)
	plot(x=xlimits.mine, y=c(0,1), xlim=xlimits.mine, col=NULL, axes=F, ann=F,  las=1, log="x")
	for(i in seq(-10, 10, 0.01)){
		z <- i
		rr <- RR*exp(z*lnse)
		p <- (pnorm(abs(z), lower.tail=F))*2
		ci <-((p-1)*100)
		row.i <- cbind(z, p, rr, lnRR, lnse, ci)
		set.i <- rbind( row.i, set.i)
		}
	set.i <- data.frame(na.exclude(set.i))
	lines(x=set.i$rr, y=set.i$p, pch=20, lwd=2, col="blue")
		
	legend("topright", legend=c("",paste("RR(95% CI) = ", as.character(round(RR, digits=2)), " (",as.character(round(lower.cl, digits=2)),", ", as.character(round(upper.cl, digits=2)) ,")", sep="")), cex=.8, text.col="blue")

	abline(v=1, col=1)