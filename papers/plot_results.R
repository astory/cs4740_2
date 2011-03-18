#source('experiment_design.R')
#design=treatments
lineCountsChar=c('10000','100000','996759')
lineCounts=as.numeric(lineCountsChar)
design=read.csv('experiment_design.csv')
scores=read.csv('results.csv')
results=merge(design,scores,by='file')
baseline=93

startplot=function(x,xlab='N-gram length',mainadd='',...){
	plot(score~x,results,type='n',axes=F,
		xlab=xlab,
                ylab='Score',ylim=c(0,120),
		main=paste('Performance',mainadd),
		...
	)
	par(las=2);axis(2,at=c(seq(0,100,20),baseline),labels=c(paste(seq(0,100,20),'%',sep=''),'BL'));par(las=1)
#	abline(h=baseline)
}
startplot.n=function(...) {
	startplot(results$n,...)
	axis(1,at=results$n)
}

startplot.l=function(...) {
	startplot(log(results$size),xlab='Number of words in the training set',...)
	axis(1,at=log(lineCounts),labels=lineCountsChar)
}


posPlot.n=function() {
startplot.n(mainadd='using the full training corpus')

points(score~n,subset(results,size==996759 & smooth.gram=='Off' & unk=='Off'),type='l',lty=1,lwd=1,col=1)
points(score~n,subset(results,size==996759 & smooth.gram=='Off' & unk=='On'),type='l',lty=1,lwd=1,col=2)
points(score~n,subset(results,size==996759 & smooth.gram=='On' & unk=='Off'),type='l',lty=2,lwd=2,col=1)
points(score~n,subset(results,size==996759 & smooth.gram=='On' & unk=='On'),type='l',lty=2,lwd=2,col=2)


legend('top',c("Smoothing off, UNK off",'Smoothing off, UNK on,','Smoothing on, UNK off','Smoothing on, UNK on'),
	lty=rep(1:2,each=2),
	lwd=rep(c(1,2),each=2),
	col=rep(1:2,2)
)
}


posPlot.l=function(){
startplot.l(mainadd='using tag bigrams')
results.l=results
results.l$log=log(results.l$size)
points(score~log,subset(results.l,n==2 & smooth.gram=='On' & unk=='Off'),type='l',lty=2,lwd=2,col=1)
points(score~log,subset(results.l,n==2 & smooth.gram=='On' & unk=='On'),type='l',lty=2,lwd=2,col=2)

legend('top',c('UNK off','UNK on'),
	lty=2,
	lwd=2,
	col=1:2
)
}


#Test here
main=function(){
	pdf('plot1.pdf')
	par(mfrow=c(1,2))
	posPlot.n()
	posPlot.l()
	dev.off()
}
