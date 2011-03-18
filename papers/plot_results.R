#source('experiment_design.R')
#design=treatments
design=read.csv('experiment_design.csv')
scores=read.csv('results.csv')
results=merge(design,scores,by='file')

startplot=function(x,xlab='N-gram length',...){
	plot(score~x,results,type='n',axes=F,
		xlab=xlab,
                ylab='Score (% correct)',
		main='Performance of the part-of-speech tagger',
		...
	)
	axis(2)
}
startplot.n=function() {
	startplot(results$n)
	axis(1,at=results$n)
}

startplot.l=function() {
	startplot(results$log.train.line,xlab='Number of words in the training set')
	axis(1,at=log(lineCounts),labels=lineCounts)
}

par(mfrow=c(2,3))

startplot.n()
points(score~n,subset(results,smooth.gram=='On'),type='l')
points(score~n,subset(results,smooth.gram=='Off'),type='l')

startplot.n()
points(score~n,subset(results,unk=='On'),type='l')
points(score~n,subset(results,unk=='Off'),type='l')


results.l=results
results.l$log=log(results.l$size)

startplot.l()
points(score~log,subset(results.l,smooth.gram=='On'),type='l')
points(score~log,subset(results.l,smooth.gram=='Off'),type='l')

startplot.l()
points(score~log,subset(results.l,unk=='On'),type='l')
points(score~log,subset(results.l,unk=='Off'),type='l')

