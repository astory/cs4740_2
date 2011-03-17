source('experiment_design.R')
design=treatments
scores=read.csv('dummy_results.csv')
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
points(score~n,subset(results,smooth.gram=='None'),type='l')
points(score~n,subset(results,smooth.gram=='AddOne'),type='l')
points(score~n,subset(results,smooth.gram=='GoodTuring'),type='l')

startplot.n()
points(score~n,subset(results,smooth.lex=='None'),type='l')
points(score~n,subset(results,smooth.lex=='AddOne'),type='l')
points(score~n,subset(results,smooth.lex=='GoodTuring'),type='l')

startplot.n()
points(score~n,subset(results,unk=='None'),type='l')
points(score~n,subset(results,unk=='FirstOccurrance'),type='l')
points(score~n,subset(results,unk=='Count=1'),type='l')

startplot.l()
points(score~log.train.line,subset(results,smooth.gram=='None'),type='l')
points(score~log.train.line,subset(results,smooth.gram=='AddOne'),type='l')
points(score~log.train.line,subset(results,smooth.gram=='GoodTuring'),type='l')

startplot.l()
points(score~log.train.line,subset(results,smooth.lex=='None'),type='l')
points(score~log.train.line,subset(results,smooth.lex=='AddOne'),type='l')
points(score~log.train.line,subset(results,smooth.lex=='GoodTuring'),type='l')

startplot.l()
points(score~log.train.line,subset(results,unk=='None'),type='l')
points(score~log.train.line,subset(results,unk=='AddOne'),type='l')
points(score~log.train.line,subset(results,unk=='GoodTuring'),type='l')
