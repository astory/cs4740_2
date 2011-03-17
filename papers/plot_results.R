source('experiment_design.R')
design=treatments[-5] #Remove the score=?
scores=read.csv('results.csv')
results=merge(design,scores,by='file')

startplot=function(){
	plot(score~n,results,type='n',axes=F,
		xlab='N-gram length',ylab='Score',
		main='Performance of the part-of-speech tagger'
	)
	axis(1,at=min(results$n):max(results$n))
	axis(2)
}

startplot()
points(score~n,subset(results,smooth.gram=='None'),type='l')
points(score~n,subset(results,smooth.gram=='AddOne'),type='l')
points(score~n,subset(results,smooth.gram=='GoodTuring'),type='l')

startplot()
points(score~n,subset(results,smooth.lex=='None'),type='l')
points(score~n,subset(results,smooth.lex=='AddOne'),type='l')
points(score~n,subset(results,smooth.lex=='GoodTuring'),type='l')

startplot()
points(score~n,subset(results,unk=='None'),type='l')
points(score~n,subset(results,unk=='FirstOccurrance'),type='l')
points(score~n,subset(results,unk=='Count=1'),type='l')

