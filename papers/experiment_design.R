library(AlgDesign)

lineCounts=c(100,1000,10000,996759)
treatments=gen.factorial(c(length(lineCounts),2,2,2,2), nVars=5,varNames=c('log.train.line','n','smooth.gram','smooth.lex','unk'),factors=c(1,3:5),center=F)
treatments$n=treatments$n+1
levels(treatments$smooth.gram)=c('None','Add One')
levels(treatments$smooth.lex)=c('None','Add One')
levels(treatments$unk)=c('None','First Occurrence')
levels(treatments$log.train.line)=log(lineCounts)
treatments$log.train.line=as.numeric(as.character(treatments$log.train.line))
#treatments$score='?'
#treatments$time='?'

#If we do look at time, particularly if we do it very naively,
#like by running bash date right before and right after
#running the main program, we should use a randomized design
#with replication and take the minimum times for each
#treatment combination. So let's do that:

treatments$file=paste(1:nrow(treatments),'.pos',sep='')

#This is a different id from treatments$id
id=order(runif(2*nrow(treatments)))
treatments.rand=rbind(treatments,treatments)[id,]
row.names(treatments.rand)=sort(id)

#If we're only concerned with the score of the predictions,
#we don't really need to randomize. I'd suggest against it
#because it could be confusing.

#treatments$time=NULL

#Also run the baseline, but I think it'll be less confusing
#if it's not in the same frames. Hmm well
baseline=rep('?',2)
names(baseline)=c('score','time')

write.csv(treatments,file='experiment_design.csv',quote=F,row.names=F)
