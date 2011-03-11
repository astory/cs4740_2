library(AlgDesign)

treatments=gen.factorial(c(4,2,2,2), nVars=4,varNames=c('n','smooth.gram','smooth.lex','unk'),factors=2:4,center=F)
treatments$n=treatments$n+1
levels(treatments$smooth.gram)=c('None','GoodTuring')
levels(treatments$smooth.lex)=c('None','GoodTuring')
levels(treatments$unk)=c('None','FirstOccurrence')
treatments$score='?'
treatments$time='?'

#If we do look at time, particularly if we do it very naively,
#like by running bash date right before and right after
#running the main program, we should use a randomized design
#with replication and take the minimum times for each
#treatment combination. So let's do that:

id=order(runif(2*nrow(treatments)))
treatments.rand=rbind(treatments,treatments)[id,]
row.names(treatments.rand)=sort(id)

#If we're only concerned with the score of the predictions,
#we don't really need to randomize. I'd suggest against it
#because it could be confusing.

treatments$time=NULL
