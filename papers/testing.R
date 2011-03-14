#Get the experimental design
source('experiment_design.R')
treatments$id=row.names(treatments)

#Get the results
response=read.csv('results.csv')

#Merge them
results=merge(treatments,results,by='id')[-5:-6] #Remove the dummy id and score variables
