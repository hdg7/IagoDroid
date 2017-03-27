#!/usr/bin/Rscript

library("GA")
library("proxy")
library("MASS")
library("e1071")
library("RWeka")
library("party")
#library(caret)
#library("fpc")
#library(ROCR)

#Lee la configuración de un individuo y la modifica respecto al clasificador
#Args[1] fichero del clasificador
#Args[2] lista de individuos
#Args[3] familia in
#Args[4] familia out
#Args[5] generations

args <- commandArgs(TRUE)
individual <-readRDS(args[2])
familyInName <- args[3]
targetFamily <- args[4]
maxiter <- as.numeric(args[5])
increment <- 0.6

#Esto es provisional, pero cojo el primero del fichero
individualIndex <- which(individual[,"class"]==familyInName)

total <- 0
count <- length(individualIndex)
for (index in individualIndex)
{ 
	model <- readRDS(args[1])
	probabilities <- predict(model,  type="prob", individual[index,-which(colnames(individual)=="class")],probability = TRUE)
	maximumProb<-probabilities[,as.character(individual[index,"class"])]
	currentClass <- as.character(individual[index,"class"])
	names<-colnames(individual)
	#The individual is wrongly classified
	if(currentClass!=colnames(probabilities)[which(probabilities==max(probabilities))])
       	{	
		count <- count - 1
		#Ignore the instance
	}
	else
	{
		fitness <- function(populus)
		{
			#print(populus)
			populus<-round(populus)
			#I need the prediction to change the family
			element<-as.data.frame(t(populus))
			colnames(element)<-names[-which(names(individual)=="class")]
			probabilities <- predict(model,  type="prob", element, probability = TRUE)
			if(sum(targetFamily==colnames(probabilities)[which(probabilities==max(probabilities))]) >= 1)
				1
			else if (currentClass==colnames(probabilities)[which(probabilities==max(probabilities))])
				0
			else	
				probabilities[,targetFamily] + 0.5*sum(probabilities)/length(probabilities)
		}
		minv = as.numeric(individual[index,-which(names(individual)=="class")])
		maxv = minv+increment
		GA <- ga(type="real-valued",fitness=fitness,min=minv,max=maxv,maxiter=maxiter)
		eva <- as.data.frame(round(GA@solution))
		colnames(eva) <- colnames(individual[index,-which(names(individual)=="class")])
		prediction <- predict(model, eva)
		if(sum(targetFamily==prediction) >= 1)
		{
			total <- total + 1
			changes <- sweep(as.matrix(eva),2,as.numeric(individual[index,-which(names(individual)=="class")]))
			print(apply(changes,1,sum))
			print(changes)
		}
	}
}
print("Predictions")
print(total/count)
