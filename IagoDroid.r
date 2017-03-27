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
#Args[2] el individuo a modificar (con cabecera)
#Args[3] individuo a comprobar
#Args[4] conversions from 0.5 to whatever
#Args[5] generations
#Args[6] output

args <- commandArgs(TRUE)
individual <- read.table(args[2],sep=",",head=TRUE)
#individual <-readRDS(args[2])
indId <- as.numeric(args[3])
maxiter <- as.numeric(args[5])
increment <- as.numeric(args[4])

#Esto es provisional, pero cojo el primero del fichero
individual <- individual[indId,]

#saveRDS(mod, "mymodel.rds")
model <- readRDS(args[1])
#prediction <- predict(model, individual[-which(names(individual)=="class")])
probabilities <- predict(model,  type="prob", individual[-which(names(individual)=="class")],probability = TRUE)
maximumProb<-probabilities[,as.character(individual[1,"class"])]
#currentClass <- colnames(probabilities)[which(probabilities==max(probabilities))]
currentClass <- as.character(individual[1,"class"])
names<-colnames(individual)
fitness <- function(populus)
{
#	print(populus)
	populus<-round(populus)
	#I need the prediction to change the family
	element<-as.data.frame(t(populus))
	colnames(element)<-names[-which(names(individual)=="class")]
	probabilities <- predict(model,  type="prob", element, probability = TRUE)
	if(currentClass!=colnames(probabilities)[which(probabilities==max(probabilities))])
		1
	else	
		maximumProb-probabilities[,as.character(individual[1,"class"])]
}
minv = as.numeric(individual[-which(names(individual)=="class")])
maxv = minv+increment
GA <- ga(type="real-valued",fitness=fitness,min=minv,max=maxv,maxiter=maxiter)
#out<-cbind(round(GA@solution),as.vector(GA@fitness))
eva <- as.data.frame(round(GA@solution))
colnames(eva) <- colnames(individual[-which(names(individual)=="class")])
prediction <- predict(model, eva)
out<-cbind(eva,prediction)
colnames(out)<-colnames(individual)
changes <- apply(sweep(as.matrix(out[-which(names(out)=="class")]),2,as.numeric(individual[-which(names(individual)=="class")])),1,sum)
print(out)
print(currentClass)
print("Changes")
print(changes)
out<-cbind(out, changes)
oriclass <- rep(currentClass,length(changes))
out <- cbind(out,oriclass)
saveRDS(out, paste(paste(args[6],"out",sep='/'),args[3],"ind.rds",sep='.'))
saveRDS(GA, paste(paste(args[6],"ga",sep='/'),args[3],"ind.rds",sep='.'))
