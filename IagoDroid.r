#!/usr/bin/Rscript

#
#    Copyright (C) 2017 by Alejandro Calleja <accortin@inf.uc3m.es>,
#    Alejandro Martín <alejandro.martin@uam.es> and Hector D. Menendez 
#    <hector.david.1987@gmail.com>
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, version 3 of the License.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.


library("GA")
library("proxy")
library("MASS")
library("e1071")
library("RWeka")
library("party")

args <- commandArgs(TRUE)
individual <- read.table(args[2],sep=",",head=TRUE)
indId <- as.numeric(args[3])
maxiter <- as.numeric(args[5])
increment <- as.numeric(args[4])

individual <- individual[indId,]

model <- readRDS(args[1])
probabilities <- predict(model,  type="prob", individual[-which(names(individual)=="class")],probability = TRUE)
maximumProb<-probabilities[,as.character(individual[1,"class"])]
currentClass <- as.character(individual[1,"class"])
names<-colnames(individual)
fitness <- function(populus)
{
	populus<-round(populus)
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
