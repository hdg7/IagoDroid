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
individual <-readRDS(args[2])
familyInName <- args[3]
targetFamily <- args[4]
maxiter <- as.numeric(args[5])
increment <- 0.6

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
	if(currentClass!=colnames(probabilities)[which(probabilities==max(probabilities))])
       	{	
		count <- count - 1
	}
	else
	{
		fitness <- function(populus)
		{
			populus<-round(populus)
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
