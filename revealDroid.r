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

library(e1071)
library(foreign)
library(caret)
library("fpc")
library(ROCR)
library(rJava) 

args <- commandArgs("TRUE")

datos <- read.arff(args[1])
datos <- datos[,-1]
rownames(datos) <- datos[,1]
datos <- datos[,-1]
datos[is.na(datos)]<-0
datos <- datos[,-nearZeroVar(datos)]
attach(datos)

totaldata <- length(datos[,1])

indices <- seq(1,totaldata)
trainIndices <- sample(indices,length(datos[,1])*2/3)
testIndices <- indices[-trainIndices]

x <- subset(datos[testIndices,], select = -class)
y <- class[testIndices]

library("RWeka")
library(party)
model <- J48(class~., datos[trainIndices,])
pred <- predict(model, x)
tab<-table(pred, y)
mconf <- confusionMatrix(tab)
print(mconf$overall[1])
.jcache(model$classifier) 
saveRDS(model, "myrevealdroidJ48.rds")

allclasses<-unique(sort(datos[,"class"]))
ind<-data.frame(Date=as.Date(character()),
                 File=character(), 
                 User=character(), 
                 stringsAsFactors=FALSE)
for (i in seq(1, length(allclasses)))
{
	ind<- rbind(ind,datos[sample(which(datos[,'class']==allclasses[i]),10),])

}
saveRDS(ind, "individuals.rds")
write.table(ind,file="individualsReveal.txt",sep=',')
