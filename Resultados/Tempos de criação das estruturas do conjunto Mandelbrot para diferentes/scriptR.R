library(reshape)
library(ggplot2)
library(scales)
library(mgcv)


pontos = read.table("Particionamento.tsv",sep = ",",header = T) 

m<-qplot(Nucleos,TempoAlocacao, data= pontos,ylim=c(0,0.75),xlab="Quantidade de sparks",ylab="Tempo", colour=Particionamento   , geom = c("point", "smooth") )+theme(axis.text=element_text(size=26,colour="black"),axis.title=element_text(size=20,face="bold"))+ theme(axis.text.x=element_text(colour="black",size=20),axis.text.y=element_text(colour="black",size=20),legend.text  = element_text(colour="black", size=20),legend.title  = element_text(colour="black", size=18))
