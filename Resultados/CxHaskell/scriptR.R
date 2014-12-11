library(reshape)
library(ggplot2)
library(scales)
library(mgcv)
pontos= read.table("cxhaskell.tsv",sep = ",",header = T)
m<-qplot(Nucleos,Tempo+Alocacao, data= pontos,ylim=c(0,10),colour=Linguagem,main="Mandelbrot",xlab="a) Quantidade de Nucleos",ylab="Tempo",geom = c("point", "smooth"),method = "loess", formula = y ~ log(x))+theme(axis.text=element_text(size=26,colour="black"),axis.title=element_text(size=20,face="bold"))+ theme(axis.text.x=element_text(colour="black",size=20),axis.text.y=element_text(colour="black",size=20),legend.text  = element_text(colour="black", size=18),legend.title  = element_text(colour="black", size=18))+ theme(panel.background = element_rect(fill = 'white', colour = 'red'))
 

 