library(reshape)
library(ggplot2)
library(scales)
library(mgcv)

pontos = read.table("haskell_sequencialO32.tsv",sep = ",",header = T) 
pontosa= read.table("EscalabilidadeFraca.tsv",sep = ",",header = T)

pontosa[6] = pontos[2] 

m<-qplot(Pontos,Tempo.1/Tempo, data= pontosa,ylim=c(0,6),log='x',xlab="Quantidade de Pontos(escala log)",ylab="Aceleração", colour=Nucleos , geom = c("point", "smooth"),method = "loess", formula = y ~ log(x))+theme(axis.text=element_text(size=26,colour="black"),axis.title=element_text(size=20,face="bold"))+ theme(axis.text.x=element_text(colour="black",size=20),axis.text.y=element_text(colour="black",size=20),legend.text  = element_text(colour="black", size=20),legend.title  = element_text(colour="black", size=18))
m+scale_x_log10(breaks = trans_breaks("log10",function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+ theme(axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"))

