
#carregando dados

seapa<-read_excel(here("Data","Raw" ,'seapa.xlsx'),sheet='analise')
names(seapa)
hist(seapa$QTD_BOVINO_MAIS36MES_M[seapa$QTD_BOVINO_MAIS36MES_M>5],xlim=c(0,1000))
summary(seapa$QTD_BOVINO_MAIS36MES_M[seapa$QTD_BOVINO_MAIS36MES_M>5])


##Tamanho da popula??o em cada extrato
kable(table(seapa$MESOREGIAO[seapa$QTD_BOVINO_MAIS36MES_M>5]))

prob<-rbind(
length(seapa$MESOREGIAO[seapa$MESOREGIAO=='CENTRO OCIDENTAL RIO-GRANDENSE'&seapa$QTD_BOVINO_MAIS36MES_M>5])/length(seapa$MESOREGIAO[seapa$QTD_BOVINO_MAIS36MES_M>5]),
length(seapa$MESOREGIAO[seapa$MESOREGIAO=='CENTRO ORIENTAL RIO-GRANDENSE'&seapa$QTD_BOVINO_MAIS36MES_M>5])/length(seapa$MESOREGIAO[seapa$QTD_BOVINO_MAIS36MES_M>5]),
length(seapa$MESOREGIAO[seapa$MESOREGIAO=='METROPOLITA DE PORTO ALEGRE'&seapa$QTD_BOVINO_MAIS36MES_M>5])/length(seapa$MESOREGIAO[seapa$QTD_BOVINO_MAIS36MES_M>5]),
length(seapa$MESOREGIAO[seapa$MESOREGIAO=='NORDESTE RIO-GRANDENSE'&seapa$QTD_BOVINO_MAIS36MES_M>5])/length(seapa$MESOREGIAO[seapa$QTD_BOVINO_MAIS36MES_M>5]),
length(seapa$MESOREGIAO[seapa$MESOREGIAO=='NOROESTE RIO-GRANDENSE'&seapa$QTD_BOVINO_MAIS36MES_M>5])/length(seapa$MESOREGIAO[seapa$QTD_BOVINO_MAIS36MES_M>5]),
length(seapa$MESOREGIAO[seapa$MESOREGIAO=='SUDESTE RIO-GRANDENSE'&seapa$QTD_BOVINO_MAIS36MES_M>5])/length(seapa$MESOREGIAO[seapa$QTD_BOVINO_MAIS36MES_M>5]),
length(seapa$MESOREGIAO[seapa$MESOREGIAO=='SUDOESTE RIO-GRANDENSE'&seapa$QTD_BOVINO_MAIS36MES_M>5])/length(seapa$MESOREGIAO[seapa$QTD_BOVINO_MAIS36MES_M>5])
)


##tamanho de amostra para estimar uma prevalencia entre rebanhos de 30%

p<-0.5
e<-0.1
n<-1.96^2*p*(1-p)/e^2

#Sorteio das propriedades
amostra<-rbind(
sample_n(filter(seapa, MESOREGIAO=='CENTRO OCIDENTAL RIO-GRANDENSE'& QTD_BOVINO_MAIS36MES_M>5), size=round(prob*n,0)[1],replace=F),
sample_n(filter(seapa, MESOREGIAO=='CENTRO ORIENTAL RIO-GRANDENSE'& QTD_BOVINO_MAIS36MES_M>5), size=round(prob*n,0)[2],replace=F),
sample_n(filter(seapa, MESOREGIAO=='METROPOLITA DE PORTO ALEGRE'& QTD_BOVINO_MAIS36MES_M>5), size=round(prob*n,0)[3],replace=F),
sample_n(filter(seapa, MESOREGIAO=='NORDESTE RIO-GRANDENSE'& QTD_BOVINO_MAIS36MES_M>5), size=round(prob*n,0)[4],replace=F),
sample_n(filter(seapa, MESOREGIAO=='NOROESTE RIO-GRANDENSE'& QTD_BOVINO_MAIS36MES_M>5), size=round(prob*n,0)[5],replace=F),
sample_n(filter(seapa, MESOREGIAO=='SUDESTE RIO-GRANDENSE'& QTD_BOVINO_MAIS36MES_M>5), size=round(prob*n,0)[6],replace=F),
sample_n(filter(seapa, MESOREGIAO=='SUDOESTE RIO-GRANDENSE'& QTD_BOVINO_MAIS36MES_M>5), size=round(prob*n,0)[7],replace=F)
)



rbind(
  length(amostra$MESOREGIAO[amostra$MESOREGIAO=='CENTRO OCIDENTAL RIO-GRANDENSE'&amostra$QTD_BOVINO_MAIS36MES_M>5])/length(amostra$MESOREGIAO[amostra$QTD_BOVINO_MAIS36MES_M>5]),
  length(amostra$MESOREGIAO[amostra$MESOREGIAO=='CENTRO ORIENTAL RIO-GRANDENSE'&amostra$QTD_BOVINO_MAIS36MES_M>5])/length(amostra$MESOREGIAO[amostra$QTD_BOVINO_MAIS36MES_M>5]),
  length(amostra$MESOREGIAO[amostra$MESOREGIAO=='METROPOLITA DE PORTO ALEGRE'&amostra$QTD_BOVINO_MAIS36MES_M>5])/length(amostra$MESOREGIAO[amostra$QTD_BOVINO_MAIS36MES_M>5]),
  length(amostra$MESOREGIAO[amostra$MESOREGIAO=='NORDESTE RIO-GRANDENSE'&amostra$QTD_BOVINO_MAIS36MES_M>5])/length(amostra$MESOREGIAO[amostra$QTD_BOVINO_MAIS36MES_M>5]),
  length(amostra$MESOREGIAO[amostra$MESOREGIAO=='NOROESTE RIO-GRANDENSE'&amostra$QTD_BOVINO_MAIS36MES_M>5])/length(amostra$MESOREGIAO[amostra$QTD_BOVINO_MAIS36MES_M>5]),
  length(amostra$MESOREGIAO[amostra$MESOREGIAO=='SUDESTE RIO-GRANDENSE'&amostra$QTD_BOVINO_MAIS36MES_M>5])/length(amostra$MESOREGIAO[amostra$QTD_BOVINO_MAIS36MES_M>5]),
  length(amostra$MESOREGIAO[amostra$MESOREGIAO=='SUDOESTE RIO-GRANDENSE'&amostra$QTD_BOVINO_MAIS36MES_M>5])/length(amostra$MESOREGIAO[amostra$QTD_BOVINO_MAIS36MES_M>5])
)



#Exportar o banco de daddos com as propriedades sorteadas

write.csv2(amostra,file=here("Data","Processed","amostra.csv"))




###############################
#Segunda rodada de amostragem #
###############################

#carregando dados

seapa1<-subset(seapa, is.na(amostra1))
names(seapa1)

hist(seapa1$QTD_BOVINO_MAIS36MES_M[seapa1$QTD_BOVINO_MAIS36MES_M>5])
summary(seapa1$QTD_BOVINO_MAIS36MES_M[seapa1$QTD_BOVINO_MAIS36MES_M>5])

prob<-rbind(
  length(seapa$MESOREGIAO[seapa$MESOREGIAO=='CENTRO OCIDENTAL RIO-GRANDENSE'&seapa$QTD_BOVINO_MAIS36MES_M>5])/length(seapa$MESOREGIAO[seapa$QTD_BOVINO_MAIS36MES_M>5]),
  length(seapa$MESOREGIAO[seapa$MESOREGIAO=='CENTRO ORIENTAL RIO-GRANDENSE'&seapa$QTD_BOVINO_MAIS36MES_M>5])/length(seapa$MESOREGIAO[seapa$QTD_BOVINO_MAIS36MES_M>5]),
  length(seapa$MESOREGIAO[seapa$MESOREGIAO=='METROPOLITA DE PORTO ALEGRE'&seapa$QTD_BOVINO_MAIS36MES_M>5])/length(seapa$MESOREGIAO[seapa$QTD_BOVINO_MAIS36MES_M>5]),
  length(seapa$MESOREGIAO[seapa$MESOREGIAO=='NORDESTE RIO-GRANDENSE'&seapa$QTD_BOVINO_MAIS36MES_M>5])/length(seapa$MESOREGIAO[seapa$QTD_BOVINO_MAIS36MES_M>5]),
  length(seapa$MESOREGIAO[seapa$MESOREGIAO=='NOROESTE RIO-GRANDENSE'&seapa$QTD_BOVINO_MAIS36MES_M>5])/length(seapa$MESOREGIAO[seapa$QTD_BOVINO_MAIS36MES_M>5]),
  length(seapa$MESOREGIAO[seapa$MESOREGIAO=='SUDESTE RIO-GRANDENSE'&seapa$QTD_BOVINO_MAIS36MES_M>5])/length(seapa$MESOREGIAO[seapa$QTD_BOVINO_MAIS36MES_M>5]),
  length(seapa$MESOREGIAO[seapa$MESOREGIAO=='SUDOESTE RIO-GRANDENSE'&seapa$QTD_BOVINO_MAIS36MES_M>5])/length(seapa$MESOREGIAO[seapa$QTD_BOVINO_MAIS36MES_M>5])
)


#Sorteio das propriedades
n2=150
amostra<-rbind(
  sample_n(filter(seapa1, MESOREGIAO=='CENTRO OCIDENTAL RIO-GRANDENSE'& QTD_BOVINO_MAIS36MES_M>5), size=round(prob*n2,0)[1],replace=F),
  sample_n(filter(seapa1, MESOREGIAO=='CENTRO ORIENTAL RIO-GRANDENSE'& QTD_BOVINO_MAIS36MES_M>5), size=round(prob*n2,0)[2],replace=F),
  sample_n(filter(seapa1, MESOREGIAO=='METROPOLITA DE PORTO ALEGRE'& QTD_BOVINO_MAIS36MES_M>5), size=round(prob*n2,0)[3],replace=F),
  sample_n(filter(seapa1, MESOREGIAO=='NORDESTE RIO-GRANDENSE'& QTD_BOVINO_MAIS36MES_M>5), size=round(prob*n2,0)[4],replace=F),
  sample_n(filter(seapa1, MESOREGIAO=='NOROESTE RIO-GRANDENSE'& QTD_BOVINO_MAIS36MES_M>5), size=round(prob*n2,0)[5],replace=F),
  sample_n(filter(seapa1, MESOREGIAO=='SUDESTE RIO-GRANDENSE'& QTD_BOVINO_MAIS36MES_M>5), size=round(prob*n2,0)[6],replace=F),
  sample_n(filter(seapa1, MESOREGIAO=='SUDOESTE RIO-GRANDENSE'& QTD_BOVINO_MAIS36MES_M>5), size=round(prob*n2,0)[7],replace=F)
)


cbind(
  prob,
rbind(
  length(amostra$MESOREGIAO[amostra$MESOREGIAO=='CENTRO OCIDENTAL RIO-GRANDENSE'&amostra$QTD_BOVINO_MAIS36MES_M>5])/length(amostra$MESOREGIAO[amostra$QTD_BOVINO_MAIS36MES_M>5]),
  length(amostra$MESOREGIAO[amostra$MESOREGIAO=='CENTRO ORIENTAL RIO-GRANDENSE'&amostra$QTD_BOVINO_MAIS36MES_M>5])/length(amostra$MESOREGIAO[amostra$QTD_BOVINO_MAIS36MES_M>5]),
  length(amostra$MESOREGIAO[amostra$MESOREGIAO=='METROPOLITA DE PORTO ALEGRE'&amostra$QTD_BOVINO_MAIS36MES_M>5])/length(amostra$MESOREGIAO[amostra$QTD_BOVINO_MAIS36MES_M>5]),
  length(amostra$MESOREGIAO[amostra$MESOREGIAO=='NORDESTE RIO-GRANDENSE'&amostra$QTD_BOVINO_MAIS36MES_M>5])/length(amostra$MESOREGIAO[amostra$QTD_BOVINO_MAIS36MES_M>5]),
  length(amostra$MESOREGIAO[amostra$MESOREGIAO=='NOROESTE RIO-GRANDENSE'&amostra$QTD_BOVINO_MAIS36MES_M>5])/length(amostra$MESOREGIAO[amostra$QTD_BOVINO_MAIS36MES_M>5]),
  length(amostra$MESOREGIAO[amostra$MESOREGIAO=='SUDESTE RIO-GRANDENSE'&amostra$QTD_BOVINO_MAIS36MES_M>5])/length(amostra$MESOREGIAO[amostra$QTD_BOVINO_MAIS36MES_M>5]),
  length(amostra$MESOREGIAO[amostra$MESOREGIAO=='SUDOESTE RIO-GRANDENSE'&amostra$QTD_BOVINO_MAIS36MES_M>5])/length(amostra$MESOREGIAO[amostra$QTD_BOVINO_MAIS36MES_M>5])
)
)



#Exportar o banco de daddos com as propriedades sorteadas


write.csv2(amostra,file=here("Data","Processed","amostra2.csv"))
