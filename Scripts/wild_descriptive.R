

#Packages to be used
packages<-c("readxl","knitr","ggplot2","here",
            "tidyverse","car","emmeans","lsmeans", "multcomp","patchwork")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# Creating directories
dir.create(here("Figures"))
dir.create(here("Outputs"))


# Read data from the data file
wild<-read_xlsx(here("Data","Raw","Silvestres2.xlsx"))%>%
  mutate(status2=ifelse(status=="Neg",0,1))


# E coli status distribution
table(wild$Ecoli)

# Table with groups
kable(table(wild$grupo))

# Cross-table animal group and E coli status
table(wild$Ecoli,wild$grupo)

# Type status distribution
table(wild$status)



p1<-data.frame(table(wild$Ecoli,wild$grupo))%>%
  add_row(Var1 = c("0","1"), Var2=c("Total","Total"),Freq = c(41,95))%>%
  mutate(Prop=Freq/c(90,90,32,32,14,14,136,136),
         text =ifelse(Freq < 1, NA, Freq))%>%
  ggplot(aes(x =Var2, y=Prop, fill = factor(Var1)))+
  theme_minimal()+
  geom_bar(stat = "identity")+
  geom_text(
    aes(label = text, y =Prop), stat = "identity",
    position = position_fill(vjust = .5)
  )+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_discrete(labels=c('Negative', 'Positive'))+
  scale_x_discrete(labels=c("Ave" = "Birds", "Mamífero" = "Mammals",
                            "Réptil" = "Reptiles","Total"="Total"))+
  labs(fill = expression(italic("Enterobacteria")),
       title = "A")+
  ylab("Proportion")+
  xlab("")+
  theme(axis.title.x = element_blank(),
    legend.position="bottom")+
  guides(fill = guide_legend(label.position = "bottom"))

table(wild$status2[wild$Ecoli==1],wild$grupo[wild$Ecoli==1])


p2<-data.frame(table(wild$status2[wild$Ecoli==1],wild$grupo[wild$Ecoli==1]))%>%
  add_row(Var1 = c("0","1"), Var2=c("Total","Total"),Freq = c(47,48))%>%
  mutate(Prop=Freq/c(58,58,23,23,14,14,95,95),
         text =ifelse(Freq < 1, NA, Freq))%>%
  ggplot(aes(x =Var2, y=Prop, fill = factor(Var1)))+
  theme_minimal()+
  geom_bar(stat = "identity")+
  geom_text(
    aes(label = text, y =Prop), stat = "identity",
    position = position_fill(vjust = .5)
  )+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_discrete(labels=c('Negative', 'Positive'))+
  scale_x_discrete(labels=c("Ave" = "Birds", "Mamífero" = "Mammals",
                            "Réptil" = "Reptiles","Total"="Total"))+
  labs(fill = expression(italic("EPEC|STEC|EHEC")),
       title = "B")+
  ylab("Proportion")+
  xlab("")+
  theme(axis.title.x = element_blank(),
    legend.position="bottom")+
  guides(fill = guide_legend(label.position = "bottom"))


(p1+p2)
ggsave(here("Figures","Figure1.jpg"),width = 7,heigh=5,units = "in",dpi=600,bg="white")

# Status distribution (at least one type present=1, 0 otherwise)
table(wild$status2)

# General proportion of positive samples for the pathotypes after PCR

summary(glm(status2~1,data=wild,family = binomial(link="logit"))->model0.1)

emmeans(model0.1,~1,type="response")

# Comparison of proportion of positive samples for the pathotypes after PCR between animal groups

summary(glm(status2~grupo,data=wild,family = binomial(link="logit"))->model1)

Anova(model1,type='III')

# Creating the datatable for the Fisher test

wild<-wild%>%
  mutate(status=ifelse(status=="STX1" | status=="STX2","STEC",status),
         STEC=ifelse(STX1==1 | STX2==1,1,0))


data_fis<-data.frame(matrix(c(table(wild$grupo,wild$EPEC)[,2],
                              table(wild$grupo,wild$EHEC)[,2],
                              table(wild$grupo,wild$STEC)[,2],
                              table(wild$grupo,wild$status)[,3])
                            ,byrow=T,nrow = 4)
          )

row.names(data_fis)<-c("EPEC","EHEC","STEC","Neg")

names(data_fis)<-c("bird","mammals","reptiles")


# Run the Fisher exact test

teste3<-fisher.test(data_fis)

level_order<-c("Neg","EHEC","EPEC","STEC") #Pathotypes order



# Plot the results
data_fis%>%
  rownames_to_column( var = "Pathotype")%>%
  gather(key="Group",value="Count",-Pathotype)%>%
    mutate(Prop=Count/c(98,98,98,98,39,39,39,39,16,16,16,16),
       text =ifelse(Count < 1, NA, Count))%>%
  ggplot(aes(x =Group, y=Prop, fill = factor(Pathotype, level = level_order)))+
  theme_minimal()+
  geom_bar(stat = "identity")+
     geom_text(
     aes(label = text), stat = "identity",
     position = position_fill(vjust = .5)
   )+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_discrete(labels=c("Neg",'EHEC', 'EPEC',"STEC"))+
  scale_x_discrete(labels=c("bird" = "Birds", "mammals" = "Mammals",
                            "reptiles" = "Reptiles"))+
  labs(fill = expression(italic("Pathotype")),
       caption = paste("Fisher's Exact p-value = ",round(teste3$p.value,2)))+
  ylab("Proportion")+
  xlab("")+
  theme(axis.title.x = element_blank(),
        legend.position="bottom")+
  guides(fill = guide_legend(label.position = "bottom"))

ggsave(here("Figures","Figure2.jpg"),width = 5,heigh=5,units = "in",dpi=600,bg="white")
