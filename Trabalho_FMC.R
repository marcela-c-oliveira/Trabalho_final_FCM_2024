# Packages ####
pacotes <- c(
  "tidyverse"
)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# Database ####
rm(list=ls())
df=read.csv2("Data.csv",h=T,d=",",na.strings=".")

df[99,21] <- "Bite.Grill.1"
df$Timepoint <- ifelse(df$Timepoint == "Pre-castration", 0, 1)

df <- df %>% arrange(Observers)
df <- df %>% arrange(Timepoint)
df <- df %>% arrange(Video)

dfw=filter(df,Base=='Stelio')
dfpw=filter(df,!Base=='Stelio')

#dfexp=filter(dfw,!Categoria..Experient.ou.Student.=='Student')
#dfstu=filter(dfw,!Categoria..Experient.ou.Student.=='Experient')
#dfs1=filter(dfw,Observers=='S1')
#dfs2=filter(dfw,Observers=='S2')
#dfs3=filter(dfw,Observers=='S3')
#dfs4=filter(dfw,Observers=='S4')
#dfs5=filter(dfw,Observers=='S5')
#dfs6=filter(dfw,Observers=='S6')
#dfs7=filter(dfw,Observers=='S7')
#dfs8=filter(dfw,Observers=='S8')
#dfs9=filter(dfw,Observers=='S9')
#dfs10=filter(dfw,Observers=='S10')
#dfa1=filter(dfw,Observers=='A1')
#dfa2=filter(dfw,Observers=='A2')
#dfa3=filter(dfw,Observers=='A3')
#dfgs=filter(dfw,Observers=='GS')

dfw$Categoria..Experient.ou.Student.=as.factor(dfw$Categoria..Experient.ou.Student.)
dfw$Analgesia=as.factor(dfw$Analgesia)
dfw$Timepoint=as.factor(dfw$Timepoint)

View(dfw)
str(dfw)

df_c1=filter(dfw,Timepoint=='1')
df_c0=filter(dfw,Timepoint=='0')


# Matrix with expected values ####

table(df_c1$Categoria..Experient.ou.Student.,df_c1$Analgesia)

value11 <- (7 + 47) * (7 + 33) / 140
value12 <- (33 + 53) * (7 + 33) / 140
value21 <- (7 + 47) * (47 + 53) / 140
value22 <- (33 + 53) * (47 + 53) / 140

expected <- matrix(
  round(c(value11, value12, value21, value22), 1), 
  nrow = 2, 
  byrow = TRUE,
  dimnames = list(
    c("Experients", "Students"), 
    c("No_analgesia", "Analgesia")
  )
)

print(expected)

# Como a tabela de valores esperados n?o apresentou nenhum n?mero menor que 5, podemos usar o teste de qui quadrado, n?o ? necess?rio/obrigat?rio usar fisher

# Chi square test OR Fisher test ####

chisq.test(table(df_c1$Categoria..Experient.ou.Student.,df_c1$Analgesia)) # p-value = 0.002309

fisher.test(table(df_c1$Categoria..Experient.ou.Student.,df_c1$Analgesia)) # p-value = 0.001101


# Graph - scores using UPAPS ####

dfw$Timepoint <- factor(dfw$Timepoint, 
                        levels = c("0", "1"), 
                        labels = c("Antes da castra??o", "Depois da castra??o"))

dfw$Categoria..Experient.ou.Student. <- factor(dfw$Categoria..Experient.ou.Student., 
                                               levels = c("Experient", "Student"), 
                                               labels = c("Experientes", "Alunos"))


png("Boxplot_pontua??es.png",width=12,height=5,units='in',res=400, 
    family="sans")
ggplot(dfw,aes(y=UPAPS,x=Categoria..Experient.ou.Student.,fill=Categoria..Experient.ou.Student.))+
  geom_boxplot(position=position_dodge(.1),outlier.shape = NA)+
  stat_summary(fun=mean,geom="point",size=5,shape=23,color="black",fill = rgb(0, 0, 0, alpha = 0.3),position=position_dodge(0.1))+
  geom_hline(yintercept=4, color="gray30", linewidth=0.8)+
  facet_wrap(~ Timepoint) +
  labs(title = "Pontua??o da dor em porcos antes e ap?s castra??o",
       x = "Avaliadores",
       y = "Pontua??o da UPAPS")+
  scale_y_continuous(n.breaks=14, limits = c(-0.1, 15))+
  scale_fill_viridis_d()+
  theme_minimal()+theme(axis.text=element_text(size=10),
                        axis.title=element_text(size=12,face="bold"),
                        strip.text = element_text(size = 10,face="bold"),
                        plot.margin = unit(c(.3,.3,.3,.3),"cm"),
                        legend.position = 'none')
dev.off()


# Graph - proportions of analgesic indication ####

tabela <- table(dfw$Categoria..Experient.ou.Student.,dfw$Analgesia)
tabela_0 <- table(df_c0$Categoria..Experient.ou.Student.,df_c0$Analgesia)
tabela_1 <- table(df_c1$Categoria..Experient.ou.Student.,df_c1$Analgesia)

prop <- data.frame(
  Timepoint = c("Antes da castra??o", "Antes da castra??o", "Depois da castra??o", "Depois da castra??o"),
  Categoria = c("Experientes", "Alunos", "Experientes", "Alunos"),
  Analgesia = c(0, 33, 82.5, 53)
)

png("Propor??es.png",width=12,height=5,units='in',res=400, 
    family="sans")
ggplot(data = prop, aes(x = factor(Categoria), y = Analgesia, fill = Categoria)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Timepoint) +
  labs(
    title = "Indica??o analg?sica em porcos antes e ap?s castra??o",
    x = "Avaliadores",
    y = "Porcentagem de indica??o analg?sica"
  ) +
  theme_minimal()+theme(legend.position = 'none') +
  scale_fill_manual(values = c("Alunos" = "red", "Experientes" = "green"))
dev.off()


# c
# C2
#3


