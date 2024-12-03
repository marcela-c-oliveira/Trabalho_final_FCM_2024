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

dfw$Categoria..Experient.ou.Student.=as.factor(dfw$Categoria..Experient.ou.Student.)
dfw$Analgesia=as.factor(dfw$Analgesia)
dfw$Timepoint=as.factor(dfw$Timepoint)

View(dfw)
str(dfw)

df_c1=filter(dfw,Timepoint=='1')
df_c0=filter(dfw,Timepoint=='0')


# Matrix with expected values - before castration ####

df_c0$Analgesia <- factor(df_c0$Analgesia, 
                          levels = c("0", "1"), 
                          labels = c("No analgesia", "Analgesia"))

table(df_c0$Categoria..Experient.ou.Student.,df_c0$Analgesia)

value11_c0 <- (40 + 67) * (40 + 0) / 140
value12_c0 <- (33 + 0) * (40 + 0) / 140
value21_c0 <- (40 + 67) * (67 + 33) / 140
value22_c0 <- (33 + 0) * (67 + 33) / 140

expected_pre <- matrix(
  round(c(value11_c0, value12_c0, value21_c0, value22_c0), 1), 
  nrow = 2, 
  byrow = TRUE,
  dimnames = list(
    c("Experients", "Students"), 
    c("No analgesia", "Analgesia")
  )
)

print(expected_pre)

# Como a tabela de valores esperados nao apresentou nenhum numero menor que 5, podemos usar o teste de qui quadrado, nao e necessario/obrigatorio usar fisher

# Chi square test OR Fisher test - before castration ####

chisq.test(table(df_c0$Categoria..Experient.ou.Student.,df_c0$Analgesia)) # p-value = 8.304e-05

fisher.test(table(df_c0$Categoria..Experient.ou.Student.,df_c0$Analgesia)) # p-value = 3.563e-06


# Matrix with expected values - after castration ####

df_c1$Analgesia <- factor(df_c1$Analgesia, 
                        levels = c("0", "1"), 
                        labels = c("No analgesia", "Analgesia"))

table(df_c1$Categoria..Experient.ou.Student.,df_c1$Analgesia)

value11_c1 <- (7 + 47) * (7 + 33) / 140
value12_c1 <- (33 + 53) * (7 + 33) / 140
value21_c1 <- (7 + 47) * (47 + 53) / 140
value22_c1 <- (33 + 53) * (47 + 53) / 140

expected_post <- matrix(
  round(c(value11_c1, value12_c1, value21_c1, value22_c1), 1), 
  nrow = 2, 
  byrow = TRUE,
  dimnames = list(
    c("Experients", "Students"), 
    c("No analgesia", "Analgesia")
  )
)

print(expected_post)

# Como a tabela de valores esperados nao apresentou nenhum numero menor que 5, podemos usar o teste de qui quadrado, nao e necessario/obrigatorio usar fisher

# Chi square test OR Fisher test ####

chisq.test(table(df_c1$Categoria..Experient.ou.Student.,df_c1$Analgesia)) # p-value = 0.002309

fisher.test(table(df_c1$Categoria..Experient.ou.Student.,df_c1$Analgesia)) # p-value = 0.001101


# Plot - scores using UPAPS ####

dfw$Timepoint <- factor(dfw$Timepoint, 
                        levels = c("0", "1"), 
                        labels = c("Antes da castração", "Depois da castração"))

dfw$Categoria..Experient.ou.Student. <- factor(dfw$Categoria..Experient.ou.Student., 
                                               levels = c("Experient", "Student"), 
                                               labels = c("Experientes", "Alunos"))


png("Boxplot_pontuacoes.png",width=8.3,height=5.5,units='in',res=400, 
    family="sans")
ggplot(dfw,aes(y=UPAPS,x=Categoria..Experient.ou.Student.,fill=Categoria..Experient.ou.Student.))+
  geom_boxplot(position=position_dodge(.1),outlier.shape = NA)+
  geom_point(position=position_jitter(width=.15,height=.2),size=2.5,shape=16,alpha=0.7,color="gray10")+
  stat_summary(fun=mean,geom="point",size=7,shape=23,color="green",fill = rgb(0, 0, 0, alpha = 0.4),position=position_dodge(0.1))+
  geom_hline(linetype="dashed",yintercept=4, color="gray20", linewidth=0.8)+
  facet_wrap(~Timepoint) +
  labs(title = "Pontuação da dor em porcos antes e após castração",
       x = "Avaliadores",
       y = "Pontuação da UPAPS")+
  scale_y_continuous(n.breaks=14, limits = c(0, 15))+
  scale_fill_manual(values = c("Experientes" = "orange2", "Alunos" = "steelblue4"))+
  theme_minimal()+theme(axis.text=element_text(size=15),
                        axis.title=element_text(size=19,face="bold"),
                        strip.text = element_text(size=16),
                        plot.title = element_text(size = 21, face = "bold"),
                        plot.margin = unit(c(.3,.3,.3,.3),"cm"),
                        legend.position = 'none')
dev.off()


# Plot - proportions of analgesic indication ####

tabela <- table(dfw$Categoria..Experient.ou.Student.,dfw$Analgesia)
tabela_0 <- table(df_c0$Categoria..Experient.ou.Student.,df_c0$Analgesia)
tabela_1 <- table(df_c1$Categoria..Experient.ou.Student.,df_c1$Analgesia)

prop <- data.frame(
  Timepoint = c("Antes da castração", "Antes da castração", "Depois da castração", "Depois da castração"),
  Categoria = c("Experientes", "Alunos", "Experientes", "Alunos"),
  Analgesia = c(0, 33, 82.5, 53)
)

png("Proporcoes.png",width=8.7,height=6,units='in',res=400, 
    family="sans")
ggplot(data = prop, aes(x = factor(Categoria), y = Analgesia, fill = Categoria)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::percent(Analgesia / 100)), position = position_dodge(width = 0.9), vjust = -0.3, size = 4.7) +
  geom_text(data = subset(prop, Categoria == "Experientes" & Timepoint == "Depois da castração"), aes(x = factor(Categoria), y = Analgesia, label = "*"), position = position_dodge(width = 0.9), vjust = -.8, size = 10, inherit.aes = FALSE) +
  geom_text(data = subset(prop, Categoria == "Experientes" & Timepoint == "Antes da castração"), aes(x = factor(Categoria), y = Analgesia, label = "*"), position = position_dodge(width = 0.9), vjust = -.8, size = 10, inherit.aes = FALSE) +
  facet_wrap(~ Timepoint) +
  labs(
    title = "Indicação analgésica em porcos antes e após castração",
    x = "Avaliadores",
    y = "Porcentagem de indicação analgésica"
  ) +
  scale_fill_manual(values = c("olivedrab4", "violetred4"))+
  scale_y_continuous(n.breaks=6, limits = c(0, 100))+
  theme_minimal()+theme(axis.text=element_text(size=15),
                        axis.title=element_text(size=19,face="bold"),
                        strip.text = element_text(size=16),
                        plot.title = element_text(size = 21, face = "bold"),
                        plot.margin = unit(c(.3,.3,.3,.3),"cm"),
                        legend.position = 'none')
dev.off()


