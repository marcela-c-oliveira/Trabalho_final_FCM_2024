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

# Como a tabela de valores esperados não apresentou nenhum número menor que 5, podemos usar o teste de qui quadrado, não é necessário/obrigatório usar fisher

# Chi square test OR Fisher test ####

chisq.test(table(df_c1$Categoria..Experient.ou.Student.,df_c1$Analgesia)) # p-value = 0.002309

fisher.test(table(df_c1$Categoria..Experient.ou.Student.,df_c1$Analgesia)) # p-value = 0.001101


