####################################
#' Limpeza inicial dos dados       #
#'                                 #
####################################


#### Pacotes que serão usados ####

  # Bibliotecas para manipulação de dados
  library(tidyverse)
  library(magrittr)
  
  # Bibliotecas para tabelas
  library(knitr)
  library(kableExtra)
  
  # Bibliotecas para análise de texto
  library(tidytext)
  library(tm)
  
  # Ler excel
  library(readxl)
  
  # Bibliotecas de visualização de dados
  library(plotly)


#### Importando os dados e Juntando em um único arquivo ####

  # Identificando todos os arquivos do diretório com extensão .csv
  arquivos = dir() 
  arquivos = arquivos[arquivos %>%  str_detect(pattern = ".csv")]
  
  # Lendo os arquivos da Catho e Vagas
  Catho = arquivos[arquivos %>%  str_detect(pattern = "Catho")]
  Vagas = arquivos[arquivos %>%  str_detect(pattern = "Vagas")]
  
  dadosCatho = lapply(Catho, read_csv)
  dadosVagas = lapply(Vagas, read_csv)
  
  # Colocando a coluna Pesquisa nos dados que não possui
  dadosCatho = lapply(1:length(dadosCatho), function(i){
    if ( ncol(dadosCatho[[i]])<11){
      dadosCatho[[i]] %>% mutate(Pesquisa = NA, Site = "Catho") 
    } else {
      dadosCatho[[i]] %>% mutate(Site = "Catho") }
  })
  
  dadosVagas = lapply(1:length(dadosVagas), function(i){
    if ( ncol(dadosVagas[[i]])<11){
      dadosVagas[[i]] %>% mutate(Pesquisa = NA, Site = "Vagas.com") 
    } else {
      dadosVagas[[i]] %>% mutate(Site = "Vagas.com") }
  })
  
  dadosCathot = do.call(rbind, dadosCatho)
  dadosVagast = do.call(rbind, dadosVagas)
  
  # Inserindo Ufs
  Siglas = read_excel("/cloud/project/InfoDemografico/Siglas.xls")
  names(Siglas) = c('UF','Cidade')
  
  Adds = data.frame(UF = c('DF','MG','CE','PE'),
                    Cidade = c('DISTRITO FEDERAL','MINAS GERAIS','CEARA','PERNAMBUCO'))
  Siglas = rbind(Siglas, Adds)
  
  # Função para remover acentos
  RM_Acentos = function(x) iconv(x, to = "ASCII//TRANSLIT")
  
  # Tratando nome das cidade
  dadosVagast %<>% mutate(Cidade = toupper(Cidade))
  Siglas$Cidade = RM_Acentos(Siglas$Cidade)
  dadosVagast$Cidade  = RM_Acentos(dadosVagast$Cidade)
  
  dadosVagast = merge(x = dadosVagast, y = Siglas, by = 'Cidade', all.x = T)
  dadosVagast[is.na(dadosVagast[,'UF']),]$Cidade = 'NAO INFORMADO'
  
  # Adicionando Empresa
  dadosCathot %<>% mutate(Empresa = NA)
  
  # Reordenando as colunas
  dadosCathot = dadosCathot[,names(dadosVagast)]
  
  # Juntando os dados
  Dados = rbind(dadosVagast, dadosCathot)

### Tratando os dados ###
  
## Organizando Perfil da vaga

  # Igualando Selecionando estágio
  Dados                                                              %<>% 
    mutate(Perfil = ifelse(Perfil == 'Estagiário', 'Estágio',
                           ifelse(Perfil != 'Estágio','Profissional',Perfil)))

## Organizando DataPubli

# Transformando DataPubli em data

  Dados1_n = which(str_detect(Dados$DataPubli,'/'))
  Dados2_n = which(str_detect(Dados$DataPubli,'Há ') | str_detect(Dados$DataPubli,'ago'))
  Dados3_n = which(Dados$DataPubli %in% c("Ontem","Hoje",'Today',"Yesterday"))
  
  Dados1 = Dados[Dados1_n,]
  Dados2 = Dados[Dados2_n,]
  Dados3 = Dados[Dados3_n,]
  
  Dados4 = Dados[-c(Dados1_n, Dados2_n,Dados3_n),]
  
  Dados1 %<>% 
    mutate(DataPubli = as.Date(DataPubli, "%d/%m/%Y"))
  
  Dados2 %<>% 
    mutate(DataPubli = as.numeric(removeWords(DataPubli,c('Há ',' dias',' days ago'))) + DataPesquisa) %>% 
    mutate(DataPubli = as.Date(DataPubli, "%d/%m/%Y"))
  
  Dados3 %<>% mutate(DataPubli = case_when(
    DataPubli %in% c("Ontem","Yesterday") ~ DataPesquisa - 1,
    DataPubli %in% c("Hoje",'Today')      ~ DataPesquisa
  ))
  
  # Dados4 %<>% 
  #   mutate(DataPubli = as.Date(as.numeric(DataPubli), origin = "2019-12-30"))
  
  Dados4 %<>% mutate(DataPubli = DataPesquisa - as.numeric(DataPubli)/(8.64e+7))
  
  Dadost = rbind(Dados1,Dados2, Dados3, Dados4)


## Organizando NomeVaga

# Removendo algumas palavras
  # Atenção! Esse campo não altera a coluna original. Servirá somente para fazer uma WordCloud dos nomes das vagas
  
  stopwordd = c("Estagiário", "Estágio", "Pleno", "Sênior", "Júnior", "SR", "JR", "PL", "área de", "na área de") %>% tolower()
  
  nomevagas = Dados$NomeVaga %>% tolower() %>% 
    removeWords( words = stopwordd) %>% 
    removePunctuation() %>% 
    str_replace_all(pattern = "business intelligence", replacement = "bi") %>% 
    str_trim() %>% data.frame(stringsAsFactors = F)

## Organizando Salário

# Agrupar em
# 
# - A Combinar
# - [1.000, 2.000)
# - [2.000, 3.000)
# - [3.000, 4.000)
# - [4.000, 5.000)
# - [5.000, 6.000)
# - [6.000, 7.000)
# - [7.000, 8.000)
# - [8.000, 9.000)
# - [9.000, 10.000)
# - [10.000, ~]

Dados %<>% 
  mutate(Salario = Salario %>%  
                    gsub(pattern = "Salário a combinar",  replacement = "A Combinar" ) %>% 
                    str_remove_all(pattern = "De")                                     %>% 
                    str_replace_all("Até ", "")                                        %>%
                    str_replace_all( "\\$", "")                                        %>% 
                    str_replace_all("R ", "")                                          %>% 
                    str_replace_all(pattern = "\\.",replacement =  "")                 %>% 
                    str_replace_all(pattern = ",",replacement =  ".")                  %>% 
                    str_trim()                                                         %>% 
                    str_replace_all("10 a 14", "10000 a 14000") )                      %>% 
  mutate(Salario = sapply(Salario, function(i){
    if (i != "A Combinar"){
      b = i %>% str_split(pattern = " a ") %>% .[[1]] %>% .[1] %>% as.numeric()
      case_when(b <= 1000 ~ "Até 1.000",
                b > 1000 & b < 2000~ "(1.000, 2.000)",
                b >= 2000 & b < 3000~ "[2.000, 3.000)",
                b >= 3000 & b < 4000~ "[3.000, 4.000)",
                b >= 4000 & b < 5000~ "[4.000, 5.000)",
                b >= 5000 & b < 6000~ "[5.000, 6.000)",
                b >= 6000 & b < 7000~ "[6.000, 7.000)",
                b >= 7000 & b < 8000~ "[7.000, 8.000)",
                b >= 8000 & b < 9000~ "[8.000, 9.000)",
                b >= 9000 & b < 10000~ "[9.000, 10.000)",
                b >= 10000 ~ "[10.000, ~]")
    } else {
      i
    }
  }))

#[^[:alnum:]]
## Organizando Pesquisa

Dados %<>%  mutate(Pesquisa = Pesquisa %>% 
                     tolower()  %>% 
                       str_replace_all("-", " "),# Substituindo a pontuação por espaço
                    NomeVaga = NomeVaga %>% tolower() %>% 
                      str_replace_all("b.i", "bi") %>% 
                      str_replace_all(pattern = "[[:punct:]]", replacement = " "),
                   Descricao = Descricao %>% tolower() %>% 
                     str_replace_all("b.i", "bi") %>% 
                     str_replace_all(pattern = "[[:punct:]]", replacement = " ") # Substituindo a pontuação por espaço
                     ) 

d = Dados %>% filter(is.na(Pesquisa) | Pesquisa == "de emprego")

#bb = Dados$Pesquisa %>% unique() %>% na.omit() %>% as.character()

bb = c("estatístic"   ,        "cientista de dados"   ,
        "data science"  ,        "business intelligence",
        "big data"       ,       "data scientist"        ) 
d %>%  group_by(Pesquisa) %>%  count()

d %<>% mutate(Pesquisa = case_when(
  str_detect(NomeVaga, pattern = "estatístic")~ "estatístico",
  str_detect(NomeVaga, "cientista de dados")  ~ "cientista de dados",
  str_detect(NomeVaga, "data science") ~ "data science",
  str_detect(NomeVaga, "business intelligence") | str_detect(NomeVaga, "bi")~ "business intelligence",
  str_detect(NomeVaga, "big data") ~ "big data",
  str_detect(NomeVaga, "data scientist") ~ "data scientist"
  
)) 

d[is.na(d$Pesquisa),] = d %>% filter(is.na(Pesquisa)) %>% 
  mutate(Pesquisa = case_when(
    str_detect(Descricao, pattern = "estatístic")~ "estatístico",
    str_detect(Descricao, "cientista de dados")  ~ "cientista de dados",
    str_detect(Descricao, "data science") ~ "data science",
    str_detect(Descricao, "business intelligence") | str_detect(Descricao, " bi ")~ "business intelligence",
    str_detect(Descricao, "big data") ~ "big data",
    str_detect(Descricao, "data scientist") ~ "data scientist"
)) 



tes = d %>% filter(is.na(Pesquisa)) 

tes %<>% mutate(Pesquisa = case_when(
  str_detect(Descricao, pattern = "estatístic")~ "estatístico",
  str_detect(Descricao, "cientista de dados")  ~ "cientista de dados",
  str_detect(Descricao, "data science") ~ "data science",
  str_detect(Descricao, "business intelligence") | str_detect(Descricao, "bi")~ "business intelligence",
  str_detect(Descricao, "big data") ~ "big data",
  str_detect(Descricao, "data scientist") ~ "data scientist"
  
)) 

cc = sapply(1:nrow(tes), function(i){
  sapply(bb, function(x){
    y = x %>% str_split(" ", simplify = TRUE)
    a = str_detect(tes$Descricao[i], pattern = y)
    ifelse(mean(a) == 1, TRUE, FALSE)
})  
 
}) %>% t()

y = x %>% str_split(" ", simplify = TRUE)
str_detect(tes$Descricao[23], pattern = y)

str_detect(d$Descricao[23],pattern = bb)

grepl(pattern, x, ignore.case = T)

apply(cc, 1, function(x){sum(x,na.rm = T)})

zeros =  which(apply(cc, 1, function(x){sum(x,na.rm = T)}) == 0)


idunico = Dados$idVaga %>% unique()
teste = Dados[Dados$idVaga == idunico[4],]
naquant = is.na(teste$Pesquisa) %>% sum()
if (naquant >= nrow(teste)){
  
}
Pe = teste %>% group_by(Pesquisa) %>% count() %>% na.omit()
maximo = Pe[which.max(Pe$n), 1]
teste %<>% mutate(Pesquisa = ifelse(is.na(Pesquisa), maximo, Pesquisa))  


b = which(Dados$Pesquisa == "de emprego")

deemprego = Dados[b,]

idunico = deemprego$idVaga %>% unique()

deempregoL = Dados[-b,] 

a = Dados[Dados$idVaga == "15321622",]

tes = sapply( deemprego$idVaga, function(x){x %in% deempregoL$idVaga})

# deemprego %>% 
#   mutate(Pesquisa = case_when(
#     Pesquisa == 
#   
# ))

#str_detect("analista estatístico", pattern = "estatístic")




# Limpando a área da Descrição
Dados %<>% mutate(Descricao = Descricao %>% 
                    str_replace_all(pattern = "[[:punct:][:blank:]]+", replacement = " ") %>% # Substituindo a pontuação por espaço
                    tolower()) %>% 
  mutate(Descricao = RM_Acentos(Descricao))

pa = function(x, pattern){as.integer(grepl(pattern, x, ignore.case = T))}

Dados %<>% mutate(Pacote_Office = Descricao %>%  pa("pacote office"),
                     Excel = Descricao %>% pa("excel"),
                     SQL = Descricao %>% pa("sql"),
                     Word = Descricao %>% pa("word"),
                     SAS = Descricao %>% pa(" sas "),
                     SPSS = Descricao %>% pa("spss"),
                     java = Descricao %>% pa("java"),
                     R = Descricao %>% pa(" r "),
                     Tableau = Descricao %>% pa("tableau"),
                     Ingles = Descricao %>% pa("ingles") ,
                     Hadoop = Descricao %>% pa("hadoop"), 
                     Spark = Descricao %>% pa("spark"),
                     PosG = (Descricao %>% pa("pos graduacao")) + 
                       (Descricao %>% pa("mestrado")) + 
                       (Descricao %>% pa("doutorado")),
                     Machine_learning = (Descricao %>% pa("machine learning")) + 
                       (Descricao %>% pa("aprendizado de maquina")),
                     PowerBI = Descricao %>% pa("power bi"),
                     Python = Descricao %>% pa("python"),
                     Stata = Descricao %>% pa("stata"), 
                     Azure = Descricao %>% pa("azure"),
)












# Dados %>% 
#   distinct(idVaga, .keep_all = T) %>% 
#   group_by(Salario) %>% 
#   summarise ( n = n()) %>% 
#   arrange(desc(n))

case_when(
#  str_detect("estatístico - cientista de dados", pattern = "estatístic") ~ "estatístico", 
  str_detect("estatístico - cientista de dados", pattern = "cientista de dados")~"ds")

# Dados %>% 
#   distinct(idVaga, Cidade, .keep_all = T) %>% group_by(Perfil) %>% summarise(n = n()) %>% arrange(desc(n))


# Identificar os programas requeridos

# Quantidade de vagas ofertadas no período


Dados %>% select(idVaga, Cidade) %>% unique() %>% nrow()
Dados %>% distinct(idVaga, Cidade, .keep_all = T) %>%  select(QuantidadeVaga) %>% sum()
