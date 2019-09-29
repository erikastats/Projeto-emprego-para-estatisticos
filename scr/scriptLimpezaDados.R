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
  
  # Bilioteca pra limpesa de dados
  library(tm)
    library(stringr)

  # Ler excel
  library(readr)
  library(readxl)


  
#### Importando os dados e Juntando em um único arquivo ####

  # Identificando todos os arquivos do diretório com extensão .csv
  setwd("../data")
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
  Siglas = read_excel("../InfoDemografico/Siglas.xls")
  names(Siglas) = c('UF','Cidade')
  
  Adds = data.frame(UF = c('DF','MG','CE','PE'),
                    Cidade = c('DISTRITO FEDERAL','MINAS GERAIS','CEARA','PERNAMBUCO'))
  Siglas = rbind(Siglas, Adds)
  
  # Função para remover acentos
  RM_Acentos = function(x) {iconv(x,from="UTF-8",to="ASCII//TRANSLIT")}
  
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
  

  Dados4 %<>% mutate(DataPubli = DataPesquisa - as.numeric(DataPubli)/(8.64e+7))
  
  Dados = rbind(Dados1,Dados2, Dados3, Dados4)


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
                      str_replace_all(pattern = "[[:punct:]]", replacement = " ") %>% 
                      RM_Acentos(),
                   Descricao = Descricao %>% tolower() %>% 
                     str_replace_all("b.i", "bi") %>% 
                     str_replace_all(pattern = "[[:punct:]]", replacement = " ") %>% # Substituindo a pontuação por espaço
                       RM_Acentos(),
                   Cidade = Cidade %>% toupper() %>% 
                     RM_Acentos()
                     ) 

d = Dados %>% filter(is.na(Pesquisa) | Pesquisa == "de emprego")

Dados[is.na(Dados$Pesquisa) | Dados$Pesquisa == "de emprego",] = Dados[is.na(Dados$Pesquisa) | Dados$Pesquisa == "de emprego",] %>% mutate(Pesquisa = case_when(
  str_detect(NomeVaga, pattern = "estatistic")~ "estatístico",
  str_detect(NomeVaga, "cientista de dados")  ~ "cientista de dados",
  str_detect(NomeVaga, "data science") ~ "data science",
  str_detect(NomeVaga, "business intelligence") | str_detect(NomeVaga, "bi")~ "business intelligence",
  str_detect(NomeVaga, "big data") ~ "big data",
  str_detect(NomeVaga, "data scientist") ~ "data scientist"
)) 


Dados[is.na(Dados$Pesquisa),] = Dados %>% 
                                  filter(is.na(Pesquisa)) %>% 
                                  mutate( Pesquisa = case_when(
                                    str_detect(Descricao, pattern = "estatistic") | 
                                        str_detect(Descricao, pattern = "series temporais") |
                                        str_detect(Descricao, pattern = "statistical")~ "estatístico",
                                    str_detect(Descricao, "cientista de dados")|
                                        str_detect(Descricao, "machine learning")|
                                        str_detect(Descricao, "cientistas de dados")  ~ "cientista de dados",
                                    str_detect(Descricao, "data science") ~ "data science",
                                    str_detect(Descricao, "business intelligence")|
                                        str_detect(Descricao, " bi ")| 
                                        str_detect(Descricao, "power bi") ~ "business intelligence",
                                    str_detect(Descricao, "big data") ~ "big data",
                                    str_detect(Descricao, "data scientist") ~ "data scientist"
                                )) 

Dados %<>% filter(!is.na(Pesquisa))

# Acrescentando Requisitos

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




