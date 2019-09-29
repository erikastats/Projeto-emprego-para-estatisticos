####################
#'  Dados Catho   ##
####################
DadosCatho = function(){
  # Bibliotecas
  
  library(tidyverse)
  library(rvest)
  library(stringr)
  
  # Links de pesquisa sendo Profissional e Estagiário, respectivamente
  
  linkEP = "https://www.catho.com.br/vagas/?q=Estatistico&pais_id=31&faixa_sal_id_combinar=1&perfil_id=1&order=score&where_search=1&how_search=2"
  linkEE = "https://www.catho.com.br/vagas/?q=Estatistico&pais_id=31&faixa_sal_id_combinar=1&perfil_id=3&order=score&where_search=1&how_search=2"
  linkCD = "https://www.catho.com.br/vagas/?q=Cientista+de+dados&pais_id=31&faixa_sal_id_combinar=1&perfil_id=1&order=score&where_search=1&how_search=2"
  linkDS = "https://www.catho.com.br/vagas/?q=Data+Scientist&pais_id=31&faixa_sal_id_combinar=1&perfil_id=1&order=score&where_search=1&how_search=2"
  linkDSE = "https://www.catho.com.br/vagas/?q=Data+Scientist&pais_id=31&faixa_sal_id_combinar=1&perfil_id=3&order=score&where_search=1&how_search=2"
  linkBI = "https://www.catho.com.br/vagas/?q=BUSINESS+INTELLIGENCE&pais_id=31&faixa_sal_id_combinar=1&perfil_id=1&order=score&where_search=1&how_search=2"
  linkbiE = "https://www.catho.com.br/vagas/?q=BUSINESS+INTELLIGENCE&pais_id=31&faixa_sal_id_combinar=1&perfil_id=3&order=score&where_search=1&how_search=2"
  
  ###########################################################3
  #' Cria o data.frame com as informações
  #'
  #' @param linkPesquisa,tipo Link de Pesquisa de vaga de estatístico na Catho, Tipo de vaga (profissional ou Estagiário)
  #'
  #' @return DataFrame id da Vaga, Cargo ofertado, Cidade da vaga, Estado da vaga, Quantidade de vagas ofertadas,
  #'  Texto de descrição com pré-requisitos, Data da publicação, Data da realização da pesquisa,
  #'  Perfil pesquisado
  ##########################################################
  
  
  informacoes = function(linkPesquisa, tipo){
    
    
    
    # link inicial e conteúdo incial
    pagina = linkPesquisa %>%  
      read_html(encoding = "UTF-8")
    
    # Quantidade de páginas com vagas
    qPaginas = pagina %>% 
      html_nodes(".infos p") %>% 
      html_text() %>% 
      str_remove_all("\n            Total de anúncios: ") %>% 
      str_remove_all("\n") %>% 
      str_replace_all("\\.", "") %>% 
      str_trim() %>% 
      as.numeric()
    
    np = ceiling(qPaginas/10)
    
    Pesquisa = pagina %>% 
      html_nodes("ul.breadcrumb li") %>% 
      html_text() %>% .[3] %>% 
      str_remove_all("\n") %>%
      str_trim()
    
    # Novos links
    links = paste0(linkPesquisa, "&page=",1:np)
    
    # lista de Dataframe das informações de cada página
    dadospagina = lapply(links, function(link){
      
      #Importando as informações da página
      infpagina = link %>%  read_html(encoding = "UTF-8")
      
      #Separando informações
      dadosemprego = infpagina %>% html_nodes(".boxVaga")
      
      #id da Vaga
      idVaga = dadosemprego %>% html_attr("id")
      
      #Cargo anunciado
      NomeVaga = dadosemprego %>% html_nodes(".viewVagaAction") %>% 
        html_text() %>% str_remove_all(pattern =  "\n    ") %>% 
        str_remove_all(pattern = "\n")
      
      #Caso haja ofertas com mais de uma vaga
      QuantidadeVaga = dadosemprego %>%
        html_attr("data-quantidadevagas")  %>% as.numeric()
      qmais = which(QuantidadeVaga !=1)
      
      #Local onde está sendo ofertado a vaga
      Cidade = dadosemprego %>% 
        html_nodes("span a") %>% 
        html_text() %>% 
        str_sub(end = -4L) %>% 
        str_replace_all(pattern = "-", replacement = " ")
      UF = dadosemprego %>% 
        html_nodes("span a") %>% 
        html_text() %>% 
        str_sub(start = -2L)
      
      Local = data.frame(Cidade, UF, stringsAsFactors = F) 
      #names(Local) = c("CIDADE","UF")
      if (length(qmais) != 0){
        if (nrow(Local) == length(idVaga)){
          qmais = NULL
        } else{
          Localdupli = Local[-cumsum(QuantidadeVaga),]
          Local = Local[cumsum(QuantidadeVaga),]
          novaquanti = QuantidadeVaga[qmais] - 1
          QuantidadeVaga[qmais] = 1}
      }
      
      #Texto de informações e requisitos
      Descricao = sapply(dadosemprego,function(x){
        a = x %>% 
          html_nodes("div div") %>% 
          html_text() %>% 
          unique() 
        a[length(a)] %>% 
          str_replace_all(pattern = ",", replacement = ";") %>% 
          str_trim() %>% 
          str_remove_all("\n") %>% 
          str_remove_all("\r") %>%
          str_remove_all("\\?") })
      
      #Salário Oferecido
      Salario = dadosemprego %>% 
        html_nodes(".salarioLocal") %>% 
        html_text()
      
      #Data da publicação
      DataPubli = dadosemprego %>%
        html_attr("data-gtm-dimension-44")
      
      #Gerando data.frame final
      df = data.frame(idVaga, NomeVaga, Local, Salario, QuantidadeVaga, 
                      DataPesquisa = Sys.Date(), Perfil = tipo, Descricao, 
                      DataPubli,
                      stringsAsFactors = F)
      if (length(qmais) != 0){
        dfnovo = lapply(1:length(qmais), function(i){
          
          Localp = Localdupli[1:novaquanti[i],]
          Localdupli = Localdupli[-c(1:novaquanti[i]),]
          
          data.frame(df[qmais[i],1:2], Localp, df[qmais[i],5:10])
          
        })
        dfnovo = do.call(rbind,dfnovo)
        names(dfnovo) = names(df)
        df = rbind(df, dfnovo)
      } 
      
      df %>% arrange(idVaga) # Ordenando por idVaga
    })
    
    da = do.call(rbind,dadospagina )
    da %>% mutate(Pesquisa = Pesquisa)# Transformando a lista gerada de cada página em data.frame
  } 
  
  
  ################# Aplicando a função
  
  dadoProfissional = informacoes(linkEP, "Profissional")
  dadoEstagiario = informacoes(linkEE, "Estagiário")
  dadoCientistadedados = informacoes(linkCD, "Profissional")
  dadoDataScientist = informacoes(linkDS, "Profissional")
  dadoDSEstagio = informacoes(linkDSE, "Estagiário")
  dadoBIP = informacoes(linkbiE,"Profissional")
  dadoBIE = informacoes(linkBI, "Estagiário")
  
  
  dados = rbind(dadoProfissional, 
                dadoEstagiario,
                dadoCientistadedados,
                dadoDataScientist,
                dadoDSEstagio,
                dadoBIP,
                dadoBIE)
  nome = Sys.Date()  %>% as.character() %>% str_remove_all(pattern = "-")
  #write_csv(dados, path = paste0(nome, "DadosEmprego.csv"))
  write_excel_csv(dados, path = paste0(nome, "DadosCatho.csv"))
 }
