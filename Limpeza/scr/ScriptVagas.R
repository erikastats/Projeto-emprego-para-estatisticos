#Web Scraping Site Vagas.com
DadosVaga = function(){
  #Bibliotecas
  library(xml2)
  library(rvest)
  library(stringr)
  library(tidyverse)
  library(magrittr)
  
  #Palavra para busca no site
  Palavra_chave=c("Estatístico", 
                  "big-data", 
                  "cientista-de-dados", 
                  "data-science", 
                  "BUSINESS-INTELLIGENCE")
  
  
  informacoes = function(palavra){
    
    #Descobrindo o número de paginas com vagas para a palavras chave escolhida
    url = paste0("https://www.vagas.com.br/vagas-de-",palavra,"?pagina=",1,"&amp")
    
    numero_paginas = url            %>% 
      read_html()                   %>%
      html_nodes("#maisVagas")    %>%
      html_attr("data-total")
    
    #   Caso a palavra chave retorne uma quantidade insuficiente de vagas para preencher
    # mais do que uma pagina, esse codigo corige o valor caracter(0) para 1 da
    # variavel numero.paginas
    
    if(length(numero_paginas) == 0 ){ numero_paginas=1 }
    
    url_site = paste0("https://www.vagas.com.br/vagas-de-", Palavra_chave ,"?pagina=", 1:numero_paginas,"&amp")
    
    #    Extraindo o adicional que indentifica o link que 
    #  informa mais detalhes para cada vaga
    
    link_vaga = lapply(url_site, function(link){
      link                                  %>% 
        read_html(encoding = "UTF-8")       %>%
        html_nodes(".link-detalhes-vaga")   %>%
        html_attr("href")
    }) %>% unlist()
    
    data_publi = lapply(url_site, function(link){
      link                                  %>% 
        read_html(encoding = "UTF-8")       %>%
        html_nodes(".data-publicacao")   %>%
        html_text()
    }) %>% unlist()
    
    
    # for (i in 1:numero_paginas) {
    #   Detalhe_vaga.T = url_site[i]          %>% 
    #     read_html(encoding = "UTF-8")       %>%
    #     html_nodes(".link-detalhes-vaga")   %>%
    #     html_attr("href")
    # 
    # Detalhe_vaga = c(Detalhe_vaga.T, Detalhe_vaga)
    # }
    
    
    #Detalhe_vaga = lapply(url_site, function(url_i){
    #                A = url_i     %>% 
    #                read_html(encoding = "UTF-8")       %>%
    #                html_nodes(".link-detalhes-vaga")   %>%
    #                html_attr("href")
    #  R = data.frame(A)}) 
    #Detalhe_vaga = do.call(rbind, Detalhe_vaga ))
    
    #   Salvando o link completo para cada vaga mais detalhada
    url_vaga = paste0("https://www.vagas.com.br", link_vaga)
    
    Dados = lapply(url_vaga, function(url){
      
      #Lendo informações da vaga
      url_vaga_i = url                            %>% 
        read_html(encoding = "UTF-8")
      
      #Coletando o nome da vaga
      NomeVaga = url_vaga_i                      %>%
        html_nodes(".nome-do-cargo")    %>%
        html_text(trim = T)
      
      #Coletando a data de publicação da vaga
      # Data = url_vaga_i                               %>%
      #   html_nodes("li:nth-child(4) div span")   %>%
      #   html_text(trim = T) 
      
      #Coletando a descrição de cada vaga
      Descricao = url_vaga_i                   %>%
        html_nodes(".texto")         %>%
        html_text(trim = T)          %>%
        str_remove_all("[\n\r?,;-]") %>% 
        str_replace_all(" +", " ")   %>%
        str_trim()
      #str_replace_all(pattern = ",", replacement = "*")   %>%
      #str_replace_all(pattern = ";", replacement = "*")   %>% 
      #str_trim() 
      
      #Coletando para qual cargo é a vaga
      Perfil  = url_vaga_i                                %>%
        html_nodes("li:nth-child(3)  div  span")  %>%
        html_attr("title") %>% .[1]
      
      #Coletando para qual cidade é a vaga
      Cidade = url_vaga_i                      %>%
        html_nodes(".info-localizacao") %>%
        html_attr("title")
      
      #Coletando id de cada vaga
      idVaga = url_vaga_i                  %>%
        html_nodes(".id-da-vaga")   %>%
        html_text()
      
      #Quantidade de vagas por anúncio
      QuantidadeVaga = url_vaga_i                   %>% 
        html_nodes(".qtdPosicoes")   %>%
        html_text()                  %>% 
        str_remove_all("[\n\r?,;-]") %>% 
        str_remove_all("vagas")      %>%
        str_trim()                   %>% 
        as.numeric()
      if ( length(QuantidadeVaga) == 0){ QuantidadeVaga = 1}
      
      #Coletando perfil da vaga
      # Perfil = url_vaga_i                     %>%
      #   html_nodes("div span > span")  %>%
      #   html_text()                    %>% 
      #   str_remove_all("\n")           %>%
      #   str_trim()                     %>%
      #   paste0(collapse = "")
      
      #Coletando para qual empresa é a vaga
      Empresa = url_vaga_i                    %>%
        html_nodes(".empresaVaga")    %>%
        html_text(trim = T)
      
      #Coletando o salário respectivo a vaga
      Salario = url_vaga_i                               %>%
        html_nodes("li:nth-child(1) div span")   %>%
        html_text(trim = T)                      %>%
        str_remove_all("\n")                     %>% 
        str_replace_all(" +", " ")
      
      #Resultado = data.frame(id, Nome, Empresa, Cargo, Salario, QuantidadeVaga = c(rep(1,length(id))), DataPesquisa = Sys.Date(),  Descricao)
      
      
      data.frame(idVaga, NomeVaga, Cidade,  Salario, QuantidadeVaga, 
                 DataPesquisa = Sys.Date(),  Perfil,   Descricao, Empresa, stringsAsFactors = F)
    })  
    dados = do.call(rbind, Dados )        #Fim da coleta de dados
    dados %>% mutate(DataPubli = data_publi, Pesquisa = palavra)
    
  }
  
  # Fazendo todas as pesquisas de emprego relacionado a estatístico
  
  Conteudo = lapply(Palavra_chave, informacoes)
  Conteudo = do.call(rbind, Conteudo ) %>% unique()
  
  
  # Exportando dados em formato .csv
  dia = Sys.Date() %>% str_remove_all("-")
  write_excel_csv(Conteudo, path = paste0(dia,"DadosVagas.csv"))
}
