# Pacotes Dashboard
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinythemes)

# Pacote para Gráfico
library(plotly)


#----------------------------------------------------------------
#------------------------- Header -------------------------------
#----------------------------------------------------------------
    {
Header = dashboardHeaderPlus(
        
        # Caracteristica do cabeçalho
        title = 'Érica',
        
        
        # Menu de configurações a direita
        enable_rightsidebar = TRUE,
        rightSidebarIcon = 'gears'
        
) # Fim do Header
}  

#----------------------------------------------------------------
#------------------------- Sidebar -------------------------------
#----------------------------------------------------------------
    {
Sidebar = dashboardSidebar(
    
  sidebarMenu(
     menuItem('Home',            tabName = 'Home',        icon = icon('dashboard')    ),
     menuItem('Vagas',           tabName = 'Vagas',       icon = icon('folder-open')  ),
     menuItem('Salário',         tabName = 'Salario',     icon = icon('dollar-sign')  ),
     menuItem('Requisitos',      tabName = 'Requisitos',  icon = icon('funnel-dollar')),
     menuItem('Micro Dados',     tabName = 'Microdados',  icon = icon('dollar-sign')  ),
     menuItem('Sobre',           tabName = 'Sobre',       icon = icon('folder-open')  )
   ) # Fim do sidebarMenu
         
) # Fim do Sidebar
} 
 
#----------------------------------------------------------------
#------------------------- Bory ---------------------------------
#----------------------------------------------------------------
    {
      
# Parte visual do Dashboard
Body = dashboardBody(
  
  # Função para adicionar informação em cada aba criadas no menu inicial
  tabItems(

    # Inicio da aba Home do menu
    tabItem(tabName = 'Home',
            fluidRow(
              infoBoxOutput("InfReceita",width = 3)
            ) # Fim do fluidRow da aba Home
    ), # Fim da aba Home
        
    # Inicio da aba Home do menu
    tabItem(tabName = 'Vagas',
            fluidRow(
              
              column(width = 10, offset = 1,
              
              infoBoxOutput("QTVagas",width = 4),
              infoBoxOutput("QTProfissional",width = 4),
              infoBoxOutput("QTEstagio",width = 4)
                     
              ) # Fim column dos infoBox
              
              
            ) # Fim do fluidRow da aba Home
    ), # Fim da aba Home
    
    # Inicio da aba Salário do menu
    tabItem(tabName = 'Salario',
      fluidRow(
  
      # Mostrando Gráfico mapa do salário
      plotlyOutput('SalarioGrafico')
  
      ) # Fim do fluidRow da aba salário
    ), # Fim da aba Salário
    
    # Inicio da aba Número de vagas do menu
    tabItem(tabName = 'NumeroVagas',
            fluidRow(
              
            ) # Fim do fluidRow da aba Número de vagas
    ), # Fim da aba Número de vagas Tipo da vaga
    
    # Inicio da aba Tipo da vaga do menu
    tabItem(tabName = 'TipoVaga',
            fluidRow(
              
            ) # Fim do fluidRow da aba Tipo da vaga
    ) # Fim da aba Tipo da vaga
     
  ) # Fim do tabItems
) # Fim do Bory
}

#----------------------------------------------------------------
#------------------------- UI -----------------------------------
#----------------------------------------------------------------
    {
UI = dashboardPagePlus(
  
  # Alterações na pagina geral
  skin = 'green', 
  
  # Menu a direita da página
  rightsidebar = rightSidebar(),
  title        = 'Right Sidebar',
  
Header,Sidebar,Body) # Fim do UI
}


#----------------------------------------------------------------
#------------------------- Server -------------------------------
#----------------------------------------------------------------
    {
Server = function(input, output) { 
  
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
  
  # Limpeza inicial dos dados
  {

  source("A.R")
  
  ### Tratando os dados ###
  
  ## Organizando Perfil da vaga
  
  # Igualando Selecionando estágio
  Dados                                                              %<>% 
    mutate(Perfil = ifelse(Perfil == 'Estagiário', 'Estágio',
                           ifelse(Perfil != 'Estágio','Profissional',Perfil)))
  }
  
  # infoBox página Vagas
  {
    
    # Descobrindo número de vagas
    NVagas = Dados %>% select(idVaga, Perfil, QuantidadeVaga)
    
    
    InfoBoxVagas = Dados %>% select(Perfil) %>% filter(Perfil == "Estágio") %>% count()
    
    
    # Caixa de informação da quantidade total de vagas
    output$QTVagas = renderInfoBox({
      infoBox(title    = "Total de Vagas", 
              subtitle = '3000',
              icon     = icon("credit-card"),
              fill     = T,
              color    = "blue") })
    
    # Caixa de informação da quantidade total de vagas para profissionais
    output$QTProfissional = renderInfoBox({
      infoBox(title    = "Vagas para Profissionais", 
              subtitle = '1800',
              icon     = icon("credit-card"),
              fill     = T,
              color    = "blue") })
    
    # Caixa de informação da quantidade total de vagas para estágio
    output$QTEstagio = renderInfoBox({
      infoBox(title    = "Vagas de Estágio", 
              subtitle = '1200',
              icon     = icon("credit-card"),
              fill     = T,
              color    = "blue") })
    
  }
  
  
  } # Fim do Server
}

#----------------------------------------------------------------
#------------------------- App ----------------------------------
#----------------------------------------------------------------
    {
shinyApp(UI, Server)
}

