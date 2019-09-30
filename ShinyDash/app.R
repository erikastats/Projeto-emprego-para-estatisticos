##### Pacotes #####

# Dashboard
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinythemes)

# Gráfico
library(plotly)

# Manipulação de dados
library(tidyverse)
library(DT)

# Importação de dados
library(readr)

##### Importando os dados #####


Dados = read_csv("Dados.csv")
DadosUnicos = Dados %>% distinct(idVaga, Cidade, .keep_all = T)
Requisitos = names(DadosUnicos)[14:39] %>% str_replace_all("_", " ")



#----------------------------------------------------------------
#------------------------- Header -------------------------------
#----------------------------------------------------------------
    {
Header = dashboardHeaderPlus(
        
        # Caracteristica do cabeçalho
        title = 'Stats Vagas',
        
        
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
#------------------------- Right Sidebar -------------------------------
#----------------------------------------------------------------
{
  Rightsidebar = rightSidebar(
    background = "dark",
    rightSidebarTabContent(
      id = "RSB",
      icon = "filter",
      title = "",
      active = TRUE,
      pickerInput(
        inputId = "Req",
        label = "Requisitos", 
        choices = Requisitos,
        multiple = TRUE
      )
    )
  ) # Fim do rightSidebar
    

} 



#----------------------------------------------------------------
#------------------------- Body ---------------------------------
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
      
               valueBoxOutput("SAcombinar"),
               valueBoxOutput("SComValor"),
               box(dataTableOutput("TabelaSalario")),
  
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
  rightsidebar = Rightsidebar,
  title        = 'Right Sidebar',
  
Header,Sidebar,Body) # Fim do UI
}


#----------------------------------------------------------------
#------------------------- Server -------------------------------
#----------------------------------------------------------------
    {
Server = function(input, output) { 

  
#-------------------------VAgas---------------------------------------
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
#-------------------------Salario---------------------------------------  
  
  output$SAcombinar = renderValueBox({
      Acombinar = DadosUnicos %>% filter(Salario == "A Combinar") %>% nrow()
      valueBox(Acombinar, 
             subtitle = "Salário a Combinar", 
             icon = icon("hand-holding-usd"),
              color = "orange")
  })
  
  output$SComValor = renderValueBox({
    QSalarios = DadosUnicos %>% filter(Salario != "A Combinar") %>% nrow()
    valueBox(QSalarios, 
             subtitle = "Salário com valor", 
             icon = icon("money-bill-wave-alt"),
             color = "lime")
  })
  
  output$TabelaSalario = renderDT({
    DadosUnicos
  })
  } # Fim do Server
}

#----------------------------------------------------------------
#------------------------- App ----------------------------------
#----------------------------------------------------------------
    {
shinyApp(UI, Server)
}

