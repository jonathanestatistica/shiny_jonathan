library(tidyverse)
library(shiny)
library(bslib)  # para escolher diferentes temas

# lendo os resultados:
results <- readRDS("results/results.rds")
tab_results <- readRDS("results/tab_results.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Análise gráfica do vício relativo"),
  
  #theme = bs_theme(bootswatch = "darkly"),
  #theme = bs_theme(bootswatch = "yeti"),
  #theme = bs_theme(bootswatch = "journal"),
  #theme = bs_theme(bootswatch = "lumen"),
  #theme = bs_theme(bootswatch = "lux"),
  
  
  fluidRow(
    column(3,
           # selecionando o tamanho amostral:
           checkboxGroupInput(
             inputId = "sizes",
             label = "Tamanho da amostra",
             choices = c("n50", "n100"),
             selected = c("n50", "n100")
           ),
           
           # funções de ligação:
           checkboxGroupInput(
             inputId = "links",
             label = "Funções de Ligação",
             choices = c("logit", "probit", "cloglog"),
             selected = c("logit", "probit", "cloglog")
           ),
    ),
    column(9, 
           # Mostra o gráfico
           mainPanel(
             plotOutput("rbPlots")
           )         
    ),
    
    titlePanel("Resumo dos resultados da simulação de Monte Carlo"),
    fluidRow(
      column(12,
             mainPanel(
               dataTableOutput("tabResults")
             ) 
      )
      
    )
    
    
  )
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$rbPlots <- renderPlot({
    results %>%
      filter(size %in% input$sizes, link %in% input$links) %>%
      ggplot(aes(x = coef, y = rb, color = link)) +
      geom_boxplot() +
      geom_abline(intercept = 0, slope = 0, color = "blue") +
      facet_grid(size ~ link) +
      labs(x = "coefficients", y = "Relative Bias (%)") +
      theme(legend.position="bottom")
    
    
  })
  
  output$tabResults <- renderDataTable(
    tab_results %>%
      filter(size %in% input$sizes,
             link %in% input$links)
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
