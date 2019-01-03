library(shiny)
library(DT)
library(shinyjs)
library(htmltools)
library(bootstrap)
library(plyr)
library(dplyr)
library(uuid)
library(ggplot2)
library(plotly)

setwd("C:/Users/Haowen/Desktop/BA/Y3S2/BT4222")

value <- read.csv("C:/Users/Haowen/Desktop/BA/Y3S2/BT4222/accountDailyNetWorthV2.csv")
coin <- read.csv("C:/Users/Haowen/Desktop/BA/Y3S2/BT4222/accountDailyNetWorthByCoins.csv")

ui <- navbarPage(windowTitle = "Crypto Guru",
                 img(src="crypto.png", height=20, style="padding-left:10px; padding-right:10px"),
                 tabPanel("Accounts", uiOutput('Accounts')),
                 tags$head(tags$style(HTML(".navbar { background-color: #d0cece;},
                                           #.nav navbar-nav > li[class=active]    > a {background-color: black; color:white}
                                           ")))
                 )

Crypto_page <- {
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput("pundit", "Select pundit", 
                    choices=c(" ", "anbessa100", "cryptousemaki", "CryptoYoda1338", "officialmcafee", "ZeusZissou"), 
                    selected = " "),
        uiOutput("coin_price"),
        actionButton("coin_goButton", "Go", style="color: white; background-color: #002da3; width: 160px; height: 35px"),
        hr(),
        p("Shows value of pundits and breakdown of each crypto coin type")
      ),
      mainPanel(
        plotlyOutput("graph"),
        plotlyOutput("coin"),
        plotlyOutput("user")
      )
    )
  )
}

server <- function(input, output) {
  output$Accounts <- renderUI(Crypto_page)
  temp <- function(user){
    usercoindf <<- coin[coin$selectedUser == user,]
    usercoindf1 <- usercoindf[,"symbol"]
    usercoindf1 <- as.data.frame(usercoindf1)
    apply(usercoindf1,1,FUN=function(x) toString(unique(x)))
  }
  
  crypto_coin = reactive({
    coin_pundit = input$pundit
    
    if (is.null(coin_pundit))
      return()
    temp(coin_pundit)
  })
  
  output$coin_price = renderUI({
    if (is.null(input$pundit) || input$pundit == " ")
      return()
    
    selectInput(inputId = "cryptocoin",
                label = "Select a crpyto coin",
                choices = c(" ", crypto_coin()),
                selected = " " 
    )
  })
  
  observeEvent(
    eventExpr <- input[["coin_goButton"]],
    handleExpr <- {
      userNetworth <- plot_ly() %>%
        layout(title="User Net Worth", yaxis = list(range = c(0, 1100000)))%>%
        layout(hovermode = 'compare')
      users <- unique(value$selectedUser)
      for(user in users){
        uservalue <- filter(value, selectedUser == user)
        userNetworth <- add_trace(userNetworth, x = as.Date(uservalue$date), y = uservalue$dailyNetWorth, name=user, mode = "lines")
        
      }
      output$graph <- renderPlotly({
        userNetworth
      })
      
      user_coin <- plot_ly() %>%
        layout(title="All coin holding values by user", hovermode = 'compare')
      cointype <- unique(coin$symbol)
      u=input$pundit
      for(c in cointype){
        coinvalue <- filter(coin, symbol == c & selectedUser == u)
        user_coin <- add_trace(user_coin, x = as.Date(coinvalue$currentDate), y = coinvalue$coinHoldingValue, name=c, mode = "lines")
      }
      output$coin <- renderPlotly({
        user_coin
      })
      
      coin_user <- plot_ly() %>%
        layout(title="Value of a single coin across users", hovermode = 'compare')
      users <- unique(coin$selectedUser)
      c=input$cryptocoin
      for(u in users){
        coinvalue <- filter(coin, symbol == c & selectedUser == u)
        coin_user <- add_trace(coin_user, x = as.Date(coinvalue$currentDate), y = coinvalue$coinHoldingValue, name=u, mode = "lines")
        output$user <- renderPlotly({
          coin_user
        })
      }
    }
  )
}

shinyApp(ui = ui, server = server)