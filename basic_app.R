
library(tidyverse)
library(shiny)
library(Cairo)
library(ggthemes)
library(plotly)
library(shiny)

options(scipen = 999)
options(shiny.usecairo=T)

data <- read_csv("data.csv")

ui <- fluidPage(
  titlePanel("Common ETF Investment Portfolio Performance Chart"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "ini_cap", label = "Principal Investment (in US$)", value = 1000, min = NA, max = NA, step = NA),
      numericInput(inputId = "age", label = "Age", value = 22, min = NA, max = NA, step = NA),
      numericInput(inputId = "depo", label = "Annual Deposit (in US$)", value = 100, min = NA, max = NA, step = NA),
      checkboxGroupInput(inputId = "stock_choice", label = "Choose Stock ETFs", 
                         choices = list(
                           "VTI",
                           "VTV",
                           "VO",
                           "VBR",
                           "VEA",
                           "VWO",
                           "SPY"
                           ),
                         selected = list("VTI","VTV","VO","VBR","VEA","VWO","SPY")),
      sliderInput(inputId = "stockp", label = "Percent Stock (higher is riskier)", min = 0, max = 100, value = 15, step = 1),
      checkboxGroupInput(inputId = "bond_choice", label = "Choose Bond ETFs", 
                         choices = list(
                           "VBLTX",
                           "AGG",
                           "MUB",
                           "BWX",
                           "EMB"
                           ), 
                         selected = list("VBLTX","AGG","MUB","BWX","EMB")),
      submitButton(text = "See Portfolio Performance")
    ),
  
  mainPanel(
    plotlyOutput("outplot"),
    htmlOutput("text")
  )
)
)


server <- function(input, output) {
  output$text <- renderUI({
    HTML(
      "<p>ETF: Exchange-Traded Fund.</p>
<p>Tax not included in earnings estimate.&nbsp;</p>
<p>Total returns is calculated from 10-year past APY (Average Percentage Yield) data for included ETFs), assuming equal distribution between ETFs in the same category.</p>
<ol>
  <li>Bearish: Lower quartile point of APY data using normal distribution.</li>
  <li>Bullish: Upper quartile point of APY data using normal distribution.</li>
  <li>Mean: Mean of APY data.</li>
  <li>Median: Median of APY data.</li>
</ol>
<p>This information has been prepared for informational purposes only and do not constitute financial, legal, tax or any other advice.</p>
<p>
  <br>
</p>"
    )
  })
  
  output$outplot <- renderPlotly({
    age <- input$age
    ini_cap <- input$ini_cap
    depo <- input$depo
    stockp <- input$stockp
    
    bondp <- 100 - stockp
    len <- 67 - age
    length <- seq(1,1+len,1)
    ylength <- length+2019
    stock_choice <- c("VTI","VTV","VO","VBR","VEA","VWO","SPY")
    bond_choice <- c("VBLTX","AGG","MUB","BWX","EMB")
    
    stock1 <- data %>% 
      filter(
        ticker %in% stock_choice
      ) %>% 
      select(-c(label)) %>% 
      pivot_longer(
        cols=-c(ticker,etf_type),
        names_to = "pastyear",
        values_to = "rate"
      ) %>% 
      select(rate) %>% 
      summarise(
        mid = median(rate),
        mea = mean(rate),
        min = qnorm(.33, mea, sd=sd(rate)),
        max = qnorm(.66, mea, sd=sd(rate))
      )
    
    bond1 <- data %>% 
      filter(
        ticker %in% bond_choice
      ) %>% 
      select(-c(label)) %>% 
      pivot_longer(
        cols=-c(ticker,etf_type),
        names_to = "pastyear",
        values_to = "rate"
      ) %>% 
      select(rate) %>% 
      summarise(
        mid = median(rate),
        mea = mean(rate),
        min = qnorm(.33, mea, sd=sd(rate)),
        max = qnorm(.66, mea, sd=sd(rate))
      )
    
    rate <- cbind(etf_type=c("stock","bond"),rbind(stock1,bond1)) %>% 
      pivot_longer(
        cols = -etf_type,
        names_to = "del",
        values_to = "value"
      ) %>% 
      pivot_wider(
        names_from = etf_type,
        values_from = value
      ) %>% 
      mutate(
        final = (stock * stockp/100) + (bond * bondp/100)
      ) %>% 
      select(
        del,final
      ) %>% 
      pivot_wider(
        names_from = del,
        values_from = final
      )
    
    maxP = ini_cap
    minP = ini_cap
    midP = ini_cap
    meaP = ini_cap
    maxdf <- NULL
    mindf <- NULL
    middf <- NULL
    meadf <- NULL
    
    for (i in length){
      maxP <- round(maxP*(1+(rate$max/100))+depo,2)
      maxdf <- cbind(maxdf, maxP)
    }
    for (i in length){
      minP <- round(minP*(1+(rate$min/100))+depo,2)
      mindf <- cbind(mindf, minP)
    }
    for (i in length){
      midP <- round(midP*(1+(rate$mid/100))+depo,2)
      middf <- cbind(middf, midP)
    }
    for (i in length){
      meaP <- round(meaP*(1+(rate$mea/100))+depo,2)
      meadf <- cbind(meadf, meaP)
    }
    
    
    maxdf <- as.vector(maxdf)
    mindf <- as.vector(mindf)
    middf <- as.vector(middf)
    meadf <- as.vector(meadf)
    
    df <- data.frame(cbind(Year=ylength,`Bullish`=maxdf,`Bearish`=mindf,Median=middf,Mean=meadf))
    
    p <- df %>% 
      pivot_longer(
        cols = -Year,
        names_to = "Outlook",
        values_to = "Balance"
      ) %>% 
      ggplot(aes(x=Year,y=Balance,color=`Outlook`))+
      geom_line(size=1.5,alpha=1)+
      labs(
        title = paste("Retirement at age 67 in", 2019-age+67),
        y = "Balance (in US$)"
      )+
      xlim(
        low=2019,
        high=2019+len)+
      scale_color_manual(
        labels=c("Bearish: Top Quartile (SD)",
                 "Bullish: Bottom Quartile (SD)",
                 "Mean",
                 "Median"),
        values=c("lightcoral","chartreuse","cyan","royalblue")
        )+
      theme_minimal()
    
    ggplotly(p) %>% 
      layout(
        autosize = T,
        hovermode = 'x')
  })
}
shinyApp(ui = ui, server = server)