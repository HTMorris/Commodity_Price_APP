# load package
library(tidyverse)
library(tsibble)
library(janitor)
library(lubridate)
library(shiny)
library(shinyWidgets)
library(zoo)
library(slider)
library(bslib)
library(thematic)
library(ragg)
library(DT)
library(scales)
library(trelliscopejs)


library(readxl)
url <- "https://thedocs.worldbank.org/en/doc/5d903e848db1d1b83e0ec8f744e55570-0350012021/related/CMO-Historical-Data-Monthly.xlsx"
destfile <- "CMO_Historical_Data_Monthly.xlsx"
curl::curl_download(url, destfile)
CMO_Historical_Data_Monthly <- read_excel(destfile, sheet="Monthly Prices")


label <- CMO_Historical_Data_Monthly %>% slice(4) %>% 
  pivot_longer(everything(), values_to = "indicator") %>% 
  left_join(  CMO_Historical_Data_Monthly %>% slice(5) %>% 
                pivot_longer(everything(), values_to = "measurement"), by="name") %>% 
  mutate(indicator_measure = glue::glue("{indicator} {measurement}")) %>% 
  slice(-1) %>% 
  pull(indicator_measure)


df_colname <- c("date", label)

pchange <- function(.x,n){
  change = difference(.x, lag = n)/lag(.x, n)
  change
}


com_price <- CMO_Historical_Data_Monthly %>% 
  slice(-c(1:6))%>% 
  set_names(df_colname) %>% 
  mutate(date = ym(date),
         date = ceiling_date(date, "month")-1,
         year = year(date),
         month = month(date, label = TRUE)
  ) %>% 
  relocate(date, year, month, everything()) %>% 
  pivot_longer(-c(1:3), names_to = "commodity", values_to = "price_m") %>% 
  mutate(price_m = parse_number(price_m),
         month_year = as.yearmon(date,format="%b-%y"),
         month_year_lb = glue::glue("{month}-{year}")) %>% 
  group_by(commodity) %>% 
  mutate(price_avg3m = slide_dbl(price_m, mean, .before = 2, .complete = TRUE),
         price_avg6m = slide_dbl(price_m, mean, .before = 5, .complete = TRUE),
         price_avg9m = slide_dbl(price_m, mean, .before = 8, .complete = TRUE),
         price_avg12m = slide_dbl(price_m, mean, .before = 11, .complete = TRUE),
         
         
         
         # across(ends_with(c("_m", "_avg3m")), ~ difference(., lag=1, differences = 1)/lag(.,n=1), .names = "{.col}_monthpc"),
         # across(ends_with(c("_m", "_avg3m")), ~ difference(., lag=3, differences = 1)/lag(.,n=3), .names = "{.col}_quarterpc"),
         # across(ends_with(c("_m", "_avg3m")), ~ difference(., lag=12, differences = 1)/lag(.,n=12), .names = "{.col}_yearpc")
  ) %>% 
  ungroup() %>% 
  relocate(starts_with("month"), .after=month) %>% 
  pivot_longer(price_m:price_avg12m, names_to = "indicator", values_to ="price" ) %>% 
  mutate(.by = c(commodity, indicator),
         price_1mthpc = price/lag(price, n=1)-1,  # monthly change
         price_3mthpc = price/lag(price, n=3)-1, # quarterly change
         price_6mthpc = price/lag(price, n=6)-1,  # 6 monthly change
         price_9mthpc = price/lag(price, n=9)-1, # 9 month change
         price_12mthpc = price/lag(price, n=12)-1, # 12 month change
         
  ) %>% 
  pivot_longer(price_1mthpc:price_12mthpc, names_to = "indicator_pc", values_to = "percent_change") %>% 
  mutate(
    # formating
    price = formattable::currency(price, symbol="US$", digits=2L),
    percent_change = formattable::percent(percent_change, digits=1),
    indicator = case_when(
      indicator=="price_m" ~ "Average 1-month Prices",
      indicator=="price_avg3m" ~ "Average 3-month Prices",
      indicator=="price_avg6m" ~ "Average 6-month Prices",
      indicator=="price_avg9m" ~ "Average 9-month Prices",
      indicator=="price_avg12m" ~ "Average 12-month Prices",
      
    ),
    indicator_pc = case_when(
      indicator_pc=="price_1mthpc" ~ "1-month Percent Change",
      indicator_pc=="price_3mthpc" ~ "3-month Percent Change",
      indicator_pc=="price_6mthpc" ~ "6-month Percent Change",
      indicator_pc=="price_9mthpc" ~ "9-month Percent Change",
      indicator_pc=="price_12mthpc" ~ "12-month Percent Change",
      
    ),
    
    
    
  )


# If you want `{ragg}` to handle the font rendering in a Shiny app
options(shiny.useragg = TRUE)

# Call thematic_shiny() prior to launching the app, to change
# R plot theming defaults for all the plots generated in the app
#thematic_shiny(font = "auto")
thematic_on(bg = "#222229", fg = "white", accent = "red")

ui_comprice <- fluidPage(
  #add style
  theme = bs_theme(bg = "#00a1af",
                   fg = "#000000",
                   base_font = font_google("Righteous")),
  
  titlePanel("Global Commodity Prices"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(inputId = "start_date", label = "Select Start Date", choices = unique(com_price %>% select(month_year)), 
                  selected = first(com_price$month_year) ),
      selectInput(inputId = "end_date", label = "Select End Date", choices = unique(com_price$month_year), 
                  selected = last(com_price$month_year) ),
      
      selectInput(inputId = "indicator", label = "Select Average Price Period", choices = unique(com_price$indicator), 
                  selected = "Aveage 1-month Prices" ),
      selectInput(inputId = "indicator_pc", label = "Select Price Change Period", choices = unique(com_price$indicator_pc), 
                  selected = "1-month Percent Change" ),
      
      pickerInput(inputId = "commodity", label = "Select Commodities", choices = sort(unique(com_price$commodity)),
                  multiple = TRUE
                  
      ),
      pickerInput(inputId = "month", label = "Select Period", choices = c(levels(com_price$month)), selected = levels(com_price$month),
                  multiple = TRUE
      ),
      
      
      
    ),
    
    mainPanel( 
      tabsetPanel( 
        tabPanel("Plot - Levels", plotly::plotlyOutput("plot_l")),
        tabPanel("Plot - Percent Change)", plotly::plotlyOutput("plot_pc")),
        # tabPanel("Plot Quarterly Per Cent Change", plotly::plotlyOutput("plot_mp_pc3")),
        # tabPanel("Plot 12 Monthly Per Cent Change (Year-over-Year)", plotly::plotlyOutput("plot_mp_pc12")),
        # tabPanel("Plot Average Three Monthly", plotly::plotlyOutput("plot_3avgmp")),
        # tabPanel("Plot Averge Three Monthly Per Cent Change (Month-over-Month)", plotly::plotlyOutput("plot_3avgmp_pc1")),
        # tabPanel("Plot Averge Three Month Quarterly Per Cent Change", plotly::plotlyOutput("plot_3avgmp_pc3")),
        # tabPanel("Plot Averge Three Monthly Yearly Per Cent Change (Year-over-Year)", plotly::plotlyOutput("plot_3avgmp_pc12")),
        
        tabPanel("Table Levels", DT::dataTableOutput("table_Price") ),
        tabPanel("Table Percent Change", DT::dataTableOutput("table_pc") )
        
      )
      
      
      
    )
    
    
    
  )
)

# server <- function(input, output, session){}
# 
# shinyApp(ui=ui_comprice, server = server)




server_comprice <- function(input, output, session){
  
  
  data <- reactive({
    
    
    com_price %>% 
      filter(month_year>=input$start_date & month_year<=input$end_date) %>% 
      filter(commodity %in% c(input$commodity)) %>% 
      filter(month %in% c(input$month)) %>% 
      filter(indicator==input$indicator) %>% 
      filter(indicator_pc==input$indicator_pc)
    
    
    
  })
  
  # create data for table
  table_data <- reactive({
    data() %>% 
      select(commodity, month_year, price) %>% 
      pivot_wider(names_from = month_year, values_from = c(price))
    
  })
  
  
  
  check <- reactive({      validate(
    need(input$commodity !="", "Please select Commodity/ies"),
    
  )
  })
  
  output$plot_l <- plotly::renderPlotly({
    
    check()
    
    data() %>% 
      #  mutate( price = formattable::currency(price_m, symbol = "US$")) %>% 
      
      ggplot(aes(x=date, y=price, colour=commodity ))+
      geom_line()+
      #scale_y_continuous(labels = formattable::currency)+
      scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))+ 
      scale_x_date(
        NULL,
        # breaks = scales::breaks_width("3 months"), 
        labels = scales::label_date_short()
      ) +
      labs(title = glue::glue("Selected Commodities for the period {input$start_date} to {input$end_date}"),
           y = input$indicator
      )+
      #  theme_bw()+
      facet_wrap(vars(commodity),
                 # ncol = 2 ,
                 # nrow = 2 ,
                 scales = "free_y",
                 # width = 800,
                 # as_plotly = TRUE,
                 
                 labeller = label_wrap_gen(50)
                 
      )
    
    
  })
  
  # Percent Change
  
  output$plot_pc <- plotly::renderPlotly({
    
    check()
    
    
    data() %>% 
      #mutate(price_m_monthpc = formattable::percent(price_m_monthpc,digits = 1)) %>% 
      ggplot(aes(x=date, y=percent_change , colour=commodity ))+
      geom_line()+
      scale_y_continuous(labels = scales::percent)+
      labs(title = glue::glue("Selected Commodities for the period {input$start_date} to {input$end_date}"),
           y = input$indicator_pc
      )+
      #theme_bw()+
      facet_wrap(vars(commodity), ncol = 3 ,scales = "free", labeller = label_wrap_gen(50))
    
    
  })
  
  
  
  output$table_Price <- DT::renderDataTable({
    check()
    
    
    # month_data <-  data() %>% 
    #    select(commodity, month_year, price, percent_change) %>% 
    #    pivot_wider(names_from = month_year, values_from = c(price, percent_change))
    #    
    
    datatable( table_data(),
               
               filter = 'top', 
               
               caption = htmltools::tags$caption(
                 style = 'caption-side: bottom; text-align: center;',
                 'Source: ', htmltools::em('World Bank Commodity Monthly Prices')
               ),
               extensions = 'Buttons', options = list(
                 pageLength = 15, autoWidth = TRUE,
                 
                 dom = 'Bfrtip',
                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
               )
               
    ) %>% 
      formatCurrency(2:ncol(table_data()), currency = "$") 
    
    
  })
  
  output$table_pc <- DT::renderDataTable({
    check()
    
    
    # create data for table
    table_data_pc <- reactive({
      data() %>% 
        select(commodity, month_year, percent_change) %>% 
        pivot_wider(names_from = month_year, values_from = c( percent_change))
      
    })   
    
    datatable( table_data_pc(),
               
               filter = 'top', 
               
               caption = htmltools::tags$caption(
                 style = 'caption-side: bottom; text-align: center;',
                 'Source: ', htmltools::em('World Bank Commodity Monthly Prices')
               ),
               extensions = 'Buttons', options = list(
                 pageLength = 15, autoWidth = TRUE,
                 
                 dom = 'Bfrtip',
                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
               )
               
    ) %>% 
      formatPercentage(2:ncol(table_data()), digits = 1) 
    
    
  })
  
  
  
  
  
}




shinyApp(ui=ui_comprice, server = server_comprice)


library(rsconnect)
rsconnect::deployApp("D:/Learn/PROJECTS/APP/commodity_price_app")


