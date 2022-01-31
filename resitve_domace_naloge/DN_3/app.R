library(shiny)
library(bslib)
library(zoo)
library(lubridate)
library(scales)
library(tidyverse)

options(stringsAsFactors = F)
production <- read.csv("production.csv")
sales <- read.csv("sales.csv")

get_subset_production <- function(data, store, categ, brand){
    a <- data
    if (store != 'All'){a <- a[(a$store_name == store), ]}
    if (categ != 'All'){a <- a[(a$category_name == categ), ]}
    if (brand != 'All'){a <- a[(a$brand_name == brand), ]}
    return(a)
}

ui <- navbarPage(
    theme = bs_theme(version = 4, bootswatch = "darkly"),
    "Bike Stores",
    #################### PRODUCTION PAGE #####################
    tabPanel(
        "Production",
        sidebarLayout(
            sidebarPanel(
                width = 3,
                h3("Current Stocks", br(),  br()),
                h4("Filter:"),
                selectInput("stock_store",
                            "Store:",
                            choices = c("All",
                                        "Santa Cruz Bikes",
                                        "Baldwin Bikes",
                                        "Rowlett Bikes"
                                        ),
                            selected = "All"),
                selectInput("stock_category",
                            "Category:",
                            choices = c("All",
                                        "Children Bicycles",
                                        "Comfort Bicycles",
                                        "Cruisers Bicycles",
                                        "Cyclocross Bicycles",
                                        "Electric Bikes",
                                        "Mountain Bikes",
                                        "Road Bikes"
                            ),
                            selected = "All"),
                selectInput("stock_brand",
                            "Brand:",
                            choices = c("All",
                                        "Electra",
                                        "Haro",
                                        "Heller",
                                        "Pure Cycles",
                                        "Ritchey",
                                        "Strider",
                                        "Sun Bicycles",
                                        "Surly",
                                        "Trek"
                            ),
                            selected = "All"),
            ),
            
            mainPanel(
                width = 9,
                fluidRow(
                    column(3,
                           wellPanel(
                               h6("Number of bikes:  "),
                               h4(textOutput("n_bikes_stock", inline = F))
                                )
                           ),
                    column(3,
                           wellPanel(
                               h6("Total production cost:"),
                               h4(textOutput("prod_cost_stock", inline = F))
                               )
                           ),
                    column(3,
                           wellPanel(
                               h6("Total potential revenue:"),
                               h4(textOutput("rev_stock", inline = F))
                           )
                    ),
                    column(3,
                           wellPanel(
                               h6("Total potential margin:"),
                               h4(textOutput("mar_stock", inline = F))
                           )
                    )
                    ),
                h2(" "),
                fluidRow(
                    column(6,
                           wellPanel(
                               h6("Top 5 most abundant bikes in stock:"),
                               plotOutput("abund_stock_model")
                           )
                    ),
                    column(6,
                           wellPanel(
                               h6("Top 5 most expensive bikes (per bike) in stock:"),
                               plotOutput("expens_stock_model")
                           )
                    )
                ),
                )
            )
        ),
    #################### SALES PAGE #####################
    tabPanel(
        "Sales",
        fluidRow(
            column(3,
                   offset = 0,
                   wellPanel(
                       h4("Sales History:"),
                       dateRangeInput("sales_hist", 
                                      label = "Period:",
                                      min =  min(sales$order_date),
                                      max =  max(sales$order_date),
                                      start =  min(sales$order_date),
                                      end =  max(sales$order_date),
                                      startview = "year"),
                       selectInput("freq_sales_hist",
                                   label = "Time step:",
                                   choices = c("years",
                                               "quarters",
                                               "months",
                                               "weeks"),
                                   selected = "months"),
                       # h6(br()),
                       checkboxInput("sales_trend",
                                     label = "Plot trend line",
                                     value = T),
                       radioButtons("sales_target",
                                    label = h5("Select the visualization target variable:"),
                                    choices = c("Revenue",
                                                "Quantity",
                                                "Margin"))
                   )),
            column(9,
                   fluidRow(
                       column(12,
                              offset = 0,
                              h4("Orders:"), 
                              plotOutput("hist_plot_sales", height = "100%"))),
                fluidRow(
                    column(4, h4("Best 3 Employees:"), plotOutput("sales_empl")),
                    column(4, h4("Best 3 Stores:"), plotOutput("sales_stores")),
                    column(4, h4("Top 3 Best Customers:"), plotOutput("sales_cust"))
                )
            )
        ),
    )
    
)


server <- function(input, output) {
    
    updated_production <- reactive({get_subset_production(production,
                                                  input$stock_store,
                                                  input$stock_category,
                                                  input$stock_brand)})
    
    output$n_bikes_stock <- renderText({
        sum(updated_production()$quantity)
    })
    
    output$prod_cost_stock <- renderText({
        dollar(-sum(updated_production()$quantity * 
                      updated_production()$prod_cost),
               suffix =  "€",
               prefix = "")
    })
    
    output$rev_stock <- renderText({
        dollar(sum(updated_production()$quantity * 
                      updated_production()$list_price),
               suffix =  "€",
               prefix = "")
    })
    
    output$mar_stock <- renderText({
        dollar(sum((updated_production()$quantity * 
                      updated_production()$list_price) -
                  (updated_production()$quantity * 
                       updated_production()$prod_cost)),
                  suffix =  "€",
                  prefix = "")
    })
    
    output$abund_stock_model <- renderPlot({
        updated_production() %>% select(product_name, quantity) %>%
            group_by(product_name) %>% 
            summarise(quantity = sum(quantity)) %>%
            arrange(desc(quantity)) %>%
            slice_head(n = 5) %>%
            ggplot(., aes(x = product_name, y = quantity, fill = product_name)) +
            geom_bar(stat = "identity") + 
            ylab("Quantity")+
            labs(fill = "product names:") +
            theme(axis.title.x = element_blank()) + 
            theme_light(base_size = 16) +
            theme(axis.text.x = element_blank(),
                  axis.title.x = element_blank(),
                  legend.position = "bottom",
                  legend.direction="vertical")
    })
    
    
    output$expens_stock_model <- renderPlot({
        updated_production()%>% select(product_name, list_price) %>%
            group_by(product_name) %>% 
            summarise(list_price = max(list_price)) %>%
            arrange(desc(list_price)) %>%
            slice_head(n = 5) %>%
            ggplot(., aes(x = product_name, y = list_price, fill = product_name)) +
            geom_bar(stat = "identity") +
            labs(fill = "product names:") +
            ylab("Price per bike (€)") +
            theme(axis.title.x = element_blank()) + 
            theme_light(base_size = 16) +
            theme(axis.text.x = element_blank(),
                  axis.title.x = element_blank(),
                  legend.position = "bottom",
                  legend.direction="vertical")
    })
    
    ####################### Sales reactivity ###################################
    updated_sales <- reactive({
        sales %>% mutate(
            order_date = ymd(order_date),
            date_splits = cut.Date(order_date, input$freq_sales_hist),
            revenue = list_price * quantity * (1 - discount),
            Margin = revenue - prod_cost) %>%
            filter(order_date >= input$sales_hist[1],
                   order_date <= input$sales_hist[2])
    })
    
    output$hist_plot_sales <- renderPlot({
        pl <- updated_sales() %>%
            group_by(date_splits) %>% 
            summarise(Revenue =  sum(revenue),
                      Margin = sum(Margin),
                      Quantity = sum(quantity)) %>%
            mutate(date_splits = ymd(date_splits))%>%
            ggplot(., aes_string(x = "date_splits", y = input$sales_target)) + 
            geom_line(group = 1, size = 1.05, color = "#025AB2") +
            geom_point(size = 2.5, color = "#EA7157") +
            scale_x_date(breaks = date_breaks("1 year")) +
            theme(axis.title.x = element_blank()) + 
            theme_light(base_size = 16)
        if (input$sales_trend){
            pl <- pl + geom_smooth(aes(color = "Trend"), ) +
                scale_color_manual(values = c("#965D5D")) +
                labs(color = " ")
        }
        pl
    }, height = 250)
    
    output$sales_empl <- renderPlot({
        updated_sales() %>%
            mutate(Employee = paste(last_name_staff, first_name_staff, sep = ", ")) %>%
            group_by(staff_id) %>% summarise(
                Employee = min(Employee),
                Quantity = sum(quantity),
                Revenue = sum(revenue),
                Margin = sum(Margin)
            ) %>% arrange(desc(Revenue)) %>% slice_head(n = 3) %>% 
            ggplot(., aes_string(x = "Employee", y = input$sales_target, fill = "Employee"))+
                geom_bar(stat = "identity") + 
                labs(fill = "") +
                theme(axis.title.x = element_blank()) + 
                theme_light(base_size = 16) +
                theme(axis.text.x = element_blank(),
                      axis.title.x = element_blank(),
                      legend.position = "bottom",
                      legend.direction="horizontal",
                      legend.margin = margin(l = -50)) +
            scale_fill_discrete(guide = guide_legend(nrow = 2,
                                                     label.theme = element_text(hjust = 0.5)))
        
    }, height = 300, width = 300)
    
    output$sales_stores <- renderPlot({
        updated_sales() %>%
            group_by(store_name) %>% 
            summarise(Quantity = sum(quantity),
                      Revenue = sum(revenue),
                      Margin = sum(Margin)) %>% 
            arrange(desc(Revenue)) %>% slice_head(n = 3) %>% 
            ggplot(., aes_string(x = "store_name", y = input$sales_target, fill = "store_name"))+
            geom_bar(stat = "identity") + 
            labs(fill = "") +
            theme(axis.title.x = element_blank()) + 
            theme_light(base_size = 16) +
            theme(axis.text.x = element_blank(),
                  axis.title.x = element_blank(),
                  legend.position = "bottom",
                  legend.direction="horizontal",
                  legend.margin = margin(l = -50)) +
            scale_fill_discrete(guide = guide_legend(nrow = 2,
                                                     label.theme = element_text(hjust = 0.5)))
        
    }, height = 300, width = 300)
    output$sales_cust <- renderPlot({
        updated_sales() %>%
            mutate(Customer = paste(last_name, first_name, sep = ", ")) %>%
            group_by(customer_id) %>%
            summarise(Customer = min(Customer),
                      Quantity = sum(quantity),
                      Revenue = sum(revenue),
                      Margin = sum(Margin)) %>% 
            arrange(desc(Revenue)) %>% slice_head(n = 3) %>% 
            ggplot(., aes_string(x = "Customer", y = input$sales_target, fill = "Customer"))+
            geom_bar(stat = "identity") + 
            labs(fill = "") +
            theme(axis.title.x = element_blank()) + 
            theme_light(base_size = 16) +
            theme(axis.text.x = element_blank(),
                  axis.title.x = element_blank(),
                  legend.position = "bottom",
                  legend.direction="horizontal",
                  legend.margin = margin(l = -50)) +
            scale_fill_discrete(guide = guide_legend(nrow = 2,
                                                     label.theme = element_text(hjust = 0.5)))
        
    }, height = 300, width = 300)
    # output$sales_orders
}

# Run the application 
shinyApp(ui = ui, server = server)
