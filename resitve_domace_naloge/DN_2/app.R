library(shiny)
library(ggplot2)

encoding <- "latin1"

ui <- fluidPage(

    titlePanel("Stranke"),

    sidebarLayout(
        sidebarPanel(
            textInput("ime", 
                      label = "Ime stranke:"),
            dateInput("rd", 
                      label = "Datum rojstva:"),
            radioButtons("spol",
                         label = "Spol:",
                         choices = c("M", "Ž", "Drugo"),
                         selected = "Drugo"),
            textInput("kraj", 
                      label = "Kraj bivanja:"),
            dateInput("zacetek",
                      label = "Datum začetka uporabe storitve:"),
            selectInput("storitev",
                        label = "Storitev, na katero se naroča:",
                        choices = c("Storitev A",
                                    "Storitev B",
                                    "Storitev C"),
                        multiple = F),
            actionButton("shrani", "Shrani v datoteko")
        ),

        mainPanel(
            tableOutput("tabela"),
            plotOutput("graf")
        )
    )
)

server <- function(input, output) {
    stranka <- reactive({
        data.frame(
            ime = input$ime,
            rd = input$rd,
            spol = input$spol,
            kraj = input$kraj,
            zacetek = input$zacetek,
            storitev = input$storitev
        )
    })
    
    observeEvent(input$shrani,
                 {
                 stranke <- read.csv("stranke.csv", encoding = encoding)
                 stranke <- rbind(stranke, stranka())
                 write.csv(stranke, "stranke.csv", 
                           row.names = F, encoding = encoding)
                 }
                 )
    output$tabela <- renderTable({
        df <- read.csv("stranke.csv", encoding = encoding)
        df[order(df$zacetek, decreasing = T)[1:5], ]
    })
    
    output$graf <- renderPlot({
        df <- read.csv("stranke.csv", encoding = encoding)
        df$zacetek <-  as.Date(df$zacetek, format = "%d/%m/%Y")
        ggplot(df, aes(x = zacetek))+
            geom_line(stat =  "count", group = 1, size = 1.3)+
            scale_x_date(date_breaks = "weeks")+
            ggtitle("Nove stranke")+
            xlab("Datum začetka uporabe storitve")+
            ylab("Število novih strank")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
