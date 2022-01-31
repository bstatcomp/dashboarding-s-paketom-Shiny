library(shiny)
library(ggplot2)

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
}

# Run the application 
shinyApp(ui = ui, server = server)
