ui <- fluidPage(
  
  titlePanel("Primer odzivne aplikacije."),
  
  sidebarLayout(
    
    sidebarPanel(
      textInput("ime", "Vnesite vaše ime: ") # vhod
    ),
    
    mainPanel(
      h3("Pozdravljen/a, ", 
         textOutput("izpis", inline = T)      # izhod
      ),
    )
  )
)

server <- function(input, output){
  
  output$izpis <- renderText({
    ime <- input$ime
    if(nchar(ime) < 2){ #pri praznem polju ali 1 črki izpiši navodila.
      ime <- "prosim vnesi svoje ime."
    }else{
      if(ime == "datascience"){
        ime <- "DataScience@UL-FRI"
      }else{
        ime <- paste(toupper(substr(ime, 1, 1)), #prva črka naj bo velika
                     substr(ime, 2, nchar(ime)), #ostale črke skopiramo
                     "!",                        #dodamo še klicaj
                     sep = "")
      }
    }
    ime
  })
  
}

shinyApp(ui = ui, server = server)

