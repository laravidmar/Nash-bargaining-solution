library(shiny)
ui <- fluidPage( 
  titlePanel("Analiza Nashevega modela pogajanja"),
  sidebarLayout(
    sidebarPanel(
      h2('Vsebina'),
      p('Enofazna igra pogajanja'), 
      p('Dvofazna igra pogajanja')
    ),
    mainPanel(
      h2('Opis problema'),
      p('V teoriji iger je kooperativna igra oziroma Nashava igra pogajanja, igra pri kateri skupine igralcev med seboj tekmuje.
      Pri tem si igraleci želijo maksimizirati svojo koristnost. Igralci med seboj lahko 
        delujejo usklajeno in morda izmenjajo koristnosti. Tako pride do sporazuma.'),
      p('V aplikaciji so predstavljene bimatricne igre, to so igre z dvemi igralci. Omejili pa smo se samo na prenosljive dobrine.'),
      
      
      actionButton("action", label = "ENOFAZNA IGRA POGAJANJA"),
      hr(),
      fluidRow(column(2, verbatimTextOutput("value"))),
      actionButton("action", label = "DVOFAZNA IGRA POGAJANJA"),
      
      hr(),
      fluidRow(column(2, verbatimTextOutput("value2")))
      
      
    )
      )
  )

# Define server logic ----
server <- function(input, output) {
  output$value <- renderPrint({ })
}

# Run the app ----
shinyApp(ui = ui, server = server)
