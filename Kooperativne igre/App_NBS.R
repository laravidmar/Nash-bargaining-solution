library("shiny")
ui <- fluidPage(
  tabsetPanel(
    id = "tabs",
    tabPanel(
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
          p('V aplikaciji so predstavljene bimatricne igre, to so igre z dvemi igralci. Omejili pa smo se samo na prenosljive dobrine.
            To so dobrine, kjer so koristi igralcev med seboj neposredno primerljive in posledično lahko to
            koristnost igralci med seboj smiselno prenašajo.'),
          h3('Izberite željeno igro'),
          actionButton(inputId = "EIP", label = "ENOFAZNA IGRA POGAJANJA"),
          br(),
          em('Izhajamo direktno iz matrične igre. Akcije prvega igralca so vrstice matrike, stolpci pa so akcije drugega igralca.'),
          br(),
          br(),
          actionButton(inputId = "DIP", label = "DVOFAZNA IGRA POGAJANJA"),
          br(),
          em('Izhajamo iz strateške igre za 2 igralca, torej imamo dve vhodni matriki.')
        )
      ),
      title = "Analiza Nashevega modela pogajanja",
      
  ),
    tabPanel(
      title = "Enofazna igra",
      titlePanel("Analiza enofazne igre pogajanja"),
      sidebarLayout(
            sidebarPanel(
              helpText('Izberi velikost matriko koristnosti za igralca.'),
              
              sliderInput("slider1", label = h3("Število vrstic"), min = 2, 
                          max = 10, value = 2),
              sliderInput("slider2", label = h3("Število stolpcev"), min = 2, 
                          max = 10, value = 2),
              helpText('Z izbrano porazdelitvijo bodo generirane vrednosti v matriki koristnosti.'),
              checkboxGroupInput("checkGroup", label = h3("Vrste porazdelitev"), 
                                 
                                 choices = list("Normalna porazdelitev" = 'norm', "Beta porazdelitev" = 'beta',
                                                "Inverzna gama porazdelitev" = 'invgama', "Eksponentna porazdelitev" = 'exp'),
                                 selected = 1),
              helpText('Če izberete eno porazdelitev bo predatvlejno .... .'),
              helpText('Če izberete več porazdelitev bodo predatvljene primerjeve.')
            ),
        mainPanel(), position = c('left', 'right'), fluid = TRUE)

    
    ),
    tabPanel(
      title = "Dvofazna igra",
      titlePanel("Analiza dvofazne igre pogajanja"),
      sidebarLayout(
        sidebarPanel(
          helpText('Izberi velikost matriko koristnosti za igralca.'),
          
          sliderInput("slider3", label = h3("Število vrstic"), min = 2, 
                      max = 10, value = 2),
          sliderInput("slider4", label = h3("Število stolpcev"), min = 2, 
                      max = 10, value = 2),
          helpText('Z izbrano porazdelitvijo bodo generirane vrednosti v matriki koristnosti.'),
          radioButtons("radio", label = h3("Porazdelitev za prvega igralca"),
                       choices = list("Normalna porazdelitev" = 'norm', "Beta porazdelitev" = 'beta',
                                      "Inverzna gama porazdelitev" = 'invgama', "Eksponentna porazdelitev" = 'exp'), 
                       selected = 1),
          checkboxGroupInput("checkGroup2", label = h3("Porazdelitev za drugega igralca"), 
                             choices = list("Normalna porazdelitev" = 'norm', "Beta porazdelitev" = 'beta',
                                            "Inverzna gama porazdelitev" = 'invgama', "Eksponentna porazdelitev" = 'exp'),
                             selected = 1)
        ),
        mainPanel(), position = c('left', 'right'), fluid = TRUE)
      

    )
    
      
    )
  )

server <- function(input, output, session){
  observeEvent(input$EIP, {
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Enofazna igra")
  })
  observeEvent(input$DIP, {
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Dvofazna igra")
  })
  
  output$value <- renderPrint({ input$slider1 }) #vrednost za n
  output$value <- renderPrint({ input$slider2 }) #vrednost za m
  output$value <- renderPrint({ input$slider3 }) #vrednost za n dvofazna igra
  output$value <- renderPrint({ input$slider4 }) #vrednost za m dvofazna igra
  
  output$value <- renderPrint({ input$checkGroup }) #porazdelitev za enofazno igro
  output$value <- renderPrint({ input$checkGroup2 }) #porazdelitev za dvofazno igro P2 --> vec izbir
  output$value <- renderPrint({ input$radio }) #porazdelitev za P1 --> ena izbira
}
shinyApp(ui = ui, server = server)