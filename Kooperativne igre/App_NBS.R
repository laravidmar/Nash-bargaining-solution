library("shiny")
library('rgl')
library("shinyjs")
library(car)
library(ggplot2)
library(dplyr)
cars$time <- cars$dist/cars$speed

ui <- fluidPage(
  tabsetPanel(
    id = "tabs",
    tabPanel(
      titlePanel("Analiza Nashevega modela pogajanja"),
      sidebarLayout(
        sidebarPanel(
          h2('Vsebina',  align = "center"),
          br(), 
          p('Enostopenska igra pogajanja',  align = "center"), 
          p('Dvostopenjska igra pogajanja',  align = "center")
        ),
        mainPanel(
          h2('Opis problema',  align = "center"),
          p('V teoriji iger je kooperativna igra oziroma Nashava igra pogajanja, igra pri kateri skupine igralcev med seboj tekmuje.
      Pri tem si igraleci želijo maksimizirati svojo koristnost. Igralci med seboj lahko 
        delujejo usklajeno in morda izmenjajo koristnosti. Tako pride do sporazuma.'),
          p('V aplikaciji so predstavljene bimatricne igre, to so igre z dvemi igralci. Omejili pa smo se samo na prenosljive dobrine.
            To so dobrine, kjer so koristi igralcev med seboj neposredno primerljive in posledično lahko to
            koristnost igralci med seboj smiselno prenašajo.'),
          br(),
          h3('Izberite željeno igro'),
          actionButton(inputId = "EIP", label = "ENOSTOPENSKA IGRA POGAJANJA"),
          br(),
          em('Izhajamo iz strateške igre za 2 igralca. Pri tem izhajamo iz točke (0,0), to je tudi status quo.'),
          br(),
          br(),
          actionButton(inputId = "DIP", label = "DVOSTOPENSKA IGRA POGAJANJA"),
          br(),
          em('Izhajamo iz strateške igre za 2 igralca. Prvo igralca izhajata iz točke (0,0), potem pa uporabita svoje grozilni strategiji. Status quo 
             je pri tej igri ravno grozilna strategija posameznega igralca.'),
          br(),
          br(),
          br(),
          tags$img(src = "business-finance.png", height = 300, width = 350)
          
        )
      ),
      title = "Analiza Nashevega modela pogajanja",
      
  ),
    tabPanel(
      title = "Enostopenska igra",
      titlePanel("Analiza enostopenske igre pogajanja"),
      sidebarLayout(
            sidebarPanel(
              helpText('Izberi velikost matrike koristnosti za igralca.'),
              
              sliderInput("n1", label = h4(strong("Število vrstic"),  align = "center"), min = 2, 
                          max = 50, value = 10),
              sliderInput("m1", label = h4(strong("Število stolpcev"),  align = "center"), min = 2, 
                          max = 50, value = 10),
              helpText('Z izbrano porazdelitvijo bodo generirane vrednosti v strateški igri.'),
              radioButtons("por1", label = h4(strong("Porazdelitev za prvega igralca"),  align = "center"),
                           choices = list("Normalna porazdelitev" = 'norm', "Beta porazdelitev" = 'beta',
                                          "Inverzna gama porazdelitev" = 'invgama', "Eksponentna porazdelitev" = 'exp'), 
                           selected = 1),
              checkboxGroupInput("por11", label = h4(strong("Porazdelitev za drugega igralca"),  align = "center"), 
                                 
                                 choices = list("Normalna porazdelitev" = 'norm', "Beta porazdelitev" = 'beta',
                                                "Inverzna gama porazdelitev" = 'invgama', "Eksponentna porazdelitev" = 'exp'),
                                 selected = 1),
              helpText('Če izberete eno porazdelitev bo predatvlejno .... .'),
              helpText('Če izberete več porazdelitev bodo predatvljene primerjeve.')
            ),
        mainPanel(
          textOutput('proba'),
          plotOutput(outputId = "plot1"),
          plotOutput(outputId = "nbs1")
        ), position = c('left', 'right'), fluid = TRUE)

    
    ),
    tabPanel(
      title = "Dvostopenska igra",
      titlePanel("Analiza dvostopenske igre pogajanja"),
      sidebarLayout(
        sidebarPanel(
          helpText('Izberi velikost matrike koristnosti za igralca.'),
          
          sliderInput("n2", label = h4(strong("Število vrstic"),  align = "center"), min = 2, 
                      max = 50, value = 10),
          sliderInput("m2", label = h4(strong("Število stolpcev"),  align = "center"), min = 2, 
                      max = 50, value = 10),
          helpText('Z izbrano porazdelitvijo bodo generirane vrednosti v strateški igri.'),
          radioButtons("por12", label = h4(strong("Porazdelitev za prvega igralca"),  align = "center"),
                       choices = list("Normalna porazdelitev" = 'norm', "Beta porazdelitev" = 'beta',
                                      "Inverzna gama porazdelitev" = 'invgama', "Eksponentna porazdelitev" = 'exp'), 
                       selected = 1),
          checkboxGroupInput("por22", label = h4(strong("Porazdelitev za drugega igralca"),  align = "center"), 
                             choices = list("Normalna porazdelitev" = 'norm', "Beta porazdelitev" = 'beta',
                                            "Inverzna gama porazdelitev" = 'invgama', "Eksponentna porazdelitev" = 'exp'),
                             selected = 1)
        ),
        mainPanel(
          textOutput('vrednosti'),
          plotOutput(outputId = "plot2"),
          rglwidgetOutput("plot",  width = 800, height = 600)          
        ), position = c('left', 'right'), fluid = TRUE)
      

    )
    
      
    )
  )

server <- function(input, output, session){
  observeEvent(input$EIP, {
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Enostopenska igra")
  })
  observeEvent(input$DIP, {
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Dvostopenska igra")
  })
  
  output$value <- renderPrint({ input$n1 }) #vrednost za n
  output$value <- renderPrint({ input$m1 }) #vrednost za m
  output$value <- renderPrint({ input$n2 }) #vrednost za n dvofazna igra
  output$value <- renderPrint({ input$m2 }) #vrednost za m dvofazna igra
  
  output$value <- renderPrint({ input$por1 }) #porazdelitev za P1 enofazna --> ena izbira
  output$value <- renderPrint({ input$por11 }) #porazdelitev za enofazno igro za P2
  output$value <- renderPrint({ input$por22 }) #porazdelitev za dvofazno igro P2 --> vec izbir
  output$value <- renderPrint({ input$por12 }) #porazdelitev za P1 --> ena izbira
  
  
  output$proba <- renderText({
    c(input$por1, input$n1, input$m1)
  })
  
  output$plot <- renderRglwidget({
    rgl.open(useNULL=T)
    dat <- enofazna_enaka_por_big_n(input$por12, input$por22, input$n2, input$m2)
    scatter3d(x=dat$igralec1, y=dat$igralec2, z=dat$velikost_matrike)
    rglwidget()
  })
  
  output$nbs1 <- renderPlot({
    A <- matrika(input$por1, input$n1, input$m1)
    B <- matrika(input$por11, input$n1, input$m1)
    gt_nbs(matrix = TRUE, X =A-B, solution_only = TRUE,  hullcolor ='bisque3') 
    
  }) 
    
  output$plot1 <- renderPlot({
    dol <- length(input$por11)
    if(dol == 1 ){
      dat <- enofazna_enaka_por(input$por1, input$por11, input$n1, input$m1)
      ggplot(data = dat) +
        geom_point(aes(x = velikost_matrike, y = Vrednost, 
                       color = igralec ), cex = 2, 
                   position = position_dodge(width = 0.3),
                   stat = "identity")+
        
        labs(x = "Velikost matrike")+
        ggtitle("Višina izplačil glede na velikost matrike")
      
     
    }else if (dol == 2){
      dat <- enofazna_vec_porazdelitev(por0=input$por1 ,por1 = input$por11[1], por2= input$por11[2], por3 = 0, por4 = 0, input$n1, input$m1 )
      ggplot(dat) + 
        geom_bar( aes(x = Porazdelitve, y = vrednosti, 
                      color = igralec, fill = igralec ), 
                  position = "dodge2", stat = "identity")+
        labs(x = 'Vrsta porazdelitve')
        ggtitle("Velikost izplačil posameznega igralca")
    }else if (dol == 3){
      dat <- enofazna_vec_porazdelitev(por0=input$por1 ,por1 = input$por11[1], por2= input$por11[2], por3 = input$por11[3], por4 = 0, input$n1, input$m1 )
      ggplot(dat) + 
        geom_bar( aes(x = Porazdelitve, y = vrednosti, 
                      color = igralec, fill = igralec ), 
                  position = "dodge2", stat = "identity")+
        labs(x = 'Vrsta porazdelitve')
        ggtitle("Velikost izplačil posameznega igralca")
    }else if (dol == 4){
      dat <- enofazna_vec_porazdelitev(por0=input$por1 ,por1 = input$por11[1], por2= input$por11[2], por3 = input$por11[3], por4 = input$por11[4], input$n1, input$m1 )
      ggplot(dat) + 
        geom_bar( aes(x = Porazdelitve, y = vrednosti, 
                      color = igralec, fill = igralec ), 
                  position = "dodge2", stat = "identity")+
        labs(x = 'Vrsta porazdelitve')+
        ggtitle("Velikost izplačil posameznega igralca")
    }
  })
    
  
  
  
  
  output$vrednosti <- renderText({
    c(input$por22, input$n2, input$m2)
  })  
  
  
    output$plot2 <- renderPlot({
      dol <- length(input$por22)
      if(dol == 1 ){
        dat <- dvofazna_enaka_por(input$por12, input$por22, input$n2, input$m2)
        p <- persp(dat$velikost_matrike, dat$igralec1, dat$igralec2)
        
        scatter3d( 
          dat$velikost_matrike, dat$igralec1, dat$igralec2)
      }else if (dol == 2){
        dat <- dvofazna_vec_porazdelitev(por0=input$por12 ,por1 = input$por22[1], por2= input$por22[2], por3 = 0, por4 = 0, input$n2, input$m2 )
        ggplot(dat) + 
          geom_bar( aes(x = Porazdelitve, y = vrednosti, 
                        color = igralec, fill = igralec ), 
                    position = "dodge2", stat = "identity")+
          ggtitle("Velikost izplačil posameznega igralca")
      }else if (dol == 3){
        dat <- dvofazna_vec_porazdelitev(por0=input$por12 ,por1 = input$por22[1], por2= input$por22[2], por3 = input$por22[3], por4 = 0, input$n2, input$m2 )
        ggplot(dat) + 
          geom_bar( aes(x = Porazdelitve, y = vrednosti, 
                        color = igralec, fill = igralec ), 
                    position = "dodge2", stat = "identity")+
          ggtitle("Velikost izplačil posameznega igralca")
      }else if (dol == 4){
        dat <- dvofazna_vec_porazdelitev(por0=input$por12 ,por1 = input$por22[1], por2= input$por22[2], por3 = input$por22[3], por4 = input$por22[4], input$n2, input$m2 )
        ggplot(dat) + 
          geom_bar( aes(x = Porazdelitve, y = vrednosti, 
                        color = igralec, fill = igralec ), 
                    position = "dodge2", stat = "identity")+
          ylab('Vrsta porazdelitve')+
          ggtitle("Velikost izplačil posameznega igralca")
      }
    
    })
    
  
}
shinyApp(ui = ui, server = server)