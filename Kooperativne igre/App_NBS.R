library("shiny")
library('rgl')
library("shinyjs")
library(car)
library(ggplot2)
library(dplyr)

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
          p('V aplikaciji so predstavljene bimatrične igre, to so igre z dvema igralcema. Omejili smo se na prenosljive dobrine.
            To so dobrine, kjer so koristi igralcev med seboj neposredno primerljive in posledično lahko to
            koristnost igralci med seboj smiselno prenašajo.'),
          br(),
          h3('Izberite željeno igro'),
          actionButton(inputId = "EIP", label = "ENOSTOPENSKA IGRA POGAJANJA"),
          br(),
          em('Izhajamo iz strateške igre za 2 igralca. Pogajanje pričnemo iz točke (0,0), to je tudi status quo.'),
          br(),
          br(),
          actionButton(inputId = "DIP", label = "DVOSTOPENSKA IGRA POGAJANJA"),
          br(),
          em('Izhajamo iz strateške igre za 2 igralca. Prvo igralca pričneta v točki (0,0), potem pa uporabita svoje grozilni strategiji. Status quo 
             je pri tej igri ravno grozilna strategija posameznega igralca.'),
          br(),
          br(),
          br(),
          tags$img(src = "business-finance.png", height = 300, width = 350),
          helpText('Vir: https://www.vectorstock.com/royalty-free-vector/cartoon-business-finance-money-set-scales-stack-vector-21925200')
          
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
                          max = 40, value = 10),
              sliderInput("m1", label = h4(strong("Število stolpcev"),  align = "center"), min = 2, 
                          max = 40, value = 10),
              helpText('Z izbrano porazdelitvijo bodo generirane vrednosti v strateški igri.'),
              radioButtons("por1", label = h4(strong("Porazdelitev za prvega igralca"),  align = "center"),
                           choices = list("N(3, 0.7)" = 'norm', "Beta(5,1)" = 'beta',
                                          "Invgama(2, 0.5)" = 'invgama', "Exp(3)" = 'exp'), 
                           selected = 1),
              checkboxGroupInput("por11", label = h4(strong("Porazdelitev za drugega igralca"),  align = "center"), 
                                 
                                 choices = list("N(3, 0.7)" = 'norm', "Beta(5,1)" = 'beta',
                                                "Invgama(2, 0.5)" = 'invgama', "Exp(3)" = 'exp'),
                                 selected = 1),
            ),
        mainPanel(
          plotOutput(outputId = "nbs1"),
          textOutput('nbsO'),
          br(), 
          br(),
          plotOutput(outputId = "plot1"),
          textOutput('plot1O'),
          br(),
          br(),
          rglwidgetOutput("dol1",  width = 800, height = 600)
        ), position = c('left', 'right'), fluid = TRUE)

    
    ),
    tabPanel(
      title = "Dvostopenska igra",
      titlePanel("Analiza dvostopenske igre pogajanja"),
      sidebarLayout(
        sidebarPanel(
          helpText('Izberi velikost matrike koristnosti za igralca.'),
          
          sliderInput("n2", label = h4(strong("Število vrstic"),  align = "center"), min = 2, 
                      max = 40, value = 10),
          sliderInput("m2", label = h4(strong("Število stolpcev"),  align = "center"), min = 2, 
                      max = 40, value = 10),
          helpText('Z izbrano porazdelitvijo bodo generirane vrednosti v strateški igri.'),
          radioButtons("por12", label = h4(strong("Porazdelitev za prvega igralca"),  align = "center"),
                       choices = list("N(3, 0.7)" = 'norm', "Beta(5,1)" = 'beta',
                                      "Invgama(2, 0.5)" = 'invgama', "Exp(3)" = 'exp'), 
                       selected = 1),
          checkboxGroupInput("por22", label = h4(strong("Porazdelitev za drugega igralca"),  align = "center"), 
                             choices = list("N(3, 0.7)" = 'norm', "Beta(5,1)" = 'beta',
                                            "Invgama(2, 0.5)" = 'invgama', "Exp(3)" = 'exp'),
                             selected = 1)
        ),
        mainPanel(
          plotOutput(outputId = "nbs2"),
          textOutput('nbs11'),
          br(),
          br(),
          plotOutput(outputId = "plot2"),
          textOutput('plot22'),
          br(),
          br(),
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
  #vrednosti 
  
  output$value <- renderPrint({ input$n1 }) #vrednost za n
  output$value <- renderPrint({ input$m1 }) #vrednost za m
  output$value <- renderPrint({ input$n2 }) #vrednost za n dvofazna igra
  output$value <- renderPrint({ input$m2 }) #vrednost za m dvofazna igra
  
  output$value <- renderPrint({ input$por1 }) #porazdelitev za P1 enofazna --> ena izbira
  output$value <- renderPrint({ input$por11 }) #porazdelitev za enofazno igro za P2
  output$value <- renderPrint({ input$por22 }) #porazdelitev za dvofazno igro P2 --> vec izbir
  output$value <- renderPrint({ input$por12 }) #porazdelitev za P1 --> ena izbira
  
  
  #enostopenske porazdelitev
  
  
  
  #naredi graficno porazdelitev pogajanja 
  output$nbs1 <- renderPlot({
    graf_enofazna(input$por1, input$por11,input$n1, input$m1 )
    
    output$nbsO <- renderText({
      'Predstavljen je grafični prikaz sporazuma, kjer igralca začneta v statusu quo (točka SQ), to je točka (0,0) in potem se sporazumeta. 
      Točka sporazuma je označena z NBS.'
    })
  }) 
  
  #graf 3D
  output$dol1 <- renderRglwidget({
    rgl.open(useNULL=T)
    dol <- length(input$por11)
    if (dol == 1 & ((input$n1 * input$m1) > 90)){
      dat <- izbrana_porazdelitev_1(input$por1, input$por11, input$n1, input$m1)
      scatter3d(x=dat$igralec1, y=dat$igralec2, z=dat$velikost_matrike, surface.alpha = 0.1, fogtype = 'none', fit = 'smooth',
                xlab = 'Izplacila prvega igralca', ylab ='Izplacila drugega igralca', zlab ='Velikost matrike (n x m)')
      rglwidget()
      
    }
    
  })
  
 

    
  #primerjanje glede na razlicne porazdelitve 2 ali vec 
  output$plot1 <- renderPlot({
    dol <- length(input$por11)
    if ( dol == 1 & ((input$n1 * input$m1) <= 90)){
      dat <- enofazna_enaka_por(input$por1, input$por11, input$n1, input$m1)
      ggplot(data = dat) +
        geom_point(aes(x = velikost_matrike, y = Vrednost, 
                       color = igralec ), cex = 2, 
                   position = position_dodge(width = 0.3),
                   stat = "identity")+
        
        labs(x = "Velikost matrike (n x m)")+
        ggtitle("Višina izplačil glede na velikost matrike")
      
      
    }else if (dol == 2){
      dat <- enofazna_vec_porazdelitev(por0=input$por1 ,por1 = input$por11[1], por2= input$por11[2], por3 = 0, por4 = 0, input$n1, input$m1 )
      ggplot(dat) + 
        geom_bar( aes(x = Porazdelitve, y = vrednosti, 
                      color = igralec, fill = igralec ), 
                  position = "dodge2", stat = "identity")+
        labs(x = 'Vrsta porazdelitve')+
        ggtitle("Višina izplačil posameznega igralca")+
        theme(plot.title = element_text(size=14, face="bold"))
        
    }else if (dol == 3){
      dat <- enofazna_vec_porazdelitev(por0=input$por1 ,por1 = input$por11[1], por2= input$por11[2], por3 = input$por11[3], por4 = 0, input$n1, input$m1 )
      ggplot(dat) + 
        geom_bar( aes(x = Porazdelitve, y = vrednosti, 
                      color = igralec, fill = igralec ), 
                  position = "dodge2", stat = "identity")+
        labs(x = 'Vrsta porazdelitve')+
        ggtitle("Višina izplačil posameznega igralca")+
        theme(plot.title = element_text(size=14, face="bold"))
    }else if (dol == 4){
      dat <- enofazna_vec_porazdelitev(por0=input$por1 ,por1 = input$por11[1], por2= input$por11[2], por3 = input$por11[3], por4 = input$por11[4], input$n1, input$m1 )
      ggplot(dat) + 
        geom_bar( aes(x = Porazdelitve, y = vrednosti, 
                      color = igralec, fill = igralec ), 
                  position = "dodge2", stat = "identity")+
        labs(x = 'Vrsta porazdelitve')+
        ggtitle("Višina izplačil posameznega igralca")+
        theme(plot.title = element_text(size=14, face="bold"))
      
    }
  })
  output$plot1O <- renderText({
    dol <- length(input$por11)
    if (dol == 1 & ((input$n1 * input$m1) <= 90)){
      'Kot je razvidno so izplačila prvega in drugega igralca enaka, neglede na velikost matrike. Spreminja pa se velikost le teh. Lahko bi rekli, da se velikost
      izplačil linearno povečuje z številom akcij.'
    }else if(dol == 1 & ((input$n1 * input$m1) > 90)){
      'Za večje matrike so izplačila predstavljena v 3D grafu. Vrednosti se z večanjem število akcij povečujejo. Pri nekaterih porazdelitvah se to zgodi hitreje. '
      
    }else if (dol >= 2){
      paste('Prikazana so izplačila, kjer ima prvi igralec porazdelitev', input$por1, '. Porazdelitev drugega pa se spreminja. Opazimo lahko, da so razlike pri vrednostih glede na porazdelitev, kar je pričakovano.')
    }
    
  })
  
  
  
  #dvostopenska porazdelitev
  
  #naredi graficno porazdelitev pogajanja 
  output$nbs2 <- renderPlot({
    graf_dvofazna(input$por12, input$por22,input$n2, input$m2 )
    
    
    output$nbs11 <- renderText({
      'Predstavljen je grafični prikaz sporazuma, kjer igralca začneta v statusu quo (točka SQ), to je točka grožnje, ki jo dobimo s pomočjo maxmin strategije posameznega igralca. 
      Točka sporazuma je označena z NBS.'
    }) 
    
  }) 
  
  #3D graf za velike matrike
  output$plot <- renderRglwidget({
    rgl.open(useNULL=T)
    dol <- length(input$por22)
    if (dol == 1 & ((input$n2 * input$m2) > 90)){
      dat <- izbrana_porazdelitev(input$por12, input$por22, input$n2, input$m2)
      scatter3d(x=dat$igralec1, y=dat$igralec2, z=dat$velikost_matrike, surface.alpha = 0.1, fogtype = 'none', fit = 'smooth',
                xlab = 'Izplacila prvega igralca', ylab ='Izplacila drugega igralca', zlab ='Velikost matrike (n x m)', 
                main = 'Višina izplačil glede na velikost matrike')
      rglwidget()
      
    }
  })
  
  #primerjava porazdelitev + za dol=1 graf do velikosti matrike 
    output$plot2 <- renderPlot({
      dol <- length(input$por22)
      if (dol == 1 & ((input$n2 * input$m2) <= 90) ){
        dat <- dvofazna_enaka_por(input$por12, input$por22, input$n2, input$m2)
        ggplot(data = dat) +
          geom_point(aes(x = velikost_matrike, y = Vrednost, 
                         color = igralec ), cex = 2, 
                     position = position_dodge(width = 0.3),
                     stat = "identity")+
          
          labs(x = "Velikost matrike (n x m)")+
          ggtitle("Višina izplačil glede na velikost matrike")+
          theme(plot.title = element_text(size=14, face="bold"))
      }else if (dol == 2){
        dat <- dvofazna_vec_porazdelitev(por0=input$por12 ,por1 = input$por22[1], por2= input$por22[2], por3 = 0, por4 = 0, input$n2, input$m2 )
        ggplot(dat) + 
          geom_bar( aes(x = Porazdelitve, y = vrednosti, 
                        color = igralec, fill = igralec ), 
                    position = "dodge2", stat = "identity")+
          ggtitle("Velikost izplačil posameznega igralca")+
          theme(plot.title = element_text(size=14, face="bold"))
      }else if (dol == 3){
        dat <- dvofazna_vec_porazdelitev(por0=input$por12 ,por1 = input$por22[1], por2= input$por22[2], por3 = input$por22[3], por4 = 0, input$n2, input$m2 )
        ggplot(dat) + 
          geom_bar( aes(x = Porazdelitve, y = vrednosti, 
                        color = igralec, fill = igralec ), 
                    position = "dodge2", stat = "identity")+
          ggtitle("Velikost izplačil posameznega igralca")+
          theme(plot.title = element_text(size=14, face="bold"))
      }else if (dol == 4){
        dat <- dvofazna_vec_porazdelitev(por0=input$por12 ,por1 = input$por22[1], por2= input$por22[2], por3 = input$por22[3], por4 = input$por22[4], input$n2, input$m2 )
        ggplot(dat) + 
          geom_bar( aes(x = Porazdelitve, y = vrednosti, 
                        color = igralec, fill = igralec ), 
                    position = "dodge2", stat = "identity")+
          ggtitle("Velikost izplačil posameznega igralca")+
          theme(plot.title = element_text(size=14, face="bold"))
      }
    
    })
    output$plot22 <- renderText({
      dol <- length(input$por22)
      if (dol == 1 & ((input$n2 * input$m2) <= 90)){
        'V tabeli so predstavljene vrednosti izplačil posameznega igralca glede na velikost matrike. Kot je razvidno so izplačila različna glede na 
        igralca. Težko pa bi rekli, da vidimo kakšen vzorec povezan z višino izplena in velikostjo matrik.'
      }else if(dol == 1 & ((input$n2 * input$m2) > 90)){
        'Za večje matrike so izplačila predstavljena v 3D grafu. Vrednosti so razpršene, vendar lahko vidimo naraščujoč trend.'
        
      }else if (dol >= 2){
        paste('Prikazana so izplačila, kjer ima prvi igralec porazdelitev', input$por12, '. Porazdelitev drugega pa se spreminja.')
      }
      
    })
    
  
}
shinyApp(ui = ui, server = server)