# Weapon profile 1, 2
  # Base, Dice, bonus
  # CritR, CritM, Crit19
  # Switch button

# Attack rate
  # BAB, Alacrity, Doublestrike
  # https://www.ddo.com/forums/showthread.php/275144

# Fighting Style
  # Offhand:  attack %, offhand doublestrike
  # SWF: attack rate bonus, bashing
  # TWF: glancing blows
  # Wolf
  # Bear
  # Unarmed
  # Bow
  # Repeater
  # Inquisitive
  # Thrown, incl Shuriken: Expertise, Ninja Spy, LD boost # Other thrown
  # http://mmlddo.com/DDODPS.html

# MP, RP

# Sneak Attack
  # Dice, bonus

# Misc
  # Helpless
  # Fortification Bypass

# Procs
  # Add proc: type (hit/crit/vorpal), dice, bonus damage, scaling, scales with...

# Boosts
  # Haste, doublestrike, damage, to-hit

# Special attacks
  # Add: dice, weapon dice, bonus damage, scaling, scales with...


# Import/export

# Stats (mean, st. dev, max)

# Vis: Fortification vs dps plot

# Boss:  Fort, DR


library(shiny)
library(shinyjs)
library(tidyverse)

ui <- fluidPage(useShinyjs(),
        titlePanel("Deeps Calc v0.00001"),
        tabsetPanel(
          tabPanel("Melee",
            column(width = 12, br(),
              fluidRow(
                column(width = 4,
                  wellPanel(selectInput("Cstyle", label = "Combat Style",
                    choices = c("Two-Weapon / Unarmed"="twfu", "Two-Handed"="thf", 
                                "Single-Weapon / S&B"="swfsb", "Wolf / Bear"="animal")
                    )
                  )
                ),
                column(width = 8,
                  fluidRow(
                    ### Two-weapon fighting / Unarmed.  Difference b/c for BAB / attack rate differences
                       conditionalPanel(condition = 'input.Cstyle == "twfu"',
                          column(width = 2,
                            h5(tags$b("TWF Feats")),
                            checkboxInput("TWF", "TWF", T),
                            uiOutput("ITWF"), uiOutput("GTWF"), textOutput("totTWF")
                          ),
                          column(width = 2,
                            checkboxInput("Unarmed", "Unarmed", F)
                          ),
                          column(width = 4,
                            textInput("OffTWF", "Offhand Attack %", value = "20"),
                            textInput("OffDBS", "Offhand Doublestrike %", value = "20"))
                          ),
                    ### Two-handed fighting
                       conditionalPanel(condition = 'input.Cstyle == "thf"',
                          column(width = 2,
                            checkboxInput("THF", "THF", T),
                            uiOutput("ITHF"), uiOutput("GTHF"), textOutput("totTHF")
                          ),
                          column(width = 4,
                            textInput("GlancingC", "Glancing Blows % Damage", value = "20"),
                            textInput("GlancingD", "Glancing Blows % Proc", value = "20")
                          )
                       ),
                    ### Single-weapon fighting and Sword-and-board
                    ##Add disclaimers here about SWF - S&B *only* for bards
                       conditionalPanel(condition = 'input.Cstyle == "swfsb"',
                          column(width = 2,
                            checkboxInput("SWF", "SWF", T),
                            uiOutput("ISWF"), uiOutput("GSWF")
                          ),
                          column(width = 2,
                            checkboxInput("ISB", "Improved Shield Bashing", F),
                            checkboxInput("SM", "Shield Mastery", F),
                            uiOutput("ISM"),
                            checkboxInput("LSM", "Legendary Shield Mastery (Sentinel)", F)
                          ),
                          column(width = 4, h5("Additional bonuses"),
                             textInput("BashC", "% Shield Bash", value = "20")
                          )
                       ),
                    # ### Animal Forms.  Differences in attack rate
                        conditionalPanel(condition = 'input.Cstyle == "animal"',
                          column(width = 4,
                            radioButtons("NF", "Natural Fighting Feats", choices = c(0, 1, 2, 3), inline = T)
                          ),
                          column(width = 4,
                            radioButtons("form", "Form",
                                         choices = c("Wolf"="wolf", "Winter Wolf"="wwolf",
                                                     "Bear"="bear", "Dire Bear"="dbear"), inline = T )
                          )
                    )
                  )
                )
              )
            )
          ),
          # Fighting Style
          # Wolf
          # Bear
          # Unarmed

          # http://mmlddo.com/DDODPS.html
          tabPanel("Ranged",
            column(width=12, br(),
              fluidRow(
                selectInput("CStyle", label = "Combat Style",
                            choices = c("Bow", "Crossbow", "Repeater",
                                        "Inquisitive", "Thrown")
                )
              )
              # Ranged is interesting because combat style affects animation but (mostly) not options.
                  # PBS, PS vs. IPS, Quick Draw, Rapid Reload, Rapid Shot
                  # Zen Archery, Manyshot, Fusillade need to count as conditional boosts
              
              # Bow
              # Repeater
              # Inquisitive
              # Thrown
            )
          )
        )
)


  
server <- shinyServer(function(input, output, session) {

  output$ITWF <- renderUI ({
    if(input$TWF==1 && input$Cstyle=='twfu') {
      checkboxInput("ITWF", "ITWF", F) }
  })
  output$GTWF <- renderUI ({
    if(all(input$ITWF==1,input$TWF==1,input$Cstyle=='twfu')) {
      checkboxInput("GTWF", "GTWF", F) }
  })
  
  output$ITHF <- renderUI ({
    if(input$THF==1 && input$Cstyle == 'thf') {
      checkboxInput("ITHF", "ITHF", F) }
  })
  output$GTHF <- renderUI ({
    if(all(input$ITHF==1,input$THF==1,input$Cstyle == 'thf')) {
      checkboxInput("GTHF", "GTHF", F) }
  })
  
  output$ISWF <- renderUI ({
    if(input$SWF==1 && input$Cstyle == 'swfsb') {
      checkboxInput("ISWF", "ISWF", F) }
  })
  output$GSWF <- renderUI ({
    if(all(input$ISWF==1,input$SWF==1,input$Cstyle == 'swfsb')) {
      checkboxInput("GSWF", "GSWF", F) }
  })
  
  output$ISM <- renderUI ({
    if(input$SM==1 && input$Cstyle == 'swfsb'){
      checkboxInput("ISM", "ISM", F)
    }
  })
  
  output$totTWF <- renderText({
    paste(20 + 20*input$TWF +
             20*ifelse(!is.null(input$ITWF),input$ITWF*input$TWF, 0) +
             20*ifelse(!is.null(input$GTWF),input$GTWF*input$ITWF*input$TWF, 0))
  })
  output$totTHF <- renderText({
    paste(20 + 10*input$THF +
            10*ifelse(!is.null(input$ITHF),input$ITHF*input$THF, 0) +
            10*ifelse(!is.null(input$GTHF),input$GTHF*input$ITHF*input$THF, 0))
  })
})

shinyApp(ui, server)
