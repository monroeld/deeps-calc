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
  # Shuriken: Expertise, Ninja Spy, LD boost
  # Other thrown
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
                       selectInput("Cstyle", label = "Combat Style",
                                   choices = c("Two-Weapon", "Two-Handed", 
                                               "Single-Weapon",
                                               "Unarmed", "Wolf", "Bear")
                       )
                ),
                # Need to figure out how to toggle off feats that aren't displayed
                column(width = 8,
                  fluidRow(
                       conditionalPanel(condition = 'input.Cstyle == "Two-Weapon"',
                          column(width = 2,
                            checkboxInput("TWF", "TWF", T),
                            p(id = "TWF",
                              checkboxInput("ITWF", "ITWF", T), condition = "TWF"),
                            p(id = "ITWF",
                              checkboxInput("GTWF", "GTWF", T), condition = "ITWF"),
                          textOutput("totTWF")
                          ),
                          column(width = 4,
                            textInput("OffTWF", "Offhand Attack %", value = "20"),
                            textInput("OffDBS", "Offhand Doublestrike %", value = "20"))
                          ),
                       conditionalPanel(condition = 'input.Cstyle == "Two-Handed"',
                          column(width = 2,
                            checkboxInput("THF", "THF", T),
                            p(id = "THF",
                              checkboxInput("ITHF", "ITHF", T), condition = "THF"),
                            p(id = "ITHF",
                              checkboxInput("GTHF", "GTHF", T), condition = "ITHF")
                          ),
                          column(width = 4,
                            textInput("GlancingC", "Glancing Blows % Damage", value = "20"),
                            textInput("GlancingD", "Glancing Blows % Proc", value = "20")
                          )
                       ),
                       conditionalPanel(condition = 'input.Cstyle == "Single-Weapon"',
                          column(width = 2,
                            checkboxInput("SWF", "SWF", T),
                            p(id = "SWF",
                              checkboxInput("ISWF", "ISWF", T), condition = "SWF"),
                            p(id = "ISWF",
                              checkboxInput("GSWF", "GSWF", T), condition = "ISWF"),
                            checkboxInput("ISB", "Improved Shield Bashing", F),
                            checkboxInput("SM", "Shield Mastery", F),
                            p(id = "SM",
                              checkboxInput("ISM", "ISWF", F), condition = "SM"),
                            checkboxInput("LSM", "Legendary Shield Mastery (Sentinel)", F)
                          ),
                          column(width = 4, "Additional bonuses",
                             textInput("BashingC", "% Shield Bash", value = "20")
                          )
                       )
                  )
                )
              )
            )
          ),
          # Fighting Style
          # SWF: attack rate bonus, bashing
          # Wolf
          # Bear
          # Unarmed
          # Bow
          # Repeater
          # Inquisitive
          # Shuriken: Expertise, Ninja Spy, LD boost
          # Other thrown
          # http://mmlddo.com/DDODPS.html
          tabPanel("Ranged",
            column(width=12, br(),
              fluidRow(
                selectInput("CStyle", label = "Combat Style",
                            choices = c("Bow", "Crossbow", "Repeater",
                                        "Inquisitive", "Shuriken", "Thrown")
                )
              )
            )
          )
        )
)


  
server <- shinyServer(function(input, output, session) {
  observe({
    toggle(id = "ITWF", condition = input$TWF)
    toggle(id = "GTWF", condition = input$ITWF)
    toggle(id = "ITHF", condition = input$THF)
    toggle(id = "GTHF", condition = input$ITHF)
  })
  observe({
    updateCheckboxInput(session, inputId = "GTWF", label = NULL, value = "input$ITWF == 0")
  })
  output$totTWF <- renderText({
    paste0(20 + 20*input$TWF + 20*input$ITWF + 20*input$GTWF)
  })
})

shinyApp(ui, server)
