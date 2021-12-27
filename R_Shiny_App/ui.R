####################################################
##R ui script
##Auther: Rabin Paudel
##Date: 05/06/2021
###################################################
library(caret)
library(dplyr)
library(Lahman)
library(ggplot2)
library(shiny)



#modify the Teams object (creating new teams object) as per instructions
teams <- Teams %>% filter(yearID >= 1901, lgID  %in% c("AL", "NL")) %>% 
          mutate(TB = H + X2B + 2 * X3B + 3 * HR, WinP = W/G, RperG = R/G, 
          HperG = HR/G, TobpG = TB/G, StopG = SO/G, StopG = SO/BB, WhiP = 3 *
          (H + BB)/IPouts) %>% select(-c(franchID, divID, Rank,G,Ghome,W,L,
          DivWin, WCWin, LgWin, WSWin, name, park, attendance, BPF, PPF, 
          teamIDBR, teamIDlahman45, teamIDretro))


###################################################
#Create Input

shinyUI(fluidPage(
  titlePanel("Visualizing Baseball Data Over Time"),
  fluidRow(
 
    sidebarLayout(
        sidebarPanel(
          sliderInput(inputId = "yearID",
                      label="Include Years in Range",
                      min = 1901, max = 2020,
                      value = 1961, animate=TRUE),
          
          br(),
         
          selectInput(inputId = "var", 
                      label = "Statistic to Plot", 
                      choices = c("X2B", "X3B", "BB","SO","SB", "CS","HBP", "HR", "SF", "RA", "ER", "ERA", 
                                  "CG", "SHO", "SV", "IPouts", "HA", "HRA",
                                  "BBA", "SOA", "E", "DP", "FP","WinP", "RperG",
                                  "HperG", "TobpG", "StopG", "WhiP"),
                      selected = "HR"),
        
          checkboxGroupInput("plot", 
                            label= " ", 
                            choices = list(
                              "Color by League?" = "point", 
                              "Add in trend across time?" = "valu"), 
                            selected = " "),
          #Not necessary try to practice 
          "More text",
          br(),
          
        a(href = "http://127.0.0.1:3682/.", target = "_blank", "assignment link")
         
          

        ),
        mainPanel(
          #Out put function 
          plotOutput(outputId =  "pointPlot"),
          br(),
          DT::dataTableOutput( outputId = "summary")
      )
    )
  )

))

