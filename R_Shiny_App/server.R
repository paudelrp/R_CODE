####################################################
##R server script 
##Author: Rabin Paudel
##Date : 05/06/2021
###################################################

#read in packages we need
library(dplyr)
library(ggplot2)
library(Lahman)
library(shiny)
library(lattice)
library(DT)
#modify the Teams object (creating new teams object) as per instructions
teams <- Teams %>% filter(yearID >= 1901, lgID  %in% c("AL", "NL")) %>% 
              mutate(TB = H + X2B + 2 * X3B + 3 * HR, WinP = W/G, RperG = R/G, 
              HperG = HR/G, TobpG = TB/G, StopG = SO/G, StopG = SO/BB, WhiP = 3 *
              (H + BB)/IPouts) %>% select(-c(franchID, divID, Rank,G,Ghome,W,L,
              DivWin, WCWin, LgWin, WSWin, name, park, attendance, BPF, PPF, 
              teamIDBR, teamIDlahman45, teamIDretro))
#####################################################
#Basic syntax for a shiny server
shinyServer (function(input, output) {
  output$summary <- DT::renderDataTable({
    p <- input$yearID
    var1 <- input$var
    teamsSub <- teams[, c(p ="yearID", var1), drop = FALSE]
    x <- aggregate(teamsSub[[var1]] ~ yearID + yearID, data = teamsSub, FUN = mean)
    x[,2] <- round(x[, 2], digits = 2)
    names(x)[2] <- paste0("Avg")
    x
  
    })
  #Create Output plot with the help of renderplot.
  output$pointPlot <- renderPlot({
   
    x <- as.numeric(input$yearID)
    y <- input$var
    
    a <- as.character(teams$lgID)
    g <-  ggplot(teams, aes_string(x = "yearID", y = y, na.rm=TRUE)) 
    
    if (isTRUE(input$plot == "point")) {
      g <-  g + geom_point(na.rm = TRUE,aes_string(fill = as.factor(a), colour = factor({{a}}))) + scale_fill_discrete(name = "lgID",  
            labels = c("AL", "NL")) + scale_color_manual(name="lgID", values = c("red", "blue"))
      plot(g)
      
    }
    else if  (isTRUE(input$plot == "valu" )) {
      g <-  g  + geom_point(na.rm = TRUE, aes_string(fill = as.factor(a), colour = factor({{a}})))  + geom_smooth(na.rm = FALSE, aes_string(fill = as.factor(a), 
            colour = factor({{a}}))) + scale_fill_discrete(name = "lgID",  labels = c("AL", "NL")) + scale_color_manual(name="lgID", 
            values = c("red", "blue"))
      plot(g)
    }
    else {
            g <- g + geom_point(na.rm = TRUE) 
      plot(g)
    }
  
  
  })
  
})

