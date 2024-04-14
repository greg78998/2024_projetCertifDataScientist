library(dygraphs)
library(ggplot2)
library(dplyr)
library(shiny)
library(htmlwidgets)
library(shiny)
library(htmlwidgets)
library(pivottabler)
library(tidyr)




# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$h2_title1 <- renderUI({
        h3(paste0("Statistiques", sep = ""))
    })
    output$h2_title2 <- renderUI({
        h3(paste0("Modélisation", sep = ""))
    })
    output$h2_title3 <- renderUI({
        h3(paste0("France métropolitaine", sep = ""))
    })
    output$h2_title4 <- renderUI({
        h3(paste0("En difficulté demain?", sep = ""))
    })
    
    output$tab1_PT <- DT::renderDataTable({
       
       datatable(
           db_defaillance %>% pivot_wider(id_cols = annee, names_from = mois, values_from = nb_defaillance),
           caption = "Nombre de défaillance sur la période récente", 
           rownames = FALSE)
    })
    
    output$def_table2 <- DT::renderDataTable({
        db_defaillance
    })
    output$def_table3 <- DT::renderDataTable({
        db_defaillance
    })
    output$def_table4 <- DT::renderDataTable({
        db_defaillance
    })

})
