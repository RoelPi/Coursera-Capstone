#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    titlePanel("A multifunctional search engine", windowTitle = "Multifunctional Search Engine"),
    sidebarLayout(
        sidebarPanel(
            textInput('textHistory','Search Term','who is the'),
            checkboxInput('suggestInput', 'Suggest Next Word', value = TRUE, width = NULL)
    ),
    mainPanel(
        div(class='alert alert-info',style="overflow:auto",
               img(id='googleIcon',style='height:24px;width:24px;float:left;margin-right:15px;display:block;',src='google.png'),
               uiOutput('googleLink')
        ),
        div(class='alert alert-danger',style="overflow:auto",
            img(id='googleIcon',style='height:24px;width:24px;float:left;margin-right:15px;display:block;',src='duck.png'),
            uiOutput('duckLink')
        ),
        div(class='alert alert-success',style="overflow:auto",
            img(id='ecoIcon',style='height:24px;width:24px;float:left;margin-right:15px;display:block;',src='ecosia.ico'),
            uiOutput('ecoLink')
        ),
        div(class='alert alert-warning',style="overflow:auto",
            img(id='giphyIcon',style='height:24px;width:24px;float:left;margin-right:15px;display:block;',src='giphy.png'),
            uiOutput('giphyLink')
        )
    )
  )
))
