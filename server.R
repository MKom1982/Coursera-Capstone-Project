library(shiny)
source("Cap_02_Support functions file.R")

shinyServer(
    function(input, output) {
        output$text1 <- renderText({
            paste(txt_flt(wrd_catch(input$txt_in)))
        })
        output$text2 <- renderText({
            paste(txt_flt(pred_catch(input$txt_in)))
        })
    }
)