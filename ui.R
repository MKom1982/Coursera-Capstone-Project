suppressPackageStartupMessages(c(
    library(shinythemes),
    library(shiny),
    library(tm),
    library(stringr),
    library(markdown),
    library(stylo)))

shinyUI(navbarPage("Coursera Data Science Capstone Project",
                   theme = shinytheme("sandstone"),
                   tabPanel("Word Prediction App",
                            fluidRow(
                                column(3),
                                column(6,
                                       tags$div(textInput("txt_in",
                                                label = h4("Please input your text and press spacebar:"),
                                                value = ),
                                        tags$span(style="color:blue",("Offensive, vulgar, etc. words are censored with asterisk (*)")),
                                                br(),
                                                h4("Predicted next word:"),
                                        tags$span(style="color:darkred",
                                                tags$strong(tags$h3(textOutput("text2")))),
                                                br(),
                                                h4("Your text with proposed continuation:"),
                                        tags$em(tags$h4(textOutput("text1"))),
                                                align="center")
                                ),
                                column(3)
                            )
                   ),
                   tabPanel("Further details",
                            fluidRow(
                                column(3),
                                column(6,     
                                       tags$span(style="color:blue",("This app was built as a part of Capstone project of Coursera Data Science Course."),
                                                 tags$p("If you would like to learn more about used data, please check this ", tags$a(href = "https://rpubs.com/MKomisarz/CCP_W2R", " report"),", where you can find some explanatory analysis of the data."),
                                                 tags$p("Application and its way of working is presented " , tags$a(href = "http://rpubs.com/MNidhi/FinalPresentation", "here",".")),
                                                 tags$p("Please be informed that due to very high RAM and disk space usage when creating ngrams, I have 
                                                        limited their level up to trigrams, despite of initially assumed creation of ngrams of level 4 or even 5. 
                                                        Also, for the app database I have taken samples of 5% of original files. Therefore, you may 
                                                        feel that word prediction is not so good as you would expect it to be.
                                                        In such cases, I am counting on your understanding :)"),
                                                 tags$p("Files with R code & source files used for work predicting are uploaded to " , tags$a(href = "https://github.com/Nidhi-M/Word-Prediction-App", "GitHub"),"."),
                                       )))),
                tags$br()
)
)