library(shiny)
library(shinyjs)
library(ggplot2)
library(plyr)
library(colourpicker)
library(DT)
library(shinythemes)
library(caret)
library("superpc")
library(pander)
library(knitr)

fluidPage(
                tags$head(includeScript("google-analytics.js")),
                navbarPage("GAC: Gene Associations with Clinical", 
                           
                           tabPanel("Read Me",  
                                    fluidRow(
                                      column(12,
                                             htmlOutput("ReadMe1")                                             #htmlOutput("blurp"))
                                      )
                                    )),    
                           tabPanel("Super PC Time to Event Outcome",
                                    fluidRow(
                                      column(2,
                                             wellPanel(
                                               h3("Input Time to Event file"),
                                               selectInput("file4",label= "Select an example Clinical ds or upload your own with 'Load my own'", 
                                                           choices = c("Example ds File"="Example4", "Load my own data" = "load_my_own")),
                                               conditionalPanel("input.file4 == 'load_my_own'",
                                                                fileInput('file41', 'Choose file to upload (maximum size 500 MB).', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                               conditionalPanel("input.file4 == 'Example4'",
                                                                downloadButton('downloadEx3', 'Download Example data')
                                               )),
                                             wellPanel(
                                               h3("Input Expression file"),
                                               selectInput("file5",label= "Select an example Expression ds or upload your own with 'Load my own'", 
                                                           choices = c("Example ds File"="Example5", "Load my own data" = "load_my_own")),
                                               conditionalPanel("input.file5 == 'load_my_own'",
                                                                fileInput('file51', 'Choose file to upload (maximum size 500 MB).', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                               conditionalPanel("input.file5 == 'Example5'",
                                                                downloadButton('downloadEx4', 'Download Example data')
                                               )),
                                             wellPanel(
                                               h3("Choose Options"),
                                               radioButtons("split2", "Split data:", list("70-30" = 70,"60-40" = 60,"50-50" = 50), selected = 60),
                                               radioButtons("cutoff2", "Numer of Iterations for cross validation:", list("100" = 100, "150" = 150, "200" = 200), selected = 100),
                                               radioButtons("nfold1", "Numer of folds:", list("2" = 2, "5" = 5, "10" = 10), selected = 2),
                                               br(),
                                               actionButton("goButton1", "Run analysis")
                                             ),
                                             wellPanel(
                                               h3("Input Fold IDs"),
                                               radioButtons("foldid2", "Input Fold IDs to Replicate Previous Results:", list("Yes" = 1, "No" = 0), selected = 0),
                                               selectInput("file22fold2",label= "If Yes, Select input ids for example data or upload your own with 'Load my own'", 
                                                           choices = c("Example ds File"="Example2", "Load my own data" = "load_my_own")),
                                               conditionalPanel("input.file22fold2 == 'load_my_own'",
                                                                fileInput('file221fold2', 'Choose file to upload (maximum size 500 MB).', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                               conditionalPanel("input.file22fold2 == 'Example2'",
                                                                downloadButton('downloadEx2fold2', 'Download Example data')
                                               )),
                                             wellPanel(
                                               h4("Download the List for Important Features or Genes"),
                                               downloadButton('downloadSuperPC2', 'Download Super PC Results')
                                             ),
                                             wellPanel(
                                               h4("Download a table with fold ids to upload to replicate results"),
                                               downloadButton('downloadfoldid2', 'Download Fold IDs to Replicate')
                                             )
                                      ),
                                      column(10,
                                             htmlOutput("pv3"),
                                             verbatimTextOutput('ex_p2'),
                                             htmlOutput("pv4"),
                                             plotOutput("predictplot2", height= 600, width = 600),
                                             htmlOutput("title2"),
                                             DT::dataTableOutput("SuperPC2")
                                      )
                                    )),
                           tabPanel("Super PC Continuous Outcome",
                                    fluidRow(
                                      column(2,
                                             wellPanel(
                                               h3("Input Continuous outcome file"),
                                               selectInput("file6",label= "Select an example Continuous outcome ds or upload your own with 'Load my own'", 
                                                           choices = c("Example ds File"="Example6", "Load my own data" = "load_my_own")),
                                               conditionalPanel("input.file6 == 'load_my_own'",
                                                                fileInput('file61', 'Choose file to upload (maximum size 500 MB).', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                               conditionalPanel("input.file6 == 'Example6'",
                                                                downloadButton('downloadEx5', 'Download Example data')
                                               )),
                                             wellPanel(
                                               h3("Input Expression file"),
                                               selectInput("file7",label= "Select an example Expression ds or upload your own with 'Load my own'", 
                                                           choices = c("Example ds File"="Example7", "Load my own data" = "load_my_own")),
                                               conditionalPanel("input.file7 == 'load_my_own'",
                                                                fileInput('file71', 'Choose file to upload (maximum size 500 MB).', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                               conditionalPanel("input.file7 == 'Example7'",
                                                                downloadButton('downloadEx6', 'Download Example data')
                                               )),
                                             wellPanel(
                                               h3("Choose Options"),
                                               radioButtons("split3", "Split data:", list("70-30" = 70,"60-40" = 60,"50-50" = 50), selected = 60),
                                               radioButtons("cutoff3", "Numer of Iterations for cross validation:", list("100" = 100, "150" = 150, "200" = 200), selected = 100),
                                               radioButtons("nfold2", "Numer of folds:", list("2" = 2, "5" = 5, "10" = 10), selected = 2),
                                               radioButtons("Predicted2", "Predicted Values Type:", list("Continuous" = 0, "Discrete" = 1), selected = 0),
                                               br(),
                                               actionButton("goButton2", "Run analysis") 
                                            ),
                                             wellPanel(
                                               h3("Input Fold IDs"),
                                               radioButtons("foldid3", "Input Fold IDs to Replicate Previous Results:", list("Yes" = 1, "No" = 0), selected = 0),
                                               selectInput("file22fold3",label= "If Yes, Select input ids for example data or upload your own with 'Load my own'", 
                                                           choices = c("Example ds File"="Example2", "Load my own data" = "load_my_own")),
                                               conditionalPanel("input.file22fold3 == 'load_my_own'",
                                                                fileInput('file221fold3', 'Choose file to upload (maximum size 500 MB).', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                               conditionalPanel("input.file22fold3 == 'Example2'",
                                                                downloadButton('downloadEx2fold3', 'Download Example data')
                                               )),
                                             wellPanel(
                                               h4("Download the List for Important Features or Genes"),
                                               downloadButton('downloadSuperPC3', 'Download Super PC Results')
                                             ),
                                             wellPanel(
                                               h4("Download a table with fold ids to upload to replicate results"),
                                               downloadButton('downloadfoldid3', 'Download Fold IDs to Replicate')
                                             )
                                      ),
                                      column(10,
                                             htmlOutput("pv5"),
                                             verbatimTextOutput('ex_p3'),
                                             htmlOutput("pv6"),
                                             plotOutput("predictplot3", height= 600, width = 600),
                                             verbatimTextOutput('ex_out3'),
                                             htmlOutput("title3"),
                                             DT::dataTableOutput("SuperPC3")
                                      )
                                    )),
                           tabPanel("Super PC Binary Outcome",
                                    fluidRow(
                                      column(2,
                                             wellPanel(
                                               h3("Input Expression with Binary Outcome file"),
                                               selectInput("file22",label= "Select an example ds or upload your own with 'Load my own'", 
                                                           choices = c("Example ds File"="Example2", "Load my own data" = "load_my_own")),
                                               conditionalPanel("input.file22 == 'load_my_own'",
                                                                fileInput('file221', 'Choose file to upload (maximum size 500 MB).', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                               conditionalPanel("input.file22 == 'Example2'",
                                                                downloadButton('downloadEx2', 'Download Example data')
                                               )),
                                             wellPanel(
                                               h3("Choose Options"),
                                               radioButtons("split1", "Split data:", list("70-30" = 70,"60-40" = 60,"50-50" = 50), selected = 60),
                                               radioButtons("cutoff1", "Numer of Iterations for cross validation:", list("100" = 100, "150" = 150, "200" = 200), selected = 100),
                                               radioButtons("nfold3", "Numer of folds:", list("2" = 2, "5" = 5, "10" = 10), selected = 2),
                                               radioButtons("Predicted1", "Predicted Values Type:", list("Continuous" = 0, "Discrete" = 1), selected = 0),
                                               br(),
                                               actionButton("goButton3", "Run analysis")  
                                             ),
                                             wellPanel(
                                               h3("Input Fold IDs"),
                                               radioButtons("foldid1", "Input Fold IDs to Replicate Previous Results:", list("Yes" = 1, "No" = 0), selected = 0),
                                               selectInput("file22fold",label= "If Yes, Select input ids for example data or upload your own with 'Load my own'", 
                                                           choices = c("Example ds File"="Example2", "Load my own data" = "load_my_own")),
                                               conditionalPanel("input.file22fold == 'load_my_own'",
                                                                fileInput('file221fold', 'Choose file to upload (maximum size 500 MB).', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                               conditionalPanel("input.file22fold == 'Example2'",
                                                                downloadButton('downloadEx2fold', 'Download Example data')
                                               )),
                                             wellPanel(
                                               h4("Download the List for Important Features or Genes"),
                                               downloadButton('downloadSuperPC', 'Download Super PC Results')
                                             ),
                                             wellPanel(
                                               h4("Download a table with fold ids to upload to replicate results"),
                                               downloadButton('downloadfoldid1', 'Download Fold IDs to Replicate')
                                             )
                                      ),
                                      column(10,
                                             htmlOutput("pv"),
                                             verbatimTextOutput('ex_p'),
                                             htmlOutput("pv2"),
                                             #helpText('Plot for Predicted Principal Component Between Observed Outcome'),
                                             plotOutput("predictplot", height= 600, width = 600),
                                             #helpText('Statistics for Predicted Principal Component Between Observed Outcome'),
                                             verbatimTextOutput('ex_out1'),
                                             #helpText('Tests for Predicted Principal Component Between Observed Outcome'),
                                             verbatimTextOutput('ex_out2'),
                                             htmlOutput("title"),
                                             DT::dataTableOutput("SuperPC")
                                      )
                                    )),
                           tabPanel("Forest Plot",  
                                    fluidRow(
                                      column(2,
                                             wellPanel(
                                               h3("Input your file"),
                                               selectInput("file1",label= "Select an example ds or upload your own with 'Load my own'", 
                                                           choices = c("Example ds File"="Example", "Load my own data" = "load_my_own")),
                                               conditionalPanel("input.file1 == 'load_my_own'",
                                                                fileInput('file2', 'Choose file to upload (maximum size 500 MB).', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                               conditionalPanel("input.file1 == 'Example'",
                                                                downloadButton('downloadEx', 'Download Example data')
                                               )),
                                             wellPanel(
                                               h3("Cosmetic Changes"),
                                               sliderInput("font_size", "Font size:", min = 0, max = 10, value = 6, step= 0.5),
                                               
                                               textInput("Left_text_label", "Left_text_label", "Longer Overall Survival"),
                                               textInput("Right_text_label", "Right_text_label", "Shorter Overall Survival"),
                                               sliderInput("legend_font_size", "Text label Font size:", min = 0, max = 10, value = 4, step= 0.5),
                                               
                                               textInput("scaledata", "Scale data", "0.2,0.6,0.8,1,1.2,1.8,5"),
                                               sliderInput("ID_position2", "Gene location slider (L-R):", min = -20, max = 10, value = 0.25,step= 0.25),
                                               colourInput("col_low", "Color for good group", "blue", returnName = TRUE, palette = "limited", showColour = "background"),
                                               colourInput("col_mid", "Color for intermediate group", "black", returnName = TRUE, palette = "limited", showColour = "background"),
                                               colourInput("col_high", "Color for bad group", "red", returnName = TRUE, palette = "limited", showColour = "background")
                                             ),
                                             wellPanel(
                                               textInput("fname", "Type the file name you would like to save as", value = "Forest_Plot"),
                                               downloadButton('downloadFP', 'Download Forest Plot')
                                             )
                                      ),
                                      column(10,
                                             plotOutput("Forestplot", height= 800, width = 1500)
                                             #htmlOutput("blurp"))
                                      )
                                    )),    
                           tabPanel("Tutorial",
                                    tags$iframe(src= "GAC_Tutorial.pdf", width = 1800, height = 1000)),
                           navbarMenu("About Us",
                                      tabPanel("How to Cite",
                                               fluidRow(
                                                 column(8, offset = 2, 
                                                        "The National Cancer Institute (NCI) requires that publications acknowledge the Winship Cancer Institute CCSG support, and they are tracking compliance. When using this tool to report results in your publication, please include the following statement in the acknowledgment section of your publication(s):",
                                                        br(),
                                                        br(),
                                                        em("Research reported in this publication was supported in part by the Biostatistics and Bioinformatics Shared Resource of Winship Cancer Institute of Emory University and NIH/NCI under award number P30CA138292. The content is solely the responsibility of the authors and does not necessarily represent the official views of the National Institutes of Health.")
                                                 ))),
                                      tabPanel("Contact Us",
                                               fluidRow(
                                                 column(8, offset = 2,
                                                        "This tool was prepared by members of the Winship Biostatistics and Bioinformatics Shared Resource (BBISR) of Emory University.",
                                                        br(),
                                                        a(href="https://bbisr.winship.emory.edu/", "https://bbisr.winship.emory.edu/"),
                                                        br(),
                                                        br(),
                                                        "Authors- Manali Rupji, dual M.S., Xinyan (Abby) Zhang, MPH. & Jeanne Kowalski Ph.D.",
                                                        br(),
                                                        "Maintainer- Manali Rupji 'manali(dot)rupji(at)emory(dot)edu'")
                                                )),
                                      tabPanel("Feedback",
                                               fluidRow(
                                                 column(8, offset = 2,
                                                       #br(),
                                                        "As a Biostatistics and Bioinformatics core, we are actively improving and expanding our NGS analysis services and analysis products. For any questions, comments, or suggestions, please email the developer at manali(dot)rupji(at)emory(dot)edu."
                                                 )))
                           )
                ))

  



