suppressPackageStartupMessages(library(shiny))

fluidPage(
  # Application title:
  titlePanel("ATGC Dynamics Solver"),
  
  # Sidebar with a number of filtering input:
  sidebarLayout(
    sidebarPanel(
      h4("Mutation Rate Constants"),
      selectInput(inputId = "muttype",
                  label   = "Select the model",
                  choices = c("Non Symmetric", "Strand-Symmetric", "Hypercube", "Hypercube ZNE")),
      
      #-----------------------------------------------------------------------------------------------------
      conditionalPanel(condition = 'input.muttype=="Strand-Symmetric" | input.muttype=="Non Symmetric"',
                       tags$head(tags$style(
                         type="text/css",
                         "#logo img {max-width: 70%; width: auto; max-height: 100%;
                         padding-bottom: 10px}"
                       )),
                       imageOutput("logo", height="240px")),
      
      conditionalPanel(condition = 'input.muttype=="Hypercube" | input.muttype=="Hypercube ZNE"',
                       tags$head(tags$style(
                         type="text/css",
                         "#logo6 img {max-width: 70%; width: auto; max-height: 100%;
                         padding-bottom: 10px}"
                       )),
                       imageOutput("logo6", height="220px")),
      #-----------------------------------------------------------------------------------------------------
      
      #-----------------------------------------------------------------------------------------------------
      conditionalPanel(condition = 'input.muttype=="Strand-Symmetric" | input.muttype=="Hypercube ZNE"',
                       tags$head(tags$style(
                         type="text/css",
                         "#logo2 img {max-width: 100%; width: auto; max-height: 100%;
                         padding-top: 10px;}"
                       )),
                       imageOutput("logo2", height="240px"),
                       #-- sliders for Strand-Symmetric and Hypercube ZNE
                       sliderInput('k.ag.tc', 'k_AG&TC',
                                   min=0, max=1.5,
                                   value=0.545,
                                   step=0.001, round=FALSE),
                       
                       sliderInput('k.ct.ga', 'k_CT&GA',
                                   min=0, max=1.5,
                                   value=1.055,
                                   step=0.001, round=FALSE),
                       
                       sliderInput('k.ac.tg', 'k_AC&TG',
                                   min=0, max=1.5,
                                   value=0.175,
                                   step=0.001, round=FALSE),
                       
                       sliderInput('k.ca.gt', 'k_CA&GT',
                                   min=0, max=1.5,
                                   value=0.285,
                                   step=0.001, round=FALSE),
                       
                       sliderInput('k.at.ta', 'k_AT&TA',
                                   min=0, max=1.5,
                                   value=0.205,
                                   step=0.001, round=FALSE),
                       
                       sliderInput('k.cg.gc', 'k_CG&GC',
                                   min=0, max=1.5,
                                   value=0.235,
                                   step=0.001, round=FALSE)
                       #-- end of sliders for Strand-Symmetric and Hypercube ZNE
      ),
      #-----------------------------------------------------------------------------------------------------
      #-----------------------------------------------------------------------------------------------------
      conditionalPanel(condition = 'input.muttype=="Non Symmetric"',
                       tags$head(tags$style(
                         type="text/css",
                         "#logo3 img {max-width: 100%; width: auto; max-height: 100%;
                         padding-top: 10px;}"
                       )),
                       imageOutput("logo3", height="220px"),
                       #-- sliders for Non Symmetric
                       sliderInput('k.ag', 'k_AG',
                                   min=0, max=1.5,
                                   value=0.545,
                                   step=0.001, round=FALSE),
                       
                       sliderInput('k.tc', 'k_TC',
                                   min=0, max=1.5,
                                   value=0.545,
                                   step=0.001, round=FALSE),
                       
                       sliderInput('k.ct', 'k_CT',
                                   min=0, max=1.5,
                                   value=1.055,
                                   step=0.001, round=FALSE),
                       
                       sliderInput('k.ga', 'k_GA',
                                   min=0, max=1.5,
                                   value=1.055,
                                   step=0.001, round=FALSE),
                       
                       sliderInput('k.ac', 'k_AC',
                                   min=0, max=1.5,
                                   value=0.175,
                                   step=0.001, round=FALSE),
                       
                       sliderInput('k.tg', 'k_TG',
                                   min=0, max=1.5,
                                   value=0.175,
                                   step=0.001, round=FALSE),
                       
                       sliderInput('k.ca', 'k_CA',
                                   min=0, max=1.5,
                                   value=0.285,
                                   step=0.001, round=FALSE),
                       
                       sliderInput('k.gt', 'k_GT',
                                   min=0, max=1.5,
                                   value=0.285,
                                   step=0.001, round=FALSE),
                       
                       sliderInput('k.at', 'k_AT',
                                   min=0, max=1.5,
                                   value=0.205,
                                   step=0.001, round=FALSE),
                       
                       sliderInput('k.ta', 'k_TA',
                                   min=0, max=1.5,
                                   value=0.205,
                                   step=0.001, round=FALSE),
                       
                       sliderInput('k.cg', 'k_CG',
                                   min=0, max=1.5,
                                   value=0.235,
                                   step=0.001, round=FALSE),
                       
                       sliderInput('k.gc', 'k_GC',
                                   min=0, max=1.5,
                                   value=0.235,
                                   step=0.001, round=FALSE)
                       #-- end of sliders for Non Symmetric
      ),
      #-----------------------------------------------------------------------------------------------------
      #-----------------------------------------------------------------------------------------------------
      conditionalPanel(condition = 'input.muttype=="Hypercube"',
                       #-- sliders for Hypercube
                       sliderInput('kAA2AC', 'kAA2AC',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kAA2AG', 'kAA2AG',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kAA2AT', 'kAA2AT',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kAA2CA', 'kAA2CA',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kAA2GA', 'kAA2GA',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kAA2TA', 'kAA2TA',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kAC2AA', 'kAC2AA',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kAC2AG', 'kAC2AG',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kAC2AT', 'kAC2AT',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kAC2CC', 'kAC2CC',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kAC2GC', 'kAC2GC',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kAC2TC', 'kAC2TC',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kAG2AA', 'kAG2AA',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kAG2AC', 'kAG2AC',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kAG2AT', 'kAG2AT',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kAG2CG', 'kAG2CG',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kAG2GG', 'kAG2GG',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kAG2TG', 'kAG2TG',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kAT2AA', 'kAT2AA',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kAT2AC', 'kAT2AC',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kAT2AG', 'kAT2AG',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kCA2AA', 'kCA2AA',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kCA2CC', 'kCA2CC',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kCA2CG', 'kCA2CG',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kCA2CT', 'kCA2CT',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kCA2GA', 'kCA2GA',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kCA2TA', 'kCA2TA',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kCC2AC', 'kCC2AC',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kCC2CA', 'kCC2CA',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kCC2CG', 'kCC2CG',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kCC2CT', 'kCC2CT',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kCC2GC', 'kCC2GC',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kCC2TC', 'kCC2TC',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kCG2AG', 'kCG2AG',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kCG2CA', 'kCG2CA',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kCG2CC', 'kCG2CC',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kGA2AA', 'kGA2AA',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kGA2CA', 'kGA2CA',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kGA2GC', 'kGA2GC',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kGA2GG', 'kGA2GG',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kGA2GT', 'kGA2GT',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kGA2TA', 'kGA2TA',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kGC2AC', 'kGC2AC',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kGC2CC', 'kGC2CC',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kGC2GA', 'kGC2GA',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kTA2AA', 'kTA2AA',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kTA2CA', 'kTA2CA',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE),
                       
                       sliderInput('kTA2GA', 'kTA2GA',
                                   min=0, max=1.5,
                                   value=runif(1, min=0, max=1.5),
                                   step=0.001, round=FALSE)
                       #-- end of sliders for Hypercube
      )
    ),
    
    # Show a plot of the generated distribution
    #-- mainPanel
    mainPanel(
      #-- tabsetPanel
      tabsetPanel(type = "tabs",
                  #-- tabPanel
                  tabPanel("Explore",
                           ##---------------------------------------------------
                           conditionalPanel(condition = 'input.muttype=="Strand-Symmetric"',
                                            tags$head(tags$style(
                                              type="text/css",
                                              "#logo4 img {max-width: 100%; width: auto; max-height: 100%;
                                     padding-bottom: 10px;}"
                                            )),
                                            imageOutput("logo4", height="200px")),
                           
                           conditionalPanel(condition = 'input.muttype=="Non Symmetric"',
                                            tags$head(tags$style(
                                              type="text/css",
                                              "#logo5 img {max-width: 100%; width: auto; max-height: 100%;
                                     padding-bottom: 10px;}"
                                            )),
                                            imageOutput("logo5", height="200px")),
                           
                           conditionalPanel(condition = 'input.muttype=="Hypercube"',
                                            tags$head(tags$style(
                                              type="text/css",
                                              "#logo7 img {max-width: 100%; width: auto; max-height: 100%;
                                     padding-bottom: 10px;}"
                                            )),
                                            imageOutput("logo7", height="220px")),
                           
                           conditionalPanel(condition = 'input.muttype=="Hypercube ZNE"',
                                            tags$head(tags$style(
                                              type="text/css",
                                              "#logo8 img {max-width: 100%; width: auto; max-height: 100%;
                                     padding-bottom: 10px;}"
                                            )),
                                            imageOutput("logo8", height="200px")),
                           ##---------------------------------------------------
                           ##**************************
                           conditionalPanel(condition = 'input.muttype=="Strand-Symmetric" | input.muttype=="Non Symmetric"',
                                            ##---------------------------------------------------
                                            h4("Initial (t=0) Genome Composition"),
                                            fluidRow(
                                              column(width=3,
                                                     sliderInput('Gcont', 'G content, wt',
                                                                 min=0, max=10,
                                                                 value=5,
                                                                 step=0.1, round=TRUE)
                                              ),
                                              column(width=3,
                                                     sliderInput('Ccont', 'C content, wt',
                                                                 min=0, max=10,
                                                                 value=5,
                                                                 step=0.1, round=TRUE)
                                              ),
                                              column(width=3,
                                                     sliderInput('Acont', 'A content, wt',
                                                                 min=0, max=10,
                                                                 value=5,
                                                                 step=0.1, round=TRUE)
                                              ),
                                              column(width=3,
                                                     sliderInput('Tcont', 'T content, wt',
                                                                 min=0, max=10,
                                                                 value=5,
                                                                 step=0.1, round=TRUE)
                                              )
                                            )
                                            ##---------------------------------------------------
                           ),
                           ##**************************
                           conditionalPanel(condition = 'input.muttype=="Hypercube" | input.muttype=="Hypercube ZNE"',
                                            ##---------------------------------------------------
                                            h4("Initial (t=0) Genome Composition"),
                                            fluidRow(
                                              column(width=3,
                                                     sliderInput('GAcont', 'GA content, wt',
                                                                 min=0, max=10,
                                                                 value=5,
                                                                 step=0.1, round=TRUE)
                                              ),
                                              column(width=3,
                                                     sliderInput('CAcont', 'CA content, wt',
                                                                 min=0, max=10,
                                                                 value=5,
                                                                 step=0.1, round=TRUE)
                                              ),
                                              column(width=3,
                                                     sliderInput('AAcont', 'AA content, wt',
                                                                 min=0, max=10,
                                                                 value=5,
                                                                 step=0.1, round=TRUE)
                                              ),
                                              column(width=3,
                                                     sliderInput('TAcont', 'TA content, wt',
                                                                 min=0, max=10,
                                                                 value=5,
                                                                 step=0.1, round=TRUE)
                                              )
                                            ),
                                            
                                            fluidRow(
                                              column(width=3,
                                                     sliderInput('GTcont', 'GT content, wt',
                                                                 min=0, max=10,
                                                                 value=5,
                                                                 step=0.1, round=TRUE)
                                              ),
                                              column(width=3,
                                                     sliderInput('CTcont', 'CT content, wt',
                                                                 min=0, max=10,
                                                                 value=5,
                                                                 step=0.1, round=TRUE)
                                              ),
                                              column(width=3,
                                                     sliderInput('ATcont', 'AT content, wt',
                                                                 min=0, max=10,
                                                                 value=5,
                                                                 step=0.1, round=TRUE)
                                              ),
                                              column(width=3,
                                                     sliderInput('TTcont', 'TT content, wt',
                                                                 min=0, max=10,
                                                                 value=5,
                                                                 step=0.1, round=TRUE)
                                              )
                                            ),
                                            
                                            fluidRow(
                                              column(width=3,
                                                     sliderInput('GGcont', 'GG content, wt',
                                                                 min=0, max=10,
                                                                 value=5,
                                                                 step=0.1, round=TRUE)
                                              ),
                                              column(width=3,
                                                     sliderInput('CGcont', 'CG content, wt',
                                                                 min=0, max=10,
                                                                 value=5,
                                                                 step=0.1, round=TRUE)
                                              ),
                                              column(width=3,
                                                     sliderInput('AGcont', 'AG content, wt',
                                                                 min=0, max=10,
                                                                 value=5,
                                                                 step=0.1, round=TRUE)
                                              ),
                                              column(width=3,
                                                     sliderInput('TGcont', 'TG content, wt',
                                                                 min=0, max=10,
                                                                 value=5,
                                                                 step=0.1, round=TRUE)
                                              )
                                            ),
                                            
                                            fluidRow(
                                              column(width=3,
                                                     sliderInput('GCcont', 'GC content, wt',
                                                                 min=0, max=10,
                                                                 value=5,
                                                                 step=0.1, round=TRUE)
                                              ),
                                              column(width=3,
                                                     sliderInput('CCcont', 'CC content, wt',
                                                                 min=0, max=10,
                                                                 value=5,
                                                                 step=0.1, round=TRUE)
                                              ),
                                              column(width=3,
                                                     sliderInput('ACcont', 'AC content, wt',
                                                                 min=0, max=10,
                                                                 value=5,
                                                                 step=0.1, round=TRUE)
                                              ),
                                              column(width=3,
                                                     sliderInput('TCcont', 'TC content, wt',
                                                                 min=0, max=10,
                                                                 value=5,
                                                                 step=0.1, round=TRUE)
                                              )
                                            )
                                            ##---------------------------------------------------
                           ),
                           ##**************************
                           plotOutput("EvoPlot")
                  )
                  #-- tabPanel
      )
      #-- tabsetPanel
    )
    #-- mainPanel
    
  )
)
