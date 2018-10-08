library(shiny)

shinyUI(fluidPage(

  # Application title:
  titlePanel("ATGC Dynamics Solver"),

  # Sidebar with a number of filtering input:
  sidebarLayout(
    sidebarPanel(
      # imageOutput("logo", height="240px"),

      h4("Mutation Rate Constants"),
      selectInput(inputId = "muttype",
                  label   = "Select the model",
                  choices = c("Non Symmetric", "Strand-Symmetric", "Hypercube", "Hypercube ZNE")),

      #-----------------------------------------------------------------------------------------------------
      conditionalPanel(condition = 'input.muttype=="Strand-Symmetric" | input.muttype=="Non Symmetric"',
                       imageOutput("logo", height="240px")),

      conditionalPanel(condition = 'input.muttype=="Hypercube" | input.muttype=="Hypercube ZNE"',
                       imageOutput("logo6", height="240px")),
      #-----------------------------------------------------------------------------------------------------

      #-----------------------------------------------------------------------------------------------------
      conditionalPanel(condition = 'input.muttype=="Strand-Symmetric" | input.muttype=="Hypercube ZNE"',
                       imageOutput("logo2", height="80px"),
                       #-- sliders for Strand-Symmetric
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
                       #-- end of sliders for Strand-Symmetric
                       ),
      #-----------------------------------------------------------------------------------------------------
      #-----------------------------------------------------------------------------------------------------
      conditionalPanel(condition = 'input.muttype=="Non Symmetric"',

                       imageOutput("logo3", height="80px"),
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

                       #imageOutput("logo3", height="110px"),
                       #-- sliders for Non Symmetric
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
                       #-- end of sliders for Non Symmetric

                      # L <- NULL
                      # for(i in rates){
                      #   L <- c(L, paste("sliderInput('",i,"', '",i,"',",sep=""))
                      #   L <- c(L, paste("            min=0, max=1.5,",sep=""))
                      #   L <- c(L, paste("            value=runif(1, min=0, max=1.5),",sep=""))
                      #   L <- c(L, paste("            step=0.001, round=FALSE),",sep=""))
                      #   L <- c(L, paste("   ",sep=""))
                      # }
      )
      #-----------------------------------------------------------------------------------------------------

      #h4("Equilibration Guides"),
      #sliderInput('EQtolerance', 'Equilibration criterion, nt',
      #            min=0, max=8000,
      #            value=5,
      #            step=5, round=TRUE),
      #
      #sliderInput('CHtolerance', 'Chargaff compliance deviation',
      #            min=0, max=0.1,
      #            value=0.01,
      #            step=0.005, round=FALSE)

    #  h6("Include molecules that:"),
    #  checkboxInput(inputId="fused",   label="formed through a 'fusion',",   value=TRUE),
    #  checkboxInput(inputId="cleaved", label="formed through a 'cleavage',", value=TRUE),
    #  checkboxInput(inputId="same",    label="stayed the same.", value=TRUE)

    ),

    # Show a plot of the generated distribution
    #-- mainPanel
    mainPanel(
      #-- tabsetPanel
      tabsetPanel(type = "tabs",
        #-- tabPanel
        tabPanel("Explore",

                  #h3( verbatimTextOutput("text") ),
                  ##---------------------------------------------------
                  conditionalPanel(condition = 'input.muttype=="Strand-Symmetric"',
                                   imageOutput("logo4", height="140px")),

                  conditionalPanel(condition = 'input.muttype=="Non Symmetric"',
                                   imageOutput("logo5", height="140px")),

                  conditionalPanel(condition = 'input.muttype=="Hypercube"',
                                   imageOutput("logo7", height="140px")),

                  conditionalPanel(condition = 'input.muttype=="Hypercube ZNE"',
                                   imageOutput("logo8", height="180px")),
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


                  #########
                  #sidebarPanel()
        )
        #-- tabPanel
      )
      #-- tabsetPanel
    )
    #-- mainPanel

  )
))

