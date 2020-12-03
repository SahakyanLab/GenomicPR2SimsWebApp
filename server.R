library(shiny)
source("lib.R")
library("deSolve")

step          = 0.001 # byr
span          = 4.0   # byr
#EQtolerance   = 8000  # nucleotide difference
#CHtolerance   = 0.01  # +- from the ratio (is 0.0015 for human genome)
#  #    1 byr = 10^9 yr
################################################################################


shinyServer(function(input, output) {

  #Creating the reactive expression that filters GDB relying on the reactive GDB loading expression:
  fltrGDB <- reactive({
    withProgress(message="Loading the Genesis database...", value=5,
                 {
                  getGDB()
                  setProgress(value=25, message="Filtering the Genesis database...")
                  fGDB <<- filter.GDB(GDB=GDB,
                                      status.fused=input$fused,
                                      status.cleaved=input$cleaved,
                                      status.same=input$same)
                  setProgress(value=75, message="Done!")
                 })
    DB   <<- NULL
  })
  output$logo <- renderImage({
    list(src = "pic/logo.png",contentType = 'image/png',width = 250,height = 220,alt = "No logo is found...")
  }, deleteFile = FALSE)

  output$logo2 <- renderImage({
    list(src = "pic/logo2.png",contentType = 'image/png',width = 300,height = 200,alt = "No logo is found...")
  }, deleteFile = FALSE)

  output$logo3 <- renderImage({
    list(src = "pic/logo3.png",contentType = 'image/png',width = 300,height = 200,alt = "No logo is found...")
  }, deleteFile = FALSE)

  output$logo4 <- renderImage({
    list(src = "pic/logo4.png",contentType = 'image/png',width = 470,height = 200,alt = "No logo is found...")
  }, deleteFile = FALSE)

  output$logo5 <- renderImage({
    list(src = "pic/logo5.png",contentType = 'image/png',width = 450,height = 200,alt = "No logo is found...")
  }, deleteFile = FALSE)

  output$logo6 <- renderImage({
    list(src = "pic/logo6.png",contentType = 'image/png',width = 230,height = 200,alt = "No logo is found...")
  }, deleteFile = FALSE)

  output$logo7 <- renderImage({
    list(src = "pic/logo7.png",contentType = 'image/png',width = 900,height = 200,alt = "No logo is found...")
  }, deleteFile = FALSE)

  output$logo8 <- renderImage({
    list(src = "pic/logo8.png",contentType = 'image/png',width = 430,height = 200,alt = "No logo is found...")
  }, deleteFile = FALSE)



  ###############################
  output$EvoPlot <- renderPlot({


    # Constructing the parameters object:
    if(input$muttype=="Strand-Symmetric" | input$muttype=="Non Symmetric"){
      state  <- c(Ca=input$Acont, Cg=input$Gcont, Ct=input$Tcont, Cc=input$Ccont)
    }
    if(input$muttype=="Hypercube ZNE" | input$muttype=="Hypercube"){
      state  <- c(Caa=input$AAcont, Cac=input$ACcont, Cag=input$AGcont, Cat=input$ATcont,
                  Cca=input$CAcont, Ccc=input$CCcont, Ccg=input$CGcont, Cct=input$CTcont,
                  Cga=input$GAcont, Cgc=input$GCcont, Cgg=input$GGcont, Cgt=input$GTcont,
                  Cta=input$TAcont, Ctc=input$TCcont, Ctg=input$TGcont, Ctt=input$TTcont)
    }
    state <- 100*state/sum(state)
    if(abs(sum(state)-100)>0.0001){
      stop("The sum of the base contents should be 100%! Please, adjust the sliders correspondingly.")
    }


    # Constructing the state object:
    if(input$muttype=="Strand-Symmetric"){
      parameters <- c(kag=input$k.ag.tc, kat=input$k.at.ta, kac=input$k.ac.tg,
                      kga=input$k.ct.ga, kgt=input$k.ca.gt, kgc=input$k.cg.gc,
                      kta=input$k.at.ta, ktg=input$k.ac.tg, ktc=input$k.ag.tc,
                      kca=input$k.ca.gt, kcg=input$k.cg.gc, kct=input$k.ct.ga)  #mut/byr
    }
    if(input$muttype=="Non Symmetric"){
      parameters <- c(kag=input$k.ag, kat=input$k.at, kac=input$k.ac,
                      kga=input$k.ga, kgt=input$k.gt, kgc=input$k.gc,
                      kta=input$k.ta, ktg=input$k.tg, ktc=input$k.tc,
                      kca=input$k.ca, kcg=input$k.cg, kct=input$k.ct)  #mut/byr
    }
    if(input$muttype=="Hypercube ZNE"){
      parameters <- c(i=input$k.ca.gt, j=input$k.ac.tg, k=input$k.cg.gc,
                      l=input$k.at.ta, m=input$k.ct.ga, n=input$k.ag.tc)  #mut/byr
    }
    if(input$muttype=="Hypercube"){
      #rates <- c("kAA2AC", "kAA2AG", "kAA2AT", "kAA2CA", "kAA2GA", "kAA2TA", "kAC2AA", "kAC2AG",
      #           "kAC2AT", "kAC2CC", "kAC2GC", "kAC2TC", "kAG2AA", "kAG2AC", "kAG2AT", "kAG2CG",
      #           "kAG2GG", "kAG2TG", "kAT2AA", "kAT2AC", "kAT2AG", "kCA2AA", "kCA2CC", "kCA2CG",
      #           "kCA2CT", "kCA2GA", "kCA2TA", "kCC2AC", "kCC2CA", "kCC2CG", "kCC2CT", "kCC2GC",
      #           "kCC2TC", "kCG2AG", "kCG2CA", "kCG2CC", "kGA2AA", "kGA2CA", "kGA2GC", "kGA2GG",
      #           "kGA2GT", "kGA2TA", "kGC2AC", "kGC2CC", "kGC2GA", "kTA2AA", "kTA2CA", "kTA2GA")
      parameters <- c(kAA2AC=input$kAA2AC, kAA2AG=input$kAA2AG, kAA2AT=input$kAA2AT,
                      kAA2CA=input$kAA2CA, kAA2GA=input$kAA2GA, kAA2TA=input$kAA2TA,
                      kAC2AA=input$kAC2AA, kAC2AG=input$kAC2AG, kAC2AT=input$kAC2AT,
                      kAC2CC=input$kAC2CC, kAC2GC=input$kAC2GC, kAC2TC=input$kAC2TC,
                      kAG2AA=input$kAG2AA, kAG2AC=input$kAG2AC, kAG2AT=input$kAG2AT,
                      kAG2CG=input$kAG2CG, kAG2GG=input$kAG2GG, kAG2TG=input$kAG2TG,
                      kAT2AA=input$kAT2AA, kAT2AC=input$kAT2AC, kAT2AG=input$kAT2AG,
                      kCA2AA=input$kCA2AA, kCA2CC=input$kCA2CC, kCA2CG=input$kCA2CG,
                      kCA2CT=input$kCA2CT, kCA2GA=input$kCA2GA, kCA2TA=input$kCA2TA,
                      kCC2AC=input$kCC2AC, kCC2CA=input$kCC2CA, kCC2CG=input$kCC2CG,
                      kCC2CT=input$kCC2CT, kCC2GC=input$kCC2GC, kCC2TC=input$kCC2TC,
                      kCG2AG=input$kCG2AG, kCG2CA=input$kCG2CA, kCG2CC=input$kCG2CC,
                      kGA2AA=input$kGA2AA, kGA2CA=input$kGA2CA, kGA2GC=input$kGA2GC,
                      kGA2GG=input$kGA2GG, kGA2GT=input$kGA2GT, kGA2TA=input$kGA2TA,
                      kGC2AC=input$kGC2AC, kGC2CC=input$kGC2CC, kGC2GA=input$kGC2GA,
                      kTA2AA=input$kTA2AA, kTA2CA=input$kTA2CA, kTA2GA=input$kTA2GA)  #mut/byr
    }


    if(input$muttype == "Strand-Symmetric" | input$muttype == "Non Symmetric"){
      atgc <- SolveATGC(parameters = parameters, state = state, step = step, span = span,
                        EQtolerance = 0, CHtolerance = 0, k=1)
    }
    if(input$muttype == "Hypercube ZNE"){
      atgc <- SolveATGC(parameters = parameters, state = state, step = step, span = span,
                        EQtolerance = 0, CHtolerance = 0, k=2)
    }
    if(input$muttype == "Hypercube"){
      atgc <- SolveATGC(parameters = parameters, state = state, step = step, span = span,
                        EQtolerance = 0, CHtolerance = 0, k="2full")
    }


    if(input$muttype == "Strand-Symmetric" | input$muttype == "Non Symmetric"){
      PlotATGC(atgc=atgc, time.unit="byr", xlim=c(0,4), ylim=c(0,60))
    }
    if(input$muttype == "Hypercube ZNE" | input$muttype == "Hypercube"){
      PlotATGC(atgc=atgc, time.unit="byr", xlim=c(0,4), ylim=c(0,15))
    }


  })
  ###############################

})

