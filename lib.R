################################################################################
SolveATGC <- function(parameters=parameters, state=state, step=step, span=span,
                      EQtolerance=EQtolerance, CHtolerance, k=1){
  
  times <- seq(0, span, by=step) # byr
  
  ##########################################
  if(k==1){
    mut.mdl <- function(t, state, parameters){
      
      with(as.list(c(state, parameters)),{
        # rate of change
        dCa <- kca*Cc+kta*Ct+kga*Cg-(kac+kat+kag)*Ca
        dCg <- kag*Ca+kcg*Cc+ktg*Ct-(kga+kgt+kgc)*Cg
        dCt <- kat*Ca+kgt*Cg+kct*Cc-(kta+ktc+ktg)*Ct
        dCc <- kac*Ca+ktc*Ct+kgc*Cg-(kca+kct+kcg)*Cc
        
        # return the rate of change
        list(c(dCa, dCg, dCt, dCc))
      }) # end with(as.list ...
      
    }
  }
  
  ##########################################
  
  ##########################################
  if(k==2){
    mut.mdl <- function(t, state, parameters){
      
      with(as.list(c(state, parameters)),{
        # rate of change
        dCaa <- (Cac+Cca)*i+(Cat+Cta)*l+(Cag+Cga)*m-2*Caa*(j+l+n)
        dCac <- Ccc*i+Caa*j+Cag*k+Ctc*l+Cgc*m+Cat*n-Cac*(i+j+k+l+m+n)
        dCag <- Ccg*i+Cat*j+Cac*k+Ctg*l+Cgg*m+Caa*n-Cag*(i+j+k+l+m+n)
        dCat <- (Cag+Cct)*i+(Caa+Ctt)*l+(Cac+Cgt)*m-2*Cat*(j+l+n)
        dCca <- Ccc*i+Caa*j+Cga*k+Cct*l+Ccg*m+Cta*n-Cca*(i+j+k+l+m+n)
        dCcc <- (Cac+Cca)*j+(Ccg+Cgc)*k+(Cct+Ctc)*n-2*Ccc*(i+k+m)
        dCcg <- (Cag+Cct)*j+(Ccc+Cgg)*k+(Cca+Ctg)*n-2*Ccg*(i+k+m)
        dCct <- Ccg*i+Cat*j+Cgt*k+Cca*l+Ccc*m+Ctt*n-Cct*(i+j+k+l+m+n)
        dCga <- Cgc*i+Cta*j+Cca*k+Cgt*l+Cgg*m+Caa*n-Cga*(i+j+k+l+m+n)
        dCgc <- (Cga+Ctc)*j+(Ccc+Cgg)*k+(Cac+Cgt)*n-2*Cgc*(i+k+m)
        dCgg <- (Cgt+Ctg)*j+(Ccg+Cgc)*k+(Cag+Cga)*n-2*Cgg*(i+k+m)
        dCgt <- Cgg*i+Ctt*j+Cct*k+Cga*l+Cgc*m+Cat*n-Cgt*(i+j+k+l+m+n)
        dCta <- (Cga+Ctc)*i+(Caa+Ctt)*l+(Cca+Ctg)*m-2*Cta*(j+l+n)
        dCtc <- Cgc*i+Cta*j+Ctg*k+Cac*l+Ccc*m+Ctt*n-Ctc*(i+j+k+l+m+n)
        dCtg <- Cgg*i+Ctt*j+Ctc*k+Cag*l+Ccg*m+Cta*n-Ctg*(i+j+k+l+m+n)
        dCtt <- (Cgt+Ctg)*i+(Cat+Cta)*l+(Cct+Ctc)*m-2*Ctt*(j+l+n)
        
        # return the rate of change
        list(c(dCaa, dCac, dCag, dCat,
               dCca, dCcc, dCcg, dCct,
               dCga, dCgc, dCgg, dCgt,
               dCta, dCtc, dCtg, dCtt))
      }) # end with(as.list ...
      
    }
  }
  ##########################################
  
  ##########################################
  if(k=="2full"){
    mut.mdl <- function(t, state, parameters){
      
      with(as.list(c(state, parameters)),{
        # rate of change
        dCaa <- kCA2AA*Cca - kAA2CA*Caa + kTA2AA*Cta - kAA2TA*Caa + kGA2AA*Cga -
          kAA2GA*Caa + kAC2AA*Cac - kAA2AC*Caa + kAT2AA*Cat - kAA2AT*Caa +
          kAG2AA*Cag - kAA2AG*Caa
        dCac <- kCC2AC*Ccc - kAC2CC*Cac + kGA2GT*Ctc - kAC2TC*Cac + kGC2AC*Cgc -
          kAC2GC*Cac + kAA2AC*Caa - kAC2AA*Cac + kAT2AC*Cat - kAC2AT*Cac +
          kAG2AC*Cag - kAC2AG*Cac
        dCag <- kCG2AG*Ccg - kAG2CG*Cag + kCA2CT*Ctg - kAG2TG*Cag + kCC2CT*Cgg -
          kAG2GG*Cag + kAA2AG*Caa - kAG2AA*Cag + kAC2AG*Cac - kAG2AC*Cag +
          kAT2AG*Cat - kAG2AT*Cag
        dCat <- kAG2AT*Cct - kAT2AG*Cat + kAA2AT*Ctt - kAT2AA*Cat + kAC2AT*Cgt -
          kAT2AC*Cat + kAA2AT*Caa - kAT2AA*Cat + kAC2AT*Cac - kAT2AC*Cat +
          kAG2AT*Cag - kAT2AG*Cat
        dCca <- kAA2CA*Caa - kCA2AA*Cca + kTA2CA*Cta - kCA2TA*Cca + kGA2CA*Cga -
          kCA2GA*Cca + kCC2CA*Ccc - kCA2CC*Cca + kAG2TG*Cct - kCA2CT*Cca +
          kCG2CA*Ccg - kCA2CG*Cca
        dCcc <- kAC2CC*Cac - kCC2AC*Ccc + kGA2GG*Ctc - kCC2TC*Ccc + kGC2CC*Cgc -
          kCC2GC*Ccc + kCA2CC*Cca - kCC2CA*Ccc + kAG2GG*Cct - kCC2CT*Ccc +
          kCG2CC*Ccg - kCC2CG*Ccc
        dCcg <- kAG2CG*Cag - kCG2AG*Ccg + kCA2CG*Ctg - kCG2CA*Ccg + kCC2CG*Cgg -
          kCG2CC*Ccg + kCA2CG*Cca - kCG2CA*Ccg + kCC2CG*Ccc - kCG2CC*Ccg +
          kAG2CG*Cct - kCG2AG*Ccg
        dCct <- kAT2AG*Cat - kAG2AT*Cct + kAA2AG*Ctt - kAG2AA*Cct + kAC2AG*Cgt -
          kAG2AC*Cct + kCA2CT*Cca - kAG2TG*Cct + kCC2CT*Ccc - kAG2GG*Cct +
          kCG2AG*Ccg - kAG2CG*Cct
        dCga <- kAA2GA*Caa - kGA2AA*Cga + kCA2GA*Cca - kGA2CA*Cga + kTA2GA*Cta -
          kGA2TA*Cga + kGC2GA*Cgc - kGA2GC*Cga + kAC2TC*Cgt - kGA2GT*Cga +
          kCC2TC*Cgg - kGA2GG*Cga
        dCgc <- kAC2GC*Cac - kGC2AC*Cgc + kCC2GC*Ccc - kGC2CC*Cgc + kGA2GC*Ctc -
          kGC2GA*Cgc + kGA2GC*Cga - kGC2GA*Cgc + kAC2GC*Cgt - kGC2AC*Cgc +
          kCC2GC*Cgg - kGC2CC*Cgc
        dCgg <- kAG2GG*Cag - kCC2CT*Cgg + kCG2CC*Ccg - kCC2CG*Cgg + kCA2CC*Ctg -
          kCC2CA*Cgg + kGA2GG*Cga - kCC2TC*Cgg + kGC2CC*Cgc - kCC2GC*Cgg +
          kAC2CC*Cgt - kCC2AC*Cgg
        dCgt <- kAT2AC*Cat - kAC2AT*Cgt + kAG2AC*Cct - kAC2AG*Cgt + kAA2AC*Ctt -
          kAC2AA*Cgt + kGA2GT*Cga - kAC2TC*Cgt + kGC2AC*Cgc - kAC2GC*Cgt +
          kCC2AC*Cgg - kAC2CC*Cgt
        dCta <- kAA2TA*Caa - kTA2AA*Cta + kCA2TA*Cca - kTA2CA*Cta + kGA2TA*Cga -
          kTA2GA*Cta + kGA2TA*Ctc - kTA2GA*Cta + kAA2TA*Ctt - kTA2AA*Cta +
          kCA2TA*Ctg - kTA2CA*Cta
        dCtc <- kAC2TC*Cac - kGA2GT*Ctc + kCC2TC*Ccc - kGA2GG*Ctc + kGC2GA*Cgc -
          kGA2GC*Ctc + kTA2GA*Cta - kGA2TA*Ctc + kAA2GA*Ctt - kGA2AA*Ctc +
          kCA2GA*Ctg - kGA2CA*Ctc
        dCtg <- kAG2TG*Cag - kCA2CT*Ctg + kCG2CA*Ccg - kCA2CG*Ctg + kCC2CA*Cgg -
          kCA2CC*Ctg + kTA2CA*Cta - kCA2TA*Ctg + kGA2CA*Ctc - kCA2GA*Ctg +
          kAA2CA*Ctt - kCA2AA*Ctg
        dCtt <- kAT2AA*Cat - kAA2AT*Ctt + kAG2AA*Cct - kAA2AG*Ctt + kAC2AA*Cgt -
          kAA2AC*Ctt + kTA2AA*Cta - kAA2TA*Ctt + kGA2AA*Ctc - kAA2GA*Ctt +
          kCA2AA*Ctg - kAA2CA*Ctt
        
        # return the rate of change
        list(c(dCaa, dCac, dCag, dCat,
               dCca, dCcc, dCcg, dCct,
               dCga, dCgc, dCgg, dCgt,
               dCta, dCtc, dCtg, dCtt))
      }) # end with(as.list ...
      
    }
  }
  ##########################################
  
  library(deSolve)
  out <- ode(y=state, times=times, func=mut.mdl, parms=parameters)
  length.out <- dim(out)[1]
  length.genome <- sum(state)
  
  #********
  if(k==1){
    dif.nucl <- c(sum(out[1,c("Ca","Cg","Ct","Cc")]),
                  rowSums(abs(diff(out[,c("Ca","Cg","Ct","Cc")])))/4)

    at.ratio <- out[,"Ca"]/out[,"Ct"]
    gc.ratio <- out[,"Cg"]/out[,"Cc"]
    at.content <- (out[,"Ca"] + out[,"Ct"])*100/length.genome
    gc.content <- (out[,"Cg"] + out[,"Cc"])*100/length.genome
    at.skew <- ((out[,"Ca"] - out[,"Ct"])/(out[,"Ca"] + out[,"Ct"]))
    gc.skew    <- ((out[,"Cg"] - out[,"Cc"])/(out[,"Cg"] + out[,"Cc"]))
    
    # Genome Equilibrium tolerance
    dif <- abs(diff(out[,c("Ca","Cg","Ct","Cc")]/100))
    dif.max.min <- apply(dif, 1, function(x){max(x) - min(x)})
    Eq.time <- times[which(dif.max.min <= EQtolerance)[1]]
    
    # Chargaff compliance
    at.tol <- CHtolerance[CHtolerance$metadata == "AT_skew", "st.dev"]
    gc.tol <- CHtolerance[CHtolerance$metadata == "GC_skew", "st.dev"]
    Ch.compliance <- which(abs(gc.skew-0) <= gc.tol & abs(at.skew-0) <= at.tol)
    
    # if differences between compliant cases is 1, take first index
    # otherwise, take first index after the biggest difference
    if(length(table(diff(Ch.compliance))) == 1){
      Ch.time <- times[Ch.compliance[which(diff(Ch.compliance) == 1)[1]]]
    } else {
      Ch.time <- times[Ch.compliance[tail(which(diff(Ch.compliance) != 1), n = 1)+1]]
    }
    
    if(length(Ch.time) == 0){
      Ch.time <- 0
    }
    
    Fin.GC  <- as.vector(gc.content[length.out])
  }
  #********
  RESULTS <- NULL
  RESULTS$inp              <- NULL # input data
  RESULTS$inp$k            <- k
  RESULTS$inp$parameters   <- parameters
  RESULTS$inp$state        <- state
  RESULTS$inp$step         <- step
  RESULTS$inp$span         <- span
  RESULTS$out              <- out                 # numerical solution to the diff eqs
  
  #********
  if(k==1){
    RESULTS$dif.nucl         <- dif.nucl
    RESULTS$at.ratio         <- as.vector(at.ratio) # at.ratio dynamics vector
    RESULTS$gc.ratio         <- as.vector(gc.ratio)
    RESULTS$gc.content       <- as.vector(gc.content)
    RESULTS$Eq.time          <- Eq.time           # time to reach the equilibration tolerance limit 
    RESULTS$Ch.time          <- Ch.time           # time to reach Chargaff's tolerance limit
    RESULTS$Fin.GC           <- Fin.GC   # the G+C content at the end of the simulation
  }
  #********
  RESULTS$length.genome    <- length.genome
  RESULTS$length.out       <- length.out
  
  return(RESULTS)
}

################################################################################
PlotATGC <- function(atgc=atgc, time.unit="byr", xlim=c(0,10), ylim=c(0,60)){
  
  #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  if(as.character(atgc$inp$k)=="1"){
    Plots <- as.data.frame(atgc$out) %>%
      as_tibble() %>%
      ggplot(aes(x = time,
                 y = Ca)) + 
      geom_line(aes(y = Ca), size = 1.2, col = "forestgreen") + 
      geom_line(aes(y = Cg), size = 1.2, col = "orange") + 
      geom_line(aes(y = Ct), size = 1.2, col = "red") + 
      geom_line(aes(y = Cc), size = 1.2, col = "blue") + 
      xlim(xlim) +
      ylim(ylim) +
      labs(x = paste0("Time, ",time.unit),
           y = "Base content, %") + 
      geom_text(data = data.frame(
        xpos = 0,
        ypos = max(ylim),
        annotateText = paste(
          "t = 0 ",time.unit,"\n",
          "nG (orange) = ",round(atgc$inp$state["Cg"], 2),"\n",
          "nC (blue) = ",round(atgc$inp$state["Cc"], 2),"\n",
          "nA (green) = ",round(atgc$inp$state["Ca"], 2),"\n",
          "nT (red) = ",round(atgc$inp$state["Ct"], 2),"\n",
          "G+C = ",round(100*(atgc$inp$state["Cg"]+atgc$inp$state["Cc"])/
                           atgc$length.genome,2),"%","\n",
          "G/C = ",format(round(atgc$inp$state["Cg"]/atgc$inp$state["Cc"],2),nsmall=2),"\n",   
          "A/T = ",format(round(atgc$inp$state["Ca"]/atgc$inp$state["Ct"],2),nsmall=2), sep=""),
        hjustvar = 0, vjustvar = 1), 
        aes(x = xpos, 
            y = ypos, 
            hjust = hjustvar, 
            vjust = vjustvar, 
            label = annotateText,
            angle = 0), size = 4) + 
      geom_text(data = data.frame(
        xpos = max(xlim)-2,
        ypos = max(ylim), 
        annotateText = paste(
          "t = ",round(atgc$inp$span,2)," ",time.unit,"\n",
          "nG = ",round(as.vector(atgc$out[atgc$length.out,"Cg"]),2),"\n",
          "nC = ",round(as.vector(atgc$out[atgc$length.out,"Cc"]),2),"\n",
          "nA = ",round(as.vector(atgc$out[atgc$length.out,"Ca"]),2),"\n",
          "nT = ",round(as.vector(atgc$out[atgc$length.out,"Ct"]),2),"\n",
          "G+C = ",round(atgc$gc.content[atgc$length.out],2),"%","\n",
          "G/C = ",format(round(atgc$out[atgc$length.out,"Cg"]/atgc$out[atgc$length.out,"Cc"],2),nsmall=2),"\n",   
          "A/T = ",format(round(atgc$out[atgc$length.out,"Ca"]/atgc$out[atgc$length.out,"Ct"],2),nsmall=2), sep=""),
        hjustvar = 0, vjustvar = 1), 
        aes(x = xpos, 
            y = ypos, 
            hjust = hjustvar, 
            vjust = vjustvar, 
            label = annotateText,
            angle = 0), size = 4) + 
      theme_bw()
    
    if(atgc$Ch.time != 0){
      Plots <- Plots +
        geom_vline(xintercept = atgc$Ch.time, linetype = "dashed") +
        geom_text(data = data.frame(xpos = atgc$Ch.time,
                                    ypos =  min(ylim),
                                    annotateText = "Chargaff eq. reached",
                                    hjustvar = 0, vjustvar = 1.1),
                  aes(x = xpos,
                      y = ypos,
                      hjust = hjustvar,
                      vjust = vjustvar,
                      label = annotateText,
                      angle = 90),
                  fontface = "bold", size = 5)
    }

    if(atgc$Eq.time != 0){
      Plots <- Plots +
        geom_vline(xintercept = atgc$Eq.time) +
        geom_text(data = data.frame(xpos = atgc$Eq.time,
                                    ypos =  min(ylim),
                                    annotateText = "Genome eq. reached",
                                    hjustvar = 0, vjustvar = 1.1),
                  aes(x = xpos,
                      y = ypos,
                      hjust = hjustvar,
                      vjust = vjustvar,
                      label = annotateText,
                      angle = 90),
                  fontface = "bold", size = 5)
    }
    print(Plots)
  }
  if(as.character(atgc$inp$k)=="2" | as.character(atgc$inp$k)=="2full"){
    dinuc.names <- c("Caa", "Cac", "Cag", "Cat",
                     "Cca", "Ccc", "Ccg", "Cct",
                     "Cga", "Cgc", "Cgg", "Cgt",
                     "Cta", "Ctc", "Ctg", "Ctt")
    
    Plots <- as.data.frame(atgc$out) %>%
      as_tibble() %>%
      ggplot(aes(x = time,
                 y = Caa)) + 
      geom_line(aes(y = Cac), size = 1.2, col = "forestgreen") + 
      geom_line(aes(y = Cag), size = 1.2, col = "blue") + 
      geom_line(aes(y = Cat), size = 1.2, col = "red") + 
      geom_line(aes(y = Cca), size = 1.2, col = "orange") + 
      geom_line(aes(y = Ccc), size = 1.2, col = "purple") + 
      geom_line(aes(y = Ccg), size = 1.2, col = "pink") + 
      geom_line(aes(y = Cct), size = 1.2, col = "magenta") + 
      geom_line(aes(y = Cga), size = 1.2, col = "brown") + 
      geom_line(aes(y = Cgc), size = 1.2, col = "navyblue") + 
      geom_line(aes(y = Cgg), size = 1.2, col = "darkred") + 
      geom_line(aes(y = Cgt), size = 1.2, col = "black") + 
      geom_line(aes(y = Cta), size = 1.2, col = "cyan") + 
      geom_line(aes(y = Ctc), size = 1.2, col = "turquoise") + 
      geom_line(aes(y = Ctg), size = 1.2, col = "gold") + 
      geom_line(aes(y = Ctt), size = 1.2, col = "#FF6767") + 
      xlim(xlim) +
      ylim(ylim) +
      labs(x = paste0("Time, ",time.unit),
           y = "Di-nucleotide content, %") + 
      theme_bw()
    print(Plots)
  }
  #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
}
################################################################################

