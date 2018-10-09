################################################################################
SolveATGC <- function(parameters=parameters, state=state, step=step, span=span,
                      EQtolerance=EQtolerance, CHtolerance=CHtolerance, k=1){

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
  length.out <- length(out[,1])

  length.genome <-  sum(state)

  #********
  if(k==1){
  dif.nucl <- c( sum( out[1,c("Ca","Cg","Ct","Cc")] ),
                sapply(2:length.out, function(i){
                  sum(abs(out[i,c("Ca","Cg","Ct","Cc")] - out[(i-1),c("Ca","Cg","Ct","Cc")]))
                  }, USE.NAMES=FALSE, simplify=TRUE))

  #dinuc.names <- c("CAA", "CAC", "CAG", "CAT", "CCA", "CCC", "CCG", "CCT", "CGA", "CGC", "CGG", "CGT", "CTA", "CTC", "CTG", "CTT")
  #dif.nucl <- c( sum( out[1, dinuc.names] ),
  #               sapply(2:length.out, function(i){
  #                 sum(abs(out[i, dinuc.names] - out[(i-1), dinuc.names]))
  #               }, USE.NAMES=FALSE, simplify=TRUE))

  at.ratio <- sapply(1:length.out, function(i){
                  out[i,"Ca"]/out[i,"Ct"]
                  }, USE.NAMES=FALSE, simplify=TRUE)

  gc.ratio <- sapply(1:length.out, function(i){
                  out[i,"Cg"]/out[i,"Cc"]
                  }, USE.NAMES=FALSE, simplify=TRUE)

  gc.content <- sapply(1:length.out, function(i){
                  (out[i,"Cg"] + out[i,"Cc"])*100/length.genome
                  }, USE.NAMES=FALSE, simplify=TRUE)

  Eq.time <- times[suppressWarnings(min(which((dif.nucl<=EQtolerance)==TRUE)))]
  Ch.time <- times[suppressWarnings(
      min(which((abs(at.ratio-1)<=CHtolerance)&(abs(gc.ratio-1)<=CHtolerance)))
                 )]
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
  RESULTS$inp$EQtolerance  <- EQtolerance
  RESULTS$inp$CHtolerance  <- CHtolerance
  RESULTS$out              <- out                 # numerical solution to the diff eqs

  #********
  if(k==1){
  RESULTS$dif.nucl         <- dif.nucl
  RESULTS$at.ratio         <- as.vector(at.ratio) # at.ratio dynamics vector
  RESULTS$gc.ratio         <- as.vector(gc.ratio)
  RESULTS$gc.content       <- as.vector(gc.content)
  RESULTS$Eq.time          <- Eq.time  # time to reach the equilibration tolerance limit
  RESULTS$Ch.time          <- Ch.time  # time to reach Chargaff's tolerance limit
  RESULTS$Fin.GC           <- Fin.GC   # the G+C content at the end of the simulation
  }
  #********
  RESULTS$length.genome    <- length.genome
  RESULTS$length.out       <- length.out

  return(RESULTS)

}
################################################################################


################################################################################
PlotATGC <- function(atgc=atgc, time.unit="byr", xlim=c(0,4), ylim=c(0,60)){

  #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  if(as.character(atgc$inp$k)=="1"){

  plot(x=atgc$out[,"time"],
       y=100*atgc$out[, "Cg"]/atgc$length.genome,
       ylab="base content, %", xlab=paste("time, ",time.unit,sep=""),
       pch = 1, cex=0.01, type="l", lwd=3, col="orange",
       xlim=xlim, ylim=ylim)

  lines(x=atgc$out[,"time"],
        y=100*atgc$out[, "Ca"]/atgc$length.genome,
        pch = 1, cex=0.01, type="l", lwd=3, col="forestgreen")

  lines(x=atgc$out[,"time"],
        y=100*atgc$out[, "Cc"]/atgc$length.genome,
        pch = 1, cex=0.01, type="l", lwd=3, col="blue")

  lines(x=atgc$out[,"time"],
        y=100*atgc$out[, "Ct"]/atgc$length.genome,
        pch = 1, cex=0.01, type="l", lwd=3, col="red")

  text(x=min(xlim)+diff(xlim)/15, y=max(ylim), paste("t = 0 ",time.unit,"\n",
                                      "%G = ",round(atgc$inp$state["Cg"],1),"\n",
                                      "%C = ",round(atgc$inp$state["Cc"],1),"\n",
                                      "%A = ",round(atgc$inp$state["Ca"],1),"\n",
                                      "%T = ",round(atgc$inp$state["Ct"],1),"\n",
                                      "G+C = ",round(100*(atgc$inp$state["Cg"]+atgc$inp$state["Cc"])/
                                                    atgc$length.genome,2),"%","\n",
                                      "G/C = ",format(round(atgc$inp$state["Cg"]/atgc$inp$state["Cc"],3),nsmall=3),"\n",
                                      "A/T = ",format(round(atgc$inp$state["Ca"]/atgc$inp$state["Ct"],3),nsmall=3), sep=""),
                                      pos = 1, cex=0.6)

  text(x=max(xlim)-diff(xlim)/15, y=max(ylim), paste("t = ",round(atgc$inp$span,2)," ",time.unit,"\n",
                                      "%G = ",round(as.vector(atgc$out[atgc$length.out,"Cg"]),1),"\n",
                                      "%C = ",round(as.vector(atgc$out[atgc$length.out,"Cc"]),1),"\n",
                                      "%A = ",round(as.vector(atgc$out[atgc$length.out,"Ca"]),1),"\n",
                                      "%T = ",round(as.vector(atgc$out[atgc$length.out,"Ct"]),1),"\n",
                                      "G+C = ",round(atgc$gc.content[atgc$length.out],2),"%","\n",
                                      "G/C = ",format(round(atgc$out[atgc$length.out,"Cg"]/atgc$out[atgc$length.out,"Cc"],3),nsmall=3),"\n",
                                      "A/T = ",format(round(atgc$out[atgc$length.out,"Ca"]/atgc$out[atgc$length.out,"Ct"],3),nsmall=3), sep=""),
                                      pos = 1, cex=0.6)
  #if(!is.na(atgc$Ch.time)){
  #  abline(v=atgc$Ch.time, lty="dashed")
  #  text(y=min(ylim),  x=atgc$Ch.time, pos=4, "Chargaff eq. reached", las=1, cex=0.6, srt=90)
  #}

  #if(!is.na(atgc$Eq.time)){
  #  abline(v=atgc$Eq.time)
  #  text(y=min(ylim),  x=atgc$Eq.time, pos=4, "Genome eq. reached", las=1, cex=0.6, srt=90)
  #}

  }
  #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

  #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  if(as.character(atgc$inp$k)=="2" | as.character(atgc$inp$k)=="2full"){

    dinuc.names <- c("Caa", "Cac", "Cag", "Cat",
                     "Cca", "Ccc", "Ccg", "Cct",
                     "Cga", "Cgc", "Cgg", "Cgt",
                     "Cta", "Ctc", "Ctg", "Ctt")

    plot(x=atgc$out[,"time"],
         y=100*atgc$out[, dinuc.names[1] ]/atgc$length.genome,
         ylab="di-nucleotide content, %", xlab=paste("time, ",time.unit,sep=""),
         pch = 1, cex=0.01, type="l", lwd=3, col=1,
         xlim=xlim, ylim=ylim)

    for(dm in 2:16){
      #rgb(runif(5),runif(5),runif(5))
        lines(x=atgc$out[,"time"],
              y=100*atgc$out[, dinuc.names[dm]]/atgc$length.genome,
              pch = 1, cex=0.01, type="l", lwd=3, col=dm)
    }


  }
  #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


}
################################################################################


