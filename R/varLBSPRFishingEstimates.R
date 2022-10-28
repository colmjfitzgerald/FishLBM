varFishingAtLength <- function(optPars, optVarcov, optFleetPars, StockPars, LenMids){
  # calculate variance of "fishing"-at-length (quantities such as selectivity, SPR, etc. at length)
  # LenMids from LBSPR calculations or SizeBins? 
  SLF <- sapply(LenMids,  selectivityLength, gearSelectivity = optFleetPars$selectivityCurve, 
                SL1 = optFleetPars$SL1,  SL2 = optFleetPars$SL2, 
                SLMin = optFleetPars$SLMin, SLmesh = 1)  # SLMin = NULL?
  
  # delta method
  pdSFpdx2 <- -SLF*(1-SLF)*log(19)*(exp(optPars[2])/exp(optPars[3])) 
  pdSFpdx3 <- -SLF*(1-SLF)*log(19)*(LenMids/StockPars$Linf - exp(optPars[2]))/exp(optPars[3])
  varSLF <- pdSFpdx2^2*optVarcov[2,2] + pdSFpdx3^2*optVarcov[3,3] + 2*pdSFpdx2*pdSFpdx3*optVarcov[2,3]
  
  return(list(meanSLF = SLF, varSLF = varSLF))
}

selectivityLength <- function(LenMids, gearSelectivity, SL1, SL2, SLMin = NULL, SLmesh = 1){
  selLen <- switch(gearSelectivity, 
                   Logistic = 1.0/(1+exp(-log(19)*(LenMids-SL1)/(SL2-SL1))),
                   logNorm = exp(-0.5*((log(LenMids)-log((SL1)*SLmesh))/(SL2))^2), 
                   Normal.loc = exp(-0.5*((LenMids-(SL1*SLmesh))/(SL2))^2),
                   Normal.sca = exp(-0.5*((LenMids-(SL1*SLmesh))/(SLmesh*SL2^0.5)^2)) # from Millar & Holst 1997
  )
  if(!is.null(SLMin)) selLen[LenMids < SLMin] <- 0
  return(selLen)
}

varSPR <- function(optMLEPars, optVarcov, optFleetPars, StockPars, SizeBins) {
  
  # calculate SPR confidence intervals with finite difference
  x_mle <- optMLEPars
  # transform ML estimators to fishing parameters
  x_scale <- c(1, rep(StockPars$Linf, length(x_mle)-1))
  fEst <- exp(x_mle)*x_scale
  if(length(fEst) == 3){
    fEst[3] <- fEst[2] + fEst[3]
  }
  
  # finite difference
  delta_x <- 0.0001
  x_plus_dx <- x_mle + delta_x
  x_minus_dx <- x_mle - delta_x
  
  fplusdf_Est <- exp(x_plus_dx)*x_scale
  fminusdf_Est <- exp(x_minus_dx)*x_scale
  if(length(fEst) == 3){
    fplusdf_Est[3] <- fplusdf_Est[2] + fplusdf_Est[3]
    fminusdf_Est[3] <- fminusdf_Est[2] + fminusdf_Est[3]
  }
  
  # log(F/M)
  if(optFleetPars$selectivityCurve == "logistic" || optFleetPars$selectivityCurve == "Logistic"){
    diffFleetPars <- optFleetPars[c("FM", "SL1", "SL2", "selectivityCurve")]
  } else if(optFleetPars$selectivityCurve == "exponentialLogistic") {
    diffFleetPars <- optFleetPars[c("FM", "SL1", "SL2", "SL3", "selectivityCurve")]
  } else {
    diffFleetPars <- optFleetPars[c("FM", "SL1", "SL2", "selectivityCurve", "SLMin", "SLmesh")]
  }
  diffFleetPars[1] <- fplusdf_Est[1]
  SPR_plus <- simLBSPRDome(StockPars, diffFleetPars, SizeBins)$SPR
  diffFleetPars[1] <- fminusdf_Est[1]
  SPR_minus <- simLBSPRDome(StockPars, diffFleetPars, SizeBins)$SPR
  dSPRdx1 <- (SPR_plus - SPR_minus)/(2*delta_x)
  if(optFleetPars$selectivityCurve == "logistic" || optFleetPars$selectivityCurve == "Logistic"){
    diffFleetPars <- optFleetPars[ c("FM", "SL1", "SL2", "selectivityCurve")]
    # SL50
    diffFleetPars[2] <- fplusdf_Est[2]
    SPR_plus <- simLBSPRDome(StockPars, diffFleetPars, SizeBins)$SPR
    diffFleetPars[2] <- fminusdf_Est[2]
    SPR_minus <- simLBSPRDome(StockPars, diffFleetPars, SizeBins)$SPR
    dSPRdx2 <- (SPR_plus - SPR_minus)/(2*delta_x)
    
    diffFleetPars <- optFleetPars[ c("FM", "SL1", "SL2", "selectivityCurve")]
    # SL95
    diffFleetPars[3] <- fplusdf_Est[3]
    SPR_plus <- simLBSPRDome(StockPars, diffFleetPars, SizeBins)$SPR
    diffFleetPars[3] <- fminusdf_Est[3]
    SPR_minus <- simLBSPRDome(StockPars, diffFleetPars, SizeBins)$SPR
    dSPRdx3 <- (SPR_plus - SPR_minus)/(2*delta_x)
  } 
  
  varSPR <- optVarcov[1,1]*(dSPRdx1)^2
  if(dim(optVarcov)[1] == 3){
    varSPR <- varSPR +   optVarcov[2,2]*(dSPRdx2)^2 + optVarcov[3,3]*(dSPRdx2)^2 +
      2*optVarcov[1,2]*dSPRdx1*dSPRdx2 + 2*optVarcov[2,3]*dSPRdx2*dSPRdx3  
    # variance decreases because of this 2*varcov[1,2]*dSPRdx1*dSPRdx2 term
  } 
  varSPR
  
  #lowerciSPR <- ifelse(testOpt$lbPars["SPR"] - 1.96*sqrt(varSPR) < 0, 0, testOpt$lbPars["SPR"] - 1.96*sqrt(varSPR))
  #upperciSPR <- testOpt$lbPars["SPR"] + 1.96*sqrt(varSPR)
  
  return(varSPR)
}