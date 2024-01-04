# EQETool.R ####
## OLD Version: no development.
# DO NOT USE ####
## Ecological Quality Extent (EQE) tool to quantify the impacts and recovery of
## environmental indices
## A number of user-generated parameters are required to inform the spatial
## and temporal scales of pressures

## user inputs:
## use same symbology as the Cesar paper:

# Ecological quality dimension:
## Qmax is the maximum value of the index under reference conditions
## Qmin is the minimum value that the EQR will reach as a result of the disturbance

# Spatial dimension (Sp):
# calculated as the surface area of the ‘footprint’ of the pressure as a
# proportion of the assessment area

EQE <- function(Tactdur = 4, #activity duration
                Tdetdur = 1, #Temporal deterioration duration
                Trecdur = 3, #Temporal recovery
                Ttot = 10, #Temporal total (timescale under consideration)
                Sext = 386.8, #Spatial extent
                Stot = 1143,#Total area over which assessment is considered
                Qpot = 0.1, #Potential quality (abs)
                Qexp = 1, #Expected quality (abs)
                Htot = 100,#Total area of a given habitat
                Hres = 1, #Resistance of habitat to impact
                Hrec = 1, #Recovery of habitat following impact
                Ttotp = 1, #Temporal total (proportion)
                Stotp  =1, #Total area (proportion)
                Qexpp = 1 #Expected quality (proportion)
                ){
  Stra <- Sext * 0.5; #Spatial transition
  Qab <- ifelse(Tactdur >= Tdetdur, #Observed Quality (abs)
                Qpot,
                (Qexp - ((Tactdur/Tdetdur)*(Qexp - Qpot))));
  Qtab <- (Qexp - Qab); #Quality transition
  Hlos <- (1/(1-(((Sext+(0.5*Stra))^2)/(Htot^2)))); #Habitat loss factor
  Hcres <- Tdetdur * Hres; #Habitat resistance corrected temp transition
  Hcrec <- Trecdur * Hrec; #Habitat recovery corrected temp transition
  Tdp <- Hcres/Ttot; #Temporal: deterioration (proportion)
  Tex <- ifelse((Tactdur < Hcres), 0, (Tactdur - Hcres)); #Temporal extent
  Tep <- Tex/Ttot; #Temporal extent (proportion)
  Trp <- Hcrec/Ttot; #Temporal recovery (proportion)
  Sep <- Sext/Stot; #Spatial extent (proportion)
  Stp <- Stra/Stot; #Spatial transition (proportion)
  Qob <- Qab/Qexp; #Observed quality (proportion)
  Qtra <- Qtab/Qexp; ##Quality transition (proportion)
  v1 <- (Tep * Sep * Qtra); #volume 1
  v2 <- 0.5*(Sep * Tdp * Qtra); #volume 2
  v3 <- 0.5*(Sep * Trp * Qtra); #volume 3
  v4 <- 0.5*(Stp * Tep * Qtra); #volume 4
  v5 <- (1/6)*(Tdp * Stp * Qtra); #volume 5
  v6 <- (1/6)*(Trp * Stp * Qtra); #volume 6
  int <- Qob; #intercept
  Tdg <- Qtra/Tdp; #Temporal deterioration gradient
  Trg <- Qtra/Trp; #Temporal Recovery Gradient
  Sgra <- Qtra/Stp; #Spatial gradient
  Totpress <- sum(v1,v2,v3,v4,v5,v6) #Total pressure volume
  
  final <- list(Tactdur=Tactdur, Tdetdur=Tdetdur,Tex=Tex,
                Trecdur=Trecdur,Ttot=Ttot, Sext=Sext,
                Stra=Stra, Stot=Stot, Qpot=Qpot,
                Qab=Qab, Qtab=Qtab, Qexp=Qexp, Htot=Htot,
                Hlos=Hlos, Hres=Hres, Hrec=Hrec, Hcres=Hcres, Hcrec=Hcrec,
                Tdp=Tdp, Tep=Tep, Trp=Trp, Ttotp=Ttotp, Sep=Sep,
                Stp=Stp, Stotp=Stotp,Qob=Qob, Qtra=Qtra, Qexpp=Qexpp,
                v1=v1, v2=v2, v3=v3, v4=v4, v5=v5, v6=v6, int=int,
                Tdg=Tdg, Trg=Trg, Sgra=Sgra, Totpress=Totpress)
  return(final)
}
