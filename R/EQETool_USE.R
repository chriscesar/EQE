# EQETool_USE.R ####
## Updated version ##
## Ecological Quality Extent (EQE) tool to quantify the impacts and recovery of
## environmental indices
## A number of user-generated parameters are required to inform the spatial
## and temporal scales of pressures

# define user inputs####
Tactdur <- 4 #activity duration
Tdetdur <- 1 #Temporal deterioration duration
Trecdur <- 3 #Temporal recovery
Ttot <- 10 #Temporal total (timescale under consideration)
Sext <- 250 #Spatial extent
Stot <- 1000 #Total area over which assessment is considered
Qpot <- 0.1 #Potential quality (abs)
Qexp <- 1 #Expected quality (abs)
Htot <- 100 #Total area of a given habitat
Hres <- 1 #Resistance of habitat to impact
Hrec <- 1 #Recovery of habitat following impact
Ttotp <- 1 #Temporal total (proportion)
Stotp <- 1 #Total area (proportion)
Qexpp <- 1 #Expected quality (proportion)

#Calculate tool parameters ####
Stra <- Sext * 0.5 #Spatial transition
Qab <- ifelse(Tactdur >= Tdetdur, #Observed Quality (abs)
              Qpot,
              (Qexp - ((Tactdur/Tdetdur)*(Qexp - Qpot))))

Qtab <- (Qexp - Qab) #Quality transition
Hlos <- (1/(1 - (((Sext + (0.5 * Stra))^2)/(Htot^2)))) #Habitat loss factor
Hcres <- Tdetdur*Hres #Habitat resistance corrected temp transition
Hcrec <- Trecdur * Hrec #Habitat recovery corrected temp transition
Tdp <- Hcres/Ttot #Temporal: deterioration (proportion)
Tex <- ifelse((Tactdur < Hcres), 0, (Tactdur - Hcres)) #Temporal extent
Tep <- Tex/Ttot #Temporal extent (proportion)
Trp <- Hcrec/Ttot #Temporal recovery (proportion)
Sep <- Sext/Stot #Spatial extent (proportion)
Stp <- Stra/Stot #Spatial transition (proportion)
Qob <- Qab/Qexp #Observed quality (proportion)
Qtra <- Qtab/Qexp ##Quality transition (proportion)
V1 <- (Tep*Sep*Qtra) #volume 1
V2 <- 0.5*(Sep*Tdp * Qtra) #volume 2
V3 <- 0.5*(Sep*Trp * Qtra) #volume 3
V4 <- 0.5*(Stp*Tep * Qtra) #volume 4
V5 <- (1/6)*(Tdp * Stp * Qtra) #volume 5
V6 <- (1/6)*(Trp * Stp * Qtra) #volume 6
Int <- Qob #intercept
Tdg <- Qtra/Tdp #Temporal deterioration gradient
Trg <- Qtra/Trp #Temporal Recovery Gradient
Sgra <- Qtra/Stp #Spatial gradient
Totpress <- sum(V1, V2, V3, V4, V5, V6) #Total pressure volume

final <- list(
  Tactdur = Tactdur, Tdetdur = Tdetdur, Tex = Tex, Trecdur = Trecdur,
  Ttot = Ttot, Sext = Sext, Stra = Stra, Stot = Stot, Qpot = Qpot,
  Qab = Qab, Qtab = Qtab, Qexp = Qexp, Htot = Htot, Hlos = Hlos,
  Hres = Hres, Hrec = Hrec, Hcres = Hcres, Hcrec = Hcrec,
  Tdp = Tdp, Tep = Tep, Trp = Trp, Ttotp = Ttotp, Sep = Sep,
  Stp = Stp, Stotp = Stotp, Qob = Qob, Qtra = Qtra, Qexpp = Qexpp,
  V1 = V1, V2 = V2, V3 = V3, V4 = V4, V5 = V5, V6 = V6,
  Int = Int, Tdg = Tdg, Trg = Trg, Sgra = Sgra, Totpress = Totpress
  )

print(paste0(" ",round(final$Totpress*100,2),"% change in habitat quality"))


### Excel code to calculate surface:

# IF(
#   ((IF(
#     AND((C$6>=Tdp),
#         (C$6<=(Tdp+Tep)),
#         ($B60<=Sep)),Int,0))+
#      (IF(OR(($B60>Sep),(C$6>=Tdp)),0,
#          (IF(Tdetdur=0,1,(Int+(Tdg*(Tdp-C$6)))))))+
#      (IF(OR((C$6<=(Tdp+Tep)),($B60>Sep)),0,
#          (IF($F$3=0,1,
#              (Int+(Trg*(C$6-(Tdp+Tep))))))))+
#      (IF(OR(($B60<=Sep),(C$6<Tdp),(C$6>(Tdp+Tep))),0,
#          (IF(Stra=0,1,(Int+(Sgra*($B60-Sep)))))))+
#      (IF(OR(($B60<=Sep),(C$6>=Tdp)),0,
#          IF(OR(Tdetdur=0,Stra=0),1,
#             (Int+(Tdg*(Tdp-C$6))+
#                (Sgra*($B60-(Sep)))))))+
#      (IF(OR((C$6<=(Tdp+Tep)),($B60<=Sep)),0,
#          (IF(OR($F$3=0,Stra=0),1,
#              (Int+(Trg*(C$6-(Tdp+Tep)))+
#                 (Sgra*($B60-Sep))))))))>1,1,
#   ((IF(AND((C$6>=Tdp),(C$6<=(Tdp+Tep)),($B60<=Sep)),Int,0))+
#      (IF(OR(($B60>Sep),(C$6>=Tdp)),0,
#          (IF(Tdetdur=0,1,
#              (Int+(Tdg*(Tdp-C$6)))))))+
#      (IF(OR((C$6<=(Tdp+Tep)),($B60>Sep)),0,
#          (IF($F$3=0,1,(Int+(Trg*(C$6-(Tdp+Tep))))))))+
#      (IF(OR(($B60<=Sep),(C$6<Tdp),(C$6>(Tdp+Tep))),0,
#          (IF(Stra=0,1,(Int+(Sgra*($B60-Sep)))))))+
#      (IF(OR(($B60<=Sep),(C$6>=Tdp)),0,
#          IF(OR(Tdetdur=0,Stra=0),1,
#             (Int+(Tdg*(Tdp-C$6))+(Sgra*($B60-(Sep)))))))+
#      (IF(OR((C$6<=(Tdp+Tep)),($B60<=Sep)),0,
#          (IF(OR($F$3=0,Stra=0),1,
#              (Int+(Trg*(C$6-(Tdp+Tep)))+(Sgra*($B60-Sep)
#                                          )))
#           ))
#       )))
