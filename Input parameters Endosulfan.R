#--------------------------INPUT PARAMETERS ENDOSULFAN PBK MODEL-------------#
# To validate the pbk model, it is important to compare it to excising in vivo (animal) data.
# experiments differ in methodology and setup, that is why it is important to input parameters closely to how they were during the experiment
# Scenarios of multiple endosulfan experiments are provided so they can be selected and directly put in the pbk model


library(rxode2)
library(tidyverse)
library(ggplot2)

#------------------------ Function Parameters-------------------------#
setParameters <- function(doses, interv, endtime, dose, fESA, fESB, bw) {
  list(
    nbr.doses = doses,
    interval = interv,
    time.0 = 0,
    time.end = endtime,
    time.frame = 0.1,
    Oral_Dose_in_mg_bw = dose,
    fraction_administered_ESA = fESA,
    fraction_administered_ESB = fESB,
    BW = bw
  )
}
#----------------------------Scenarios in vivo experiments----------------------#

#Input parameters
#parameters_general <- setParameters(1,24,)
parameters_test_metabolism <- setParameters(1,24,24,2,0.7,0.3,0.25)
parameters_IV <- setParameters(1,24,24,0.5,0.7,0.3,0.25)

parameters_LSA_rats <- setParameters(1,24,24,10,0.7,0.3,0.25)

parameters_toxicity_testes <- setParameters(53,24,1400,3,0.7,0.3,0.25)


# Faecal input data (Needham & Guitierrez Giulianotti 1997)
parameters_faeces_1 <- setParameters(1, 24, 100, 1.0, 0.6867, 0.2943, 0.165)
parameters_faeces_6 <- setParameters(1, 24, 100, 6.0, 0.6867, 0.2943, 0.165)


#-----------------------In vivo experimental data rats-------------#

#Gupta, 1978
#!5 daily oral exposure, 5 and 10 mg/kg/d. BW 0.158 and 0.154 average. 2;1 ratio
parameters_Gupta_5 <- setParameters(15, 24, 740, 5, 0.666, 0.333, 0.158)
parameters_Gupta_10 <- setParameters(15, 24, 740, 10, 0.666, 0.333, 0.154)

# Nath 1978; 
#11.0 mg/kg/day for 30 days & bw 150-175g 
#GC, endosulfan in tissues (no clear distinction what is considered)
parameters_Nath <- setParameters(30, 24, 770, 11.0, 0.7, 0.3, 0.1625) 

# Ansari, 1984 (I)
#7.5 mg/kg/day for 60 days & bw of 60-80 g Albino rats
# Endosulfan in ratio 2:1 (0.66:0.33)
parameters_Ansari_75 <- setParameters(60, 24, 1470, 7.5, 0.66, 0.33, 0.07)

# Ansari, 1984 (II)
#2.5 mg/kg/day for 60 days & bw of 60-80 g Albino rats
# Endosulfan in ratio 2:1 (0.66:0.33)
parameters_Ansari_25  <- setParameters(60, 24, 1470, 2.5, 0.66, 0.33, 0.07)

# Dikshith, 1984 (I)
#1.5 mg/kg/day for 30 days & N.D bw Female Albino rats
#Endosulfan with 65.32% Alpha and 33.68% Beta
parameters_Dikshith_15 <- setParameters(30, 24, 770, 1.5, 0.6532, 0.3368, 0.154)

# Dikshith, 1984 (II)
#5 mg/kg/day for 30 days & N.D bw Male Albino rats
#Endosulfan with 65.32% Alpha and 33.68% Beta
parameters_Dikshith_5 <- setParameters(30, 24, 744, 5.0, 0.6532, 0.3368, 0.1615)

#Leist and Mayer, 1987 (I)
# 34 mg/kg/day
parameters_LeistandMayer_34 <- setParameters(30, 24, 1500, 34.0, 0.6853, 0.2937, 0.153)
# 67.8 mg/kg/day
parameters_LeistandMayer_68 <- setParameters(30, 24, 1500, 67.8, 0.6853, 0.2937, 0.153)

# Chan and Mohd, 2005 (I)
#5 mg/kg/day for 15 days 
parameters_ChanandMohd_5 <- setParameters(15, 24, 720, 5, 0.7, 0.3, 0.08)
# Chan and Mohd, 2005 (II)
# 10 mg/kg/day for 15 days
parameters_ChanandMohd_10 <- setParameters(15, 24, 720, 10, 0.7, 0.3, 0.08)




#--------------------Parameter Selection PBK model---------------#
chosen_parameters <- parameters_toxicity_testes


#---------------------- Extraction Parameters--------------------# 
nbr.doses <- chosen_parameters$nbr.doses
interval <- chosen_parameters$interval
time.0 <- chosen_parameters$time.0
time.end <- chosen_parameters$time.end
time.frame <- chosen_parameters$time.frame
Oral_Dose_in_mg_bw <- chosen_parameters$Oral_Dose_in_mg_bw
fraction_administered_ESA <- chosen_parameters$fraction_administered_ESA
fraction_administered_ESB <- chosen_parameters$fraction_administered_ESB
BW <- chosen_parameters$BW 
