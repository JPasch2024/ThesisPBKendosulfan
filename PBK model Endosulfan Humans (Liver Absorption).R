# ------------------PBK MODEL ENDOSULFAN Humans-------------#
# ----------------SEPERATE MODEL ALPHA AND BETA ISOMERS, submodel for SULFATE METABOLITE---------#
# Jeroen Pasch, Wageningen University & Research 2023-01-11

#---------Loading Library----------#

library(rxode2)
library(tidyverse)
library(ggplot2)

#------------------------Parameters-------------------------------------#

#Simulations
amount.units               <-"umol"
time.units                 <-"h"
nbr.doses                  <-1     #number of doses
interval                   <-24      #interval between dosing in H
time.0                     <-0        #time start dosing
time.end                   <-24       #time end of simulation
time.frame                 <-0.1     #time steps of simulation
Oral_Dose_in_mg_bw         <- 2   #Dose in mg/kg-bw
Oral_Dose_in_mg_bw_A       <- Oral_Dose_in_mg_bw * 0.70 #fraction of oral endosulfan consisting of the alpha isomer
Oral_Dose_in_mg_bw_B       <- Oral_Dose_in_mg_bw * 0.30 #fraction of oral endosulfan consisting of the beta isomer
MW_Endosulfan              <- 406.93 # in g/mol #ATSDR
MW_Sulfate                 <- 422.95 # in g/mol #ATSDR
BW                         <- 73   #Body weight in Kg (ref value (ICRP, 2002))
Oral_Dose_A                <-(Oral_Dose_in_mg_bw_A * BW) / MW_Endosulfan * 1e+3       #The administered dose Alpha endosulfan in μmol
Oral_Dose_B                <-(Oral_Dose_in_mg_bw_B * BW) / MW_Endosulfan * 1e+3       #The administered dose Beta endosulfan in μmol
fraction_absorbed_A       <- 1 #0.986 #QSAR Punt et al. 2022
fraction_absorbed_B       <- 1 #0.986 #QSAR Punt et al. 2022
Oral_Dose_A_Absorbed      <- Oral_Dose_A * fraction_absorbed_A
Oral_Dose_B_Absorbed      <- Oral_Dose_B * fraction_absorbed_B
Oral_Dose_B_Absorbed      <- Oral_Dose_B * fraction_absorbed_B


#----------------Physiological Parameters Male Humans-----------------------#
# tissue volumes in % of BW (fractional) (0.9235)
#all taken from (ICRP, 2002) WP = heart + lung + Spleen + Gut, PP = skin + muscle + bone
VkiF <- 0.0042 #kidneys tissue volume
VwpF <- 0.03 #Well perfused tissue volume
VppF <- 0.518 #poorly perfused tissue volume
VliF <- 0.025 #Liver tissue volume
VbrF <- 0.020 #Brain tissue volume
VfF <- 0.249 #Fat tissue volume
VtF <- 0.0005 #Testes tissue volume
VblF <- 0.0767#Blood volume

# blood flows in % Q (so relative to the total cardiac output)
#as found in (ICRP, 2002) Qppr = average (muscle, bone and skin) QwpR = 1 - other fractions
QkiR <- 0.19 #relative kidneys blood flow
QwpR <- 0.1138 #relative well perfused tissue blood flow
QppR <- 0.27 #relative poorly perfused tissue blood flow
QliR <- 0.255 #relative liver blood flow
QbrR <- 0.12 #relative brain blood flow
QfR <- 0.05 #relative fat blood flow
QtR <- 0.0012 #relative testes blood flow (Plowchalk & Teeguarden, 2002)

# ----------------- Biochemical parameters--------------------------#
#--------General Metabolic data-------#
Microsomalprotein_li <- 39.46 #microsomal protein (mg) per gram of human liver (Zhang, 2015)
HLM_mol <- 350 #pmol P450 per mg microsomal proteins humans (Guengerich, 2009) "0.2 - 0.5 nmol P450 per mg protein" 


#--------Endosulfan_Alpha------#
Ka_A <- 1.769 #Absorption rate constant for uptake in the Small intestine in per H
Kb_A <- 0 #biliary excretion rate constant (1/h)



Vmax_A_pmol_min_pmol <- 1.48 #maximum metabolic rate pmol/min/pmol P450 ESA -> ESS (Lee et al. 2006)
Vmax_A <- (Vmax_A_pmol_min_pmol / 10^6) * 60 * (Microsomalprotein_li * VliF * BW * 1000) * HLM_mol
#first converting it to umol/min/pmol P450 using 10^6, then changing it to hourly rate (60) umol/h/pmol P450
#liver weight in g (VliF * BW * 1000) * how much protein per g (Microsomalprotein_li) * pmol P450 per mg protein
#results in Vmax_A in umol/h (for total liver P450 enzymes)

Km_A <- 7.34  # Michealis menten constant in uM (umol/L)

#---------Endosulfan_Beta-----#
Ka_B <- 1.769 #Absorption rate constant for uptake in the Small intestine in per H
Kb_B <- 0 #biliary excretion rate constant (1/h)



Vmax_B_pmol_min_pmol <- 4.40 #maximum metabolic rate pmol/min/pmol P450 B -> S (Lee et al. 2006)
Vmax_B <- (Vmax_B_pmol_min_pmol / 10^6) * 60 * (Microsomalprotein_li * VliF * BW * 1000) * HLM_mol # converted to umol/h (for total liver)
Km_B <- 6.37 # Michealis menten constant in uM (umol/L)

#---------Endosulfan_Sulfate-----#
Ka_S <- 1.458 #Absorption rate constant for uptake in the Small intestine in per H (Relevant for Billiary recirculation with no oral exposure of ESS)
Kb_S <- 0 #Biliary excretion rate constant metabolites (1/h)

Vmax_S  <- Vmax_A  #TEMP  DATA IS LACKING. 
Km_S <- Km_A # Michealis menten constant in uM (umol/L) #TEMP

# ----------------- Excretion parameters--------------------------#
#Excretion from kidney via glomerular filtration
#Glomerular filtration
GFR <- 1.8  #(mL/min/kg bw), Walton et al., 2004
# unbound fraction based on QIVIVETOOLS.WUR.NL
uf_A <- 0.054 #fraction of endosulfan alpha not bound to plasma proteins
uf_B <- 0.067 #fraction of endosulfan beta not bound to plasma proteins
uf_S <- 0.064 #fraction of endosulfan sulfate not bound to plasma proteins
GF <- GFR / 1000 * BW * 60 # (L/hr), human glomerular filtration rate

#excretion from the GI tract
GER <- 0.86 #ICRP 70 min (1.17 h) transit time stomach so 1/1.17 -> Gastric empyting rate humans 
KtSI <- 2.11 #Tsi human. 3.32 h (SOURCE) N=7 N/3.22 -> 2.11 /h (transit rate constant small intestine)
KtCol <- 0.028 #h^-1  ICRP transit time (right + left colon + rectosigmoid) = 36 hours.

# ----------------- Calculation of parameters--------------------------#
# tissue volumes in kg(=L)
Vki <- VkiF * BW; #kidneys tissue volume
Vwp <- VwpF * BW; #Well perfused tissue volume
Vpp <- VppF * BW; #poorly perfused tissue volume
Vli <- VliF * BW; #Liver tissue volume
Vbr <- VbrF * BW; #Brain tissue volume
Vf <- VfF * BW; #Fat tissue volume
Vt <- VtF * BW; #Testes tissue volume
Vbl <- VblF * BW; #(Arterial) Blood volume 

# Blood flow parameters 
Q <- 15 * BW^0.74; #total cardiac output (L/h) #Source bij zetten TEMP
# absolute blood flows (L/h)
Qki <- QkiR * Q; #kidneys blood flow
Qwp <- QwpR * Q; #well perfused tissue blood flow
Qpp <- QppR * Q; #poorly perfused tissue blood flow
Qli <- QliR * Q; #liver blood flow
Qbr <- QbrR * Q; #brain blood flow
Qf <- QfR * Q; #fat blood flow
Qt <- QtR * Q; #testes blood flow

# ----------------- Partition Coefficient--------------------------#
#--------Endosulfan_Alpha------#
# partition coefficient (PC) tissue:blood endosulfan (A) (unitless)
Pki_A <- 3.96 #PC kidney
Pwp_A <- 5.38  #PC well perfused tissues
Ppp_A <- 5.12  #PC poorly perfused tissues
Pli_A <- 6.44 #PC liver
Pbr_A <- 10.24 #PC brain
Pf_A <- 31.69 #PC fat
Pt_A <- 1.54 #PC testes

#---------Endosulfan_Beta-----#
# partition coefficient (PC) tissue:blood endosulfan (B) (unitless)
Pki_B <- 3.57 #PC kidney
Pwp_B <- 4.84 #PC well perfused tissues
Ppp_B <- 4.60 #PC poorly perfused tissues
Pli_B <- 5.79 #PC liver
Pbr_B <- 9.17 #PC brain
Pf_B <- 23.82 #PC fat
Pt_B <- 1.43 #PC testes

#---------Endosulfan_Sulfate-----#
# partition coefficient (PC) tissue:blood endosulfan Sulfate (S) (unitless)
Pki_S <- 3.65 #PC kidney
Pwp_S <- 4.94 #PC well perfused tissues
Ppp_S <- 4.71 #PC poorly perfused tissues
Pli_S <- 5.91 #PC liver
Pbr_S <- 9.38 #PC brain
Pf_S <- 25.18 #PC fat
Pt_S <- 1.45 #PC testes

# Create a data frame with all parameters (KEki weggehaald, nu alleen GF)
parameters <- data.frame(
  Pki_A, Pwp_A, Ppp_A, Pli_A, Pbr_A, Pf_A, Pt_A, Ka_A, Kb_A, Vmax_A, Km_A,
  Pki_B, Pwp_B, Ppp_B, Pli_B, Pbr_B, Pf_B, Pt_B, Ka_B, Kb_B, Vmax_B, Km_B,
  Pki_S, Pwp_S, Ppp_S, Pli_S, Pbr_S, Pf_S, Pt_S, Ka_S, Kb_S, Vmax_S, Km_S,
  GF, uf_A, uf_B, uf_S, Vki, Vwp, Vpp, Vli, Vbr, Vf, Vt, Vbl, Q, Qki, Qwp, Qpp, Qli, Qbr, Qf, Qt, GER, KtSI, KtCol
)

# Check the length of the data frame
print(length(parameters))

# Now, you can use the data frame for further analysis or as input to your function
# Define initial values
inits <- c(
  "Ast_A" = 0,
  "Ain1_A" = 0,
  "Ain2_A" = 0,
  "Ain3_A" = 0,
  "Ain4_A" = 0,
  "Ain5_A" = 0,
  "Ain6_A" = 0,
  "Ain7_A" = 0,
  "Acol_A" = 0,
  "Afec_A" = 0,
  
  "Ast_B" = 0,
  "Ain1_B" = 0,
  "Ain2_B" = 0,
  "Ain3_B" = 0,
  "Ain4_B" = 0,
  "Ain5_B" = 0,
  "Ain6_B" = 0,
  "Ain7_B" = 0,
  "Acol_B" = 0,
  "Afec_B" = 0,
  
  "Ast_S" = 0,
  "Ain1_S" = 0,
  "Ain2_S" = 0,
  "Ain3_S" = 0,
  "Ain4_S" = 0,
  "Ain5_S" = 0,
  "Ain6_S" = 0,
  "Ain7_S" = 0,
  "Acol_S" = 0,
  "Afec_S" = 0,
  
  "Ali_A" = 0,
  "Aki_A" = 0,
  "Awp_A" = 0,
  "App_A" = 0,
  "Abr_A" = 0,
  "At_A" = 0,
  "Af_A" = 0,
  "Abl_A" = 0,
  
  "Ali_B" = 0,
  "Aki_B" = 0,
  "Awp_B" = 0,
  "App_B" = 0,
  "Abr_B" = 0,
  "At_B" = 0,
  "Af_B" = 0,
  "Abl_B" = 0,
  
  "Ali_S" = 0,
  "Aki_S" = 0,
  "Awp_S" = 0,
  "App_S" = 0,
  "Abr_S" = 0,
  "At_S" = 0,
  "Af_S" = 0,
  "Abl_S" = 0,
  
  "AExcretion_ki_A" = 0,
  "TExcretion_ki_A" = 0,
  "AMR_A" = 0,
  "TMR_A" = 0,
  "AExcretion_ki_B" = 0,
  "TExcretion_ki_B" = 0,
  "AMR_B" = 0,
  "TMR_B" = 0,
  "AExcretion_ki_S" = 0,
  "TExcretion_ki_S" = 0,
  "AMR_S" = 0,
  "TMR_S" = 0
)

InitModel <- function() {
  model2 <- RxODE({
    #------------------------Compartments---------------------------------------------#
    # Concentrations in each compartment and the associated venous blood is calculated
    # Concentration in compartment (umol/L) = Amount in compartment (umol) / Volume of compartment (L)
    # Concentration in venous blood of compartment = (Concentration in compartment / tissue:blood partition coefficient) 
    
    #Concentrations liver and venous blood from liver
    # Compartments for Endosulfan_Alpha, Endosulfan_Beta, Endosulfan_Sulfate,
    Cli_A <- Ali_A / Vli
    Cli_v_A <- Cli_A / Pli_A
    
    Cli_B <- Ali_B / Vli
    Cli_v_B <- Cli_B / Pli_B
    
    Cli_S <- Ali_S / Vli
    Cli_v_S <- Cli_S / Pli_S
    
    
    #Concentrations fat and venous blood from fat
    Cf_A <- Af_A / Vf;
    Cf_v_A <- Cf_A / Pf_A;
    
    Cf_B <- Af_B / Vf;
    Cf_v_B <- Cf_B / Pf_B;
    
    Cf_S <- Af_S / Vf;
    Cf_v_S <- Cf_S / Pf_S;
    
    
    #Metabolism in liver according to Michaelis Menten kinetics
    # (Vmax * available substrate concentration) / (Km + available substrate concentration)
    
    #Endosulfan Alpha & Beta into Metabolite sulfate (S) in the liver
    # Assumption all A and B is first metabolised into S (just as Lee et al. (2006) assumed)
    
    AMR_A <- (Vmax_A * Cli_v_A) / (Km_A + Cli_v_A);
    AMR_B <- (Vmax_B * Cli_v_B) / (Km_B + Cli_v_B);
    
    #Endosulfan sulfate further metabolised into less toxic more polar compounds (assumed less relevant risk assessment)
    AMR_S <- (Vmax_S * Cli_v_S) / (Km_S + Cli_v_S);
    
    #Concentrations brain and venous blood from brain
    Cbr_A <- Abr_A / Vbr;
    Cbr_v_A <- Cbr_A / Pbr_A;
    
    Cbr_B <- Abr_B / Vbr;
    Cbr_v_B <- Cbr_B / Pbr_B;
    
    Cbr_S <- Abr_S / Vbr;
    Cbr_v_S <- Cbr_S / Pbr_S;
    
    
    #Concentrations kidneys and venous blood from kidney
    Cki_A <- Aki_A / Vki;
    Cki_v_A <- Cki_A / Pki_A;
    
    Cki_B <- Aki_B / Vki;
    Cki_v_B <- Cki_B / Pki_B;
    
    Cki_S <- Aki_S / Vki;
    Cki_v_S <- Cki_S / Pki_S;
    
    
    #Excretion kidney
    AExcretion_ki_A <- Cki_A * uf_A * GF;
    AExcretion_ki_B <- Cki_B * uf_B * GF;
    AExcretion_ki_S <- Cki_S * uf_S * GF;
    
    
    #Concentrations testes and venous blood from testes
    Ct_A <- At_A / Vt;
    Ct_v_A <- Ct_A / Pt_A;
    
    Ct_B <- At_B / Vt;
    Ct_v_B <- Ct_B / Pt_B;
    
    Ct_S <- At_S / Vt;
    Ct_v_S <- Ct_S / Pt_S;
    
    
    # Concentration well perfused tissues and venous blood from well perfused tissues
    Cwp_A <- Awp_A / Vwp;
    Cwp_v_A <- Cwp_A / Pwp_A;
    
    Cwp_B <- Awp_B / Vwp;
    Cwp_v_B <- Cwp_B / Pwp_B;
    
    Cwp_S <- Awp_S / Vwp;
    Cwp_v_S <- Cwp_S / Pwp_S;
    
    
    # Concentration poorly perfused tissues and venous blood from poorly perfused tissues
    Cpp_A <- App_A / Vpp;
    Cpp_v_A <- Cpp_A / Ppp_A;
    
    Cpp_B <- App_B / Vpp;
    Cpp_v_B <- Cpp_B / Ppp_B;
    
    Cpp_S <- App_S / Vpp;
    Cpp_v_S <- Cpp_S / Ppp_S;
    
    
    # Concentrations blood
    Cbl_A <- Abl_A / Vbl;
    
    Cbl_B <- Abl_B / Vbl;
    
    Cbl_S <- Abl_S / Vbl;
    
    
    #-------------------------Differential equations---------------------------------------------#
    # These equation describe the change in the amount (umol) in a specific compartment (d/dt(Amount in compartment))
    # This change is defined by:
    # - Adding the amount that comes into the compartment (via blood or absorption)
    # - Subtracting the amount that is metabolized
    # - Subtracting the amount that is leaving the compartment (via venous blood or excretion (e.g. urine))
    
    # Change in amount in stomach
    # Change in stomach is defined as:
    # The amount in the stomach multiplied by the negative gastric emptying rate (GER; since the amount is going out of the stomach into the intestine)
    d/dt(Ast_A) <- -GER * Ast_A;
    d/dt(Ast_B) <- -GER * Ast_B;
    d/dt(Ast_S) <- -GER * Ast_S; #Ast_S will always be zero when no Sulfate is provided orally, but just as the result of ESA and ESB metabolism
    
    # Amount in small intestine compartments -> Nog gestolen van THijs
    # The intestinal model consists of 7 compartments for the small intestine (in1 to in7), and 1 for the large intestine (col).
    # A fecal (fec) compartment catches what goes through the whole GIT
    # Absorption is limited to the small intestine due to bile acid activity
    # Change in the amount present in each compartment depends on:
    # - Influx: transition or emtpying rate from previous compartment (stomach or intestinal) multiplied by the amount in the previous compartment
    # - Absorption: absorption rate multiplied by the amount in the present compartment
    # - Transition to next compartment: transition rate multiplied by the amount of compound in the current compartment
    
    # Amount in small intestine compartments
    # The intestinal model consists of 7 compartments for the small intestine (in1 to in7), and 1 for the large intestine (col).
    # A fecal (fec) compartment catches what goes through the whole GIT
    # Absorption is limited to the small intestine due to bile acid activity
    
    d/dt(Ain1_A) <- GER * Ast_A - KtSI * Ain1_A - Ka_A*Ain1_A + Kb_A * Ali_A;    # Duodenum, bile acids secreted in duodenum
    d/dt(Ain2_A) <- KtSI * Ain1_A - KtSI * Ain2_A - Ka_A*Ain2_A;  # Jejunum
    d/dt(Ain3_A) <- KtSI * Ain2_A - KtSI * Ain3_A - Ka_A*Ain3_A;  # Jejunum
    d/dt(Ain4_A) <- KtSI * Ain3_A - KtSI * Ain4_A - Ka_A*Ain4_A;  # Ileum 
    d/dt(Ain5_A) <- KtSI * Ain4_A - KtSI * Ain5_A - Ka_A*Ain5_A;  # Ileum
    d/dt(Ain6_A) <- KtSI * Ain5_A - KtSI * Ain6_A - Ka_A*Ain6_A;  # Ileum
    d/dt(Ain7_A) <- KtSI * Ain6_A - KtSI * Ain7_A - Ka_A*Ain7_A;  # Ileum, bile acids reabsorbed in ileum
    d/dt(Acol_A) <- KtSI * Ain7_A - KtCol * Acol_A;               # No absorption in the colon (bile acids reabsorbed in ileum)
    d/dt(Afec_A) <- KtCol * Acol_A;                               # Amount in feces
    
    d/dt(Ain1_B) <- GER * Ast_B - KtSI * Ain1_B - Ka_B*Ain1_B + Kb_B * Ali_B;    # Duodenum, bile acids secreted in duodenum
    d/dt(Ain2_B) <- KtSI * Ain1_B - KtSI * Ain2_B - Ka_B*Ain2_B;  # Jejunum
    d/dt(Ain3_B) <- KtSI * Ain2_B - KtSI * Ain3_B - Ka_B*Ain3_B;  # Jejunum
    d/dt(Ain4_B) <- KtSI * Ain3_B - KtSI * Ain4_B - Ka_B*Ain4_B;  # Ileum 
    d/dt(Ain5_B) <- KtSI * Ain4_B - KtSI * Ain5_B - Ka_B*Ain5_B;  # Ileum
    d/dt(Ain6_B) <- KtSI * Ain5_B - KtSI * Ain6_B - Ka_B*Ain6_B;  # Ileum
    d/dt(Ain7_B) <- KtSI * Ain6_B - KtSI * Ain7_B - Ka_B*Ain7_B;  # Ileum, bile acids reabsorbed in ileum
    d/dt(Acol_B) <- KtSI * Ain7_B - KtCol * Acol_B;               # No absorption in the colon (bile acids reabsorbed in ileum)
    d/dt(Afec_B) <- KtCol * Acol_B;                               # Amount in feces
    
    d/dt(Ain1_S) <- GER * Ast_S - KtSI * Ain1_S - Ka_S*Ain1_S + Kb_S * Ali_S;    # Duodenum, bile acids secreted in duodenum
    d/dt(Ain2_S) <- KtSI * Ain1_S - KtSI * Ain2_S - Ka_S*Ain2_S;  # Jejunum
    d/dt(Ain3_S) <- KtSI * Ain2_S - KtSI * Ain3_S - Ka_S*Ain3_S;  # Jejunum
    d/dt(Ain4_S) <- KtSI * Ain3_S - KtSI * Ain4_S - Ka_S*Ain4_S;  # Ileum 
    d/dt(Ain5_S) <- KtSI * Ain4_S - KtSI * Ain5_S - Ka_S*Ain5_S;  # Ileum
    d/dt(Ain6_S) <- KtSI * Ain5_S - KtSI * Ain6_S - Ka_S*Ain6_S;  # Ileum
    d/dt(Ain7_S) <- KtSI * Ain6_S - KtSI * Ain7_S - Ka_S*Ain7_S;  # Ileum, bile acids reabsorbed in ileum
    d/dt(Acol_S) <- KtSI * Ain7_S - KtCol * Acol_S;               # No absorption in the colon (bile acids reabsorbed in ileum)
    d/dt(Afec_S) <- KtCol * Acol_S;                               # Amount in feces
    
    
    
    #amount in blood
    # Here the amount in blood is calculated by adding up the amount of compound in the venous blood of all compartments 
    # (multiply tissue blood flow by concentration in venous blood)
    # From this amount, the concentration that was already in the blood is subtracted (Summed blood flows multiplied by the Concentration in blood)
    
    # For Endosulfan_Alpha
    d/dt(Abl_A) <- ((Qli * (Cli_v_A - Cbl_A)) + (Qki * (Cki_v_A - Cbl_A)) + (Qbr * (Cbr_v_A - Cbl_A)) + (Qf * (Cf_v_A - Cbl_A)) + (Qt * (Ct_v_A - Cbl_A)) + (Qwp * (Cwp_v_A - Cbl_A)) + (Qpp * (Cpp_v_A - Cbl_A)));
    
    # For Endosulfan_Beta
    d/dt(Abl_B) <- ((Qli * (Cli_v_B - Cbl_B)) + (Qki * (Cki_v_B - Cbl_B)) + (Qbr * (Cbr_v_B - Cbl_B)) + (Qf * (Cf_v_B - Cbl_B)) + (Qt * (Ct_v_B - Cbl_B)) + (Qwp * (Cwp_v_B - Cbl_B)) + (Qpp * (Cpp_v_B - Cbl_B)));
    
    # For Endosulfan_Sulfate
    d/dt(Abl_S) <- ((Qli * (Cli_v_S - Cbl_S)) + (Qki * (Cki_v_S - Cbl_S)) + (Qbr * (Cbr_v_S - Cbl_S)) + (Qf * (Cf_v_S - Cbl_S)) + (Qt * (Ct_v_S - Cbl_S)) + (Qwp * (Cwp_v_S - Cbl_S)) + (Qpp * (Cpp_v_S - Cbl_S)));
    
    
    #Amount in fat
    d/dt(Af_A) <- Qf * (Cbl_A - Cf_v_A);
    d/dt(Af_B) <- Qf * (Cbl_B - Cf_v_B);
    d/dt(Af_S) <- Qf * (Cbl_S - Cf_v_S);
    
    
    #Cumulative (Total) metabolized in liver
    d/dt(TAMR_A) <- AMR_A;
    d/dt(TAMR_B) <- AMR_B;
    d/dt(TAMR_S) <- AMR_S;
    
    #Amount in liver   
    #Amount liver is what absorbed in all GI tract SI compartments, minus the Endosulfan metabolised and biliary excretion back to the SI. In addition to the endosulfan in the blood.
    d/dt(Ali_A) <- (Ka_A*Ain1_A + Ka_A*Ain2_A + Ka_A*Ain3_A + Ka_A*Ain4_A + Ka_A*Ain5_A + 
                      Ka_A*Ain6_A + Ka_A*Ain7_A) + Qli * (Cbl_A - Cli_v_A) - AMR_A - Kb_A * Ali_A;
    d/dt(Ali_B) <- (Ka_B*Ain1_B + Ka_B*Ain2_B + Ka_B*Ain3_B + Ka_B*Ain4_B + Ka_B*Ain5_B + 
                          Ka_B*Ain6_B + Ka_B*Ain7_B - Kb_B * Ali_B) + Qli * (Cbl_B - Cli_v_B) - AMR_B;
    d/dt(Ali_S) <-(Ka_S*Ain1_S + Ka_S*Ain2_S + Ka_S*Ain3_S + Ka_S*Ain4_S + Ka_S*Ain5_S + Ka_S*Ain6_S + Ka_S*Ain7_S - Kb_S * Ali_S) + Qli * (Cbl_S - Cli_v_S) + AMR_A + AMR_B - AMR_S;
    
    
    #Amount in brain
    d/dt(Abr_A) <- Qbr * (Cbl_A - Cbr_v_A);
    d/dt(Abr_B) <- Qbr * (Cbl_B - Cbr_v_B);
    d/dt(Abr_S) <- Qbr * (Cbl_S - Cbr_v_S);
    
    
    #Amount in testes
    d/dt(At_A) <- Qt * (Cbl_A - Ct_v_A);
    d/dt(At_B) <- Qt * (Cbl_B - Ct_v_B);
    d/dt(At_S) <- Qt * (Cbl_S - Ct_v_S);
    
    
    #Amount in well perfused tissue
    d/dt(Awp_A) <- Qwp * (Cbl_A - Cwp_v_A);
    d/dt(Awp_B) <- Qwp * (Cbl_B - Cwp_v_B);
    d/dt(Awp_S) <- Qwp * (Cbl_S - Cwp_v_S);
    
    
    #Amount in poorly perfused tissue
    d/dt(App_A) <- Qpp * (Cbl_A - Cpp_v_A);
    d/dt(App_B) <- Qpp * (Cbl_B - Cpp_v_B);
    d/dt(App_S) <- Qpp * (Cbl_S - Cpp_v_S);
    
    
    #Cumultative (total) amount excreted in urine
    d/dt(TExcretion_ki_A) <- AExcretion_ki_A;
    d/dt(TExcretion_ki_B) <- AExcretion_ki_B;
    d/dt(TExcretion_ki_S) <- AExcretion_ki_S;
    
    
    #Amount in kidney
    # Normal kidney amount minus the amount excreted into urine
    d/dt(Aki_A) <- Qki * (Cbl_A - Cki_v_A) - AExcretion_ki_A;
    d/dt(Aki_B) <- Qki * (Cbl_B - Cki_v_B) - AExcretion_ki_B;
    d/dt(Aki_S) <- Qki * (Cbl_S - Cki_v_S) - AExcretion_ki_S;
    
    
    
  })
  return(model2)
}

model2 <- InitModel()

#------------------------Events (administered dose) -------------------------------------#
ev <- eventTable(amount.units = amount.units, time.units = time.units) %>%
  et(dose = Oral_Dose_A_Absorbed, cmt = "Ast_A", ii = interval, nbr.doses = nbr.doses) %>%
  et(dose = Oral_Dose_B_Absorbed, cmt = "Ast_B", ii = interval, nbr.doses = nbr.doses) %>%
  et(seq(from = time.0, to = time.end, by = time.frame))


solve.pbk <- solve(model2, parameters, events = ev, inits, cores = 4)

#View(solve.pbk)

# Extract the simulated data from the solve.pbk object
sim_data_human <- as.data.frame(solve.pbk)


#------------------------Mass Balance-------------------------------------#
colsAlpha <-c("Ast_A", "Ain1_A", "Ain2_A", "Ain3_A", "Ain4_A", "Ain5_A", "Ain6_A", "Ain7_A", "Acol_A", "Afec_A", "Ali_A", "Aki_A", "Awp_A", "App_A", "Abr_A", "At_A", "Af_A", "Abl_A", "TExcretion_ki_A", "TAMR_A")
colsBeta <-c("Ast_B","Ain1_B", "Ain2_B", "Ain3_B", "Ain4_B", "Ain5_B", "Ain6_B", "Ain7_B", "Acol_B", "Afec_B", "Ali_B", "Aki_B", "Awp_B", "App_B", "Abr_B", "At_B", "Af_B", "Abl_B", "TExcretion_ki_B", "TAMR_B")
colsSulfate <-c("Ast_S","Ain1_S", "Ain2_S", "Ain3_S", "Ain4_S", "Ain5_S", "Ain6_S", "Ain7_S", "Acol_S", "Afec_S", "Ali_S", "Aki_S", "Awp_S", "App_S", "Abr_S", "At_S", "Af_S", "Abl_S", "TExcretion_ki_S", "TAMR_S")
colsAll <- c("Ast_A","Ast_B","Ast_S","Ali_A", "Aki_A", "Awp_A", "App_A", "Abr_A", "At_A", "Af_A", "Abl_A",
            "Ali_B", "Aki_B", "Awp_B", "App_B", "Abr_B", "At_B", "Af_B", "Abl_B",
             "Ali_S", "Aki_S", "Awp_S", "App_S", "Abr_S", "At_S", "Af_S", "Abl_S",
             "Ain1_A", "Ain2_A", "Ain3_A", "Ain4_A", "Ain5_A", "Ain6_A", "Ain7_A", "Acol_A", "Afec_A", "Ain1_B", "Ain2_B", "Ain3_B", "Ain4_B", "Ain5_B", "Ain6_B", "Ain7_B", "Acol_B", "Afec_B", "Ain1_S", "Ain2_S", "Ain3_S", "Ain4_S", "Ain5_S", "Ain6_S", "Ain7_S", "Acol_S", "Afec_S",
             "TExcretion_ki_A", "TExcretion_ki_B", "TExcretion_ki_S", "TAMR_S")

CalcBodAll <- rowSums(sim_data_human[, colsAll])
CalcBodAlpha <- rowSums(sim_data_human[, colsAlpha])
CalcBodBeta <- rowSums(sim_data_human[, colsBeta])
CalcBodSulfate <- rowSums(sim_data_human[, colsSulfate])
# Calculate what is goes in. This is the administered amount of compound, and for the sulfate submodel, this is the amount metabolized.

TotBodAll <- Oral_Dose_A_Absorbed + Oral_Dose_B_Absorbed  #Amount administered (in umol)
TotBodAlpha <- Oral_Dose_A_Absorbed 
TotBodBeta <- Oral_Dose_B_Absorbed
TotBodSulfate <- sim_data_human[, "TAMR_A"] + sim_data_human[, "TAMR_B"]  # Amount Sulfate metabolized from Alpha & Beta

# Calculate mass balance
# Total administered or produced amount - minus what is present in the model + 1 (=100%). Done for each timepoint
MassBall    <- TotBodAll - CalcBodAll + 1
MassBalAlpha  <- TotBodAlpha - CalcBodAlpha + 1
MassBalBeta  <- TotBodBeta - CalcBodBeta + 1
MassBalSulfate  <- TotBodSulfate - CalcBodSulfate + 1

# Calculate mass error (% of mass lost)
# Difference between total amount administered or produced and the amount in the (sub)model. 
# This is divided by the total amount administered or produced and multiplied by 100 to give a percentage
ErrorAll    <- (TotBodAll - CalcBodAll) / (TotBodAll + 10^(-30)) * 100
ErrorAlpha    <- (TotBodAlpha - CalcBodAlpha) / (TotBodAlpha + 10^(-30)) * 100
ErrorBeta    <- (TotBodBeta - CalcBodBeta) / (TotBodBeta + 10^(-30)) * 100
ErrorSulfate    <- (TotBodSulfate - CalcBodSulfate) / (TotBodSulfate + 10^(-30)) * 100

# Compile Mass Balance data into a dataframe
MassBal  <- cbind("time" = sim_data_human[,"time"], MassBall, ErrorAll, MassBalAlpha, ErrorAlpha, MassBalBeta, 
                  ErrorBeta, MassBalSulfate, ErrorSulfate)
MassBal <- as.data.frame(MassBal)

ggplot(data = MassBal) +
  geom_line(aes(x = time,
                y = MassBall)) +
  geom_line(aes(x = time,
                y = ErrorAll)) +
  labs(title = "MB All",
       x = "Time (hours)",
       y = "Concentration (umol/L)")

ggplot(data = MassBal) +
  geom_line(aes(x = time,
                y = MassBalAlpha)) +
  geom_line(aes(x = time,
                y = ErrorAlpha)) +
  labs(title = "MB Alpha",
       x = "Time (hours)",
       y = "Concentration (umol/L)")

ggplot(data = MassBal) +
  geom_line(aes(x = time,
                y = MassBalBeta),
            color = "red") +
  geom_line(aes(x = time,
                y = ErrorBeta),
            color = "green") +
  labs(title = "MB Beta",
       x = "Time (hours)",
       y = "Concentration (umol/L)")

ggplot(data = MassBal) +
  geom_line(aes(x = time,
                y = MassBalSulfate),
            color = "orange") +
  geom_line(aes(x = time,
                y = ErrorSulfate),
            color = "blue") +
  labs(title = "MB Sulfate",
       x = "Time (hours)",
       y = "Concentration (umol/L)")

#------------------------Graphing plot-------------------------------------#

#select theme
my_theme <- function() {
  theme_minimal() +
    theme(
      text = element_text(size = 12, family = "sans"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold"),
      axis.line = element_line(color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_rect(fill = "white"),
      legend.position = "right",
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12, face = "bold")
    )
}

# Choose the compartment you want to plot (e.g., Cbr_X etc)
compartment_name <- "Cwp_A"
#Compartments can be added to calculate the sums of the different endosulfan compounds. 
#compartment_name2 <- "Ct_B" 
#compartment_name3 <- "Cbr_S" 

# Create a ggplot object to plot the concentration over time (same here: compartments can be plotted summed)
ggplot(data = sim_data_human, aes(x = time, y = .data[[compartment_name]] #+.data[[compartment_name2]] #+.data[[compartment_name3]]
)) +
  geom_line()        +
  labs(
    x = "Time (h)",
    y = "Concentration (umol/L)",
    title = paste("Endosulfan", compartment_name, "concentration")
  ) + 
  #scale_y_log10() +
  my_theme()

# y_values <- sim_data_human[[compartment_name]] #+ sim_data_human[[compartment_name2]]
# y_values
