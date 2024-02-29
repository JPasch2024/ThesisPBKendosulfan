# ------------------PBK MODEL ENDOSULFAN RATS-------------#
# ----------------SEPERATE ALPHA AND BETA ISOMERS, SULFATE METABOLITE---------#
#Jeroen Pasch, Wageningen University & Research 2023-01-11
#Input parameters can be found in "Input parameters Endosulfan.R". Needs to be defined first, so that they can be used in global environment as values
# PBK model output can automatically be compared to excising in vivo data in the file "Plotting in vivo data V2.R"

library(rxode2)
library(tidyverse)
library(ggplot2)
#install.packages("plotly")
#library(plotly)
#install.packages("FME")
library(FME)

amount.units               <-"umol"
time.units                 <-"h"

MW_Endosulfan              <- 406.93 # in g/mol
MW_Sulfate                 <- 422.95 # in g/mol

Oral_Dose_in_mg_bw_A       <- Oral_Dose_in_mg_bw * fraction_administered_ESA #fraction of oral endosulfan consisting of the alpha isomer
Oral_Dose_in_mg_bw_B       <- Oral_Dose_in_mg_bw * fraction_administered_ESB #fraction of oral endosulfan consisting of the beta isomer

Oral_Dose_A                <-(Oral_Dose_in_mg_bw_A * BW) / MW_Endosulfan * 1e+3       #The administered dose Alpha endosulfan in μmol
Oral_Dose_B                <-(Oral_Dose_in_mg_bw_B * BW) / MW_Endosulfan * 1e+3       #The administered dose Beta endosulfan in μmol

Oral_Dose_A_Absorbed      <- Oral_Dose_A 
Oral_Dose_B_Absorbed      <- Oral_Dose_B 

#----------------Physiological Parameters Male Humans-----------------------#
# tissue volumes in % of BW (fractional) (0.907)
#all taken from (ICRP, 2002) WP = heart + lung + Spleen + Gut, PP = skin + muscle + bone
VkiF <- 0.007 #kidneys tissue volume
VwpF <- 0.037 #Well perfused tissue volume
VppF <- 0.667 #poorly perfused tissue volume
VliF <- 0.034 #Liver tissue volume
VbrF <- 0.006 #Brain tissue volume
VfF <- 0.07 #Fat tissue volume
VtF <- 0.0012 #Testes tissue volume
VblF <- 0.074 #Blood volume

# blood flows in % Q (so relative to the total cardiac output)
#as found in (Brown 1997) Qppr = average (muscle, bone and skin) Qwpr = 1 - other fractions
QkiR <- 0.141 #relative kidneys blood flow
QwpR <- 0.1292 #relative well perfused tissue blood flow
QppR <- 0.458 #relative poorly perfused tissue blood flow
QliR <- 0.174 #relative liver blood flow
QbrR <- 0.02 #relative brain blood flow
QfR <- 0.07 #relative fat blood flow
QtR <- 0.0078 #relative testes blood flow (Plowchalk & Teeguarden, 2002)

# -----------------  Absorption parameters--------------------------#

#--------Endosulfan_Alpha------#
Ka_A <- 1.113 #Absorption rate constant for uptake in the Small intestine in per H
Kb_A <- 0  #biliary excretion rate constant (1/h)


#---------Endosulfan_Beta-----#
Ka_B <- 1.113 #Absorption rate constant for uptake in the Small intestine in per H
Kb_B <- 0 #biliary excretion rate constant (1/h)


#---------Endosulfan_Sulfate-----#
# biochemical parameters endosulfan sulfate
Ka_S <- 0.917 #Absorption rate constant for uptake in the Small intestine in per H (zal alleen maar relevant zijn door Billiary recirculation)
Kb_S <- 0 #Biliary excretion rate constant metabolites (1/h)

#--------------------------Excretion parameters------------------------------
#Excretion from kidney via glomerular filtration
#Glomerular filtration
GFR <- 5.2  #(mL/min/kg BW), Walton et al., 2004
# unbound fraction based on QIVIVETOOLS.WUR.NL
uf_A <- 0.054 #fraction of endosulfan alpha not bound to plasma proteins
uf_B <- 0.067 #fraction of endosulfan beta not bound to plasma proteins
uf_S <- 0.064 #fraction of endosulfan sulfate not bound to plasma proteins
GF <- GFR / 1000 * BW * 60 # (l/h), rat glomerular filtration rate

#excretion from the GI tract
GER <- 6  #transit time stomach so 1/0.05 -> Gastric empyting rate rats 
KtSI <- 4.76 #Tsi rats. 88 min (Grandoni et al. 2019) N=7 N/1.47 -> 4.76 /h (transit rate constant small intestine)
KtCol <- 0.263 #1/h  transit time colon 228 minutes(Grandoni et al. 2019) (3.8 hours, so rate of 0.26)

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
Q <- 15 * BW^0.74; #total cardiac output (L/h) #Brown et al. 1997
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
Pki_A <- 5.07 #PC kidney
Pwp_A <- 7.16  #PC well perfused tissues
Ppp_A <- 4.65  #PC poorly perfused tissues
Pli_A <- 5.53 #PC liver
Pbr_A <- 13.02 #PC brain
Pf_A <- 37.35 #PC fat
Pt_A <- 2.28 #PC testes

#---------Endosulfan_Beta-----#
# partition coefficient (PC) tissue:blood endosulfan (B) (unitless)
Pki_B <- 4.32 #PC kidney
Pwp_B <- 6.12 #PC well perfused tissues
Ppp_B <- 3.96 #PC poorly perfused tissues
Pli_B <- 4.69 #PC liver
Pbr_B <- 10.97 #PC brain
Pf_B <- 27.31 #PC fat
Pt_B <- 2.00 #PC testes

#---------Endosulfan_Sulfate-----#
# partition coefficient (PC) tissue:blood endosulfan Sulfate (S) (unitless)
Pki_S <- 4.45 #PC kidney
Pwp_S <- 6.33 #PC well perfused tissues
Ppp_S <- 4.08 #PC poorly perfused tissues
Pli_S <- 4.85 #PC liver
Pbr_S <- 11.35 #PC brain
Pf_S <- 29.01 #PC fat
Pt_S <- 2.04 #PC testes

#-------------Metabolic parameters--------------#
#--------General Metabolic data-------#
Microsomalprotein_li <- 32 #microsomal protein (mg) per g of rat liver ()

#ESA metabolic parameters
#in vitro rat data metabolism
Vmax_A_nmol_min_mg_protein <- 3.71
   #maximum metabolic rate umol ESA/h/mg rat liver microsomes
Vmax_A <- Vmax_A_nmol_min_mg_protein * (Microsomalprotein_li * Vli*1000)*60/1000 #umol/h
#liver weight in g (Vli in grams) * scaling factor mg to umol/h. Times 60 to obtain hourly rate. Divided to nmol -> umol
Km_A <- 0.7911  #uM

#ESB metabolic parameters 
#ESB assumed to follow same connection to ESA as seen in human in vitro metabolic parameters data
Vmax_B <- Vmax_A / 0.336339044 #umol/h
Km_B <- Km_A / 1.1523 #uM

# No data available, for now assumes same as ESA metabolism. 
Vmax_S  <- Vmax_A #TEMP  DATA IS LACKING. 
Km_S <- Km_A #TEMP


# Create a data frame with all parameters
parameters <- data.frame(
  Pki_A, Pwp_A, Ppp_A, Pli_A, Pbr_A, Pf_A, Pt_A, Ka_A, Kb_A, Vmax_A, Km_A,
  Pki_B, Pwp_B, Ppp_B, Pli_B, Pbr_B, Pf_B, Pt_B, Ka_B, Kb_B, Vmax_B, Km_B,
  Pki_S, Pwp_S, Ppp_S, Pli_S, Pbr_S, Pf_S, Pt_S, Ka_S, Kb_S, Vmax_S, Km_S,
  GF, uf_A, uf_B, uf_S, Vki, Vwp, Vpp, Vli, Vbr, Vf, Vt, Vbl, Q, Qki, Qwp, Qpp, Qli, Qbr, Qf, Qt, GER, KtSI, KtCol
)

# Check the length of the data frame
#print(length(parameters))

#view(parameters)

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
    #amount metabolized from Endosulfan into Metabolite sulfate (S) in liver
    
    #KIJKEN HOE HET ZIT MET TOTALE METABOLITES AND SPECIFIEK SULFATE VOOR HET MODEL
    #totale metabolisme (uit de isomeren modellen)
    #AMR_A <- (Vmax_A * (Cli_A*uf_A)) / (Km_A + (Cli_A*uf_A));#inclusion uf for metabolism
    AMR_A <- (Vmax_A * Cli_v_A) / (Km_A + Cli_v_A);
    AMR_B <- (Vmax_B * Cli_v_B) / (Km_B + Cli_v_B);
    #metabolisme in sulfaat (input sulfaat model)
    # AMR_AS
    #Misschien zoiets als simpels as een factor, van het percentage ESA -> ESS bijvoorbeeld 40% (factor <- 0.4) en dat AMR_A & factor doen bij input sulfaat
    
    #AMR_S (ADD when I have these, furhter metabolism of ES sulfate)
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
    d/dt(Ast_S) <- -GER * Ast_S;
    
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
    
    d/dt(Ain1_A) <- GER * Ast_A - KtSI * Ain1_A - Ka_A*Ain1_A; #+ Kb_A * Ali_A;    # Duodenum, bile acids secreted in duodenum
    d/dt(Ain2_A) <- KtSI * Ain1_A - KtSI * Ain2_A - Ka_A*Ain2_A;  # Jejunum
    d/dt(Ain3_A) <- KtSI * Ain2_A - KtSI * Ain3_A - Ka_A*Ain3_A;  # Jejunum
    d/dt(Ain4_A) <- KtSI * Ain3_A - KtSI * Ain4_A - Ka_A*Ain4_A;  # Ileum 
    d/dt(Ain5_A) <- KtSI * Ain4_A - KtSI * Ain5_A - Ka_A*Ain5_A;  # Ileum
    d/dt(Ain6_A) <- KtSI * Ain5_A - KtSI * Ain6_A - Ka_A*Ain6_A;  # Ileum
    d/dt(Ain7_A) <- KtSI * Ain6_A - KtSI * Ain7_A - Ka_A*Ain7_A;  # Ileum, bile acids reabsorbed in ileum
    d/dt(Acol_A) <- KtSI * Ain7_A - KtCol * Acol_A;               # No absorption in the colon (bile acids reabsorbed in ileum)
    d/dt(Afec_A) <- KtCol * Acol_A;                                 # Amount in feces
    
    d/dt(Ain1_B) <- GER * Ast_B - KtSI * Ain1_B - Ka_B*Ain1_B + Kb_B * Ali_B;    # Duodenum, bile acids secreted in duodenum
    d/dt(Ain2_B) <- KtSI * Ain1_B - KtSI * Ain2_B - Ka_B*Ain2_B;  # Jejunum
    d/dt(Ain3_B) <- KtSI * Ain2_B - KtSI * Ain3_B - Ka_B*Ain3_B;  # Jejunum
    d/dt(Ain4_B) <- KtSI * Ain3_B - KtSI * Ain4_B - Ka_B*Ain4_B;  # Ileum 
    d/dt(Ain5_B) <- KtSI * Ain4_B - KtSI * Ain5_B - Ka_B*Ain5_B;  # Ileum
    d/dt(Ain6_B) <- KtSI * Ain5_B - KtSI * Ain6_B - Ka_B*Ain6_B;  # Ileum
    d/dt(Ain7_B) <- KtSI * Ain6_B - KtSI * Ain7_B - Ka_B*Ain7_B;  # Ileum, bile acids reabsorbed in ileum
    d/dt(Acol_B) <- KtSI * Ain7_B - KtCol * Acol_B;               # No absorption in the colon (bile acids reabsorbed in ileum)
    d/dt(Afec_B) <- KtCol * Acol_B;                                 # Amount in feces
    
    d/dt(Ain1_S) <- GER * Ast_S - KtSI * Ain1_S - Ka_S*Ain1_S + Kb_S * Ali_S;    # Duodenum, bile acids secreted in duodenum
    d/dt(Ain2_S) <- KtSI * Ain1_S - KtSI * Ain2_S - Ka_S*Ain2_S;  # Jejunum
    d/dt(Ain3_S) <- KtSI * Ain2_S - KtSI * Ain3_S - Ka_S*Ain3_S;  # Jejunum
    d/dt(Ain4_S) <- KtSI * Ain3_S - KtSI * Ain4_S - Ka_S*Ain4_S;  # Ileum 
    d/dt(Ain5_S) <- KtSI * Ain4_S - KtSI * Ain5_S - Ka_S*Ain5_S;  # Ileum
    d/dt(Ain6_S) <- KtSI * Ain5_S - KtSI * Ain6_S - Ka_S*Ain6_S;  # Ileum
    d/dt(Ain7_S) <- KtSI * Ain6_S - KtSI * Ain7_S - Ka_S*Ain7_S;  # Ileum, bile acids reabsorbed in ileum
    d/dt(Acol_S) <- KtSI * Ain7_S - KtCol * Acol_S;               # No absorption in the colon (bile acids reabsorbed in ileum)
    d/dt(Afec_S) <- KtCol * Acol_S;                                 # Amount in feces
    
    
    
    #amount in blood
    # Here the amount in blood is calculated by adding up the amount of compound in the venous blood of all compartments 
    # (multiply tissue blood flow by concentration in venous blood)
    # From this amount, the concentration that was already in the blood is subtracted (Summed blood flows multiplied by the Concentration in blood)
    
    # For Endosulfan_Alpha
    d/dt(Abl_A) <- (Ka_A*Ain1_A + Ka_A*Ain2_A + Ka_A*Ain3_A + Ka_A*Ain4_A + Ka_A*Ain5_A + Ka_A*Ain6_A + Ka_A*Ain7_A) + ((Qli * (Cli_v_A - Cbl_A)) + (Qki * (Cki_v_A - Cbl_A)) + (Qbr * (Cbr_v_A - Cbl_A)) + (Qf * (Cf_v_A - Cbl_A)) + (Qt * (Ct_v_A - Cbl_A)) + (Qwp * (Cwp_v_A - Cbl_A)) + (Qpp * (Cpp_v_A - Cbl_A)));
    
    # For Endosulfan_Beta
    d/dt(Abl_B) <- (Ka_B*Ain1_B + Ka_B*Ain2_B + Ka_B*Ain3_B + Ka_B*Ain4_B + Ka_B*Ain5_B + Ka_B*Ain6_B + Ka_B*Ain7_B) + ((Qli * (Cli_v_B - Cbl_B)) + (Qki * (Cki_v_B - Cbl_B)) + (Qbr * (Cbr_v_B - Cbl_B)) + (Qf * (Cf_v_B - Cbl_B)) + (Qt * (Ct_v_B - Cbl_B)) + (Qwp * (Cwp_v_B - Cbl_B)) + (Qpp * (Cpp_v_B - Cbl_B)));
    
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
    
    #amount absorbed into liver
   #Uptake_A <- (Ka_A*Ain1_A + Ka_A*Ain2_A + Ka_A*Ain3_A + Ka_A*Ain4_A + Ka_A*Ain5_A + 
       #Ka_A*Ain6_A + Ka_A*Ain7_A)
    #Amount in liver   
    #d/dt(Ali_A) <- (Ka_A*Ain1_A + Ka_A*Ain2_A + Ka_A*Ain3_A + Ka_A*Ain4_A + Ka_A*Ain5_A +
                      #Ka_A*Ain6_A + Ka_A*Ain7_A) + Qli * (Cbl_A - Cli_v_A) - AMR_A - Kb_A * Ali_A;
    d/dt(Ali_A) <- Qli * (Cbl_A - Cli_v_A) - AMR_A - Kb_A * Ali_A;
    d/dt(Ali_B) <- Qli * (Cbl_B - Cli_v_B) - AMR_B - Kb_B * Ali_B;
    d/dt(Ali_S) <-(Ka_S*Ain1_S + Ka_S*Ain2_S + Ka_S*Ain3_S + Ka_S*Ain4_S + Ka_S*Ain5_S + Ka_S*Ain6_S + Ka_S*Ain7_S - Kb_S * Ali_S) + Qli * (Cbl_S - Cli_v_S) + AMR_A + AMR_B - AMR_S;
    #AMR_A should be replaced by AMR_A specifically metabolised into ESS, or with addition of a fraction (e.g. AMR_A * 0.4 for 40%)
    # Assumption all A and B is metabolised into S (just as Lee et al. (2006) assumed)
    #AMR_S "
    
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

#Single oral dose
if (nbr.doses == 1) {
ev <- eventTable(amount.units = amount.units, time.units = time.units) %>%
  et(dose = Oral_Dose_A_Absorbed, cmt = "Ast_A") %>%
  et(dose = Oral_Dose_B_Absorbed, cmt = "Ast_B") %>%
  et(seq(from = time.0, to = time.end, by = time.frame))
} else {
ev <- eventTable(amount.units = amount.units, time.units = time.units) %>%
  et(dose = Oral_Dose_A_Absorbed, cmt = "Ast_A", ii = interval, nbr.doses = nbr.doses) %>%
  et(dose = Oral_Dose_B_Absorbed, cmt = "Ast_B", ii = interval, nbr.doses = nbr.doses) %>%
  et(seq(from = time.0, to = time.end, by = time.frame))
}
#
# ev <- eventTable(amount.units = amount.units, time.units = time.units) %>%
#   et(dose = Oral_Dose_A_Absorbed, cmt = "Abl_A", nbr.doses = nbr.doses) %>%
#   et(dose = Oral_Dose_B_Absorbed, cmt = "Abl_B", nbr.doses = nbr.doses) %>%
#   et(seq(from = time.0, to = time.end, by = time.frame))
 solve.pbk <- solve(model2, parameters, events = ev, inits, cores = 4)

#View(solve.pbk)

# Extract the simulated data from the solve.pbk object
sim_data_Rats <- as.data.frame(solve.pbk)


#-----------------------------------Mass Balance-------------------------------------------#
colsAlpha <-c("Ast_A", "Ain1_A", "Ain2_A", "Ain3_A", "Ain4_A", "Ain5_A", "Ain6_A", "Ain7_A", "Acol_A", "Afec_A", "Ali_A", "Aki_A", "Awp_A", "App_A", "Abr_A", "At_A", "Af_A", "Abl_A", "TExcretion_ki_A", "TAMR_A")
colsBeta <-c("Ast_B","Ain1_B", "Ain2_B", "Ain3_B", "Ain4_B", "Ain5_B", "Ain6_B", "Ain7_B", "Acol_B", "Afec_B", "Ali_B", "Aki_B", "Awp_B", "App_B", "Abr_B", "At_B", "Af_B", "Abl_B", "TExcretion_ki_B", "TAMR_B")
colsSulfate <-c("Ast_S","Ain1_S", "Ain2_S", "Ain3_S", "Ain4_S", "Ain5_S", "Ain6_S", "Ain7_S", "Acol_S", "Afec_S", "Ali_S", "Aki_S", "Awp_S", "App_S", "Abr_S", "At_S", "Af_S", "Abl_S", "TExcretion_ki_S", "TAMR_S")
colsAll <- c("Ast_A","Ast_B","Ast_S","Ali_A", "Aki_A", "Awp_A", "App_A", "Abr_A", "At_A", "Af_A", "Abl_A",
             "Ali_B", "Aki_B", "Awp_B", "App_B", "Abr_B", "At_B", "Af_B", "Abl_B",
             "Ali_S", "Aki_S", "Awp_S", "App_S", "Abr_S", "At_S", "Af_S", "Abl_S",
             "Ain1_A", "Ain2_A", "Ain3_A", "Ain4_A", "Ain5_A", "Ain6_A", "Ain7_A", "Acol_A", "Afec_A", "Ain1_B", "Ain2_B", "Ain3_B", "Ain4_B", "Ain5_B", "Ain6_B", "Ain7_B", "Acol_B", "Afec_B", "Ain1_S", "Ain2_S", "Ain3_S", "Ain4_S", "Ain5_S", "Ain6_S", "Ain7_S", "Acol_S", "Afec_S",
             "TExcretion_ki_A", "TExcretion_ki_B", "TExcretion_ki_S", "TAMR_S")

CalcBodAll <- rowSums(sim_data_Rats[, colsAll])
CalcBodAlpha <- rowSums(sim_data_Rats[, colsAlpha])
CalcBodBeta <- rowSums(sim_data_Rats[, colsBeta])
CalcBodSulfate <- rowSums(sim_data_Rats[, colsSulfate])
# Calculate what is goes in. This is the administered amount of compound, and for the sulfate submodel, this is the amount metabolized.

TotBodAll <- Oral_Dose_A_Absorbed + Oral_Dose_B_Absorbed #Amount administered (in umol)
TotBodAlpha <- Oral_Dose_A_Absorbed
TotBodBeta <- Oral_Dose_B_Absorbed
TotBodSulfate <- sim_data_Rats[, "TAMR_A"] + sim_data_Rats[, "TAMR_B"]  # Amount Sulfate metabolized from Alpha & Beta

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
MassBal  <- cbind("time" = sim_data_Rats[,"time"], MassBall, ErrorAll, MassBalAlpha, ErrorAlpha, MassBalBeta, 
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

compartment_name <- "Ct_A"
compartment_name2 <- "Ct_B" #Compartments can be added to calculate the sums of the different endosulfan compounds. 

# # Create a ggplot object to plot the concentration over time (same here: compartments can be plotted summed)
ggplot(data = sim_data_Rats, aes(x = time, y = .data[[compartment_name]] +.data[[compartment_name2]]
)) +
  geom_line() +
  labs(
    x = "Time (h)",
    y = "Conc. Endosulfan (umol/L)",
    title = paste(" Concentration of", compartment_name, "over Time")
  ) +
  my_theme()

# #inspect summed data (e.g. endosulfan as ESA + ESB)
y_values <- sim_data_Rats[[compartment_name]] + sim_data_Rats[[compartment_name2]]
y_values

# # # Create a ggplot object to plot the concentration over time (same here: compartments can be plotted summed)
# ggplot(data = sim_data_Rats, aes(x = time, y = .data[[compartment_name]] #+.data[[compartment_name2]]
# )) +
#   geom_line() +
#   labs(
#     x = "Time (h)",
#     y = "Amount ESA metabolised (umol)",
#     title = paste("Amount of", compartment_name, "over Time")
#   )

#--------------- Compartments defined for in vivo experimental data-----------------#
compartment_name_Ct_A <- "Ct_A"
compartment_name_Ct_B <- "Ct_B"
compartment_name_Cbr_A <- "Cbr_A"
compartment_name_Cbr_B <- "Cbr_B"
compartment_name_Cf_A <- "Cf_A"
compartment_name_Cf_B <- "Cf_B"
compartment_name_Cli_A <- "Cli_A"
compartment_name_Cli_B <- "Cli_B"
compartment_name_Cli_S <- "Cli_S"
compartment_name_Cki_A <- "Cki_A"
compartment_name_Cki_B <- "Cki_B"
compartment_name_Cki_S <- "Cki_S"
compartment_name_Cbl_A <- "Cbl_A"
compartment_name_Cbl_B <- "Cbl_B"
compartment_name_Cbl_S <- "Cbl_S"


# #--------------------------Local Sensitivity Analysis-----------------------#
# 
# LSArun <- function(parms){
#   solve(model2, parms, inits=inits, events=ev, cores=4) # This is the solve function I use in the LSA?
# }
# # the model parameters are as a date.frame (should be vector or list) (some issues with these...)
# LSAfun <- function(parameters){
#   pars <- parameters[1,]
#   names(pars) <- colnames(parameters)
# 
# 
#   lsa <- sensFun(LSArun,
#                  parms = pars,
#                  sensvar = c("Cbl_B"), #Output variable I want testes; What do I want testes? Cmax of blood conc standard?
#                  senspar = names(pars),
#                  varscale = NULL,
#                  parscale = NULL,
#                  tiny = 0.01 # change parameter by 1%
#   )
#   #plot(summary(lsa)[order(summary(lsa)$Mean, decreasing = TRUE),]$Mean)
#   #max(summary(lsa)$Mean)
# 
#   Tmax <- solve.pbk[which.max(solve.pbk$Cbl_B), "time"] #ModelOutput? Should it be as.data.frame(solve.pbk) for my model?
# 
#   lsa <- dplyr::filter(lsa, x == Tmax)
#   lsa <- t(lsa)
#   lsa <- as.data.frame(lsa[3:nrow(lsa), ])
#   colnames(lsa) <- c("Values")
#   lsa[,1] <- as.numeric(lsa[,1])
# 
# 
#   lsa <- filter(lsa, lsa[,1] < -0.1 | lsa[,1] > 0.1) # exclude all small changes #SO SMALL IMPACT OUTPUT, MAKES SENSE
#   #lsa <- filter(lsa, rownames(lsa) != "TOP") # exclude TOP #EXPLAIN
# 
#   lsa <- data.frame("parameter" <- rownames(lsa),
#                     "Value" <- lsa[,1]
#   )
#   lsaOrder <- order(abs(lsa[,2]), decreasing = TRUE)
#   labelOrder <- lsa[lsaOrder,1]
# 
#   lsa <- lsa[lsaOrder,]
#   rownames(lsa) <- 1:nrow(lsa)
#   colnames(lsa) <- c("Parameter", "Value")
# 
#   clipr::write_clip(lsa)
# 
#   # Install and load ggplot2 if not already installed
#   # install.packages("ggplot2")
#   library(ggplot2)
# 
#   # Create the ggplot
#   figure <- ggplot(data = lsa, aes(x = Parameter, y = Value)) +
#     geom_bar(stat = "identity") +
#     ggtitle("Local sensitivity analysis") +
#     theme_classic() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
# 
#   # Print the ggplot
#   print(figure)
#   print("hey")
#   return(lsa)
# }
# localSensAnalysis <- LSAfun(parameters)
# print(localSensAnalysis)


#----------------------- Data export Excel---------------------# 
#install.packages("writexl") 
#library(writexl) 
#write_xlsx(x = sim_data_Rats, path = "sim_data_Rats.xlsx", col_names = TRUE)
#download.packages(openxlsx)
#library(openxlsx)

# # Extract the time and output columns
# output_data <- data.frame(Time = sim_data_Rats$time, Output = sim_data_Rats$Cli_A)
# 
# # Write the data to an Excel file
# write.xlsx(output_data, file = "output8.csv", rowNames = FALSE)
# 
# # Display a message indicating the process is complete
# cat("Data exported")

#str(sim_data_Rats)
