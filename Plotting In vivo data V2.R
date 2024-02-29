#--------------------------Plotting in vivo Experimental Data ENDOSULFAN PBK MODEL-------------#


# Install and load required packages
library(readxl)
library(ggplot2)
library(patchwork)

# Select excel files and sheets
excelfile <- "C:/Users/jljpa/OneDrive - Wageningen University & Research/Master Thesis/In vivo experimental data endosulfan.xlsx"

sheet_Gupta_1978 <- "Gupta, 1978"
sheet_Nath_1978 <- "Nath, 1978"
sheet_Dikshith_1984 <- "Dikshith, 1984"
sheet_Ansari_1984 <- "Ansari, 1984" 
sheet_ChanandMohd_2005 <- "Chan and Mohd, 2005"
sheet_LeistandMayer_1987 <- "Leist and Mayer, 1987"

# Read data from the specified sheet
data_Gupta_1987 <- read_excel(excelfile, sheet = sheet_Gupta_1978)
data_Nath_1978 <- read_excel(excelfile, sheet = sheet_Nath_1978)
data_Dikshith_1984 <- read_excel(excelfile, sheet = sheet_Dikshith_1984)
data_Ansari_1984 <- read_excel(excelfile, sheet = sheet_Ansari_1984)
data_ChanandMohd_2005 <- read_excel(excelfile, sheet = sheet_ChanandMohd_2005)
data_LeistandMayer_1987 <- read_excel(excelfile, sheet = sheet_LeistandMayer_1987)

#----------------------------------------Ansari,1984 (Part I (7.5))------------------------------#
#COMPARING MODEL OUTPUT WITH IN VIVO DATA FOR TESTES, KIDNEY, LIVER AND BRAIN
# 60 DAILY DOSES OF 7.5 mg/kg/day
if (nbr.doses == 60 && Oral_Dose_in_mg_bw == 7.5) {
  #--------------------------Plot Ansari 1984: Testes -------------#
  # Alpha 7.5 mg/kg Bw/ day 
  plot1 <- ggplot() +
    geom_point(data = data_Ansari_1984[3,], color = "blue",
               aes(x = time,
                   y = TestisuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Ct_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted testes concentration Alpha - Ansari", x = "Hours", y = "concentration umol l^-1")  +
    my_theme()
  
  # Beta 7.5 mg/kg Bw/ day 
  plot2 <- ggplot() +
    geom_point(data = data_Ansari_1984[4,], color = "blue",
               aes(x = time,
                   y = TestisuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Ct_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted testes concentration Beta - Ansari", x = "Hours", y = "concentration umol l^-1")  +
    my_theme()
  
  #--------------------------Plot Ansari 1984: Kidney -------------#
  # Alpha 7.5 mg/kg Bw/ day 
  plot3 <- ggplot() +
    geom_point(data = data_Ansari_1984[3,], color = "#FFD700",
               aes(x = time,
                   y = KidneyuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cki_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Kidney concentration Alpha - Ansari", x = "Hours", y = "concentration umol l^-1")  +
    my_theme()
  
  # Beta 7.5 mg/kg Bw/ day 
  plot4 <- ggplot() +
    geom_point(data = data_Ansari_1984[4,], color = "#FFD700",
               aes(x = time,
                   y = KidneyuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cki_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Kidney concentration Beta - Ansari", x = "Hours", y = "concentration umol l^-1")  +
    my_theme()
  
  #--------------------------Plot Ansari 1984: Liver -------------#
  # Alpha 7.5 mg/kg Bw/ day 
  plot5 <- ggplot() +
    geom_point(data = data_Ansari_1984[3,],  color = "purple",
               aes(x = time,
                   y = LiveruM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cli_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Liver concentration Alpha - Ansari", x = "Hours", y = "concentration umol l^-1") +
    my_theme()
  
  # Beta 7.5 mg/kg Bw/ day 
  plot6 <- ggplot() +
    geom_point(data = data_Ansari_1984[4,],  color = "purple",
               aes(x = time,
                   y = LiveruM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cli_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Liver concentration Beta - Ansari", x = "Hours", y = "concentration umol l^-1") +
    my_theme()
  
  #--------------------------Plot Ansari 1984: Brain -------------#
  # Alpha 7.5 mg/kg Bw/ day 
  plot7 <- ggplot() +
    geom_point(data = data_Ansari_1984[3,],  color = "hotpink",
               aes(x = time,
                   y = BrainuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbr_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Brain concentration Alpha - Ansari", x = "Hours", y = "concentration umol l^-1") +
    my_theme()
  
  # Beta 7.5 mg/kg Bw/ day 
  plot8 <- ggplot() +
    geom_point(data = data_Ansari_1984[4,],  color = "hotpink",
               aes(x = time,
                   y = BrainuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbr_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Brain concentration Beta - Ansari", x = "Hours", y = "concentration umol l^-1") +
    my_theme()
  
  # Display the plots
  print(plot1)
  print(plot2)
  print(plot3)
  print(plot4)
  print(plot5)
  print(plot6)
  print(plot7)
  print(plot8)
}


#----------------------------------------Ansari,1984 (Part II (2.5))------------------------------#
#COMPARING MODEL OUTPUT WITH IN VIVO DATA FOR TESTES, KIDNEY, LIVER AND BRAIN
#60 DAILY DOSES OF 2.5 mg/kg/day
# Check conditions before plotting
if (nbr.doses == 60 && Oral_Dose_in_mg_bw == 2.5) {
  #--------------------------Plot Ansari 1984: Testes -------------#
  # Alpha 2.5 mg/kg Bw/ day 
  plot1 <- ggplot() +
    geom_point(data = data_Ansari_1984[1,], color = "blue",
               aes(x = time,
                   y = TestisuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Ct_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted testes concentration Alpha", x = "Hours", y = "concentration umol l^-1")
  
  # Beta 2.5 mg/kg Bw/ day 
  plot2 <- ggplot() +
    geom_point(data = data_Ansari_1984[2,], color = "blue",
               aes(x = time,
                   y = TestisuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Ct_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted testes concentration Beta", x = "Hours", y = "concentration umol l^-1")
  
  #--------------------------Plot Ansari 1984: Kidney -------------#
  # Alpha 2.5 mg/kg Bw/ day 
  plot3 <- ggplot() +
    geom_point(data = data_Ansari_1984[1,], color = "#FFD700",
               aes(x = time,
                   y = KidneyuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cki_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Kidney concentration Alpha", x = "Hours", y = "concentration umol l^-1")
  
  # Beta 2.5 mg/kg Bw/ day 
  plot4 <- ggplot() +
    geom_point(data = data_Ansari_1984[2,], color = "#FFD700",
               aes(x = time,
                   y = KidneyuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cki_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Kidney concentration Beta", x = "Hours", y = "concentration umol l^-1")
  
  #--------------------------Plot Ansari 1984: Liver-------------#
  # Alpha 2.5 mg/kg Bw/ day 
  plot5 <- ggplot() +
    geom_point(data = data_Ansari_1984[1,],  color = "purple",
               aes(x = time,
                   y = LiveruM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cli_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Liver concentration Alpha", x = "Hours", y = "concentration umol l^-1")
  
  # Beta 2.5 mg/kg Bw/ day 
  plot6 <- ggplot() +
    geom_point(data = data_Ansari_1984[2,],  color = "purple",
               aes(x = time,
                   y = LiveruM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cli_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Liver concentration Beta", x = "Hours", y = "concentration umol l^-1")
  #--------------------------Plot Ansari 1984: Brain-------------#
  # Alpha 2.5 mg/kg Bw/ day 
  plot7 <- ggplot() +
    geom_point(data = data_Ansari_1984[1,],  color = "hotpink",
               aes(x = time,
                   y = BrainuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbr_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Brain concentration Alpha", x = "Hours", y = "concentration umol l^-1")
  # Beta 2.5 mg/kg Bw/ day 
  plot8 <- ggplot() +
    geom_point(data = data_Ansari_1984[2,],  color = "hotpink",
               aes(x = time,
                   y = BrainuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbr_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Brain concentration Beta", x = "Hours", y = "concentration umol l^-1")
  
  # Display the plots
  print(plot1)
  print(plot2)
  print(plot3)
  print(plot4)
  print(plot5)
  print(plot6)
  print(plot7)
  print(plot8)
}
#------------------------------Dikshith, 1984 (Part I (1.5 mg/kg Female))------------------------------#
#COMPARING MODEL OUTPUT WITH IN VIVO DATA FOR BlOOD, FAT TISSUE, KIDNEY, LIVER AND BRAIN
#30 DAILY DOSES OF 1.5 mg/kg/day
# Check conditions before plotting
#colnames(data_Dikshith_1984)

if (nbr.doses == 30 && Oral_Dose_in_mg_bw == 1.5) {
  #--------------------------Plot Dikshith 1984: Blood Serum -------------#
  # Alpha 1.5 mg/kg Bw/ day 
  plot1 <- ggplot() +
    geom_point(data = data_Dikshith_1984[3,], color = "red",
               aes(x = time,
                   y = BlooduM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbl_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Blood concentration Alpha", x = "Hours", y = "concentration umol l^-1")
  
  # Beta 1.5 mg/kg Bw/ day 
  plot2 <- ggplot() +
    geom_point(data = data_Dikshith_1984[4,], color = "red",
               aes(x = time,
                   y = BlooduM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbl_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Blood concentration Beta", x = "Hours", y = "concentration umol l^-1")
  
  #--------------------------Plot Dikshith 1984: Kidney -------------#
  # Alpha 1.5 mg/kg Bw/ day 
  plot3 <- ggplot() +
    geom_point(data = data_Dikshith_1984[3,], color = "#FFD700",
               aes(x = time,
                   y = KidneyuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cki_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Kidney concentration Alpha", x = "Hours", y = "concentration umol l^-1")
  
  # Beta 1.5 mg/kg Bw/ day 
  plot4 <- ggplot() +
    geom_point(data = data_Dikshith_1984[4,], color = "#FFD700",
               aes(x = time,
                   y = KidneyuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cki_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Kidney concentration Beta", x = "Hours", y = "concentration umol l^-1")
  
  #--------------------------Plot Dikshith 1984: Liver -------------#
  # Alpha 1.5 mg/kg Bw/ day 
  plot5 <- ggplot() +
    geom_point(data = data_Dikshith_1984[3,],  color = "purple",
               aes(x = time,
                   y = LiveruM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cli_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Liver concentration Alpha", x = "Hours", y = "concentration umol l^-1") +
    my_theme()
  
  # Beta 1.5 mg/kg Bw/ day 
  plot6 <- ggplot() +
    geom_point(data = data_Dikshith_1984[4,], color = "purple",
               aes(x = time,
                   y = LiveruM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cli_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Liver concentration Beta", x = "Hours", y = "concentration umol l^-1")
  
  #--------------------------Plot Dikshith 1984: Brain -------------#
  # Alpha 1.5 mg/kg Bw/ day 
  plot7 <- ggplot() +
    geom_point(data = data_Dikshith_1984[3,],  color = "hotpink",
               aes(x = time,
                   y = BrainuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbr_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Brain concentration Alpha", x = "Hours", y = "concentration umol l^-1")
  
  # Beta 1.5 mg/kg Bw/ day 
  plot8 <- ggplot() +
    geom_point(data = data_Dikshith_1984[4,],  color = "hotpink",
               aes(x = time,
                   y = BrainuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbr_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Brain concentration Beta", x = "Hours", y = "concentration umol l^-1")
  
  #--------------------------Plot Dikshith 1984: Fat -------------#
  # Alpha 1.5 mg/kg Bw/ day 
  plot9 <- ggplot() +
    geom_point(data = data_Dikshith_1984[3,], color = "orange",
               aes(x = time,
                   y = FatuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cf_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Fat concentration Alpha", x = "Hours", y = "concentration umol l^-1")
  # Beta 1.5 mg/kg Bw/ day 
  plot10 <- ggplot() +
    geom_point(data = data_Dikshith_1984[4,],  color = "orange",
               aes(x = time,
                   y = FatuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cf_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Fat concentration Beta", x = "Hours", y = "concentration umol l^-1")
  
  # Display the plots
  print(plot1)
  print(plot2)
  print(plot3)
  print(plot4)
  print(plot5)
  print(plot6)
  print(plot7)
  print(plot8)
  print(plot9)
  print(plot10)
}

#------------------------------Dikshith, 1984 (Part II (5 mg/kg Male))------------------------------#
#COMPARING MODEL OUTPUT WITH IN VIVO DATA FOR BlOOD, FAT TISSUE, KIDNEY, LIVER AND BRAIN
#30 DAILY DOSES OF 5 mg/kg/day
# Check conditions before plotting
if (nbr.doses == 30 && Oral_Dose_in_mg_bw == 5.0) {
  #--------------------------Plot Dikshith 1984: Blood Serum -------------#
  # Alpha 5.0 mg/kg Bw/ day 
  plot1 <- ggplot() +
    geom_point(data = data_Dikshith_1984[1,], color = "red",
               aes(x = time,
                   y = BlooduM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbl_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Blood concentration Alpha", x = "Hours", y = "concentration umol l^-1") +
    scale_y_log10() +
    my_theme()
  
  # Beta 5.0 mg/kg Bw/ day 
  plot2 <- ggplot() +
    geom_point(data = data_Dikshith_1984[2,], color = "red",
               aes(x = time,
                   y = BlooduM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbl_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Blood concentration Beta", x = "Hours", y = "concentration umol l^-1")
  
  #--------------------------Plot Dikshith 1984: Kidney -------------#
  # Alpha 5.0 mg/kg Bw/ day 
  plot3 <- ggplot() +
    geom_point(data = data_Dikshith_1984[1,], color = "#FFD700",
               aes(x = time,
                   y = KidneyuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cki_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Kidney concentration Alpha", x = "Hours", y = "concentration umol l^-1")
  
  # Beta 5.0 mg/kg Bw/ day 
  plot4 <- ggplot() +
    geom_point(data = data_Dikshith_1984[2,], color = "#FFD700",
               aes(x = time,
                   y = KidneyuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cki_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Kidney concentration Beta", x = "Hours", y = "concentration umol l^-1")
  
  #--------------------------Plot Dikshith 1984: Liver -------------#
  # Alpha 5.0 mg/kg Bw/ day 
  plot5 <- ggplot() +
    geom_point(data = data_Dikshith_1984[1,], color = "purple",
               aes(x = time,
                   y = LiveruM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cli_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Liver concentration Alpha", x = "Hours", y = "concentration umol l^-1")+
    my_theme()
  
  # Beta 5.0 mg/kg Bw/ day 
  plot6 <- ggplot() +
    geom_point(data = data_Dikshith_1984[2,],  color = "purple",
               aes(x = time,
                   y = LiveruM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cli_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Liver concentration Beta", x = "Hours", y = "concentration umol l^-1")
  
  #--------------------------Plot Dikshith 1984: Brain -------------#
  # Alpha 5.0 mg/kg Bw/ day 
  plot7 <- ggplot() +
    geom_point(data = data_Dikshith_1984[1,], color = "hotpink",
               aes(x = time,
                   y = BrainuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbr_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Brain concentration Alpha", x = "Hours", y = "concentration umol l^-1")
  
  # Beta 5.0 mg/kg Bw/ day 
  plot8 <- ggplot() +
    geom_point(data = data_Dikshith_1984[2,],  color = "hotpink",
               aes(x = time,
                   y = BrainuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbr_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Brain concentration Beta", x = "Hours", y = "concentration umol l^-1")
  
  #--------------------------Plot Dikshith 1984: Fat -------------#
  # Alpha 5.0 mg/kg Bw/ day 
  plot9 <- ggplot() +
    geom_point(data = data_Dikshith_1984[1,],  color = "orange",
               aes(x = time,
                   y = FatuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cf_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Fat concentration Alpha", x = "Hours", y = "concentration umol l^-1") +
    scale_y_log10() +
    my_theme()
  
  # Beta 5.0 mg/kg Bw/ day 
  plot10 <- ggplot() +
    geom_point(data = data_Dikshith_1984[2,],  color = "orange",
               aes(x = time,
                   y = FatuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cf_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "Predicted Fat concentration Beta", x = "Hours", y = "concentration umol l^-1")
  
  # Display the plots
  print(plot1)
  print(plot2)
  print(plot3)
  print(plot4)
  print(plot5)
  print(plot6)
  print(plot7)
  print(plot8)
  print(plot9)
  print(plot10)
}

if (nbr.doses == 30 && Oral_Dose_in_mg_bw == 11.0) {
  #--------------------------Plot Nath 1978: Blood Serum -------------#
  # Endosulfan technical 11.0 mg/kg Bw/ day 
  plot1 <- ggplot() +
    geom_point(data = data_Nath_1978[1,], color = "red",
               aes(x = time,
                   y = BlooduM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbl_A]] + .data[[compartment_name_Cbl_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "1. Predicted Blood concentration - Endosulfan technical 11.0 mg/kg Bw/ day", x = "Hours", y = "concentration umol l^-1")
  
  #--------------------------Plot Nath 1978: Kidney -------------#
  # Endosulfan technical 11.0 mg/kg Bw/ day 
  plot2 <- ggplot() +
    geom_point(data = data_Nath_1978[1,],  color = "#FFD700",
               aes(x = time,
                   y = KidneyuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cki_A]] + .data[[compartment_name_Cki_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "2. Predicted Kidney concentration - Endosulfan technical 11.0 mg/kg Bw/ day", x = "Hours", y = "concentration umol l^-1")
  
  #--------------------------Plot Nath 1978: Liver -------------#
  # Endosulfan technical 11.0 mg/kg Bw/ day 
  plot3 <- ggplot() +
    geom_point(data = data_Nath_1978[1,],  color = "purple",
               aes(x = time,
                   y = LiveruM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cli_A]] + .data[[compartment_name_Cli_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "3. Predicted Liver concentration - Endosulfan technical 11.0 mg/kg Bw/ day", x = "Hours", y = "concentration umol l^-1")
  
  #--------------------------Plot Nath 1978: Brain -------------#
  # Endosulfan technical 11.0 mg/kg Bw/ day 
  plot4 <- ggplot() +
    geom_point(data = data_Nath_1978[1,],  color = "hotpink",
               aes(x = time,
                   y = BrainuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbr_A]] + .data[[compartment_name_Cbr_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "4. Predicted Brain concentration - Endosulfan technical 11.0 mg/kg Bw/ day", x = "Hours", y = "concentration umol l^-1")
  
  #--------------------------Plot Nath 1978: Fat -------------#
  # Endosulfan technical 11.0 mg/kg Bw/ day 
  plot5 <- ggplot() +
    geom_point(data = data_Nath_1978[1,],  color = "orange",
               aes(x = time,
                   y = FatuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cf_A]] + .data[[compartment_name_Cf_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "5. Predicted Fat concentration - Endosulfan technical 11.0 mg/kg Bw/ day", x = "Hours", y = "concentration umol l^-1")
  
  #--------------------------Plot Nath 1978: Testes -------------#
  # Endosulfan technical 11.0 mg/kg Bw/ day 
  plot6 <- ggplot() +
    geom_point(data = data_Nath_1978[1,], color = "blue",
               aes(x = time,
                   y = TestisuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Ct_A]] + .data[[compartment_name_Ct_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "6. Predicted Testes concentration - Endosulfan technical 11.0 mg/kg Bw/ day", x = "Hours", y = "concentration umol l^-1")
  
  # Display the plots
  print(plot1)
  print(plot2)
  print(plot3)
  print(plot4)
  print(plot5)
  print(plot6)
}

if (nbr.doses == 15 && Oral_Dose_in_mg_bw == 5.0 && time.end == 740) {
  #--------------------------Plot Gupta 1987: Blood -------------#
  # Alpha 5.0 mg/kg Bw/ day 
  plot1 <- ggplot() +
    geom_point(data = data_Gupta_1987[c(1,3),], color = "red",
               aes(x = time,
                   y = PlasmauM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbl_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "1. Predicted Blood concentration Alpha - Gupta 1987", x = "Hours", y = "concentration umol l^-1")
  
  # Beta 5.0 mg/kg Bw/ day 
  plot2 <- ggplot() +
    geom_point(data = data_Gupta_1987[c(5,7),], color = "red",
               aes(x = time,
                   y = PlasmauM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbl_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "2. Predicted Blood concentration Beta - Gupta 1987", x = "Hours", y = "concentration umol l^-1")
  
  # Sulfate 5.0 mg/kg Bw/ day 
  plot3 <- ggplot() +
    geom_point(data = data_Gupta_1987[c(9,11),], color = "red",
               aes(x = time,
                   y = PlasmauM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbl_S]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "2. Predicted Blood concentration Sulfate - Gupta 1987", x = "Hours", y = "concentration umol l^-1") 
  #--------------------------Plot Gupta 1987: Brain -------------#
  # Alpha 5.0 mg/kg Bw/ day 
  plot4 <- ggplot() +
    geom_point(data = data_Gupta_1987[c(1,3),],  color = "hotpink",
               aes(x = time,
                   y = BrainuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbr_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "3. Predicted Brain concentration Alpha - Gupta 1987", x = "Hours", y = "concentration umol l^-1")
  
  # Beta 5.0 mg/kg Bw/ day 
  plot5 <- ggplot() +
    geom_point(data = data_Gupta_1987[c(5,7),],  color = "hotpink",
               aes(x = time,
                   y = BrainuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbr_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "4. Predicted Brain concentration Beta - Gupta 1987", x = "Hours", y = "concentration umol l^-1")
  
  # Display the plots
  print(plot1)
  print(plot2)
  print(plot3)
  print(plot4)
  print(plot5)
}

if (nbr.doses == 15 && Oral_Dose_in_mg_bw == 10.0 && time.end == 740) {
  #--------------------------Plot Gupta 1987: Blood -------------#
  # Alpha 5.0 mg/kg Bw/ day 
  plot1 <- ggplot() +
    geom_point(data = data_Gupta_1987[c(2,4),], color = "red",
               aes(x = time,
                   y = PlasmauM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbl_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "1. Predicted Blood concentration Alpha - Gupta 1987", x = "Hours", y = "concentration umol l^-1")
  
  # Beta 5.0 mg/kg Bw/ day 
  plot2 <- ggplot() +
    geom_point(data = data_Gupta_1987[c(6,8),], color = "red",
               aes(x = time,
                   y = PlasmauM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbl_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "2. Predicted Blood concentration Beta - Gupta 1987", x = "Hours", y = "concentration umol l^-1")
  
  # Sulfate 5.0 mg/kg Bw/ day 
  plot3 <- ggplot() +
    geom_point(data = data_Gupta_1987[c(10,12),], color = "red",
               aes(x = time,
                   y = PlasmauM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbl_S]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "3. Predicted Blood concentration Sulfate - Gupta 1987", x = "Hours", y = "concentration umol l^-1") 
  #--------------------------Plot Gupta 1987: Brain -------------#
  # Alpha 5.0 mg/kg Bw/ day 
  plot4 <- ggplot() +
    geom_point(data = data_Gupta_1987[c(2,4),],  color = "hotpink",
               aes(x = time,
                   y = BrainuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbr_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "4. Predicted Brain concentration Alpha - Gupta 1987", x = "Hours", y = "concentration umol l^-1")
  
  # Beta 5.0 mg/kg Bw/ day 
  plot5 <- ggplot() +
    geom_point(data = data_Gupta_1987[c(6,8),], color = "hotpink",
               aes(x = time,
                   y = BrainuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbr_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "5. Predicted Brain concentration Beta - Gupta 1987", x = "Hours", y = "concentration umol l^-1")
  
  # Display the plots
  print(plot1)
  print(plot2)
  print(plot3)
  print(plot4)
  print(plot5)
}

#----------------------------------------Chan and Mohd 2005 (5.0)------------------------------#
# 15 daily doses of 5.0 mg/kg/day
if (nbr.doses == 15 && Oral_Dose_in_mg_bw == 5.0 && time.end == 720) {
  
  #--------------------------Plot Chan and Mohd 2005: Blood -------------#
  # Alpha 5.0 mg/kg Bw/ day 
  plot1 <- ggplot() +
    geom_point(data = data_ChanandMohd_2005[c(1,7),], color = "red",
               aes(x = time,
                   y = BlooduM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbl_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "1. Predicted Blood concentration Alpha - Chan and Mohd 2005", x = "Hours", y = "concentration umol l^-1")
  
  # Beta 5.0 mg/kg Bw/ day 
  plot2 <- ggplot() +
    geom_point(data = data_ChanandMohd_2005[c(2,8),], color = "red",
               aes(x = time,
                   y = BlooduM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbl_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "2. Predicted Blood concentration Beta - Chan and Mohd 2005", x = "Hours", y = "concentration umol l^-1")
  
  # Beta 5.0 mg/kg Bw/ day 
  plot3 <- ggplot() +
    geom_point(data = data_ChanandMohd_2005[c(3,9),], color = "red",
               aes(x = time,
                   y = BlooduM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbl_S]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "3. Predicted Blood concentration Sulfate - Chan and Mohd 2005", x = "Hours", y = "concentration umol l^-1")
  
  #--------------------------Plot Chan and Mohd 2005: Liver -------------#
  # Alpha 5.0 mg/kg Bw/ day 
  plot4 <- ggplot() +
    geom_point(data = data_ChanandMohd_2005[c(1,7),],  color = "purple",
               aes(x = time,
                   y = LiveruM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cli_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "4. Predicted Liver concentration Alpha - Chan and Mohd 2005", x = "Hours", y = "concentration umol l^-1")
  
  # Beta 5.0 mg/kg Bw/ day 
  plot5 <- ggplot() +
    geom_point(data = data_ChanandMohd_2005[c(2,8),],  color = "purple",
               aes(x = time,
                   y = LiveruM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cli_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "5. Predicted Liver concentration Beta - Chan and Mohd 2005", x = "Hours", y = "concentration umol l^-1")
  
  # Sulfate 5.0 mg/kg Bw/ day 
  plot6 <- ggplot() +
    geom_point(data = data_ChanandMohd_2005[c(3,9),],  color = "purple",
               aes(x = time,
                   y = LiveruM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cli_S]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "6. Predicted Liver concentration Sulfate - Chan and Mohd 2005", x = "Hours", y = "concentration umol l^-1")
  
  #--------------------------Plot Chan and Mohd 2005: Kidney -------------#
  # Alpha 5.0 mg/kg Bw/ day 
  plot7 <- ggplot() +
    geom_point(data = data_ChanandMohd_2005[c(1,7),],  color = "#FFD700",
               aes(x = time,
                   y = KidneyuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cki_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "7. Predicted Kidney concentration Alpha - Chan and Mohd 2005", x = "Hours", y = "concentration umol l^-1")
  
  # Beta 5.0 mg/kg Bw/ day 
  plot8 <- ggplot() +
    geom_point(data = data_ChanandMohd_2005[c(2,8),], color = "#FFD700",
               aes(x = time,
                   y = KidneyuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cki_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "8. Predicted Kidney concentration Beta - Chan and Mohd 2005", x = "Hours", y = "concentration umol l^-1")
  
  # sulfate 5.0 mg/kg Bw/ day 
  plot9 <- ggplot() +
    geom_point(data = data_ChanandMohd_2005[c(3,9),], color = "#FFD700",
               aes(x = time,
                   y = KidneyuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cki_S]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "9. Predicted Kidney concentration Sulfate - Chan and Mohd 2005", x = "Hours", y = "concentration umol l^-1")
  
  # Display the plots
  print(plot1)
  print(plot2)
  print(plot3)
  print(plot4)
  print(plot5)
  print(plot6)
  print(plot7)
  print(plot8)
  print(plot9)
}

#----------------------------------------Chan and Mohd 2005 (10.0)------------------------------#
# 15 daily doses of 10.0 mg/kg/day
if (nbr.doses == 15 && Oral_Dose_in_mg_bw == 10.0 && time.end == 720) {
  
  #--------------------------Plot Chan and Mohd 2005: Blood -------------#
  # Alpha 10.0 mg/kg Bw/ day 
  plot1 <- ggplot() +
    geom_point(data = data_ChanandMohd_2005[c(4,10),], color = "red",
               aes(x = time,
                   y = BlooduM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbl_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "1. Predicted Blood concentration Alpha - Chan and Mohd 2005", x = "Hours", y = "concentration umol l^-1") +
    my_theme()
  
  # Beta 10.0 mg/kg Bw/ day 
  plot2 <- ggplot() +
    geom_point(data = data_ChanandMohd_2005[c(5,11),], color = "red",
               aes(x = time,
                   y = BlooduM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbl_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "2. Predicted Blood concentration Beta - Chan and Mohd 2005", x = "Hours", y = "concentration umol l^-1") +
    my_theme()
  
  # Beta 10.0 mg/kg Bw/ day 
  plot3 <- ggplot() +
    geom_point(data = data_ChanandMohd_2005[c(6,12),], color = "red",
               aes(x = time,
                   y = BlooduM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbl_S]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "3. Predicted Blood concentration Sulfate - Chan and Mohd 2005", x = "Hours", y = "concentration umol l^-1") +
    my_theme()
  
  #--------------------------Plot Chan and Mohd 2005: Liver -------------#
  # Alpha 10.0 mg/kg Bw/ day 
  plot4 <- ggplot() +
    geom_point(data = data_ChanandMohd_2005[c(4,10),],  color = "purple",
               aes(x = time,
                   y = LiveruM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cli_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "4. Predicted Liver concentration Alpha - Chan and Mohd 2005", x = "Hours", y = "concentration umol l^-1") +
    my_theme()
  
  # Beta 10.0 mg/kg Bw/ day 
  plot5 <- ggplot() +
    geom_point(data = data_ChanandMohd_2005[c(5,11),],  color = "purple",
               aes(x = time,
                   y = LiveruM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cli_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "5. Predicted Liver concentration Beta - Chan and Mohd 2005", x = "Hours", y = "concentration umol l^-1") +
    my_theme()
  
  # Sulfate 10.0 mg/kg Bw/ day 
  plot6 <- ggplot() +
    geom_point(data = data_ChanandMohd_2005[c(6,12),], color = "purple",
               aes(x = time,
                   y = LiveruM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cli_S]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "6. Predicted Liver concentration Sulfate - Chan and Mohd 2005", x = "Hours", y = "concentration umol l^-1") +
    my_theme()
  
  #--------------------------Plot Chan and Mohd 2005: Kidney -------------#
  # Alpha 10.0 mg/kg Bw/ day 
  plot7 <- ggplot() +
    geom_point(data = data_ChanandMohd_2005[c(4,10),],  color = "#FFD700",
               aes(x = time,
                   y = KidneyuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cki_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "7. Predicted Kidney concentration Alpha - Chan and Mohd 2005", x = "Hours", y = "concentration umol l^-1") +
    my_theme()
  
  # Beta 10.0 mg/kg Bw/ day 
  plot8 <- ggplot() +
    geom_point(data = data_ChanandMohd_2005[c(5,11),],  color = "#FFD700",
               aes(x = time,
                   y = KidneyuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cki_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "8. Predicted Kidney concentration Beta - Chan and Mohd 2005", x = "Hours", y = "concentration umol l^-1") +
    my_theme()
  
  # sulfate 10.0 mg/kg Bw/ day 
  plot9 <- ggplot() +
    geom_point(data = data_ChanandMohd_2005[c(6,12),], color = "#FFD700",
               aes(x = time,
                   y = KidneyuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cki_S]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "9. Predicted Kidney concentration Sulfate - Chan and Mohd 2005", x = "Hours", y = "concentration umol l^-1")
  
  # Display the plots
  print(plot1)
  print(plot2)
  print(plot3)
  print(plot4)
  print(plot5)
  print(plot6)
  print(plot7)
  print(plot8)
  print(plot9)
}

#----------------------------------------Leist and Mayer 1987 (34.0)------------------------------#
# 15 daily doses of 34.0 mg/kg/day
if (nbr.doses == 30 && Oral_Dose_in_mg_bw == 34.0 && time.end == 1500) {
  
  #--------------------------Plot Leist and Mayer 1987: Blood -------------#
  # Alpha 34.0 mg/kg Bw/ day 
  plot1 <- ggplot() +
    geom_point(data = data_LeistandMayer_1987[c(1,2),], color = "red",
               aes(x = time,
                   y = BlooduM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbl_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "1. Predicted Blood concentration Alpha - Leist and Mayer 1987", x = "Hours", y = "concentration umol l^-1")
  
  # Beta 34.0 mg/kg Bw/ day 
  plot2 <- ggplot() +
    geom_point(data = data_LeistandMayer_1987[c(5,6),],  color = "red",
               aes(x = time,
                   y = BlooduM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbl_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "2. Predicted Blood concentration Beta - Leist and Mayer 1987", x = "Hours", y = "concentration umol l^-1")
  
  # Sulfate 34.0 mg/kg Bw/ day 
  plot3 <- ggplot() +
    geom_point(data = data_LeistandMayer_1987[c(9,10),], color = "red",
               aes(x = time,
                   y = BlooduM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbl_S]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "3. Predicted Blood concentration Sulfate - Leist and Mayer 1987", x = "Hours", y = "concentration umol l^-1")
  
  #--------------------------Plot Leist and Mayer 1987: Liver -------------#
  # Alpha 34.0 mg/kg Bw/ day 
  plot4 <- ggplot() +
    geom_point(data = data_LeistandMayer_1987[c(1,2),],color = "purple",
               aes(x = time,
                   y = LiveruM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cli_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "4. Predicted Liver concentration Alpha - Leist and Mayer 1987", x = "Hours", y = "concentration umol l^-1")
  
  # Beta 34.0 mg/kg Bw/ day 
  plot5 <- ggplot() +
    geom_point(data = data_LeistandMayer_1987[c(5,6),],color = "purple",
               aes(x = time,
                   y = LiveruM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cli_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "5. Predicted Liver concentration Beta - Leist and Mayer 1987", x = "Hours", y = "concentration umol l^-1")
  
  # Sulfate 34.0 mg/kg Bw/ day 
  plot6 <- ggplot() +
    geom_point(data = data_LeistandMayer_1987[c(9,10),],color = "purple",
               aes(x = time,
                   y = LiveruM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cli_S]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "6. Predicted Liver concentration Sulfate - Leist and Mayer 1987", x = "Hours", y = "concentration umol l^-1")
  
  #--------------------------Plot Leist and Mayer 1987: Kidney -------------#
  # Alpha 34.0 mg/kg Bw/ day 
  plot7 <- ggplot() +
    geom_point(data = data_LeistandMayer_1987[c(1,2),],color = "#FFD700",
               aes(x = time,
                   y = KidneyuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cki_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "7. Predicted Kidney concentration Alpha - Leist and Mayer 1987", x = "Hours", y = "concentration umol l^-1")
  
  # Beta 34.0 mg/kg Bw/ day 
  plot8 <- ggplot() +
    geom_point(data = data_LeistandMayer_1987[c(5,6),], color = "#FFD700",
               aes(x = time,
                   y = KidneyuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cki_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "8. Predicted Kidney concentration Beta - Leist and Mayer 1987", x = "Hours", y = "concentration umol l^-1")
  
  # Sulfate 34.0 mg/kg Bw/ day 
  plot9 <- ggplot() +
    geom_point(data = data_LeistandMayer_1987[c(9,10),], color = "#FFD700",
               aes(x = time,
                   y = KidneyuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cki_S]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "9. Predicted Kidney concentration Sulfate - Leist and Mayer 1987", x = "Hours", y = "concentration umol l^-1")
  
  # Display the plots
  print(plot1)
  print(plot2)
  print(plot3)
  print(plot4)
  print(plot5)
  print(plot6)
  print(plot7)
  print(plot8)
  print(plot9)
}

#----------------------------------------Leist and Mayer 1987 (67.8)------------------------------#
# 15 daily doses of 67.8 mg/kg/day
if (nbr.doses == 30 && Oral_Dose_in_mg_bw == 67.8 && time.end == 1500) {
  
  #--------------------------Plot Leist and Mayer 1987: Blood -------------#
  # Alpha 67.8 mg/kg Bw/ day 
  plot1 <- ggplot() +
    geom_point(data = data_LeistandMayer_1987[c(3,4),],
               aes(x = time,
                   y = BlooduM
               ),
               color = "red"
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbl_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "1. Predicted Blood concentration Alpha - Leist and Mayer 1987", x = "Hours", y = "concentration umol l^-1") +
  my_theme()
  # Beta 67.8 mg/kg Bw/ day 
  plot2 <- ggplot() +
    geom_point(data = data_LeistandMayer_1987[c(7,8),],
               aes(x = time,
                   y = BlooduM
               ),
               color = "red"
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbl_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "2. Predicted Blood concentration Beta - Leist and Mayer 1987", x = "Hours", y = "concentration umol l^-1")
  
  # Sulfate 67.8 mg/kg Bw/ day 
  plot3 <- ggplot() +
    geom_point(data = data_LeistandMayer_1987[c(11,12),],
               aes(x = time,
                   y = BlooduM
               ),
               color = "red"
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cbl_S]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "3. Predicted Blood concentration Sulfate - Leist and Mayer 1987", x = "Hours", y = "concentration umol l^-1")
  
  #--------------------------Plot Leist and Mayer 1987: Liver -------------#
  # Alpha 67.8 mg/kg Bw/ day 
  plot4 <- ggplot() +
    geom_point(data = data_LeistandMayer_1987[c(3,4),],
               aes(x = time,
                   y = LiveruM
               ),
               color = "purple"
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cli_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "4. Predicted Liver concentration Alpha - Leist and Mayer 1987", x = "Hours", y = "concentration umol l^-1") +
    my_theme()
  
  # Beta 67.8 mg/kg Bw/ day 
  plot5 <- ggplot() +
    geom_point(data = data_LeistandMayer_1987[c(7,8),],
               aes(x = time,
                   y = LiveruM
               ),
               color = "purple"
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cli_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "5. Predicted Liver concentration Beta - Leist and Mayer 1987", x = "Hours", y = "concentration umol l^-1")
  
  # Sulfate 67.8 mg/kg Bw/ day 
  plot6 <- ggplot() +
    geom_point(data = data_LeistandMayer_1987[c(11,12),],
               aes(x = time,
                   y = LiveruM
               ),
               color = "purple"
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cli_S]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "6. Predicted Liver concentration Sulfate - Leist and Mayer 1987", x = "Hours", y = "concentration umol l^-1")
  
  #--------------------------Plot Leist and Mayer 1987: Kidney -------------#
  # Alpha 67.8 mg/kg Bw/ day 
  plot7 <- ggplot() +
    geom_point(data = data_LeistandMayer_1987[c(3,4),],
               aes(x = time,
                   y = KidneyuM
               ),
               color = "#FFD700"
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cki_A]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "7. Predicted Kidney concentration Alpha - Leist and Mayer 1987", x = "Hours", y = "concentration umol l^-1") +
    my_theme()
  
  # Beta 67.8 mg/kg Bw/ day 
  plot8 <- ggplot() +
    geom_point(data = data_LeistandMayer_1987[c(7,8),],
               aes(x = time,
                   y = KidneyuM
               ),
               color = "#FFD700"
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cki_B]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "8. Predicted Kidney concentration Beta - Leist and Mayer 1987", x = "Hours", y = "concentration umol l^-1")
  
  # Sulfate 67.8 mg/kg Bw/ day 
  plot9 <- ggplot() +
    geom_point(data = data_LeistandMayer_1987[c(11,12),], color = "#FFD700",
               aes(x = time,
                   y = KidneyuM
               )
    ) +
    geom_line(data = sim_data_Rats, 
              aes(x = time, 
                  y = .data[[compartment_name_Cki_S]]),
              color = "black",
              linewidth = 0.25
    ) +
    labs(title = "9. Predicted Kidney concentration Sulfate - Leist and Mayer 1987", x = "Hours", y = "concentration umol l^-1")
  
  # Display the plots
  print(plot1)
  print(plot2)
  print(plot3)
  print(plot4)
  print(plot5)
  print(plot6)
  print(plot7)
  print(plot8)
  print(plot9)
}


#print("keep going, only a few weeks left!!")
