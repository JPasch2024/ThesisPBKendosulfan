# ThesisPBKendosulfan
PBK models for endosulfan in rats and humans
To run the rat PBK model for endosulfan; 
First run the R file (input parameters Endosulfan.R). Here input parameters can be selected and added to run in the model, including scenarios of multiple in vivo endosulfan rat experiments are provided so they can be selected and directly simulated by the the pbk model.
After the input parameters are selected, run them in "PBK model Endosulfan Rats JP (Blood Absorption.R)" or "PBK model endosulfan rats (Liver absorption.R)". These two PBK models are identical except the absorption from the GI tract goes towards the blood or liver. 
(additional to the PBK model, a mass balance and local sensitivity codes are included in these files)
Afterwards, the PBK model output can be validated against data from in vivo experiments. Dowload the excel file "In vivo experimental data endosulfan.xlsx" and run the R file "Plotting In vivo data V2.R" (make sure the file path of the excel sheet is updated in R). Running the excel file will plot the correct in vivo data depending on the initial parameters.
To run the human PBK model for endosulfan; 
Only the file "PBK model Endosulfan Humans (Blood Absorption.R)" or "PBK model Endosulfan Humans (Liver Absorption.R)" need to be downloaded. Input parameters can be altered from within the file. 
