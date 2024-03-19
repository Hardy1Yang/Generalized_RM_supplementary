# Generalized_RM_supplementary

# Introduction
 The supplemental content of this study is categorized into four folders according to type: `code`, `figures`, `data`, and `log`. Data is first generated based on the `code` throughout the simulation analysis and stored in the `data` folder. During the generation process, a `log` file is created and saved in the `log` folder to record any issues that may arise, allowing for monitoring of the computer analysis progress through background jobs. Subsequently, the stored data is visualized using plotting functions. 
 
# Functions 
This repository contains the code necessary to simulate and visualize "The generalized Robbins-Monro process and its application to psychophysical experiments for threshold estimation." If you need to use any functions, please first run `source("code/Requiredpkg.R")` to ensure the necessary packages are downloaded and installed successfully. The settings related to the simulation in the `code` folder are primarily contained in `Hyper_parameter.R`, which includes the default parameter settings for the experiment. If changes are needed during the process, updating the code at the beginning of the script before starting the simulation is recommended to ensure the parameters are assigned adequately. The files related to the simulation include `Response. R`, `Methods. R`, and `Data_and_SE.R.` The function in `Response.R` generates responses based on assumptions from the literature. `Methods.R` contains the adaptive method code called `Response.R` to generate the adaptive method based on the responses. Finally, `Data_and_SE.R` includes the code necessary to create simulated data and its Monte Carlo Standard Error (SE) based on parameter requirements.

# Example Scripts
Additionally, we provide example scripts for data generation and visualization: `Data_generation.R` and `Visualization.R.` 
Details can be found within the code content.

# Contact
If you have any questions or issues regarding the research, please get in touch with us via GitHub (https://github.com/Hardy1Yang/) or email (d11227103@ntu.edu.tw).
