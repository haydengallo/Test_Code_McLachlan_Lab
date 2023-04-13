# Test_Code_McLachlan_Lab

This is documentation for running sample code including Monte Carlo simulations, visualizations, and others using a local LINKAGES workflow. Within the folder Code and data you will find 5 scripts:
monte_carlo_cleaned.R
LAI.R
weights_weather_agb_interactions.R
biomass_dbh_reconstruction.R
Long_Run_SDA_Viz.R

Step 0: If you do not have Linkages package built on your local machine follow the instructions here: https://docs.google.com/document/d/1YBvqN4Kn-wtNKLpyOCZt_uhHPg8CX0po96BUXGAQLmA/edit?usp=share_link 

Create new project in R, button is located in the top right corner of R studio interface
When creating new project, select version control/git
In the box that says “repository URL”, paste the following: https://github.com/haydengallo/Test_Code_McLachlan_Lab 
Create Project
Next, navigate to this URL: https://drive.google.com/drive/u/0/folders/1wScQED19xT2QxVzBXdBuPi_u2Y3T4VDv and download the folder titled “Met_updated”
Put this download in the same directory/folder that the newly created project is found in locally on your machine
Once this in the correct directory you should have all of the data needed to run all of the scripts in this directory 
The first script that I would look through and run is the monte_carlo_cleaned.R
This script implements a Monte Carlo sensitivity analysis across 5 possible forest sites across the Upper United States using the forest gap model LINKAGES. 
