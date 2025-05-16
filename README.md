# PhenoForge

PhenoForge is a repo containing code that processes files produced by the camera systems implemented in our growth capsules. 
![PhenoForge_byDALLE](https://github.com/user-attachments/assets/00ece90c-cb87-4bdd-a319-1afde8bea7a7)

The repo contains a prototypical pipeline for data processing and analysis of phenotyping data from 
hyperspectral, fluorescence and RGB measurements as well as meta-measurements like water-potential.
It provides functions to load and sort the respective datasets, for graphical presentation thereof 
and basic data analysis (exploratory data analysis, ANOVA) for a selection of the loaded data. 

As of now, the pipeline exists without user-interaction, but should be regarded as work in progress.
The R script "read_in_paths.R" accepts the paths to the datasets to be loaded, the 
Quarto document produces a HTML report as output file once rendered.
