# publish_fish
Code for publishing YBFMP fish data to EDI updating for 2023

## Notes on folders and process
The actual processing of data and code was done in the main publish_fish folder, using the .Rmd files. Once the raw data from data_raw were cleaned in clean_fish_tables, the data were written to data_clean. These files were read into integrate_fish_data to create the integrated dataset. 


![Workflow](metadata/YBFMP_fish_workflow.PNG)

When the processing was done, the edi_2021 folder was created using the EMLassemblyline package. Code for writing EML lives in that folder, and all the files getting published go into the data_objects folder. I did not push all of those files up to GitHub, because they are already in the data_clean folder, and some files are very large. Any file that was really large was saved as .rds in the data_clean folder, but was written as .csv for the edi publication, because it is more easily accessible. To create the .csv, you will need to rerun all the .Rmds. 

## Additional metadata
All of the SOPs and description of the QA/QC processes are located in the metadata/methods_references folder. There is also a copy of the diagram showing how all the tables are related to each other. 

