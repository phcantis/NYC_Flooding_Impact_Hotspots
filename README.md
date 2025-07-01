# Pluvial Flood Risk and Critical Infrastructures Exposure in New York City

This repository contains the analysis code for the manuscript:

**"Inequitable distribution of population exposure, social vulnerability, and critical infrastructure risk to extreme rainfall and pluvial flooding in New York City"**  
_Lead-Author et al., [PNAS, under review]_ [to be updated post-review, in order to maintain anonimity during peer-review process]

## 📄 Citation

If you use this code or build on this work, please cite:

Lead-Author, et al. (2025). Inequitable distribution of population exposure, social vulnerability, and critical infrastructure risk to extreme rainfall and pluvial flooding in New York City. Proceedings of the National Academy of Sciences (in review).

## 📁 Data Folders Set-up and Data Sharing

Data folders are too large for sharing in this repository, and the full repository takes significant amounts of time and computational power to run. 

Because of this, a script is provided download, store, and organize the input and output data of the analysis.

To download the study's data, run the script ```NYCF_data_download.R```. You will be requested to sign in and authorize access to a Google Drive account in order to download the datasets.

Once the script is run, a folder with the following structure should appear:

 ```
├── data/                 
│   ├── 1_raw/              # for input data for the study
│   ├── 2_intermediate/     # for storing intermediate datasets
│   ├── 3_output/           # for final datasets included in the manuscript and from which hotspots maps are created
│   └── 4_display/          # for graphical outputs such as plots and maps
```

## ▶️ Workflow

The analysis counts with 3 key steps, which are run sequentially to feed from the previous step - into the next one.

**1) Generate and compile exposure and vulnerability data into a database:** The script ```NYCF_report_CD_impacts.R``` is programmed to sequentially process raw input data by 1) calculating the distance to flooding hazards by each spatial feature under each different flooding scenario and 2) filtering data and calculating final indicators used in the analysis. The output of this script is a final database with all the data that is then used to produce different exposure indices / hotspot scores per risk attribute and category. Several nested sub-scripts are computed internally. Due to the potential time required to compute them all, these are commented out and their outputs are included in the data folder provided.

**2) Index building:** The script ```NYCF_Index_Build.R``` will use the exposure and social vulnerability indicators generated and stored in a database to compute composite hotspot scores useful to depict which Community Districts rank higher for each of the risk criteria examined.

**3) Plotting:** The scripts ```NYCF_Plotting_Schematic.R``` and ```NYCF_Plotting_Maps.R``` produce the bar plots and maps included in the manuscript and its Supplementary Materials.

## 📄 Data References
 
The following datasets are used as raw-input to the analysis:

| Dataset  | File Name (data/1_raw/) | Source |
| ------------- | ------------- | ------------- |
| Building Footprints  | Building_Footprints_20250507.geojson  | [NYC Open Data]([url](https://data.cityofnewyork.us/City-Government/Building-Footprints-Map-/jh45-qr5r)) | 
| Content Cell  | Content Cell  | | 
