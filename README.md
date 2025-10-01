# Dalmatian Votives
Repository for data and visualisations behind forthcoming paper on Latin votive inscriptions in Dalmatia: Coopey, E. S. (forthcoming), ‘Digital epigraphy in Dalmatia? Sources, methods, and potentials for studying votive inscriptions, in L. Perinić (ed.), _AMONG GODS AND MEN: The Cults and the Population of Roman Dalmatia According to the Votive Inscriptions_, Archaeopress.

![scaled Bubble map of the distribution of votive inscriptions in roman dalmatia](output_images/3.EDCS_votives_scatter.jpeg)
<figcaption> Bubble map of votive inscriptions in Dalmatia. CC BY-SA 4.0</figcaption>
<p> <p/>

# Contents
- /data (data from online sources (see below). .JSON, .CSV, .PARQUET files)
- /scripts (R scripts which clean and analyse data. .R files)
- - /1.dalmatia_data.R (get, clean, and compare data from EDCS, EDH, and LIRE. Save as .CSVs)
- - /2.dalmatia_votives.R (filter to just votive inscriptions, plot on map and compare three databases. Save as .JPEGs)
- - /3.temporal_distribution.R (check data ranges of LIRE data, map chronological distribution of three datasets using density() and datasetps(). Save as .JPEGs)
- - /4.type-distribution.R (filter LIRE data, map chronological distribution of common types of votive inscriptions using density() and datasetps(). Save as .JPEGs)
- - /5.military-civilian.R (compare inscriptions tagged as 'military' with rest of corpus on LIRE using density() and datasetps(). Save as .JPEGs)
- /output_images (visualations made in /scripts. .JPEG files)
- /output_data (data made in /scroipts. .CSV files)
- /shape_files (shape files drawn upon in /scripts. .CPG, .DBF, .PRJ, .SHP, .SHX files)

# Data Sources
- Epgraphic data from [LIRE v3.0](https://doi.org/10.5281/zenodo.5074773) ([CC BY 4.0](https://creativecommons.org/licenses/by-sa/4.0/deed.en)).
- Epgraphic data from [EDH](https://edh.ub.uni-heidelberg.de/data) ([CC BY-SA 4.0](http://creativecommons.org/licenses/by-sa/4.0/)).
- Epgraphic data from [EDCS](http://www.manfredclauss.de/), via the [Latin Epigraphy Scraper](https://zenodo.org/doi/10.5281/zenodo.12036539) ([GPL-3.0](https://github.com/mqAncientHistory/Lat-Epig?tab=GPL-3.0-1-ov-file#readme)).
- Roads from [AWMC](https://github.com/AWMC/geodata/tree/master/Cultural-Data/roads) ([ODC ODBL](http://opendatacommons.org/licenses/odbl/1.0/)), via [Cultural Data](https://github.com/AWMC/geodata/tree/master/Cultural-Data).

# Method
For script 2, bubble plot. For scripts 3-5, aoristic analaysis using [datplot() v1.1.1](https://cran.r-project.org/web/packages/datplot/index.html). Explained and deployed in Steinmann, L, and B Weissova. 2021. ‘Datplot: A New R Package for the Visualization of Date Ranges in Archaeology’. _Advances in Archaeological Practice_ 9(4). DOI:[10.1017/aap.2021.8](https://doi.org/10.1017/aap.2021.8).

# Version
- v0.9.1 (final draft for Zenodo)