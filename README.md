# POL837 Research Paper

This is the repository for the research paper for Simon Fraser University's POL837 *Issues in Comparative Politics* course. The repository contains replication files for the data analysis and to recompile the paper's tables and prose. 

## Data Sources

- The AmericasBarometer Survey (subscriber files) regional dataset for Ecuador. Free access datasets available at https://www.vanderbilt.edu/lapop/raw-data.php.

    - Raw data is not provided in this repository, as per the Latin American Public Opinion Project's [terms and conditons](http://datasets.americasbarometer.org/database/agreement.html). Subscribers can access the data in the provided link above, and the replication files in this repository can be used to reproduce the analysis by downloading the files and placing them in the `data/americas_barometer` directory.
    
    - Free access data sources require extra processing to match the format of the subscriber files. Please see this [GitHub repository](https://github.com/dsanchezp18/hbc-prelim) for replication code on how to process the free access data to match the subscriber files. 
    
- National Oceanic and Atmospheric Administration Physical Sciences Laboratory daily weather data for minimum and maximum temperatures and precipitation. Free access datasets available at https://psl.noaa.gov/data/gridded/data.cpc.globaltemp.html.

    - The raw data is provided in this repository to create temperature and precipitation maps, however, the algorithm to process the NetCDF files is provided in [this GitHub repository](https://github.com/laboratoriolide/ecuador-temperature-noaa). 

