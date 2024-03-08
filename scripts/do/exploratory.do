* Do File: Exploratory Analysis of Americas Barometer
* POL 837 Term Research Paper
* Simon Fraser University
* Daniel Sanchez
* Spring 2024

* Exploratory analysis of the Americas Barometer data for Ecuador, merged file.

// ------------------------------------- Preliminaries ----------------------------------------- //

* Clear my workspace

clear

* No need to set a working directory as a Stata project is being used
* The project works if clicked upon or if executing the following

// projmanager "C:\Users\user\Documents\GitHub\pol837-paper\pol837.stpr"

* Ensures compatibility with other Stata versions

version 17

* Turn off the more button to see more results in the screen

set more off

* Turns off any log that is open 

capture log close 

// ------------------------------------- Load data ----------------------------------------- //

import spss using "data/americas_barometer/ECU_merge_2004-2023_LAPOP_AmericasBarometer_v1.0_w.sav"

// ---------------------------- Exploratory analysis -------------------------------------- //

* Tabulate the cantons for year 2010

tab municipio10 if year == 2010

* Tabulate the other kind of cantons for year 2010

tab municipio if year == 2010

tab municipio04 if year == 2010

tab municipio06 if year == 2010

tab municipio08 if year == 2010

tab canton if year == 2010

tab municiopio1t if year == 2010

// ------------------------------------- Finalizing script ----------------------------------------- //

* Exit the do file

exit
