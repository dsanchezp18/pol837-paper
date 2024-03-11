# R Script: Downloading Bibliography-related Files
# POL837 Term Research Paper
# Simon Fraser University 
# Daniel Sanchez
# Spring 2024 

# This script downloads bibliography-related files for the POL837 term research paper.

# Downloading CSL's --------------------------------------------

# Download APA7 CSL from the CSL GitHub repository

download.file(
  url = "https://github.com/citation-style-language/styles/raw/master/apa.csl",
  destfile = "refs/apa7_normal.csl")

# Move this file to drafts folder

file.rename("refs/apa7_normal.csl", "drafts/apa7_normal.csl")