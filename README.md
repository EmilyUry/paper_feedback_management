# [Wetland Methane Feedback Management]
This repository contains the analysis code associated with the scientific publication:
> **[Addressing methane emissions feedbacks from global wetlands]**
> *[EA Ury, Z Zhang, B Buma]*
> *[Nature Sustainability]*

## Overview
This repository includes R scripts used to extract data from previously published sources, prepare it for analysis, and perform the statistical analyses reported in the paper.

## Repository Structure

├── Kleinen_data_extract.R # Extracts data from Kleinen et al. 2020
├── Zhang_data_extract.R # Extracts data from Zhang et al. 2017
├── Analysis_code.R # Performs statistical analysis (requires intermediate data outputs to be run through Magicc.org)

This project requires R (version 4.2.3 or higher) and the following R packages:

- `ncdf4`
- `raster`
- `ggplot2`
