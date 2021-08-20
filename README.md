# ShapingCoralTraits
Data and code used in "Shaping coral trait: plasticity more than filtering"


Preprint available at: https://doi.org/10.1101/2021.08.19.456946

## File descriptions
### Data
- **DataSheet_Mal_May18.csv**: table of raw data of measures from the field
- **env/**: directory with all the environmental data for analysis with **environment.R**
- **nubbin_outlines/**: directory with all the outlines of the nubbins, from which morphological traits are computed with **2.get_PAs.csv**

### R
- **1.environment.R**: analysis of environmental parameters
- **2.get_PAs.R**: get morphological traits of the nubbins
- **3.prepare_data_tables.R**: handle data to make tables for model fitting
- **4.model_fitting.R**:model fitting and selection for each trait and the morphospace + code for relative figures and tables 
- **5.PCAfigs.R**: code for morphospace figures
