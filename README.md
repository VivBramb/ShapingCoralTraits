# ShapingCoralTraits
Data and code used in "Shaping coral trait: plasticity more than filtering"

Preprint available at: https://doi.org/10.1101/2021.08.19.456946
Under revision in MEPS

## Abstract 
The structure of an ecosystem is usually determined by the shape of the organisms that build it, commonly known as ecosystem engineers. Understanding to what extent plasticity and environmental filtering determine variation in ecosystem engineer physical structure is necessary to predict how ecosystem structure may change. Here, we explored coral survival and the plasticity of morphological traits that are critical for habitat provision in coral reefs. We conducted a reciprocal clonal transplant experiment in which branching corals from the genera Porites and Acropora were moved to and from a deep and a shallow site within a lagoon in the Maldives. Survival and trait analyses revealed that transplant destination consistently induced the strongest changes, particularly among Acropora spp. The origin of the corals had only marginal effects on some of the traits. We also detected variation in the way individuals from the same species and site differentiate in their shape, showing that traits linked to habitat provision are phenotypically plastic. The results suggest coral phenotypic plasticity, in the quite common lagoonal conditions studied here, plays a stronger role than environmental filtering, in determining the zonation of coral morphologies, and consequently the habitats they provide for other organisms.
### Keywords
Niche construction, coral reefs, environmental filtering, phenotypic plasticity, geometric ecology

## File descriptions
### Data
- **DataSheet_Mal_May18.csv**: table of raw data of measures from the field
- **env/**: directory with all the environmental data for analysis with **1.environment.R**
- **nubbin_outlines/**: directory with all the outlines of the nubbins, from which morphological traits are computed with **2.get_PAs.csv**

### R
- **1.environment.R**: analysis of environmental parameters
- **2.get_PAs.R**: get morphological traits of the nubbins
- **3.prepare_data_tables.R**: handle data to make tables for model fitting
- **4.model_fitting.R**:model fitting and selection for each trait and the morphospace + code for relative figures and tables 
- **5.PCAfigs.R**: code for morphospace figures
- **response_to_reviewer.R**: code for SM8, which was something we checked upon suggestion of one of the reviewer and decided to include in the revision process
