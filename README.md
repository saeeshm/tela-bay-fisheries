## Project Description
This repository contains the code for my graduate thesis, as part of the International Master in Marine Biological Resources (IMBRSea) program:

**Empirical Indicators For Characterising a Multi-Species, Data-Poor Fishery: The Case of Tela Bay, Honduras**

Small-Scale Fishery (SSF) management is a pressing challenge for coastal communities worldwide. Typically characterised by multiple gears and species, SSFs are poorly served by traditional stock assessment models that target singular species. In this study, we characterise the data-limited SSFs of Tela Bay, Honduras using empirical indicators. By focusing on the fishing pattern, we demonstrate that an indicator approach can characterise dynamics for both a single commercially important species (the Lane Snapper, Lutjanus synagris) as well the entire multi-species fishery. Our results show that the state of fishing in Tela, although historically showing intensive exploitation, has improved since the introduction of co-management in 2018. Improvements are observed for both single and multi-species objectives, as increases in the targeted catch size and higher estimated yields for the Lane Snapper occur alongside an improved balance of pressure across species and sizes. The study thus provides important baseline information regarding the state of fishing in Tela, and highlights the impact of co-management in inducing a more optimal fishing pattern. Moreover, in viewing single and multi-species indicators in combination, our results draw attention to the potential for synergy between single-species management and the maintenance of  ecosystem structure under balanced harvest.

## Repository Details
The data used for this thesis are owned by the Coral Reef Alliance (CORAL), and are not available for public dissemination. As a result, the scripts here only serve to describe and share the analytical workflow used during each step of this research, from data preparation to stock analysis. They cannot be directly used to reproduce the analysis without the raw data.

The methodology for stock analysis is described in detail by the R scripts. Readers interested in learning about how the results were interpreted for stock characterization in Tela may contact me for a copy of the thesis report.

Details regarding data preparation are not made explicit in the thesis report. So, an outline for the data preparation workflow is provided below. All scripts also start with a description elaborating on the specific task they complete. Code is generously commented throughout.

### Data Preparation Scripts
These scripts complete a variety of tidying functions to prepare the data for analysis. Tidying steps were repeated for multiple catch landing databases maintained by CORAL, even though only the data for Tela Bay was used for this research. 

Broadly, the following steps were completed:
1. Tidying field names and data types, fixing or interpolating erroneous values
	a. For Tela specifically, hours fished was interpolated from the boat departure and arrival times.
2. Cleaning scientific and common names and ensuring all nomenclature was consistent with [Fishbase](https://www.fishbase.se/search.php), and the local knowledge of CORAL's regional program managers.
3. Converting fork length measurements to total length using length-length conversion coefficients from Fishbase.
4. Getting the estimated trophic level for each species from Fishbase.
5. Preparing a synthesized "Master" table joining all landings databases maintained by CORAL.