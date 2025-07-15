# fedscifund: Exploring the impact of federal science grants using R

This package contains various functions to interact with the PubMed and NIH RePORTER APIs to make it easier to analyze the downstream effects of federal science funding. An example script can be found at https://github.com/SteinAcker1/nih_cut_analysis/blob/main/nih_cut_analysis/src/main.Rmd.

## Installation Instructions

Run the following code snippet to install and load this package:

```
if(!require(devtools)) install.packages("devtools")
devtools::install_github("SteinAcker1/fedscifund")
library(fedscifund)
```

## Function Descriptions

(See detailed usage information by entering `?function_name` in your R console after loading this package.)

- `query_reporter()`: Query NIH RePORTER
- `query_pubmed()`: Query PubMed
- `create_pubmed_query_from_reporter()`: Create PubMed query based on NIH RePORTER output
- `get_citing_articles()`: Fetch PubMed articles that cite specific PMIDs
- `filter_mesh_terms()`: Filter a set of PMIDs for articles that include a set of MeSH terms
- `filter_pub_types()`: Filter a set of PMIDs for articles that match a specific publication type
- `filter_clinical_trials()`: Filter a set of PMIDs for clinical trials