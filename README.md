# BDML_prob_set_3

# Problem Set 3: Predicting housing prices  

## Project Overview  
Predicting housing prices is a longstanding challenge in both economics and real estate analytics. Housing is a heterogeneous good—each property differs in size, design, location, and amenities—making it difficult to determine a "true" market value. Against this backdrop, this study aims to develop a predictive model for housing asking prices using property-level data from Bogotá, with a specific focus on the Chapinero neighborhood. The model seeks to support a real estate start-up in identifying undervalued properties and avoiding pricing errors similar to those that led to substantial financial losses in other markets, such as Zillow’s failed iBuying initiative in the United States.


## Team Members  
- Carlos Manjarres  
- Juan Felipe Triana   
- Maya Gutiérrez
- Sebastian Trujillo

## Repository Structure  
The repository is organized as follows:  
- **`documents/`** – Contains the final report of the project.  
- **`scripts/`** – Includes all R scripts used in the analysis.  
- **`stores/`** – Stores the dataset after web scraping and preprocessing.  
- **`views/`** – Contains visual outputs (figures, tables, and model results).  

## Setup Instructions  
To run this project, you need R and the following packages:  
 - **`tidyverse`**      # Collection of R packages for data manipulation, visualization, and tidy data principles
 - **`glmnet`**         # Implements Lasso and Elastic Net regularized regression
 - **` caret`**         # Streamlined tools for training and evaluating machine learning models
 - **` Metrics`**       # Performance metrics for regression and classification models
 - **` dplyr`**         # Data manipulation (part of tidyverse, but can be loaded individually)
 - **` pROC`**          # Tools for visualizing and analyzing ROC curves
 - **` rpart`**         # Recursive partitioning for classification and regression trees
 - **` rpart.plot`**    # Enhanced plotting of decision trees built with rpart
 - **` ggplot2`**       # Grammar of graphics for data visualization (also part of tidyverse)
 - **` smotefamily`**   # Functions for applying SMOTE and other resampling techniques for imbalanced data




## Running the Scripts  
There is no `main.R` script. Instead, run the scripts in the following order:  


1 **`01_data_cleaning.R`** – Cleans the dataset and performs a preliminary inspection of the data.  ⃣
2 **`02_models.R`** – Estimates different models to predict poverty.  
3 **`03_Trees_models`** – Estimates tree based models.  
4 **`04_statistics.R`** – Data visualization and descriptive statistics. 


## Reproducibility

This project is fully reproducible. To replicate the analysis:

Clone this repository: git clone https://github.com/MayaGutiBan/BDML_PS2_g12.git
