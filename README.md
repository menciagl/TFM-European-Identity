# TFM: PREDICTING EUROPEAN IDENTITY

This repository contains all the materials necessary to replicate the study from the thesis **“European Identity: Understanding Key Predictors Across Regions and Time”** by Mencía Gómez Luna (supervisor: Francisco Villamil Fernández), submitted for the Master’s program in Computational Social Sciences.

To replicate this work, the following instructions should be followed.
There are three main folders to consider: “Database creation,” “Descriptive analysis,” and “Analyses.” Below, we describe the contents of each folder to help guide the replication process:

## 1. DATABASE CRATION
Inside this folder, we will find two additional subfolders:
 ### 1.1 Merging datasets
 In this subfolder, we'll find a R Script that merges all the datasets required for the analysis: the individual-level data from the Eurobarometers (2020–2023), combined with variables from several well-known sources such as Eurostat, the World Bank, the European Union, and The Economist Intelligence Unit, among others. This results in a dataset with **over 100,000 observations and approximately 70–80 variables.**
The dataset is uploaded in .zip format. This is not the final dataset we'll use, as it only merges the data but doesn't preprocess it. The cleaned and processed dataset is located in the other subfolder: “Preprocess data.”
### 1.2 Preprocess data
In this folder, we'll find another R script where missing values are addressed, necessary imputations are performed, and some variables are discarded to prepare the dataset for analysis. Again, the dataset is uploaded in .zip format. This cleaned and processed dataset will be used throughout the analyses.

## 2. DESCRIPTIVE ANALYSIS
Here, we have an R script containing all the **descriptive analyses related to the dependent variable** to understand its distribution and characteristics. We explore how European identity changes over years and regions, how it relates to other variables through correlations, among other methods. For these analyses, the dataset used is in subfolder 1.2 "Preprocess data".
Additionally, here we can find a **subfolder containing images and graphs of the descriptive results**. 

## 3. ANALYSES
This folder contains the bulk of the work and is divided into three subfolders:
### 3.1 Main analysis
We find one R script containing the **general analysis of the factors that predict European identity**. We use Machine learning techniques (Random Forest, Cross validation) and explanatory approches (logistic mixed model).
Again, the dataset used is in subfolder 1.2 "Preprocess data".
The results of this analyses are in the **written document under section 4.1: "General analysis"**

### 3.2 Regional analysis
Just like with the "Main analysis" we perform the **same analysis in the R script but by regions**: South, North, East and Center. We use the dataset from subfolder 1.2, "Preprocess data."
The results of this analysis are presented in the **written document under section 4.2: "Regional analysis"**

### 3.3 Temporal analysis
Just like with the "Main and Regional analyses", we perform the **same analysis in the R script but by years**: 2020, 2021, 2022 and 2023. We use the dataset from subfolder 1.2, "Preprocess data."
The results of this analysis are presented in the **written document under section 4.3: "Temporal analysis"**











Alcanzaste el límite del plan Free para GPT-4o.
