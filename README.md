# Kaggle_TPSsep2022
Kaggle Tabular Playground Series - Sep 2022

The purpose of entering this competition is to familiarize myself with Kaggle, Git and Github as well as practice and expand my DS and ML skillset. 

# Learning

Post competition notes: 

- Pipeline: The method used for building and applying pipeline functions (feature engineering transformations and encoding) is useful
- Handling time series data and in particular using Prophet was fun
- Making a commit for every submission is a useful way to always be able to revert to the exact code used to create a submission
- Ways to improve my score (main differences between my and winning solutions)
  - Predict aggregate sales and ratios as opposed to raw time series
  - Feature engineering: important dates and holidays were important
  - Diagnostics: Understand where and why the model does not perform as expected and use that insigt in the feature engineering
- For the net TS prediction challenge, try out Tidyverts

# Scripts: Structure

The naming convention of scripts is as follows: [main number]-[sub number]_[file name]

The main number dictates where the script fits into the model building cycle.
The sub number is used if it is necessary to decompose the step into several subscripts. 
The file name should describe as closely as possible what the script is about

# Output

Outputs from scripts are stored in this folder. Outputs follow the script naming convention. 

# Modeling Notes

## Diagnostics

Assessing predictive capability of a model on a validation set is an important diagnostic (for inferential models as well), but it does not tell us anything about how we might improve the model. 

It is therefore equally important to carry out model diagnostics, such as analysis of residuals, assessment of assumptions (in particular probabilistic), etc., since these may give us a hint as to how we may improve the mode. 

# Version Control: Branching Strategy

The fundamental branching philosophy is to use branches whenever it is important to be able to easily revert to the current state. 
Branches are created in the following cases
- A new iteration of the project, trying out a fundamentally new idea
- A major change affecting several existing scripts 
- Anything that requires more than a few commits to achieve / solve

# Package Management

Not necessary for this project, since it's a one-off and I'm the only contributor. 

# Credits

Lockdown data from: https://www.kaggle.com/datasets/xclimx/stay-at-home-covid-20202021