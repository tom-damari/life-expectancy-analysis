# Life Expectancy Analysis

## Project Overview

This project explores key factors influencing **life expectancy** across 101 countries using **linear regression analysis**. 
It combines real-world demographic, health, environmental, and economic data to perform a complete statistical study, including both **descriptive analysis** and **formal regression modeling**.

The objective is to identify the most impactful variables affecting life expectancy and to build an interpretable linear regression model based on that data.

The project was completed in two major parts:
- **Part A:** Exploratory Data Analysis (EDA), visualizations, outlier detection, variable classification, and descriptive statistics
- **Part B:** Linear regression modeling, variable selection, interaction terms, transformations, assumption validation, and model interpretation

**Course:** Linear Regression  
**Department:** Industrial Engineering and Management, Ben-Gurion University  
**Instructor:** Prof. Yakir Branshko  
**Team Members:** Tom Damari, Maya Yaari  
**Submission Date:** January, 2023

## Dataset
The dataset covers **101 countries**, each with multiple attributes related to public health, lifestyle, environment, and economic status.
- **Target Variable: `Life_expectancy` – Average life expectancy (in years) for each country.**
  
### Features
The dataset includes the following columns:
- **`Country`**: Name of the country
- **`Life_expectancy`**: Average life expectancy in years
- **`Outdoor_air_pollution (%)`**: Percentage of outdoor air pollution
- **`HIV - Estimated number of people that have been infected`**: Estimated number of HIV infections
- **`Malaria - Estimated number of people that have been infected`**: Estimated number of malaria infections
- **`Average_income_per_person ($)`**: Average income per person in USD
- **`Alcohol_consumption_per_person (liters, year)`**: Annual alcohol consumption per person in liters
- **`Density_per_square_km`**: Population density per square kilometer
- **`Cigarette_consumption (%)`**: Percentage of cigarette consumption
- **`Continent`**: Continent code (categorical)
- **`Member_of_OECD`**: Indicates if the country is a member of the OECD (0/1)

## Goals
- - Identify key predictors of life expectancy
- Build a reliable and interpretable linear regression model
- Visualize feature distributions and relationships through EDA
- Evaluate variable interactions and apply necessary transformations
- Validate model assumptions and improve performance using diagnostics
- Generate actionable, data-driven insights to inform public health and policy
