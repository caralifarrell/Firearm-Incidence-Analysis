# Firearm Incidence Analysis
*Individual project as part of MGSC 401 - Statistical Foundations of Data Analytics, McGill University, Desautels Faculty of Management for Fall 2023*
## The Client
The hypotheical client is focused on addressing gun violence in the United States, where over 600 individuals lose their lives daily due to firearms. The aim is to use analytics to classify and predict the severity of gun violence incidents to enhance public safety and inform policy interventions.

## The Challenge
The challenge is to analyze a comprehensive dataset of over 230,000 gun violence incidents from 2013 to 2018 to classify incidents into three severity categories: low, medium, and high. High severity is defined by mass shootings (three or more killings), medium severity involves one to two deaths, and low severity has no fatalities. The objective is to identify key factors that contribute to incident severity and to explore how to effectively predict these outcomes.

## The Approach
The approach involves several steps:

1. Data Preparation: The dataset was cleaned and simplified, removing non-relevant variables and retaining 12 predictors related to incident characteristics, such as geography, participant demographics, and gun-related factors.
2. Model Selection: Three predictive modeling techniques were employed:
  - Random Forest: Used for hyperparameter tuning and to assess variable importance while minimizing overfitting.
  - Quadratic Discriminant Analysis (QDA): Chosen for its suitability for multi-category outcomes, helping to classify incident severity effectively.
  - Classification Tree: Provides a visual representation of decision rules, enhancing interpretability and understanding of how different variables impact severity classification.
3. Analysis of Results: The performance of the models was evaluated, with Random Forest yielding a 24.26% error rate, indicating reasonable predictive accuracy. Key predictors influencing severity were identified, including the number of injured individuals and participant relationships.

## The Results 
The findings indicate that:

- The QDA model offers significant insights into predicting gun violence incident severity, showing that prior relationships among participants significantly influence outcomes.
- The Random Forest model's variable importance analysis revealed that certain features (like the number of injured) play crucial roles in classification.
- The classification tree allowed for intuitive interpretations of how different factors correlate with severity levels.
- The results suggest that high-severity incidents are more likely when individuals involved know each other, emphasizing the need for targeted educational programs to recognize warning signs.

Overall, the analysis provides a framework for understanding the dynamics of gun violence incidents and suggests actionable insights for policymakers aiming to mitigate the impact of gun violence. However, the study acknowledges limitations, including potential biases in historical data and the need for further refinement of the models used.
