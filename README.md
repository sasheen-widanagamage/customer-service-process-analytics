# Customer Service Process Analytics Using R

## Validating the Relationship Between Smooth Service Processes and Customer Experience

The project analyzes the statement:

**“Customers who experience smooth service processes report better overall experiences.”**

---

## Project Overview

Customer experience is strongly influenced by how smooth and convenient the service process is. In the airline industry, factors such as online booking, online boarding, check-in service, baggage handling, gate location, time convenience, and delays can affect passenger satisfaction.

This project uses airline customer satisfaction data to analyze whether smoother service processes are associated with better overall customer experiences.

---

## Objectives

* Analyze customer satisfaction patterns using real-world airline customer satisfaction data.
* Create a smooth service process score using key service-related variables.
* Compare customer experience across low, moderate, and high service smoothness groups.
* Test whether service process smoothness is statistically associated with customer satisfaction.
* Build a predictive model to classify customer satisfaction.

---

## Dataset

The project uses the **Airline Customer Satisfaction Dataset** from Kaggle.

The dataset contains customer, travel, service rating, and delay-related variables.

Key variables used include:

* Satisfaction
* Customer Type
* Type of Travel
* Class
* Flight Distance
* Ease of Online Booking
* Online Boarding
* Check-in Service
* Gate Location
* Baggage Handling
* Departure/Arrival Time Convenience
* Seat Comfort
* Inflight Entertainment
* Online Support
* On-board Service
* Cleanliness
* Departure Delay
* Arrival Delay

---


## Methodology

### 1. Data Cleaning and Preparation

The dataset was cleaned and prepared in R Studio. The main steps included:

* Cleaning column names
* Checking missing values
* Handling missing arrival delay values using median imputation
* Removing duplicate records
* Encoding categorical variables
* Creating a binary satisfaction target variable
* Creating a smooth service process score
* Creating an overall experience score
* Creating smooth process groups
* Detecting outliers
* Capping extreme delay values at the 99th percentile
* Preparing the dataset for statistical and predictive modelling

---

## Feature Engineering

### Smooth Service Process Score

The smooth service process score was created using:

* Ease of Online Booking
* Online Boarding
* Check-in Service
* Gate Location
* Baggage Handling
* Departure/Arrival Time Convenience

### Overall Experience Score

The overall experience score was created using:

* Seat Comfort
* Food and Drink
* Inflight WiFi Service
* Inflight Entertainment
* Online Support
* On-board Service
* Leg Room Service
* Cleanliness

---

## Analysis Approach

The project follows three main analytical stages:

### Descriptive Analytics

Descriptive analytics was used to summarize customer satisfaction patterns, compare experience scores, and identify differences across smooth service process groups.

### Inferential Analytics

Inferential analytics was used to test whether the observed relationships were statistically significant.

The following tests were applied:

* One-way ANOVA
* Tukey post-hoc test
* Chi-square test
* Pearson correlation test
* Two-sample t-test
* Confidence interval analysis

### Predictive Analytics

Predictive analytics was performed using logistic regression because the target variable was binary:

* Satisfied
* Not Satisfied

The dataset was split into:

* 80% training data
* 20% testing data

Model performance was evaluated using:

* Confusion matrix
* Accuracy
* Precision
* Recall
* F1-score
* ROC curve
* AUC

---

## Key Visualizations

### Satisfaction Rate by Smooth Service Process Group

This chart shows how satisfaction rates change across low, moderate, and high smoothness groups.

![Satisfaction Rate by Smooth Process Group](outputs/plots/05_satisfaction_rate_by_smooth_process_group.png)

---

### Overall Experience by Smooth Service Process Group

This boxplot compares customer experience scores across service process smoothness levels.

![Overall Experience by Smooth Process Group](outputs/plots/04_experience_by_smooth_process_group.png)

---

### Correlation Heatmap

This heatmap shows the relationships between service process variables, overall experience, and satisfaction.

![Correlation Heatmap](outputs/plots/08_correlation_heatmap.png)

---

### ROC Curve Comparison

This chart shows the predictive performance of the logistic regression models.

![ROC Curve Comparison](outputs/plots/11_roc_curve_model_comparison.png)

---

## Key Findings

* Customers in the high smoothness group reported better overall experience scores.
* Satisfaction rates increased as the smooth service process score increased.
* Inferential tests showed statistically significant relationships between smooth service process variables and customer satisfaction.
* The logistic regression model achieved strong predictive performance, with an AUC value of approximately 0.908.
* The selected logistic regression model performed similarly to the full model, making it more interpretable while maintaining strong predictive ability.

---

## Final Conclusion

The results support the statement:

**“Customers who experience smooth service processes report better overall experiences.”**

Descriptive analytics showed clear satisfaction and experience differences across smoothness groups. Inferential analytics confirmed statistically significant relationships. Predictive analytics showed that service process variables can support customer satisfaction prediction.

Overall, the project demonstrates that smooth and efficient service processes are strongly associated with better customer experience.

---

## Author

Sasheen Sri Widanagamage
Undergraduate Student
BSc (Hons) in Information Technology
Specializing in Data Science
Sri Lanka Institute of Information Technology (SLIIT)

---
