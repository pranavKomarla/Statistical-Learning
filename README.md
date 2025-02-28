# **LARS and Leaps Analysis for Vehicle Fuel Efficiency**

## **Project Description**  
This project analyzes vehicle fuel efficiency (MPG) across three countries: the USA, Germany, and Japan. The goal is to identify and compare the most important factors influencing MPG in each country using **LARS (Least Angle Regression)** and **Leaps (Best Subset Selection)**. This analysis provides insights into vehicle design practices and their impact on fuel efficiency.  

---

## **Features and Capabilities**  

### **1. Data Preprocessing**  
- Loaded and standardized vehicle datasets, excluding the `origin` column.  
- Split datasets into training and testing sets for model evaluation.  
- Generated second-order interaction features for better model performance.  

### **2. Feature Selection Methods**  
- **LARS (Least Angle Regression):**  
  - Identifies key predictors by selecting non-zero coefficients.  
  - Provides an efficient solution for high-dimensional data.  
- **Leaps (Best Subset Selection):**  
  - Evaluates all possible combinations of predictors to find the best subset for MPG prediction.  

### **3. Model Evaluation**  
- Plotted predictions from both models against actual MPG values.  
- Calculated **Prediction Error Sum of Squares (PRESS)** to evaluate model performance.  
- Compared LARS and Leaps to determine which features were most influential.  

### **4. Insights and Interpretations**  
- Extracted and analyzed non-zero coefficients from LARS for each country.  
- Identified design features like weight, engine size, and horsepower that influence MPG.  
- Compared results across the USA, Germany, and Japan to understand regional differences in vehicle design priorities.  

### **5. Debugging and Fixes**  
- Resolved an issue where LARS was returning no selected features (`integer(0)`) by ensuring proper coefficient computation.  
- Adjusted mean residual calculations to improve model predictions:  
  ```r
  mean(y.auto) - mean((x.auto2 %*% coef(dum)[65,]))
