# Diabetes Prediction Project

This project focuses on building a classification model to predict whether a person has diabetes based on features like age, BMI, blood pressure, and other health-related indicators. 
The goal of this project is to predict whether a person has diabetes using different machine learning algorithms such as **Decision Trees**, **Naive Bayes**, **Logistic RegressionLasso** (Ridge, Lasso and Elastic Net regressions). 
The project also handles class imbalance, which is a common problem in many real-world datasets. This is handled using sampling techniques like upsampling, downsamplingand and ROSE (Random Over-Sampling Ex- amples => is a bootstrap-based technique which aids the task of binary classification in the presence of rare classes).
The models are evaluated using metrics such as precision, recall, F1 score, and AUC (Area Under the ROC Curve).

## Data Preprocessing
1. Data Loading
2. Data Summary
3. Reordering Columns
4. Missing Data Check
5. Conversion of Categorical Variables
6. Creating Visualizations
7. Reducing Factor Levels
8. Examining Feature imortance by plotting (absolute and relative representations of data)

## Creating models
- The dataset is loaded and split into a training set (80%) and a test set (20%) using **createDataPartition** fucnction from **caret** package.

## Decision Tree
- In the script `Decision_script_binary.R` are made multiple decision tree models using the rpart function, a part of the rpart package.
- Algorithm decision trees is used to predict the target variable `Diabetes` based on other variables in the dataset.
  - The first model (Tree1) is trained on the original imbalanced dataset.
  - Second scenario trains the models by using different sampling techniques to address class imbalance in the dataset.
    -These techniques are applied using the trainControl function from the caret package with the sampling parameter set to "down", "up", or "rose".
  - Third scenario creates models by balancing data and tuning the model hyperparameters.

## Naive Bayes
- This Algorithm works best with categorical data because it calculates probabilities using frequency counts. It also works with continuous data, but only if they have Gaussian (normal) distribution. If this isn't the case, then it's necessary to convert continuous variables to categorical ones (discretization).
- **Shapiro-Wilk Test** is used to check distribution for the columns that have numerical data.
- The test rejects the hypothesis of normality when the p-value is less than or equal to 0.05.
- All columns (`BMI`, `MentHlth`, `PhysHlth`) deviate from normality (`p < 0.05`).
- **Histograms**: Visualized using `ggplot2` to observe distribution shapes.
- **Discretization**: Non-normal columns are discretized into 2 intervals using the `discretize` function from the `bnlearn` package.


### Created models:
- Here are made the same scenraios of making the models:
  - default imbalanced
  - default balanced
  - balanced with followed by threshold optimization using the Youden method
    - Threshold adjustment leads to improved recall but a trade-off with precision.

## Logistic regression

Logistic regression is a **statistical method** used for **binary classification**, meaning it predicts one of two possible outcomes (e.g., Yes/No, True/False, 0/1). Despite its name, it is actually a classification algorithm, not a regression algorithm.

1. **Linear Relationship**  
   Logistic regression starts similarly to linear regression by computing a linear combination of input features:

2. **Sigmoid Transformation**  
   The linear output  is passed through a **sigmoid function** to convert it into a probability value between 0 and 1:

3. **Decision Boundary**  
   - The model decides the predicted class based on a **threshold** (default is 0.5):
   - The threshold can be adjusted for imbalanced data or specific application needs.

4. **Cost Function**  
   To train the model, logistic regression minimizes the **log-loss (cross-entropy)** cost function:

5. **Optimization**  
   The coefficients are optimized using **gradient descent** or similar optimization algorithms to minimize the cost function.

---
### Regularization
- While creating models using Cross-validation different regularization techniques were used (Lasso, Ridge, and Elastic Net).
- Regularization improves model generalization, prevents overfitting, and handles multicollinearity by penalizing large coefficients.
  
  1. Lasso Regression (Least Absolute Shrinkage and Selection Operator)
  - **Purpose**: Performs variable selection by shrinking less important coefficients to zero.
  - **Penalty**: Adds the absolute value of coefficients (`L1 norm`) to the loss function:
      - Loss = Residual Sum of Squares + λ ∑ ∣βj∣
  - **Key Feature**: Produces sparse models, making it easier to identify key predictors.
    
  2. Ridge Regression
  - **Purpose**: Shrinks coefficients to prevent overfitting but retains all predictors.
  - **Penalty**: Adds the square of coefficients (`L2 norm`) to the loss function:
    - Loss = Residual Sum of Squares + λ ∑βj2
  - **Key Feature**: Handles multicollinearity better and is less sensitive to small data changes.

  3. Elastic Net Regression
  - Purpose: Combines Lasso and Ridge penalties for better flexibility:
    - Loss = Residual Sum of Squares + λ[α∑∣βj​∣+(1−α)∑βj2​]
  - Key Feature:
    - Controlled by parameter α:
      - α=1: Lasso
      - α=0: Ridge
      - 0<α<1: Elastic Net
  - Useful for datasets with highly correlated predictors.
---

### Created models:
- Here are made the same scenraios of making the models:
  1. default imbalanced
  2. tuned balanced
  3. tuned balanced with threshold changed using closest topleft method
  4. tuned balanced with threshold changed using youden method

## Results of the models

| Model   | Precision  | Recall  | F1 Score | prAUC   |
|---------|------------|---------|----------|---------|
| **Tree 1** | NaN        | 0       | NaN      | 0       |
| **Tree 2** | 0.3102997  | 0.7706066 | 0.4424417 | 0.1132496 |
| **Tree 3** | 0.3045448  | 0.7652283 | 0.4356929 | 0.1111096 |
| **Tree 4** | 0.3247831  | 0.7585991 | 0.4548352 | 0.1149385 |
| **Tree 5** | 0.3432343  | 0.7154472 | 0.4639092 | 0.1903192 |
| **NB 1**   | 0.3794025  | 0.4796748 | 0.4236867 | 0.3746899 |
| **NB 2**   | 0.3410552  | 0.6702939 | 0.4520837 | 0.3746172 |
| **NB 3**   | 0.2982456  | **0.8143840** | 0.4365989 | 0.3746172 |
| **LR 1**   | **0.5619641**  | 0.1803627 | 0.2730802 | **0.4408816** |
| **LR 2**   | 0.3414855  | 0.7562226 | **0.4705058** | 0.4383580 |
| **LR 3**   | 0.3393731  | 0.7637273 | 0.4699273 | 0.4383580 |
| **LR 4**   | 0.3210927  | 0.8071295 | 0.4594191 | 0.4383580 |

## Evaluation of the results

For a specific dataset where the goal is to predict whether a person has diabetes, the **Yes** class (indicating the presence of diabetes) holds greater significance and, therefore, is considered the positive class. The greater cost of errors arises from cases where individuals who actually have diabetes are classified into the negative class (No), i.e., cases that are incorrectly predicted as not having diabetes. The goal in this case is to reduce this error, known as a **False Negative** error. Reducing False Negative errors leads to an increase in the **Recall** metric, while increasing **False Positive** errors will reduce the **Precision** metric. This is the more acceptable situation for this particular problem domain.

Based on this, when observing the **Recall** metric, its highest value is achieved by the third Naive Bayes model, where the largest False Positive error occurs—cases where individuals without diabetes are misclassified as having diabetes. To reduce this error (False Positive), a good model in the context of this problem would be the fourth Logistic Regression model, where the **Recall** metric is close to the best one, and the **Precision** value is slightly better, leading to a higher **F1** score.

To achieve a better balance between both types of errors (False Positive and False Negative), the best model would be the one with the highest **F1** score, which was achieved by the second Logistic Regression model. On the other hand, the **prAUC** metric represents the area under the precision-recall curve and is typically used for datasets that are extremely imbalanced or for situations where the positive class has greater predictive importance. The **prAUC** values for all Logistic Regression models are almost the same, but higher than those of the models based on the other two algorithms. The highest value for this metric is achieved by the first Logistic Regression model, which also has the highest **Precision** value, meaning that the **Recall** value will be low due to the inverse relationship between these two metrics.

Based on all the above and considering all the metrics used for model evaluation, the best models for this specific domain are the second and fourth models obtained using the Logistic Regression algorithm. The second Logistic Regression model represents a good solution for achieving a better balance between the **Precision** and **Recall** metrics (due to the highest **F1** value), while the fourth model achieves a slightly higher **Recall** value. This model would be a better solution if the goal is to minimize False Negative errors.





