# Football Player Analysis: Classification, Regression, and Clustering

**Authors**: Tommaso Menghini (864946), Tommaso Pozzi (864654)  
**Date**: July 15, 2024  

## 📖 Project Overview
This project leverages a data-driven approach to gain a competitive edge in football analytics. Using player performance data from the 2023-2024 season, we developed machine learning models to address three key tasks:
1. **Market Value Regression**: Estimating player market values and imputing missing data using algorithms like SVR, Gaussian Processes, and KNN.  
2. **Player Position Classification**: Predicting a player's role on the field with high accuracy (nearly 90%) using SVM and Random Forest.  
3. **Player Clustering**: Identifying natural groupings of similar players using K-Means and Gaussian Mixture Models (GMM).  

## 📊 Dataset
The dataset was constructed by scraping data from two primary sources using the **R library `worldfootballR`**.

- **Data Sources**:
  - Player Performance Statistics: **FBref.com**, focusing on "Shooting", "Defense Action", and "Possession" tables.  
  - Market Values: **Transfermarkt.com**.  
- **Scope**:
  - The analysis covers the **2023-2024 season** for the **Top 5 European leagues** (Bundesliga, La Liga, Ligue 1, Premier League, Serie A).  
- **Dataset Size**:
  - The final dataset includes **1418 players** and **66 features**.  
- **Preprocessing**:
  - Only players with minutes played above the median were included to reduce noise.  
  - Player positions were simplified from seven granular roles to four main categories: Goalkeeper (GK), Defender (DF), Midfielder (MF), and Forward (FW).  

## ⚙️ Methodology and Results

### 1. Regression: Market Value Imputation
- **Objective**: To impute 173 missing market values (12% of the dataset) for the `prezzo_numerico` variable.  
- **Preprocessing**:
  - Dimensionality was reduced using PCA on standardized data (top 10 components explaining ~84% of variance).  
  - A dummy variable for the English Premier League was created to account for its players' higher market values.  
- **Algorithms Tested**: Support Vector Regressor (SVR), Gaussian Processes (GP), K-Nearest Neighbors (KNN).  
- **Results**:
  - Gaussian Processes were discarded for producing impossible negative value predictions.  
  - SVR yielded the lowest generalization error (16.642) but showed strong signs of overfitting, with 56% of the training set becoming support vectors.  
  - The **8-NN model was chosen for imputation** due to its robust performance (Generalization Error: 18.128) and lack of overfitting.  

### 2. Classification: Predicting Player Positions
- **Objective**: To build a model to accurately predict a player's position among the four simplified roles.  
- **Algorithms Tested**: Support Vector Machine (SVM) and Random Forest.  
- **Preprocessing**:
  - For SVM, PCA was used on standardized data (11 components explaining >85% variance).  
  - For Random Forest, the full, untransformed dataset was used to leverage the algorithm's feature importance capabilities.  
- **Results**:
  - **Random Forest was the superior model**, achieving an **accuracy of 90.65%** on the test set.  
  - SVM also performed well, with an accuracy of 85.84%.  
  - Both models perfectly classified goalkeepers. The primary source of error was the misclassification of versatile midfielders, whose statistics often overlap with those of defenders or forwards.  

### 3. Clustering: Discovering Player Profiles
- **Objective**: To apply unsupervised learning to identify natural player groupings based on performance data. The analysis was performed on the 11 principal components.  
- **Algorithms Tested**: K-Means Clustering and Gaussian Mixture Models (GMM).  
- **Results**:
  - **K-Means** found an optimal **k=4** clusters. One cluster perfectly isolated all 110 goalkeepers. However, the roles in other clusters were heavily mixed, with a modest average silhouette score of 0.25.  
  - **GMM** identified **5 optimal clusters** and was more flexible, successfully creating a distinct cluster for highly defensive players and separating them from defensive midfielders. Despite this, it showed more uncertainty, with a lower average silhouette score of 0.17.  
  - Both methods confirmed that while goalkeepers are statistically unique, the roles of other players are fluid and exist on a spectrum.  

## 🏆 Conclusion
The project successfully demonstrated the power of machine learning in football analytics:
- **Regression** models are effective for valuing average players but tend to underestimate elite outliers, whose value is influenced by off-field factors.  
- **Classification** techniques can predict player positions with a high **accuracy of ~90%**.  
- **Clustering** algorithms can identify robust player patterns (especially for goalkeepers) but struggle with the fluidity of other roles, highlighting the versatility of modern players.  

## 🛠️ Technologies Used
- **Language**: R  
- **Key Libraries**: `worldfootballR`, `caret`, `randomForest`, `e1071`, `mclust`, among others.  
