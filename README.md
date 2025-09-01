# Football Player Analysis: Classification, Regression, and Clustering
[cite_start]*Sfruttamento dei Dati di Prestazione per la Classificazione delle Posizioni, la Regressione dei Valori di mercato e il Clustering dei Calciatori* [cite: 1, 2]

[cite_start]**Authors**: Tommaso Menghini (864946), Tommaso Pozzi (864654) [cite: 3, 4]
[cite_start]**Date**: July 15, 2024 [cite: 5]

## 📖 Project Overview
[cite_start]This project leverages a data-driven approach to gain a competitive edge in football analytics[cite: 7]. Using player performance data from the 2023-2024 season, we developed machine learning models to address three key tasks:
1.  [cite_start]**Market Value Regression**: Estimating player market values and imputing missing data using algorithms like SVR, Gaussian Processes, and KNN[cite: 8, 9].
2.  [cite_start]**Player Position Classification**: Predicting a player's role on the field with high accuracy (nearly 90%) using SVM and Random Forest[cite: 8, 10].
3.  [cite_start]**Player Clustering**: Identifying natural groupings of similar players using K-Means and Gaussian Mixture Models (GMM)[cite: 8, 11].

## 📊 Dataset
[cite_start]The dataset was constructed by scraping data from two primary sources using the **R library `worldfootballR`**[cite: 34].

- **Data Sources**:
  - [cite_start]Player Performance Statistics: **FBref.com** [cite: 13][cite_start], focusing on "Shooting", "Defense Action", and "Possession" tables[cite: 20].
  - [cite_start]Market Values: **Transfermarkt.com**[cite: 23].
- **Scope**:
  - [cite_start]The analysis covers the **2023-2024 season** for the **Top 5 European leagues** (Bundesliga, La Liga, Ligue 1, Premier League, Serie A)[cite: 15].
- **Dataset Size**:
  - [cite_start]The final dataset includes **1418 players** and **66 features**[cite: 48].
- **Preprocessing**:
  - [cite_start]Only players with minutes played above the median were included to reduce noise[cite: 36].
  - [cite_start]Player positions were simplified from seven granular roles to four main categories: Goalkeeper (GK), Defender (DF), Midfielder (MF), and Forward (FW)[cite: 52].

## ⚙️ Methodology and Results

### 1. Regression: Market Value Imputation
- [cite_start]**Objective**: To impute 173 missing market values (12% of the dataset) for the `prezzo_numerico` variable[cite: 75].
- **Preprocessing**:
  - [cite_start]Dimensionality was reduced using PCA on standardized data (top 10 components explaining ~84% of variance)[cite: 105].
  - [cite_start]A dummy variable for the English Premier League was created to account for its players' higher market values[cite: 92].
- [cite_start]**Algorithms Tested**: Support Vector Regressor (SVR), Gaussian Processes (GP), K-Nearest Neighbors (KNN)[cite: 90].
- **Results**:
  - [cite_start]Gaussian Processes were discarded for producing impossible negative value predictions[cite: 181].
  - [cite_start]SVR yielded the lowest generalization error (16.642) but showed strong signs of overfitting, with 56% of the training set becoming support vectors[cite: 129, 126, 127].
  - [cite_start]The **8-NN model was chosen for imputation** due to its robust performance (Generalization Error: 18.128) and lack of overfitting[cite: 152, 185].

### 2. Classification: Predicting Player Positions
- [cite_start]**Objective**: To build a model to accurately predict a player's position among the four simplified roles[cite: 29].
- [cite_start]**Algorithms Tested**: Support Vector Machine (SVM) and Random Forest[cite: 206].
- **Preprocessing**:
  - [cite_start]For SVM, PCA was used on standardized data (11 components explaining >85% variance)[cite: 211, 225].
  - [cite_start]For Random Forest, the full, untransformed dataset was used to leverage the algorithm's feature importance capabilities[cite: 224].
- **Results**:
  - [cite_start]**Random Forest was the superior model**, achieving an **accuracy of 90.65%** on the test set[cite: 306].
  - [cite_start]SVM also performed well, with an accuracy of 85.84%[cite: 268].
  - [cite_start]Both models perfectly classified goalkeepers[cite: 265, 311]. [cite_start]The primary source of error was the misclassification of versatile midfielders, whose statistics often overlap with those of defenders or forwards[cite: 277].

### 3. Clustering: Discovering Player Profiles
- [cite_start]**Objective**: To apply unsupervised learning to identify natural player groupings based on performance data[cite: 30]. [cite_start]The analysis was performed on the 11 principal components[cite: 328].
- [cite_start]**Algorithms Tested**: K-Means Clustering and Gaussian Mixture Models (GMM)[cite: 333].
- **Results**:
  - [cite_start]**K-Means** found an optimal **k=4** clusters[cite: 340]. [cite_start]One cluster perfectly isolated all 110 goalkeepers[cite: 342]. [cite_start]However, the roles in other clusters were heavily mixed, with a modest average silhouette score of 0.25[cite: 369].
  - [cite_start]**GMM** identified **5 optimal clusters** and was more flexible, successfully creating a distinct cluster for highly defensive players and separating them from defensive midfielders[cite: 379, 393]. [cite_start]Despite this, it showed more uncertainty, with a lower average silhouette score of 0.17[cite: 399].
  - [cite_start]Both methods confirmed that while goalkeepers are statistically unique, the roles of other players are fluid and exist on a spectrum[cite: 352, 403].

## 🏆 Conclusion
The project successfully demonstrated the power of machine learning in football analytics:
- [cite_start]**Regression** models are effective for valuing average players but tend to underestimate elite outliers, whose value is influenced by off-field factors[cite: 407, 410].
- [cite_start]**Classification** techniques can predict player positions with a high **accuracy of ~90%**[cite: 411].
- [cite_start]**Clustering** algorithms can identify robust player patterns (especially for goalkeepers) but struggle with the fluidity of other roles, highlighting the versatility of modern players[cite: 413].

## 🛠️ Technologies Used
- **Language**: R
- [cite_start]**Key Libraries**: `worldfootballR` [cite: 34] (and likely others from the R ML ecosystem like `caret`, `randomForest`, `e1071`, `mclust`, etc.)
