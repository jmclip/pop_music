import numpy as np
import pandas as pd

from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt

# Sample data (replace with your actual data)
pop = pd.read_csv('data/songs_liwc_25_03_13.csv')

# Standardize the data (important for PCA)
scaler = StandardScaler()
scaled_data = scaler.fit_transform(pop)

# Apply PCA
pca = PCA(n_components=2)  # Choose the number of components
pca.fit(scaled_data)

# Transform the data to the new principal components
transformed_data = pca.transform(scaled_data)

# Explained variance ratio (how much variance is explained by each component)
explained_variance = pca.explained_variance_ratio_

# Visualize the results (for 2D data)
plt.scatter(transformed_data[:, 0], transformed_data[:, 1])
plt.xlabel('Principal Component 1')
plt.ylabel('Principal Component 2')
plt.title('PCA Transformed Data')
plt.show()

# Print explained variance
print("Explained variance ratio:", explained_variance)