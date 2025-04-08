# NOTE: at least 80% of this is help from AI!

import numpy as np
import pandas as pd

from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt

# Sample data (replace with your actual data)
pop = pd.read_csv('data/songs_liwc_25_03_13.csv')

# Standardize the data (important for PCA)
scaler = StandardScaler()
scaled_data = scaler.fit_transform(pop.select_dtypes((int, float)).dropna())

# Apply PCA
pca = PCA(n_components=2)  # Choose the number of components
pca.fit(scaled_data)

# Project the data onto the principal components
pc_scores = pca.transform(scaled_data)

# Get the loadings (components_)
loadings = pca.components_

# Create a DataFrame of loadings
loading_df = pd.DataFrame(loadings.T, columns=[f'PC{i+1}' for i in range(loadings.shape[0])], 
                          index=pop.select_dtypes((int, float)).columns)

# Create a figure for the biplot
plt.figure(figsize=(10, 8))

# Plot the projected data points (scores) on the first two principal components
#plt.scatter(pc_scores[:, 0], pc_scores[:, 1], alpha=0.5, label='Data points')

# Add the variable loadings (arrows) to the plot
for i in range(loadings.shape[1]):
    plt.arrow(0, 0, loadings[0, i] * 5, loadings[1, i] * 5, 
              color='r', alpha=0.75, head_width=0.1, head_length=0.1)
    plt.text(loadings[0, i] * 5.1, loadings[1, i] * 5.1, pop.select_dtypes((int, float)).columns[i], 
             color='black', ha='center', va='center')

# Set labels and title
plt.title('PCA Biplot')
plt.xlabel('PC 1')
plt.ylabel('PC 2')
plt.axhline(0, color='black',linewidth=0.5)
plt.axvline(0, color='black',linewidth=0.5)
plt.grid(True)
plt.tight_layout()
plt.show()

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