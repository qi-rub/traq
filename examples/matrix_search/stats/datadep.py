import pandas as pd
import matplotlib.pyplot as plt

# Load data
df = pd.read_csv('datadep.csv')

# Filter out rows where good == 0
df = df[df['good'] != 0]

# Plot cost vs good (g) for each unique value of n
plt.figure(figsize=(10, 6))
for n_value in sorted(df['n'].unique()):
    subset = df[df['n'] == n_value]
    subset_grouped = subset.groupby('good')['cost'].mean().reset_index()
    plt.plot(
        subset_grouped['good'], subset_grouped['cost'],
        marker='o', label=f'N={n_value}'
    )

plt.xlabel('#solutions')
plt.ylabel('Cost')
plt.title('Cost vs #solutions for NxN matrices')
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.savefig('datadep.png')
plt.close()
