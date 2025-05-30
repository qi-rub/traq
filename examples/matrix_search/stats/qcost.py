import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# Load data
df = pd.read_csv('qcost.csv')

# Group by eps and n to compute mean and standard deviation of cost
grouped = df.groupby(['eps', 'n'])['cost'].agg(['mean', 'var']).reset_index()
grouped['std'] = np.sqrt(grouped['var'])

# Plot average cost with error bars for each eps
plt.figure(figsize=(10, 6))
for eps_value in sorted(df['eps'].unique()):
    subset = grouped[grouped['eps'] == eps_value]
    plt.errorbar(
        subset['n'], subset['mean'], yerr=subset['std'],
        label=f'eps={eps_value}', marker='o', capsize=4
    )

plt.xlabel('N')
plt.ylabel('Quantum Cost')
plt.title('Stats for random NxN matrices')
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.savefig('qcost.pdf')
plt.savefig('qcost.png')
plt.close()
