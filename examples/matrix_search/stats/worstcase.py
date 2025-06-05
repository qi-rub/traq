import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# Load data
df = pd.read_csv('worstcase.csv')


# Plot cost vs good (g) for each unique value of n
plt.figure(figsize=(10, 6))
xaxis = df['n']
plt.plot(xaxis, df['cost'], marker='o', label='quantum')
plt.plot(xaxis,df['n']**2, marker='x', label='classical deterministic')
plt.plot(xaxis,df['n']**2 / 2, marker='*', label='classical randomized')

plt.xlabel('Input Size $N$')
plt.ylabel('Cost')
plt.title('')
plt.legend()
plt.grid(True)
ax = plt.gca()
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
plt.tight_layout()
plt.savefig('worstcase.png')
plt.close()
