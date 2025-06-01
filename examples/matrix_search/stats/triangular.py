import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# Load data

raw = """
n,cost
10,31876.43
500,1617653.78
1000,3269685.47
1500,5047626.53
2000,6846601.46
2500,8580440.97
3000,10413362.58
3500,12086813.44
4000,13817248.78
4500,15605274.42
5000,17450784.83
5500,19353284.74
6000,20968321.49
"""

def rand_cost(n):
	return n + 0.5 * sum((n + 1)/(r+1) for r in range(1, n))

df = pd.read_csv('triangular.csv')


# Plot cost vs good (g) for each unique value of n
plt.figure(figsize=(10, 6))
plt.plot(df['n'], df['cost'], marker='o', label='quantum')
plt.plot(df['n'],df['n']**2, marker='x', label='classical deterministic')
# plt.plot(df['n'],[rand_cost(n) for n in df['n']], marker='*', label='classical randomized')

plt.xlabel('Input Size $N$')
plt.ylabel('Cost')
plt.title('')
plt.legend()
plt.grid(True)
ax = plt.gca()
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
plt.tight_layout()
plt.savefig('triangular.png')
plt.close()

