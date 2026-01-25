import sys
import pandas as pd
import matplotlib.pyplot as plt

src = sys.stdin

df = pd.read_csv(src)

# Basic plot with all lines
plt.figure(figsize=(10, 6))
# plt.ylim(0, 10)           # Fixed range from 0 to 10

for column in df.columns[1:]:  # Skip 'turns' column
    plt.plot(df['turns'], df[column], label=column, marker='o')

plt.xlabel('Turns')
plt.ylabel('Values')
plt.title('Multiple Lines Comparison')
plt.legend()
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.show()
