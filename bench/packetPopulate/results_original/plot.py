import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import numpy as np

# Read CSV
df = pd.read_csv("combined.csv")

# Validate required columns
required_columns = {"bytePerPacket", "bytePerSec", "packet"}
missing = required_columns - set(df.columns)
if missing:
    raise ValueError(f"Missing required columns: {missing}")

plt.figure(figsize=(8, 6))

# Get unique sources
sources = df["packet"].unique()
num_sources = len(sources)

# Choose a colormap
colormap = cm.get_cmap("tab20", num_sources)  # tab20 has up to 20 distinct colors
# If more than 20 sources, you can use 'hsv' instead for continuous spectrum:
# colormap = cm.get_cmap("hsv", num_sources)

# Plot each source with a unique color
for i, source in enumerate(sources):
    group = df[df["packet"] == source]
    plt.scatter(
        group["bytePerPacket"],
        group["bytePerSec"],
        label=source,
        color=colormap(i)
    )

plt.xlabel("bytePerPacket")
plt.ylabel("bytePerSec")
plt.title("Byte Per Second vs Byte Per Packet")

# Legend outside
plt.legend(title="source_file", loc="center left", bbox_to_anchor=(1.0, 0.5))
plt.grid(True)

plt.savefig("scatter.png", bbox_inches="tight", dpi=300)
