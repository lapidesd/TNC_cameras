# import required packages
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import skccm
from skccm import Embed
from skccm import CCM
from skccm.utilities import train_test_split

flow = pd.read_csv("USPPFlowMonitoring2006_2025.csv")
names = {"Boquillas": 8, "CharlestonMesquite": 2, "Contention": 6, "Fairbank": 1, "Hereford": 5, "Hunter": 4, "Moson": 3, "St.David": 7}
flow["Camera"] = flow["Site"].map(names)

precip = pd.read_csv("precipitation_gao.csv")

flow["Date"] = pd.to_datetime(flow["Date"])
precip["date"] = pd.to_datetime(precip["date"], format = "%Y_%m_%d")

# this is a dictionary where I save the results
outputs = {'camera':[],         # this is the basin, you could loop through cameras instead
            'param_predict_flow':[],   # ccm in one direction
            'flow_predict_param':[]}  # ccm in the other direction--make sure you know which is which

for c in sorted(names.values()):
    df = flow[flow["Camera"] == c]
    df = df[["Date", "Flow Code"]].dropna()
    df = df.merge(precip[["date", f"point{c}"]], left_on = "Date", right_on = "date", how = "inner").drop(columns = ["Date", "date"])
    df = df.rename(columns = {f"point{c}": "Precip"})

    # ==========================================
    # 2. CCM PARAMETERS
    # ==========================================
    # Column names to test
    col_1 = "Precip"
    col_2 = "Flow Code"

    # CCM Hyperparameters
    lag = 1       # Time lag (tau)
    embed = 2     # Embedding dimension (E) - usually 2 or 3 for simple systems

    # ==========================================
    # 3. PREPARE DATA (EMBEDDING)
    # ==========================================
    # Extract series
    x1 = df[col_1].values
    x2 = df[col_2].values

    # Embed the time series (Phase Space Reconstruction)
    e1 = Embed(x1)
    e2 = Embed(x2)
    X1_embedded = e1.embed_vectors_1d(lag, embed)
    X2_embedded = e2.embed_vectors_1d(lag, embed)

#     # Split into training/testing (Optional but recommended for validation)
#     # In CCM, we often use the same set for "Library" and "Prediction" to test convergence
#     x1_tr, x1_te, x2_tr, x2_te = train_test_split(X1_embedded, X2_embedded, percent=1.0)
    x1_tr, x1_te, x2_tr, x2_te = X1_embedded, X1_embedded, X2_embedded, X2_embedded

    # ==========================================
    # 4. RUN CONVERGENT CROSS MAPPING
    # ==========================================
    # Initialize CCM
    ccm = CCM()

    # Define Library Lengths (L) to test convergence
    # We test from small libraries to the full length of the data
    len_tr = len(x1_tr)
    lib_lens = np.arange(10, len_tr, 20, dtype='int')

    # Fit the model (Populate the shadow manifolds)
    ccm.fit(x1_tr, x2_tr)

    # Predict:
    # predict() returns predictions of x1 given x2 (x1p) and x2 given x1 (x2p)
    x1p, x2p = ccm.predict(x1_te, x2_te, lib_lengths=lib_lens)

    # Score:
    # Returns Correlation (R^2) between predicted and actual
    sc1, sc2 = ccm.score()

    outputs["camera"].append(c)
    outputs['param_predict_flow'].append(sc2[-1])
    outputs['flow_predict_param'].append(sc1[-1])

#     # ==========================================
#     # 6. INTERPRETATION
#     # ==========================================
#     print(k)
#     print(f"Final Score ({col_2} predicting {col_1}): {sc1[-1]:.4f}")
#     print(f"Final Score ({col_1} predicting {col_2}): {sc2[-1]:.4f}")
outputs = pd.DataFrame.from_dict(outputs)
print(outputs)