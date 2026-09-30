from pathlib import Path
import numpy as np
import pandas as pd
import geopandas as gpd
import matplotlib as mpl
from matplotlib import pyplot as plt
from mpl_toolkits.axes_grid1 import make_axes_locatable

ROOT = Path(__file__).resolve().parents[1]
DATA = ROOT / "Data"
MAP = ROOT / "Map"
FIGURES = ROOT / "Figures"
CAMERA_LABELS = {
    "C7": (-110.198, 31.8),
    "C6": (-110.193, 31.76),
    "C1": (-110.183, 31.716),
    "C8": (-110.174, 31.683),
    "C2": (-110.168, 31.66),
    "C3": (-110.21, 31.604),
    "C4": (-110.16, 31.474),
    "C5": (-110.149, 31.455),
}
GAGE_STYLES = {
    "GW": dict(label = "Groundwater", marker = "o", color = "saddlebrown", zorder = 2),
    "Q": dict(label = "Stream", marker = "s", color = "gold", zorder = 2),
    "P": dict(label = "Precipitation", marker = "droplet", color = "purple", markersize = 70, zorder = 2),
    "ET": dict(label = "ET", marker = "^", color = "forestgreen", markersize = 50, zorder = 3),
}

def load_cameras():
    flow = pd.read_csv(DATA / "USPPFlowMonitoring2006_2025.csv", usecols = ["Site", "Flow Code"]).dropna(subset = ["Flow Code"])
    persistence = flow.assign(Persistence = flow["Flow Code"].isin([1, 2])).groupby("Site")["Persistence"].mean()

    cameras = gpd.read_file(MAP / "StreamflowCameras.shp")
    cameras = cameras[cameras["Name"].str.endswith(" Camera")].copy()
    cameras["Site"] = cameras["Name"].str.replace(" Camera", "", regex = False)
    return cameras.merge(persistence, left_on = "Site", right_index = True, validate = "one_to_one")

def load_gages():
    gages = pd.read_csv(MAP / "Gages.csv", dtype = {"Name": str})
    return gpd.GeoDataFrame(gages, geometry = gpd.points_from_xy(gages["Longitude"], gages["Latitude"]), crs = "EPSG:4326")

def main():
    streamline = gpd.read_file(MAP / "USPP_reach.shp")
    cameras = load_cameras()
    gages = load_gages()

    fig, (ax1, ax2) = plt.subplots(1, 2, figsize = (10, 8), gridspec_kw = {"wspace": -0.45})
    fig.subplots_adjust(left = 0.02, right = 0.98, wspace = -0.45)

    norm = mpl.colors.Normalize(vmin = 0.5, vmax = 1)

    streamline.plot(ax = ax1, zorder = 1)
    cameras.plot(column = "Persistence", ax = ax1, cmap = "Blues", markersize = 50, norm = norm, edgecolor = "black", label = "Camera", zorder = 3)
    ax1.annotate("", xy = (-110.10, 31.57), xytext = (-110.08, 31.52), arrowprops = dict(arrowstyle = "->", color = "tab:blue", linewidth = 2), zorder = 4)
    ax1.text(-110.10, 31.50, "Flow Direction", color = "tab:blue", fontsize = 9)
    ax1.annotate("N", xy = (-110.223, 31.4), xytext = (-110.23, 31.35), arrowprops = dict(arrowstyle = "->", color = "black", linewidth = 2), zorder = 4)

    xlim1 = ax1.get_xlim()
    ax1.set_xlim(xlim1[0] - 0.02, xlim1[1] + 0.01)

    divider = make_axes_locatable(ax1)
    cax = divider.append_axes("left", size = "5%", pad = 0.02)
    sm = mpl.cm.ScalarMappable(cmap = "Blues", norm = norm)
    cbar = fig.colorbar(sm, cax = cax)
    cbar.set_label("Flow Persistence")
    cbar.ax.yaxis.set_label_position("left")
    cbar.ax.yaxis.set_ticks_position("left")

    theta = np.radians(np.linspace(150, 390, 50))
    droplet = mpl.path.Path(np.vstack([[0, 1.5], np.column_stack([np.cos(theta), np.sin(theta) - 0.5]), [0, 1.5]]), closed = True)

    streamline.plot(ax = ax2, zorder = 1)
    for gage_type, style in GAGE_STYLES.items():
        style = dict(style)
        if style["marker"] == "droplet":
            style["marker"] = droplet
        gages[gages["Gage"] == gage_type].plot(ax = ax2, edgecolor = "black", **style)
    ax2.legend(loc = "lower center", bbox_to_anchor = (0.2, 0.02), fontsize = "small")

    for ax in [ax1, ax2]:
        ax.set_axis_off()
    for label, (x, y) in CAMERA_LABELS.items():
        ax1.text(x, y, label)

    fig.suptitle("Flow Persistence at Cameras", y = 0.94)

    fig.savefig(FIGURES / "map.png", format = "png", dpi = 300, bbox_inches = "tight")

if __name__ == "__main__":
    main()
