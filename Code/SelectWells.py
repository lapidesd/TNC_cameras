from pathlib import Path
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
DATA = ROOT / "Data"
GW_DIR = DATA / "GW"
WELLS_PER_CAMERA = 3
MAX_DISTANCE_KM = 5
SITE_TO_CAMERA = {
    "Fairbank": 1,
    "CharlestonMesquite": 2,
    "Moson": 3,
    "Hunter": 4,
    "Hereford": 5,
    "Contention": 6,
    "St.David": 7,
    "Boquillas": 8,
}

def haversine(lat1, lon1, lat2, lon2):
    """Return great-circle distance in kilometers."""
    earth_radius_km = 6371.0088
    lat1, lon1, lat2, lon2 = map(np.radians, [lat1, lon1, lat2, lon2])
    dlat = lat2 - lat1
    dlon = lon2 - lon1
    a = np.sin(dlat / 2) ** 2 + np.cos(lat1) * np.cos(lat2) * np.sin(dlon / 2) ** 2
    return 2 * earth_radius_km * np.arcsin(np.sqrt(a))

def main():
    flow = pd.read_csv(DATA / "USPPFlowMonitoring2006_2025.csv")
    camera_sites = (
        flow.groupby("Site", as_index=False)
        .agg(latitude=("Latitude", "first"), longitude=("Longitude", "first"))
    )
    camera_sites["camera"] = camera_sites["Site"].map(SITE_TO_CAMERA)
    camera_sites = camera_sites.dropna(subset=["camera"]).copy()

    gauges = pd.read_csv(GW_DIR / "GW_data_SPRNCA.csv")
    gauges["datetime"] = pd.to_datetime(gauges["datetime"])
    gauge_locations = (
        gauges.groupby("site_no", as_index=False)
        .agg(latitude=("dec_lat_va", "first"), longitude=("dec_long_va", "first"))
    )

    distances = np.stack([
        haversine(
            camera["latitude"], camera["longitude"],
            gauge_locations["latitude"].to_numpy(), gauge_locations["longitude"].to_numpy(),
        )
        for _, camera in camera_sites.iterrows()
    ])

    camera_indices, well_indices = np.unravel_index(np.argsort(distances, axis=None), distances.shape)
    wells_per_camera = {camera_index: [] for camera_index in range(len(camera_sites))}
    claimed_wells = set()
    for camera_index, well_index in zip(camera_indices, well_indices):
        camera_index, well_index = int(camera_index), int(well_index)
        if well_index in claimed_wells or len(wells_per_camera[camera_index]) >= WELLS_PER_CAMERA:
            continue
        if distances[camera_index, well_index] > MAX_DISTANCE_KM:
            continue
        wells_per_camera[camera_index].append(well_index)
        claimed_wells.add(well_index)

    mappings = []
    for camera_index, (_, camera) in enumerate(camera_sites.iterrows()):
        for well_index in wells_per_camera[camera_index]:
            gauge = gauge_locations.iloc[well_index]
            mappings.append(
                {
                    "site_no": gauge["site_no"],
                    "site": camera["Site"],
                    "camera": int(camera["camera"]),
                    "distance_km": distances[camera_index, well_index],
                }
            )

    mapping = pd.DataFrame(mappings)
    selected_gw = gauges[gauges["site_no"].isin(mapping["site_no"])].copy()
    selected_gw = selected_gw.merge(mapping, on="site_no", how="left")
    selected_gw["dtgw_m"] = selected_gw["height_abv_lowest_ft"] * 0.3048
    selected_gw = selected_gw.sort_values(["camera", "site_no", "datetime"])

    selected_gw.to_csv(GW_DIR / "CamSPRNCA_GW.csv", index=False)
    print(f"Wrote {len(selected_gw)} groundwater observations from {len(mapping)} wells to CamSPRNCA_GW.csv")

if __name__ == "__main__":
    main()
