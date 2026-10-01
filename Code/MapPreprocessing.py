from pathlib import Path
import pandas as pd
from dataretrieval import waterdata

ROOT = Path(__file__).resolve().parents[1]
DATA = ROOT / "Data"
MAP = ROOT / "Map"
STREAM_GAGES = ["09470500", "09470920", "09471000", "09471550"]
FLUX_TOWERS = pd.DataFrame(
    {
        "Name": ["US-CMW", "US-LS1", "US-LS2"],
        "Latitude": [31.6637, 31.5615, 31.5659],
        "Longitude": [-110.1777, -110.1403, -110.1344],
    }
)

def groundwater_gages():
    return (
        pd.read_csv(DATA / "GW" / "CamSPRNCA_GW.csv", dtype={"site_no": str})
        .drop_duplicates("site_no")
        .rename(columns={"site_no": "Name", "dec_lat_va": "Latitude", "dec_long_va": "Longitude", "camera": "Camera"})
        [["Name", "Latitude", "Longitude", "Camera"]]
        .assign(Gage="GW", Source="USGS")
    )

def stream_gages():
    sites, _ = waterdata.get_monitoring_locations(
        monitoring_location_id=[f"USGS-{site}" for site in STREAM_GAGES]
    )
    return pd.DataFrame(
        {
            "Name": sites["monitoring_location_number"],
            "Latitude": sites.geometry.y,
            "Longitude": sites.geometry.x,
            "Gage": "Q",
            "Source": "USGS",
        }
    )

def precipitation_gages():
    return (
        pd.read_csv(DATA / "Precipitation.csv", usecols=["device_name", "latitude", "longitude"])
        .groupby("device_name", as_index=False).first()
        .rename(columns={"device_name": "Name", "latitude": "Latitude", "longitude": "Longitude"})
        .assign(Gage="P")
    )

def main():
    gages = pd.concat(
        [groundwater_gages(), stream_gages(), precipitation_gages(), FLUX_TOWERS.assign(Gage="ET", Source="AmeriFlux")],
        ignore_index=True,
    )[["Gage", "Source", "Name", "Camera", "Latitude", "Longitude"]].astype({"Camera": "Int64"})
    gages.to_csv(MAP / "Gages.csv", index=False)
    print(gages.groupby("Gage").size().to_string())

if __name__ == "__main__":
    main()
