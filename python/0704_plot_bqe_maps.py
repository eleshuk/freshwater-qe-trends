"""Maps of the BQE data for data exploration: thesis Fig. 4 (BQEs per water body), Fig. 5 (change in BQEs
between cycles), Fig. 6 and Fig. S8 (status per BQE).

Run from the project root after the maps data step in run_07_figures.R, e.g.
    python python/0704_plot_bqe_maps.py
    python python/0704_plot_bqe_maps.py --joined-file joined_bqe_ref_w2010_final.parquet --spatial-suffix ""
The defaults map the less filtered exploration dataset (UK, coastal waters, angiosperms and macroalgae kept).
Requires Python 3.12 with pandas, geopandas and matplotlib.
"""
import argparse
import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import numpy as np
from matplotlib.cm import ScalarMappable
from pathlib import Path

# EPSG:3035 (ETRS89-LAEA) bounds for the Europe extent used across all maps
# Roughly equivalent to lon [-32, 45], lat [25, 72] in WGS84
EUROPE_XLIM = (2_000_000, 6_500_000)
EUROPE_YLIM = (1_300_000, 5_500_000)

# Tighter bounds for the change maps (drops Iceland, keeps Portugal, Crete and Cyprus)
CHANGE_XLIM = (2_550_000, 6_600_000)
CHANGE_YLIM = (1_380_000, 5_480_000)

# Figure size matches the printed width of a page (6.5 in), so font sizes are true point sizes
FIGSIZE = (6.5, 2.9)
DPI = 400
PANEL_TITLE_SIZE = 9
LEGEND_TITLE_SIZE = 9
LEGEND_TEXT_SIZE = 8
TICK_LABEL_SIZE = 7
SUPTITLE_SIZE = 10
LEGEND_MARKER_SIZE = 8


def spatial_files(spatial_path, suffix):
    """Per-cycle spatial BQE files written by join_bqe_spatial_data(), labelled by cycle."""
    tag = f"_{suffix}" if suffix else ""
    return {
        "Cycle 1 (2010)": Path(spatial_path) / f"bqe_spatial_joined_2010{tag}.parquet",
        "Cycle 2 (2016)": Path(spatial_path) / f"bqe_spatial_joined_2016{tag}.parquet",
        "Cycle 3 (2022)": Path(spatial_path) / f"bqe_spatial_joined_2022{tag}.parquet",
    }


def load_data(joined_path, spatial_path, europe_path, joined_file, spatial_suffix):
    """Load tabular, spatial and basemap data, reprojected to EPSG:3035."""
    europe = gpd.read_file(europe_path).to_crs(epsg=3035)
    tab = pd.read_parquet(Path(joined_path) / joined_file)

    # Union geometries across all three cycles, preferring most recent
    files = spatial_files(spatial_path, spatial_suffix)
    geom = pd.concat([
        gpd.read_parquet(f, columns=["euSurfaceWaterBodyCode", "geom", "geom_type"])
        for f in reversed(list(files.values()))
    ]).drop_duplicates("euSurfaceWaterBodyCode")
    geom = gpd.GeoDataFrame(geom, geometry="geom").to_crs(epsg=3035)

    return tab, geom, europe


def compute_bqe_counts(tab):
    """Count distinct BQEs per water body per cycle and compute pairwise differences."""
    bqe_counts = (
        tab
        .assign(cycle=tab["cYear"].astype(str).map({"2010": "c1", "2016": "c2", "2022": "c3"}))
        .groupby(["euSurfaceWaterBodyCode", "cycle"])["qeCode"]
        .nunique()
        .unstack("cycle")
    )
    bqe_counts["Cycle 1 → 2"] = bqe_counts["c2"] - bqe_counts["c1"]
    bqe_counts["Cycle 2 → 3"] = bqe_counts["c3"] - bqe_counts["c2"]
    bqe_counts["Cycle 1 → 3"] = bqe_counts["c3"] - bqe_counts["c1"]
    return bqe_counts


def style_map_axis(ax, title, xlim, ylim):
    """Apply shared extent, title and tick settings to a map panel."""
    ax.set_xlim(*xlim)
    ax.set_ylim(*ylim)
    ax.set_title(title, fontsize=PANEL_TITLE_SIZE, fontweight="bold", pad=4)
    ax.set_xticks([])
    ax.set_yticks([])


def plot_bqe_count_levels(spatial_path, spatial_suffix, europe, output_path):
    """Map of BQE count per water body, faceted by cycle."""
    cycle_frames = []

    for label, parquet_file in spatial_files(spatial_path, spatial_suffix).items():
        gdf = gpd.read_parquet(parquet_file).to_crs(epsg=3035)
        gdf["cycle"] = label
        cycle_frames.append(gdf)

    combined = gpd.GeoDataFrame(
        pd.concat(cycle_frames, ignore_index=True),
        geometry="geom"
    )

    # Discrete plasma colormap with one colour per BQE count
    max_bqe = int(combined["n_bqe"].max())
    levels = list(range(1, max_bqe + 1))
    cmap = plt.colormaps["plasma"].resampled(max_bqe)
    norm = mcolors.BoundaryNorm(
        boundaries=np.arange(0.5, max_bqe + 1.5),
        ncolors=max_bqe
    )

    cycles = ["Cycle 1 (2010)", "Cycle 2 (2016)", "Cycle 3 (2022)"]
    fig, axes = plt.subplots(1, 3, figsize=FIGSIZE, facecolor="white")
    fig.subplots_adjust(left=0.01, right=0.9, top=0.84, bottom=0.03, wspace=0.04)

    for ax, cycle in zip(axes, cycles):
        subset = combined[combined["cycle"] == cycle]
        polys = subset[subset["geom_type"] == "polygon"]
        lines = subset[subset["geom_type"] == "linestring"]

        ax.set_facecolor("#C8D8E8")
        europe.plot(ax=ax, color="#E8E4DC", edgecolor="#B0A898", linewidth=0.3)
        polys.plot(ax=ax, column="n_bqe", cmap=cmap, norm=norm, edgecolor="none", alpha=0.8)
        lines.plot(ax=ax, column="n_bqe", cmap=cmap, norm=norm, linewidth=0.5, alpha=0.8)

        style_map_axis(ax, cycle, EUROPE_XLIM, EUROPE_YLIM)

    cbar_ax = fig.add_axes([0.915, 0.1, 0.015, 0.7])
    sm = ScalarMappable(cmap=cmap, norm=norm)
    cbar = fig.colorbar(sm, cax=cbar_ax, ticks=levels)
    cbar.set_label("Number of BQEs assessed", fontsize=LEGEND_TITLE_SIZE)
    cbar.ax.tick_params(labelsize=TICK_LABEL_SIZE)

    fig.suptitle(
        "Number of BQEs assessed per water body",
        fontsize=SUPTITLE_SIZE, fontweight="bold", y=0.99
    )
    plt.savefig(output_path, dpi=DPI, bbox_inches="tight")
    plt.close()
    print(f"Saved: {output_path}")


def plot_bqe_count_change(bqe_counts, geom, europe, output_path):
    """Map of change in BQE count between cycles (only paired water bodies)."""
    merged = geom.merge(bqe_counts.reset_index(), on="euSurfaceWaterBodyCode", how="inner")

    # One bin centred on each integer change, with a neutral colour at 0
    bounds = np.arange(-6.5, 7.5, 1)
    cmap = plt.colormaps["RdYlGn"].resampled(13)
    norm = mcolors.BoundaryNorm(bounds, ncolors=cmap.N, clip=True)

    comparisons = ["Cycle 1 → 2", "Cycle 2 → 3", "Cycle 1 → 3"]
    fig, axes = plt.subplots(1, 3, figsize=FIGSIZE, facecolor="white")
    fig.subplots_adjust(left=0.01, right=0.9, top=0.92, bottom=0.03, wspace=0.04)

    for ax, comp in zip(axes, comparisons):
        ax.set_facecolor("#C8D8E8")
        europe.plot(ax=ax, color="#E8E4DC", edgecolor="#B0A898", linewidth=0.3)

        plot_data = merged[merged[comp].notna()].copy()
        polys = plot_data[plot_data["geom_type"] == "polygon"]
        lines = plot_data[plot_data["geom_type"] == "linestring"]

        polys.plot(ax=ax, column=comp, cmap=cmap, norm=norm, edgecolor="none", alpha=0.85)
        lines.plot(ax=ax, column=comp, cmap=cmap, norm=norm, linewidth=0.5, alpha=0.85)

        style_map_axis(ax, comp, CHANGE_XLIM, CHANGE_YLIM)

    cbar_ax = fig.add_axes([0.915, 0.1, 0.015, 0.7])
    sm = ScalarMappable(cmap=cmap, norm=norm)
    sm.set_array([])
    cbar = fig.colorbar(sm, cax=cbar_ax, boundaries=bounds,
                        ticks=np.arange(-6, 7, 1), spacing="uniform",
                        drawedges=True)
    cbar.set_label("Change in BQEs", fontsize=LEGEND_TITLE_SIZE)
    cbar.ax.tick_params(labelsize=TICK_LABEL_SIZE)

    plt.savefig(output_path, dpi=DPI, bbox_inches="tight")
    plt.close()
    print(f"Saved: {output_path}")


def plot_bqe_status_maps(tab, geom, europe, output_dir):
    """Map quality element status per BQE, faceted by reporting cycle."""
    WFD_COLOURS = {
        "High": "#0070C0", "Good": "#00B050", "Moderate": "#FFFF00",
        "Poor": "#FF6600", "Bad": "#FF0000",
    }
    status_order = ["High", "Good", "Moderate", "Poor", "Bad"]
    cmap = mcolors.ListedColormap([WFD_COLOURS[s] for s in status_order])
    norm = mcolors.BoundaryNorm(np.arange(-0.5, len(status_order)), cmap.N)
    status_to_int = {s: i for i, s in enumerate(status_order)}

    cycle_map = {"2010": "Cycle 1 (2010)", "2016": "Cycle 2 (2016)", "2022": "Cycle 3 (2022)"}
    cycles = list(cycle_map.values())

    for qe_code in sorted(tab["qeCode"].dropna().unique()):
        bqe_name = qe_code.split(" - ", 1)[1] if " - " in qe_code else qe_code

        subset = tab[tab["qeCode"] == qe_code].copy()
        subset["cycle"] = subset["cYear"].astype(str).map(cycle_map)
        subset["status_int"] = subset["qeStatusOrPotentialValueLabel"].map(status_to_int)
        subset = subset.dropna(subset=["status_int", "cycle"])

        merged = geom.merge(
            subset[["euSurfaceWaterBodyCode", "cycle", "status_int"]],
            on="euSurfaceWaterBodyCode",
            how="inner",
        )

        if merged.empty:
            continue

        fig, axes = plt.subplots(1, 3, figsize=FIGSIZE, facecolor="white")
        fig.subplots_adjust(left=0.01, right=0.99, top=0.84, bottom=0.2, wspace=0.04)

        for ax, cycle in zip(axes, cycles):
            cycle_data = merged[merged["cycle"] == cycle]
            polys = cycle_data[cycle_data["geom_type"] == "polygon"]
            lines = cycle_data[cycle_data["geom_type"] == "linestring"]

            ax.set_facecolor("#C8D8E8")
            europe.plot(ax=ax, color="#E8E4DC", edgecolor="#B0A898", linewidth=0.3)

            if not polys.empty:
                polys.plot(ax=ax, column="status_int", cmap=cmap, norm=norm,
                           edgecolor="none", alpha=0.85)
            if not lines.empty:
                lines.plot(ax=ax, column="status_int", cmap=cmap, norm=norm,
                           linewidth=0.5, alpha=0.85)

            style_map_axis(ax, cycle, EUROPE_XLIM, EUROPE_YLIM)

        legend_handles = [
            plt.Line2D([0], [0], marker="s", color="w",
                       markerfacecolor=WFD_COLOURS[s], markersize=LEGEND_MARKER_SIZE, label=s)
            for s in status_order
        ]
        fig.legend(
            handles=legend_handles, loc="lower center", ncol=5,
            fontsize=LEGEND_TEXT_SIZE, title="Quality element status",
            title_fontsize=LEGEND_TITLE_SIZE, frameon=False,
            bbox_to_anchor=(0.5, 0.0), handletextpad=0.2, columnspacing=1.0,
        )

        fig.suptitle(bqe_name, fontsize=SUPTITLE_SIZE, fontweight="bold", x=0.01, ha="left", y=0.99)

        safe_name = qe_code.split(" - ")[0].replace("-", "_")
        out = Path(output_dir) / f"bqe_status_{safe_name}.png"
        plt.savefig(out, dpi=DPI, bbox_inches="tight")
        plt.close()
        print(f"Saved: {out}")


def main():
    """Read the command line options and draw all maps."""
    parser = argparse.ArgumentParser(description="Draw the BQE exploration maps")
    parser.add_argument("--wfd-root", default="data/WFD", help="WFD data folder")
    parser.add_argument("--joined-file", default="joined_bqe_ref_maps_final.parquet",
                        help="Joined BQE table in parquet_tables_joined")
    parser.add_argument("--spatial-suffix", default="maps",
                        help="Suffix of the bqe_spatial_joined_<year>_<suffix>.parquet files; empty for the thesis dataset")
    parser.add_argument("--europe", default="data/drivers/ne_50m_europe.gpkg", help="Basemap of Europe")
    parser.add_argument("--output-dir", default="plots/maps", help="Folder for the map images")
    args = parser.parse_args()

    joined_path = Path(args.wfd_root) / "parquet_tables_joined"
    spatial_path = Path(args.wfd_root) / "parquet_tables_spatial"
    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    tab, geom, europe = load_data(joined_path, spatial_path, args.europe, args.joined_file, args.spatial_suffix)
    bqe_counts = compute_bqe_counts(tab)

    plot_bqe_count_levels(spatial_path, args.spatial_suffix, europe, output_path=output_dir / "bqe_count_facet.png")
    plot_bqe_count_change(bqe_counts, geom, europe, output_path=output_dir / "bqe_count_change.png")
    plot_bqe_status_maps(tab, geom, europe, output_dir=output_dir)


if __name__ == "__main__":
    main()
