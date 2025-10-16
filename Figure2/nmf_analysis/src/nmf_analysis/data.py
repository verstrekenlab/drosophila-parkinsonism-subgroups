"""Data processing operations."""

import glob
import os
import re
import sqlite3
import warnings
from datetime import datetime, timedelta
from typing import Dict, List

import pandas as pd

from . import settings as s
from .settings import logger


def get_fly_data(
    experiment_id: int, machine_name: str, region_id: int, reference_hour=9
) -> pd.DataFrame:
    """Returns the position tracking data for a single fly.

    Only data  from the start of the experiment (10am UTC) until the end of
    the experiment (10am UTC + 6 days) are returned. The final night is
    discarded because the flies are stimulated.

    Parameters
    ----------
    experiment_id : int
        The experiment ID.
    machine_name : str
        The name of the ethoscope.
    region_id : int
        The ID of the region (id ∈  [1,20])

    Returns
    -------
    pd.DataFrame
        The position tracking data

        ===================  ==============================================================
        t                    timestamp (as `datetime64`)
        x                    x-position (as `int` ∈  [0,500])
        y                    y-position (as `int` ∈  [0,50])
        xy_dist_log10x1000   log-transformed Euclidian distance from previous recording (as `int`)
        speed                corrected speed
        behavior             walking, micromovemnt or immobile
        ===================  ==============================================================

    Raises
    ------
    ValueError
        If no database for the given experiment and machine is found.
    ValueError
        If the given region id is not within the [1,20] range.

    """
    DB_glob = os.path.join(
        s.DATA_RAW, f"ID{experiment_id:04d}", "**", machine_name, "*", "*.db"
    )
    ALL_DB = list(glob.glob(DB_glob, recursive=True))

    DB = False
    if len(ALL_DB) == 0:
        raise ValueError("No database found: " + DB_glob)
    elif len(ALL_DB) > 1:
        for db in ALL_DB:
            if "raw_data" in db:
                DB = db
                break
        if not DB:
            DB = ALL_DB[0]
        warnings.warn(
            "Multiple databases found:\n" + "\n".join(ALL_DB) + "\nUsing " + DB
        )
    else:
        DB = ALL_DB[0]
    if region_id < 1 or region_id > 20:
        raise ValueError("Invalid region id specified.")

    # Open database connection
    with sqlite3.connect(DB) as con:
        # Identify the start and end of the experiment
        data_start_ts = (
            pd.read_sql_query("SELECT * from START_EVENTS", con)
            .set_index("event")
            .at["graceful_start", "t"]
        )
        experiment_start = datetime.fromtimestamp(data_start_ts) + timedelta(days=1)
        experiment_start = experiment_start.replace(
            hour=reference_hour, minute=0, second=0, microsecond=0
        )
        experiment_end = experiment_start + timedelta(days=5)

        # Fly data
        try:
            df = pd.read_sql_query(f"SELECT * from ROI_{region_id}", con).set_index(
                "id"
            )
        except sqlite3.DatabaseError as e:
            raise ValueError(f"Could not read database {DB}.\nError: {e}")
        if len(df) == 0:
            raise ValueError("No data found for the given fly")
        df.sort_values("t", inplace=True)
        df["t"] = pd.to_datetime(df["t"] + data_start_ts * 1000, unit="ms")
        df = df[
            (df["t"] >= pd.Timestamp(experiment_start))
            & (df["t"] < pd.Timestamp(experiment_end))
        ]
        if len(df) == 0:
            raise ValueError(
                "No data found for the given fly after the start of the experiment"
            )

        # Food is on opposite side for region_id > 10
        if region_id > 10:
            df["x"] = 500 - df["x"]

        # Behavioral classification
        # See https://doi.org/10.1371/journal.pbio.2003026
        df["speed"] = (
            10 ** (df.xy_dist_log10x1000 / 1000) / df.diff()["t"].dt.total_seconds()
        )
        df["speed"] = df["speed"] / (0.0042 * 3.125)
        df.loc[df.speed.lt(1), "behavior"] = "immobile"
        df.loc[df.speed.between(1, 5), "behavior"] = "micromovement"
        df.loc[df.speed.gt(5), "behavior"] = "walking"
        df["behavior"] = pd.Categorical(
            df.behavior, categories=["immobile", "micromovement", "walking"]
        )

    return df[["t", "x", "y", "xy_dist_log10x1000", "speed", "behavior"]]


def generate_metadata(raw_data_glob: str) -> pd.DataFrame:
    """Loads the metadata of all experiments and prepares DB for transformed data.

    Parameters
    ----------
    raw_data_glob : str
        Glob pattern to locate all metadata files of the experiments you want to load

    Returns
    -------
    pd.DataFrame
        The metadata of all experiments
    """
    all_data = []
    for raw_data_loc in glob.glob(raw_data_glob):
        logger.info(f"Loading dataset from: {raw_data_loc}")
        df = pd.read_csv(raw_data_loc, index_col=0)

        # Give some overview about the generated dataset
        logger.info("Preprocessing dataset from raw to transformed")
        no_flies = len(df)
        no_dead_flies = round(len(df[df.status != "OK"]) / len(df) * 100, 2)
        logger.info(f"No flies {no_flies} in the dataset")
        logger.info(f"{no_dead_flies} % died during the experiment")

        # Add ID
        _, tail = os.path.split(raw_data_loc)
        try:
            experiment_id = int(re.search("metadata_ID(\d+)\.csv", tail).group(1))
        except AttributeError:
            logger.error("Experiment ID failed to extract")
            experiment_id = 0
        df["ID"] = experiment_id
        all_data.append(df)
    return pd.concat(all_data)


def generate_vectors(
    experiment_id: int,
    machine_name: str,
    region_id: int,
    reference_hour: int,
    transformers: List,
) -> Dict[str, pd.DataFrame]:
    """Create location heatmaps for each fly in each experiment.

    Args:
        transformers (list(t.ToVector)): a list of transformers
        transformed_data_loc (str): the location of the generated transformed data you want to save

    """
    vecs = {}
    df = get_fly_data(experiment_id, machine_name, region_id, reference_hour)
    for transformer in transformers:
        vecs[transformer] = transformer.fit_transform(df)
    return vecs
