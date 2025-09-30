"""Command line interface."""

import time
import zipfile

import typer
from tqdm import tqdm

from . import data, nmf
from . import settings as s
from . import transformers as t
from . import utils
from .settings import logger

app = typer.Typer()


@app.command()
def extract(experiment_id: int = 65) -> None:
    """Extract an experiments ZIP file.

    Parameters
    ----------
    experiment_id : int
        The experiment ID.
    """
    with zipfile.ZipFile(s.DATA_RAW / f"ID{experiment_id:04d}.zip") as zf:
        for member in tqdm(zf.infolist(), desc="Extracting"):
            try:
                zf.extract(member, s.DATA_RAW)
            except zipfile.error as e:
                logger.error("Could not extract {} ({})".format(member.filename, e))


@app.command()
def generate(
    experiment_id: str = "ID0065",
    machine_name: str = "ETHOSCOPE_PV_01",
    region_id: int = 1,
    reference_time: str = "8:00 AM",
) -> None:
    """Load the dataset store in HDF database.

    Parameters
    ----------
    experiment_id : int
        The experiment id.
    machine_name : str
        The machine name.
    region_id : int
        The region id.
    """
    transformers = []
    transformers.append(t.ToLocationVector(n_bins=100, aggregate_days=True))
    transformers.append(t.ToActivityVector(aggregate_days=True))
    transformers.append(t.ToSleepVector(aggregate_days=True))
    transformers.append(t.ToImmobileVector(aggregate_days=True))
    # transformers.append(t.ToFoodVector(aggregate_days=True))
    transformers.append(t.ToWalkVector(aggregate_days=True))
    transformers.append(t.ToLocationVector(n_bins=100, aggregate_days=False))
    transformers.append(t.ToActivityVector(aggregate_days=False))
    transformers.append(t.ToSleepVector(aggregate_days=False))
    transformers.append(t.ToImmobileVector(aggregate_days=False))
    # transformers.append(t.ToFoodVector(aggregate_days=False))
    transformers.append(t.ToWalkVector(aggregate_days=False))
    reference_hour = time.strptime(reference_time, "%I:%M %p").tm_hour
    vecs = data.generate_vectors(
        int(experiment_id[2:]), machine_name, region_id, reference_hour, transformers
    )
    directory = utils.get_directory(
        s.DATA_TRANSFORMED, int(experiment_id[2:]), machine_name, region_id
    )
    directory.mkdir(parents=True, exist_ok=True)
    for transformer, df_vec in vecs.items():
        df_vec.to_csv(directory / f"{transformer}.csv")


@app.command()
def train(
    behavior: str = "ToSleepVector_aggregated",
    smoothing_sigma: int = 5,
    nb_components: int = 5,
) -> None:
    """Train a NMF model."""
    nmf.train_model(behavior, smoothing_sigma, nb_components)


if __name__ == "__main__":
    app()
