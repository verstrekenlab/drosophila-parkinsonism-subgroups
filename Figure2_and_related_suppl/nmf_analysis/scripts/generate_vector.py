import argparse
import logging
import os

from nmf_analysis import data
from nmf_analysis import settings as s
from nmf_analysis import transformers as t

logger = logging.getLogger(__name__)
logging.getLogger().setLevel(logging.INFO)


def generate() -> None:
    """
    Load the dataset, generate vectors and store in csv files.
    """
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--experiment_id",
        default=65,
        type=int,
        help="",
    )
    parser.add_argument(
        "--machine_name",
        default="ETHOSCOPE_PV_01",
        help="",
    )
    parser.add_argument(
        "--region_id",
        default=1,
        type=int,
        help="",
    )
    parser.add_argument(
        "--reference_hour",
        default=8,
        type=int,
        help="",
    )
    args = parser.parse_args()

    transformers = []
    # transformers.append(t.ToLocationVector(n=100, aggregate_days=True))
    # transformers.append(t.ToActivityVector(aggregate_days=True))
    transformers.append(t.ToSleepVector(aggregate_days=True))
    # transformers.append(t.ToImmobileVector(aggregate_days=True))
    # transformers.append(t.ToFoodVector(aggregate_days=True))
    # transformers.append(t.ToWalkVector(aggregate_days=True))
    # transformers.append(t.ToLocationVector(n=100, aggregate_days=False))
    # transformers.append(t.ToActivityVector(aggregate_days=False))
    # transformers.append(t.ToSleepVector(aggregate_days=False))
    # transformers.append(t.ToImmobileVector(aggregate_days=False))
    # transformers.append(t.ToFoodVector(aggregate_days=False))
    # transformers.append(t.ToWalkVector(aggregate_days=False))
    vecs = data.generate_vectors(
        args.experiment_id,
        args.machine_name,
        args.region_id,
        args.reference_hour,
        transformers,
    )
    outdir = os.path.join(
        s.DATA_TRANSFORMED,
        f"{args.experiment_id:04d}",
        args.machine_name,
        f"{args.region_id:02d}",
    )
    os.makedirs(outdir, exist_ok=True)
    for transformation, vector in vecs.items():
        vector.to_csv(os.path.join(outdir, f"{transformation}.csv"), index=True)


if __name__ == "__main__":
    generate()
