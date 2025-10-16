import argparse
import logging
import os

from nmf_analysis import data
from nmf_analysis import settings as s

logger = logging.getLogger(__name__)
logging.getLogger().setLevel(logging.INFO)


def generate() -> None:
    """
    Load the dataset store in HDF database.
    """
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--dataset",
        default="ID*/metadata_ID*.csv",
        help="raw dataset to generate train and test data",
    )
    args = parser.parse_args()

    input_location = os.path.join(s.DATA_RAW, args.dataset)
    df_meta = data.generate_metadata(input_location)
    df_meta.to_csv(os.path.join(s.DATA_TRANSFORMED, "meta.csv"), index=False)


if __name__ == "__main__":
    generate()
