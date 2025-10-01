import argparse
import logging

from nmf_analysis.nmf import train_model

logger = logging.getLogger(__name__)
logging.getLogger().setLevel(logging.INFO)


def train() -> None:
    """
    Train a NMF model.
    """
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--behavior",
        default="ToSleepVector_aggregated",
        type=str,
        help="",
    )
    parser.add_argument(
        "--smoothing_sigma",
        default=5,
        type=int,
        help="",
    )
    parser.add_argument(
        "--nb_components",
        default=5,
        type=int,
        help="",
    )
    args = parser.parse_args()

    train_model(args.behavior, args.smoothing_sigma, args.nb_components)


if __name__ == "__main__":
    train()
