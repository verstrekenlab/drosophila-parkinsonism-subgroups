import re
from pathlib import Path
from typing import Union


def get_directory(
    transformed_data_loc: Path,
    experiment_id: Union[int, str],
    machine_name: str,
    region_id: int,
) -> Path:
    if isinstance(experiment_id, str):
        experiment_id = int(re.sub("\D", "", experiment_id))
    return (
        transformed_data_loc
        / f"{experiment_id:04d}"
        / machine_name
        / f"{region_id:02d}"
    )
