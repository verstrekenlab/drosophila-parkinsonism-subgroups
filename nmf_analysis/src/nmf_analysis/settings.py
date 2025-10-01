"""Configuration file.
All static variables can be assigned in this settings.py file
"""

import logging
import logging.config
import sys
from pathlib import Path

import pretty_errors  # NOQA: F401 (imported but unused)
from rich.logging import RichHandler

# Directories
ROOT_DIR = Path(__file__).parent.parent.absolute()
LOGS_DIR = Path(ROOT_DIR, "logs")
DATA_DIR = Path(ROOT_DIR, "data")
DATA_RAW = Path(DATA_DIR, "raw")
DATA_TRANSFORMED = Path(DATA_DIR, "transformed")
RESULTS_DIR = Path(ROOT_DIR, "results")
MODEL_DIR = Path(ROOT_DIR, "models")

# Create dirs
LOGS_DIR.mkdir(parents=True, exist_ok=True)
DATA_DIR.mkdir(parents=True, exist_ok=True)
DATA_RAW.mkdir(parents=True, exist_ok=True)
DATA_TRANSFORMED.mkdir(parents=True, exist_ok=True)
RESULTS_DIR.mkdir(parents=True, exist_ok=True)
MODEL_DIR.mkdir(parents=True, exist_ok=True)

# Logger
logging_config = {
    "version": 1,
    "disable_existing_loggers": False,
    "formatters": {
        "minimal": {"format": "%(message)s"},
        "detailed": {
            "format": "%(levelname)s %(asctime)s [%(filename)s:%(funcName)s:%(lineno)d]\n%(message)s\n"
        },
    },
    "handlers": {
        "console": {
            "class": "logging.StreamHandler",
            "stream": sys.stdout,
            "formatter": "minimal",
            "level": logging.DEBUG,
        },
        "info": {
            "class": "logging.handlers.RotatingFileHandler",
            "filename": Path(LOGS_DIR, "info.log"),
            "maxBytes": 10485760,  # 1 MB
            "backupCount": 10,
            "formatter": "detailed",
            "level": logging.INFO,
        },
        "error": {
            "class": "logging.handlers.RotatingFileHandler",
            "filename": Path(LOGS_DIR, "error.log"),
            "maxBytes": 10485760,  # 1 MB
            "backupCount": 10,
            "formatter": "detailed",
            "level": logging.ERROR,
        },
    },
    "loggers": {
        "root": {
            "handlers": ["console", "info", "error"],
            "level": logging.INFO,
            "propagate": True,
        },
    },
}
logging.config.dictConfig(logging_config)
logger = logging.getLogger("root")
logger.handlers[0] = RichHandler(markup=True)
