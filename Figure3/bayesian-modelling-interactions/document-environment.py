# Environment capture snippet for a Jupyter notebook
# - Prints OS + Python + key runtime details
# - Captures conda/pip package state when available
# - Saves everything to a timestamped folder you can archive with the notebook

from __future__ import annotations

import json
import os
import platform
import shutil
import subprocess
import sys
from datetime import datetime
from pathlib import Path

def _run(cmd: list[str]) -> dict:
    """Run a command and return {cmd, returncode, stdout, stderr}."""
    try:
        p = subprocess.run(cmd, capture_output=True, text=True, check=False)
        return {
            "cmd": cmd,
            "returncode": p.returncode,
            "stdout": p.stdout.strip(),
            "stderr": p.stderr.strip(),
        }
    except Exception as e:
        return {"cmd": cmd, "error": repr(e)}

def _which(exe: str) -> str | None:
    p = shutil.which(exe)
    return p if p else None

ts = datetime.now().strftime("%Y%m%d_%H%M%S")
out_dir = Path(f"env_capture_{ts}")
out_dir.mkdir(exist_ok=True)

capture = {
    "timestamp_local": ts,
    "python": {
        "executable": sys.executable,
        "version": sys.version,
        "prefix": sys.prefix,
        "base_prefix": getattr(sys, "base_prefix", None),
        "path_first_entries": sys.path[:10],
    },
    "platform": {
        "platform": platform.platform(),
        "system": platform.system(),
        "release": platform.release(),
        "machine": platform.machine(),
        "processor": platform.processor(),
        "python_build": platform.python_build(),
    },
    "env_vars": {
        # useful, non-secret env vars (avoid dumping everything)
        "CONDA_DEFAULT_ENV": os.environ.get("CONDA_DEFAULT_ENV"),
        "CONDA_PREFIX": os.environ.get("CONDA_PREFIX"),
        "VIRTUAL_ENV": os.environ.get("VIRTUAL_ENV"),
        "PYTHONPATH": os.environ.get("PYTHONPATH"),
        "PATH_head": os.environ.get("PATH", "").split(os.pathsep)[:10],
    },
    "tools": {
        "conda": _which("conda"),
        "pip": _which("pip"),
        "mamba": _which("mamba"),
        "micromamba": _which("micromamba"),
        "git": _which("git"),
    },
    "commands": {},
}

# pip state (always try, using the current interpreter)
capture["commands"]["pip_freeze"] = _run([sys.executable, "-m", "pip", "freeze"])
capture["commands"]["pip_check"]  = _run([sys.executable, "-m", "pip", "check"])
capture["commands"]["pip_debug"]  = _run([sys.executable, "-m", "pip", "debug", "--verbose"])

# conda state (only if conda is present)
if capture["tools"]["conda"]:
    capture["commands"]["conda_info"]   = _run(["conda", "info"])
    capture["commands"]["conda_list"]   = _run(["conda", "list"])
    capture["commands"]["conda_export"] = _run(["conda", "env", "export", "--no-builds"])
    # "explicit" is the most reproducible for conda (pins exact artifacts/channels)
    capture["commands"]["conda_explicit"] = _run(["conda", "list", "--explicit"])

# git state (only if inside a repo and git is present)
if capture["tools"]["git"]:
    capture["commands"]["git_rev_parse"] = _run(["git", "rev-parse", "HEAD"])
    capture["commands"]["git_status"]    = _run(["git", "status", "--porcelain"])
    capture["commands"]["git_remote_v"]  = _run(["git", "remote", "-v"])
    capture["commands"]["git_diff"]      = _run(["git", "diff"])

# Optional: capture Jupyter/kernel info (best-effort)
try:
    import IPython
    capture["ipython"] = {
        "version": IPython.__version__,
        "ipython_path": getattr(IPython, "__file__", None),
    }
except Exception as e:
    capture["ipython_error"] = repr(e)

try:
    import jupyter
    capture["jupyter"] = {
        "version": getattr(jupyter, "__version__", None),
        "jupyter_path": getattr(jupyter, "__file__", None),
    }
except Exception as e:
    capture["jupyter_error"] = repr(e)

# Write artifacts
(out_dir / "env_capture.json").write_text(json.dumps(capture, indent=2), encoding="utf-8")

# Also write commonly expected plaintext files for convenience
(out_dir / "pip_freeze.txt").write_text(capture["commands"]["pip_freeze"].get("stdout", ""), encoding="utf-8")

if "conda_export" in capture["commands"]:
    (out_dir / "conda_env_export.yml").write_text(capture["commands"]["conda_export"].get("stdout", ""), encoding="utf-8")
if "conda_explicit" in capture["commands"]:
    (out_dir / "conda_explicit.txt").write_text(capture["commands"]["conda_explicit"].get("stdout", ""), encoding="utf-8")

print(f"Wrote environment capture to: {out_dir.resolve()}")
print(f"- {out_dir/'env_capture.json'}")
print(f"- {out_dir/'pip_freeze.txt'}")
if (out_dir / "conda_env_export.yml").exists():
    print(f"- {out_dir/'conda_env_export.yml'}")
if (out_dir / "conda_explicit.txt").exists():
    print(f"- {out_dir/'conda_explicit.txt'}")
