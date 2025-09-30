.PHONY: init test lint pretty precommit_install bump_major bump_minor bump_patch
.DEFAULT_GOAL := help


BIN = .venv/bin/
CODE = src/nmf_analysis

init:
	python3 -m venv .venv
	poetry install
lint:
	$(BIN)ruff check $(CODE)
	$(BIN)mypy $(CODE)

pretty:
	$(BIN)ruff check --fix $(CODE)

clean: ## clean artifacts
	@echo ">>> cleaning files"
	rm ./data/predictions/* ./data/transformed/* ./models/*.joblib || true

generate-dataset:  # No longer used. Use metadata_combined.xlsx
	@echo ">>> generating dataset"
	python ./scripts/generate_dataset.py $(ARGS)

generate-vectors: ## run ETL pipeline
	@echo ">>> generating vectors"
	parallel --bar --header : --colsep ',' python scripts/generate_vector.py --experiment_id={experiment_id} --machine_name={machine_name} --region_id={region_id} --reference_hour={reference_hour} :::: data/transformed/meta.csv

train:
	@echo ">>> training models"
	# $(BIN)python scripts/train_models.py --behavior='ToActivityVector_aggregated' --smoothing_sigma=5 --nb_components=4
	# $(BIN)python scripts/train_models.py --behavior='ToActivityVector_aggregated' --smoothing_sigma=5 --nb_components=5
	# $(BIN)python scripts/train_models.py --behavior='ToActivityVector_aggregated' --smoothing_sigma=5 --nb_components=6
	# $(BIN)python scripts/train_models.py --behavior='ToActivityVector_aggregated' --smoothing_sigma=5 --nb_components=7
	# $(BIN)python scripts/train_models.py --behavior='ToImmobileVector_aggregated' --smoothing_sigma=5 --nb_components=4
	# $(BIN)python scripts/train_models.py --behavior='ToImmobileVector_aggregated' --smoothing_sigma=5 --nb_components=5
	# $(BIN)python scripts/train_models.py --behavior='ToImmobileVector_aggregated' --smoothing_sigma=5 --nb_components=6
	# $(BIN)python scripts/train_models.py --behavior='ToImmobileVector_aggregated' --smoothing_sigma=5 --nb_components=7
	$(BIN)python scripts/train_models.py --behavior='ToSleepVector_aggregated' --smoothing_sigma=5 --nb_components=1
	$(BIN)python scripts/train_models.py --behavior='ToSleepVector_aggregated' --smoothing_sigma=5 --nb_components=2
	$(BIN)python scripts/train_models.py --behavior='ToSleepVector_aggregated' --smoothing_sigma=5 --nb_components=3
	# $(BIN)python scripts/train_models.py --behavior='ToSleepVector_aggregated' --smoothing_sigma=5 --nb_components=4
	# $(BIN)python scripts/train_models.py --behavior='ToSleepVector_aggregated' --smoothing_sigma=5 --nb_components=5
	# $(BIN)python scripts/train_models.py --behavior='ToSleepVector_aggregated' --smoothing_sigma=5 --nb_components=6
	# $(BIN)python scripts/train_models.py --behavior='ToSleepVector_aggregated' --smoothing_sigma=5 --nb_components=7
	$(BIN)python scripts/train_models.py --behavior='ToSleepVector_aggregated' --smoothing_sigma=5 --nb_components=8
	$(BIN)python scripts/train_models.py --behavior='ToSleepVector_aggregated' --smoothing_sigma=5 --nb_components=9
	$(BIN)python scripts/train_models.py --behavior='ToSleepVector_aggregated' --smoothing_sigma=5 --nb_components=10

figures:
	python ./scripts/generate_figures.py $(ARGS)

help: ## show help on available commands
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

