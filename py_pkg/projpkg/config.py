from pathlib import Path
import os

dirname = os.path.dirname(__file__)
PY_HOME = Path(dirname, "..")
DATA_DIR = Path(PY_HOME, 'data')
DL_DIR = Path(DATA_DIR, "datalake")
DW_DIR = Path(DATA_DIR, "datawarehouse")

URI_DATASET = 'https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data'

DL_FILE_HOUSING = Path(DL_DIR, "housing_data.csv")
