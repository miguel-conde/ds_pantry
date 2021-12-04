from pathlib import Path

DATA_DIR = Path('/mnt/c/Users/migue/Documents/PROYECTOS DATA SCIENCE/ds_pantry/py_pkg/data')
DL_DIR = Path(DATA_DIR, "datalake")
DW_DIR = Path(DATA_DIR, "datawarehouse")

URI_DATASET = 'https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data'

DL_FILE_HOUSING = Path(DL_DIR, "housing_data.csv")
