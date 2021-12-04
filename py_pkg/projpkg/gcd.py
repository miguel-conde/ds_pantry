# stuff to run always here such as class/def

import os
import sys
sys.path.insert(1, '..')
import projpkg as pp

import pandas as pd

def get_housing_data():
    """
    Descarga el dataset si no está en 'data/'
    """
    features_names = ['crim', 'zn', 'indus', 'chas', 'nox', 'rm', 'age', 
                  'dis', 'rad', 'tax', 'pt_ratio', 'bks', 'lstat', 'medv']
                  
    if not os.path.exists(pp.config.DL_FILE_HOUSING):
        
        uri_dataset = pp.config.URI_DATASET
        
        housing_data = pd.read_csv(uri_dataset,
                                    sep = ' ', 
                                    skipinitialspace = True,
                                    names = features_names,
                                    index_col = False)
  
        # Lo guardamos localmente para futuros usos
        housing_data.to_csv(pp.config.DL_FILE_HOUSING, index = False)

    else:
        housing_data = pd.read_csv(pp.DL_FILE_HOUSING)

    return housing_data

def soy_gcd(s):
    """
    Soy gcd

    :param s: escribe algo
    :return: lo que has escrito
    """

    print("soy gcd() " + s)


def main():
    pass

if __name__ == "__main__":
   # stuff only to run when not called via 'import' here
   main()