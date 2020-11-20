import pandas as pd
import os


def read_csv_file(file):
  
  print(os.getcwd())
  
  out = pd.read_csv(file, error_bad_lines=False, sep = ";", encoding = 'utf-8')
  # flights = flights[flights['dest'] == "ORD"]
  # flights = flights[['carrier', 'dep_delay', 'arr_delay']]
  out = out.dropna()
  
  return out
  

def in_out(in_r_lst):
  
  out_py_df = pd.DataFrame(in_r_lst)
  
  return out_py_df
