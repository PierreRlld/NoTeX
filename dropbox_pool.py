#__REQUIRED MODULES 
import os
from math import *
from os.path import basename
import zipfile
from tqdm import tqdm
import pandas as pd
import numpy as np
from shutil import copytree, rmtree

base_dropbox = "/Users/prld/Dropbox"
base_git = "/Users/prld/git/NoTeX"


#------------------------
#Source: https://linuxhint.com/python_zip_file_directory/
def retrieve_file_paths(dirName):
  # setup file paths variable
  filePaths = []
  # Read all directory, subdirectories and file lists
  for root, directories, files in os.walk(dirName):
    for filename in files:
        filePath = os.path.join(root, filename)
        if '/._' in filePath:
          pass
        else:
          filePaths.append(filePath)
  return filePaths