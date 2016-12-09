# coding==UTF8
import os,time
import pandas as pd
import numpy as np



def readacsv(file):
    with open(file,'r+') as f:
        w=pd.read_csv(file,skip_footer=1,engine='python')
    return w



if __name__ == '__main__':
    target="https://github.com/sn0wfree/kickstarter_dataset/blob/master/data/finaldata/projectdataset.csv"
    tar=readacsv(target)
    print tar
