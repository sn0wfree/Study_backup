# coding==UTF8
import pandas as pd
import numpy as np
import datetime,time





def readacsv(file):
    with open(file,'r+') as f:
        w=pd.read_csv(file,skip_footer=1,engine='python')
    return w



if __name__ == '__main__':
    test="/Users/sn0wfree/Dropbox/Book3.csv"
    test2="/Users/sn0wfree/Dropbox/Book3.1.csv"
    timestamp="/Users/sn0wfree/Dropbox/timestamp.csv"
    qrm="/Users/sn0wfree/Dropbox/crowdfundingforqrm.csv"
    #target="https://github.com/sn0wfree/kickstarter_dataset/blob/master/data/finaldata/projectdataset.csv"
    #tar=readacsv(target)
    print tar
