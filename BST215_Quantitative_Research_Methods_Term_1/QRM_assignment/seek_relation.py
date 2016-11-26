#*coding=UTF8*#
#--------------
"""this py is for Quantitative Research Methods
which I have given up by using R to programme something, that is terrible progamme langauge
"""
__version__=0.1
__pythonversion__="2.7"
__author__="sn0wfree"
#-------------------
import pandas as pd
import gc
import statsmodels.formula.api as smf
import matplotlib.pyplot as plt
from statsmodels.sandbox.regression.predstd import wls_prediction_std
#now read the orignal document(s)



def readacsv(file):
    with open(file,'r+') as f:
        w=pd.read_csv(file,skip_footer=1,engine='python')
    return w

def cor2collect_dict_list_function(corr_test):
    variables=cor_test.columns.tolist()
    #del cor_test['Unnamed: 0']
    #cor_test.index=variables
    cor_dict={}
    for x in variables:
        cor_list=[]
        for y in xrange(len(cor_test[x])):
            if cor_test[x][y]==1:
                pass

            elif cor_test[x][y]<= -0.5:
                cor_list.append((variables[y],cor_test[x][y]))
            elif cor_test[x][y] >= 0.5 and cor_test[x][y]<1:
                cor_list.append((variables[y],cor_test[x][y]))
            else:
                pass
        cor_dict[x]=cor_list
    return cor_dict
def return_cmline_for_ols(cor_dict):
    keys=cor_dict.keys()
    inp={}
    for key in keys:
        independent_variables_test_tuple=cor_dict[key]
        vars_list_head=key +"~"
        vars_list=""
        countt=0
        l=len(independent_variables_test_tuple)
        if l>1:
            for (var,cor) in independent_variables_test_tuple:

                    if cor==1:
                        pass
                    elif cor<= -0.5:
                        vars_list+=var
                        countt+=1
                    elif cor >= 0.5 and cor<1:
                        vars_list+=var
                        countt+=1
                    if countt<l:
                        vars_list+="+"
            inp[key]=vars_list_head+vars_list
        else:
            pass
    return inp

if __name__ == '__main__':
    file_names="/Users/sn0wfree/Dropbox/PhD_1st/sn0wfree.github.io/BST215_Quantitative_Research_Methods_Term_1/QRM_assignment/all.csv"
    #cor_file="/Users/sn0wfree/Dropbox/PhD_1st/sn0wfree.github.io/BST215_Quantitative_Research_Methods_Term_1/QRM_assignment/cor.csv"

    all_test=readacsv(file_names)
    #variables=all_test['Unnamed: 0'].tolist()
    #del all_test['Unnamed: 0']
    all_test
    res={}
    gc.enable()

    #cor_test=readacsv(cor_file)
    cor_test=all_test.corr()
    #print cor_test
    cor_dict=cor2collect_dict_list_function(cor_test)
    inp=return_cmline_for_ols(cor_dict)
    #print inp["pop2010"]
    keys=inp.keys()
    key="pop2010"
    goodvar_dict={}
    keys_backup=keys
    #print keys[35]
    #del keys[35]

    #print len(keys),keys[36],type(keys)
    for key in keys:
    #if key=="pop2010":

        mod_temp=smf.ols(formula=inp[key], data=all_test)
        res_temp = mod_temp.fit()
        #print res_temp.summary()
        res_data=res_temp.pvalues.to_frame()
        #print res_data
        goodvar_list=[]
        #print res_data
        for p_label in res_data.index:
            if res_data[0][p_label]<=0.1:
                if p_label!="Intercept":
                    goodvar_list.append((p_label,0.8))

        #print goodvar_list
        if goodvar_list!=[]:
            goodvar_dict[key]=goodvar_list
    #print goodvar_dict


    rr=return_cmline_for_ols(goodvar_dict)

    kk=rr.keys()
    for key in kk:
        mod_temp=smf.ols(formula=rr[key], data=all_test)
        res_temp = mod_temp.fit()
        print res_temp.summary()
