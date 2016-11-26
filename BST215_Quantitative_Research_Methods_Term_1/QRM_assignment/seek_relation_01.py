#*coding=UTF8*#
#--------------
"""this py is for Quantitative Research Methods
which I have given up by using R to programme something, that is terrible progamme langauge
"""
__version__="0.1.1"
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
def ols_write2command_test(all_list,Samematrix = 1,all_list_depen="NA",all_list_inpenden="NA"):
    if Samematrix == 1:
        all_list_depen=all_list
        all_list_inpenden=all_list
        l=len(all_list_inpenden)-1
    else:
        l=len(all_list_inpenden)
    equ={}
    for y in all_list_depen:
        depvar=y+"~"
        indepvar=""
        count=0
        for x in all_list_inpenden:
            if y==x:
                pass
            elif y!=x:
                indepvar+=x
                count+=1
                if count<(l):
                    indepvar+="+"
                elif count==(l):
                    pass
        if indepvar!=[]:
            equ[y]=depvar+indepvar
    return equ

def do_ols_test(equ_R_form,dataset):
    mod_temp=smf.ols(formula=equ_R_form, data=dataset)
    res_temp = mod_temp.fit()
    #print res_temp.summary()
    return res_temp

def find_largest_p_value(res_temp):
    #initialization
    res_t=res_temp.pvalues.to_frame()
    variables_label=res_t.index
    top_value=res_t[0][1]
    top_label=variables_label[1]
    length=len(res_t[0])#[1:]

    for x in xrange(1,length):
        if res_t[0][x]>=top_value:
            top_value=res_t[0][x]
            top_label=variables_label[x]
        else:
            pass
    return (top_label,top_value)
if __name__ == '__main__':
    file_names="/Users/sn0wfree/Dropbox/PhD_1st/sn0wfree.github.io/BST215_Quantitative_Research_Methods_Term_1/QRM_assignment/all.csv"
    #cor_file="/Users/sn0wfree/Dropbox/PhD_1st/sn0wfree.github.io/BST215_Quantitative_Research_Methods_Term_1/QRM_assignment/cor.csv"

    all_test=readacsv(file_names)
    del all_test['state']
    all_list=all_test.keys().tolist()

    #variables=all_test['Unnamed: 0'].tolist()
    #print all_list[0]
    #print len(all_list),type(all_list)
    #l=len(all_list)

    gc.enable()
#    ols_test(all_list)

    equ=ols_write2command_test(all_list)

    equation=equ.keys()
    #print equ[equation[3]]
    #print equation


    #for x in equation:#2
    x=equation[10]
    if x!=0:

        res_temp=do_ols_test(equ[x],all_test)
        print res_temp.summary()

        res_all={}
        res_all[x]=res_temp

        (top_label,top_value)=find_largest_p_value(res_temp)
        print (top_label,top_value)





    #return res_all










        #res_var=all_list.pop(i)

        #print all_list
        #for







"""
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
"""
