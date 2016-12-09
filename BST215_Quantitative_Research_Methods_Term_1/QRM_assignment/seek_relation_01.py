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
import numpy as np
import gc,copy,time
import statsmodels.formula.api as smf
import matplotlib.pyplot as plt
from statsmodels.sandbox.regression.predstd import wls_prediction_std
import multiprocessing as mp
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
    else :
        if all_list_inpenden != "NA":
            l=len(all_list_inpenden)


    equ=ols_write2command_write_part(all_list_depen,all_list_inpenden)



    return equ
def ols_write2command_write_part(all_list_depen,all_list_inpenden):
    equ={}
    if type(all_list_depen)==list:
        for y in all_list_depen:
            depvar=y+"~"
            indepvar=""
            count=0
            independ_vars=[]
            for x in all_list_inpenden:
                if y==x:
                    pass
                elif y!=x:
                    indepvar+=x
                    independ_vars.append(x)

                    count+=1
                    if count<(l):
                        indepvar+="+"
                    elif count==(l):
                        pass
            if indepvar!="":
                equ[y]=(depvar+indepvar,independ_vars)
            elif indepvar=="":
                equ[y]=("Error",independ_vars)
            #use dependent variables as key, and value include (formula , independ_vars)
        return equ
    elif type(all_list_depen)==str:
        y=all_list_depen
        depvar=y+"~"
        indepvar=""
        count=0
        independ_vars=[]
        for x in all_list_inpenden:
            if y==x:
                pass
            elif y!=x:
                indepvar+=x
                independ_vars.append(x)

                count+=1
                if count<(l):
                    indepvar+="+"
                elif count==(l):
                    pass
        if indepvar!="":
            equ[y]=(depvar+indepvar,independ_vars)
        elif indepvar =="":
            equ[y]=("Error",independ_vars)
        #use dependent variables as key, and value include (formula , independ_vars)
    return equ



def do_ols_test(all_list_depen,all_list_inpenden,dataset,printout=False):
    res_temp={}
    if all_list_depen == str:

        equ=ols_write2command_write_part(all_list_depen,all_list_inpenden)
        (f,independ_vars)=equ[all_list_depen]
        if f != "Error":
            res_temp[all_list_depen]=smf.ols(formula=f, data=dataset).fit()
            if printout:
                print res_temp[all_list_depen].summary()
            else:
                pass
            return res_temp
        else:
            res_temp[all_list_depen]="Error:Non indenpendent variable left"
    else:
        if all_list_depen ==list:
            equ=ols_write2command_write_part(all_list_depen,all_list_inpenden)
            yy=equ.keys()
            for y in yy:
                (f,independ_vars)=equ[y]
                if f != "Error":
                    res_temp[y]=smf.ols(formula=f, data=dataset).fit()
                    if printout:
                        print res_temp[y].summary()
                    else:
                        pass
                    return res_temp
                else:
                    res_temp[all_list_depen]="Error:Non indenpendent variable left"







    return res_temp

def find_largest_p_value(res_temp):
    res_pvlaue=res_temp.pvalues
    res_var=res_temp.pvalues.to_frame().index
    res_r2=res_temp.rsquared
    #res_params=res_temp.params
    top_pvalue=0
    varname="NA"
    for x in xrange(len(res_pvlaue)):
        if res_var[x] != "Intercept":
            if res_pvlaue[x]>=top_pvalue:
                if res_pvlaue[x]>0.1:
                    top_pvalue=res_pvlaue[x]
                    varname=res_var[x]
            else:
                pass
        else:
            pass

    #print (round(top_pvalue,4),varname,round(res_r2,4))
    return (top_pvalue,varname,res_r2)


def write_formula_and_do_ols_test(dataset,all_var_list,Samematrix ,all_list_depen,all_list_inpenden,printout):

    reset_formula=ols_write2command_test(all_var_list,Samematrix ,all_list_depen,all_list_inpenden)
    re_formula=reset_formula[dependent_var[0]][0]
    independ_vars=reset_formula[dependent_var[0]][1]

    res_temp=do_ols_test(re_formula,dataset,printout)
    return (res_temp,independ_vars)


class do_ols_test_class:
    def __ini__(self):
        self.dependent_var=[]
        self.independent_vars=[]
        self.OLS_variables_model="Univariate mode"
        self.cycle_mode="single mode"
        self.formula=[]
        self.ols_result=[]
        self.pvlaue=[]
        self.r2=[]
        self.top_pvalue=[]
    def do_ols(self,dependent_var,independent_vars,dataset):
        self.dependent_var=dependent_var
        self.independent_vars=independent_vars

        equ=ols_write2command_write_formula_function(self.dependent_var,self.independent_vars)
        if len(equ.keys()) == 1:
            self.OLS_variables_model="Univariate mode"
            self.cycle_mode="single mode"
        elif len(equ.keys()) == 2:
            self.OLS_variables_model="Bivariate mode"
            self.cycle_mode="double mode"
        elif len(equ.keys()) >= 2:
            self.OLS_variables_model="Multivariate mode"
            self.cycle_mode="for loop mode"
        else:
            self.OLS_variables_model="Error: cannot recognise the vairables model"
            self.cycle_mode="Error:try single mode"

        self.formula=equ
        ols_result={}
        pvalue={}
        r2={}
        top_pvalue={}
        for dependent_variable in equ.keys():
            ols_result[dependent_variable]=smf.ols(formula=equ[dependent_variable][0], data=dataset).fit()
            #print ols_result[dependent_variable].summary()
            pvalue[dependent_variable]=ols_result[dependent_variable].pvalues.to_frame()

            r2[dependent_variable]=ols_result[dependent_variable].rsquared
            #a=0
            #print pvalue[dependent_variable][0]

            #for i in pvalue[dependent_variable]:
            #    if p >= a:
            #        a=p
            #    else:
            #        pass
            #top_pvalue=
            a=0
            label="na"
            for var in pvalue[dependent_variable].index.tolist():
                if var != "Intercept":
                    if a<= pvalue[dependent_variable][0][var]:
                        a=pvalue[dependent_variable][0][var]
                        label=var

                    else:
                        pass
                else:
                    pass
            top_pvalue[dependent_variable]=(label,a)
        self.ols_result=ols_result
        self.pvalue=pvalue
        #print self.pvalue
        self.r2=r2
        self.top_pvalue=top_pvalue




def do_ols_test(formula,dataset,printout=False):

    res_temp=smf.ols(formula=formula, data=dataset).fit()
    if printout:
        print res_temp.summary()
    else:
        pass
    return res_temp






def ols_write2command_write_formula_function(all_list_depen,all_list_inpenden):
    equ={}
    l=len(all_list_inpenden)
    if type(all_list_depen)==list:
        for y in all_list_depen:
            depvar=y+"~"
            indepvar=""
            count=0
            independ_vars=[]
            for x in all_list_inpenden:
                if y==x:
                    pass
                elif y!=x:
                    indepvar+=x
                    independ_vars.append(x)

                    count+=1
                    if count< l:
                        indepvar+="+"
                    elif count == l:
                        break
            if indepvar!="":
                equ[y]=(depvar+indepvar,independ_vars)
            elif indepvar=="":
                equ[y]=("Error",independ_vars)
            #use dependent variables as key, and value include (formula , independ_vars)
        return equ
    elif type(all_list_depen)==str:
        y=all_list_depen
        depvar=y+"~"
        indepvar=""
        count=0
        independ_vars=[]
        for x in all_list_inpenden:
            if y==x:
                pass
            elif y!=x:
                indepvar+=x
                independ_vars.append(x)

                count+=1
                if count<(l):
                    indepvar+="+"
                elif count==(l):
                    pass
        if indepvar!="":
            equ[y]=(depvar+indepvar,independ_vars)
        elif indepvar =="":
            equ[y]=("Error",independ_vars)
        #use dependent variables as key, and value include (formula , independ_vars)
    return equ



def ols_optimal(y,independent_vars,all_test,p_value=0.01):
    #all_var_list = copy.deepcopy(independent_vars）
    variables = copy.deepcopy(independent_vars)#must use deepcopy

    if y in variables:
        variables.remove(y)
    else:
        pass



    #print len(all_var_list),len(variables),len(all_var_list)
    temp=do_ols_test_class()
    temp.do_ols(y,variables,all_test)
    (rm_var,p)=temp.top_pvalue[y]
    independent_vars=temp.independent_vars
    #print (rm_var,p),len(whole[depen_var].independent_vars)
    while p>p_value :
        temp_temp=do_ols_test_class()
        if rm_var != "na":
            independent_vars.remove(rm_var)
        elif rm_var not in independent_vars:
            print "%s not in list of independent_vars"%(rm_var)
            break
        elif rm_var == "na":
            break

        temp_temp.do_ols(y,independent_vars,all_test)
        (rm_var,p)=temp_temp.top_pvalue[y]

    return temp_temp


def alter_function_for_map(y):
    #global all_var_list,all_test
    return ols_optimal(y,all_var_list,all_test,0.1)


if __name__ == '__main__':
    global all_var_list,all_test
    f=time.time()
    file_names="/Users/sn0wfree/Dropbox/PhD_1st_study/BST215_Quantitative_Research_Methods_Term_1/QRM_assignment/all.csv"
    #cor_file="/Users/sn0wfree/Dropbox/PhD_1st/sn0wfree.github.io/BST215_Quantitative_Research_Methods_Term_1/QRM_assignment/cor.csv"

    all_test=readacsv(file_names)
    #print all_test

    del all_test['state']
    #print np.log(all_test)

    all_var_list=all_test.keys().tolist()
    #print len(all_var_list)

    #print len(all_var_list)


    gc.enable()
    whole=[]
    if "state" in all_var_list:
        all_var_list.remove("state")
    else:
        pass

    #variables = copy.deepcopy(all_var_list)#must use deepcopy
    #y="gunmurd"


    #whole[y]=ols_optimal(y,all_var_list,all_test)
    #print whole[y].ols_result[y].summary()

    #for y in all_var_list:
    mt=1
    pool=mp.Pool()

    #    whole[y]=ols_optimal(y,all_var_list,all_test,0.01)
    for i in xrange(1):
        if mt==1:
            a=pool.map(alter_function_for_map,all_var_list)
        else:
            a=map(alter_function_for_map,all_var_list)



    #whole=pool.apply(alter_function_for_map,(all_var_list))
    target="emitco2"

    print time.time()-f
    #yy=a[y].ols_result.keys()
    for i in xrange(len(all_var_list)):
        if all_var_list[i]==target:
            y=i
            break
        else:
            pass

    print a[y].ols_result.values()[0].summary()





    #print variables,y

    #variables=copy.deepcopy(all_var_list)#must use deepcopy

    #whole["murdpop2010x"]=ols_optimal("murdpop2010x",variables,all_test)




    #print whole["murdpop2010x"].ols_result["murdpop2010x"].summary()



    """
    #print set(whole.keys())==set(all_var_list)
    #depen_vars=whole.keys()
    for depen_var in depen_vars:


        (rm_var,p)=whole[depen_var].top_pvalue[depen_var]
        independent_vars=whole[depen_var].independent_vars

        #print (rm_var,p),len(whole[depen_var].independent_vars)
        while p>0.01 :
            temp=do_ols_test_class()
            if rm_var != "na":
                independent_vars.remove(rm_var)
            elif rm_var not in independent_vars:
                print "%s not in list of independent_vars"%(rm_var)
                break
            elif rm_var == "na":
                break

            temp.do_ols(depen_var,independent_vars,all_test)
            (rm_var,p)=temp.top_pvalue[depen_var]
            whole[depen_var]=temp
            return temp
    """

























    """

    for y in all_var_list:
    #y=depen_var[0]

        #print y
        #(equation_formula,independ_vars)=equ[y]
        #print  len(independ_vars)
        #if equation_formula !="Error":
            #initialization test
            res_temp=do_ols_test(y,all_var_list,all_test,printout = 0)
            (top_value,top_label,res_r2)=find_largest_p_value(res_temp[y])
            #print (top_value,top_label,res_r2)
            dependent_var=[]
            dependent_var.append(y)
            while 1:
                independ_vars.remove(top_label)
                if independ_vars !=[]:
                    (res_temp,independ_vars)=write_formula_and_do_ols_test(dataset=all_test,all_var_list=all_var_list,all_list_depen=dependent_var,all_list_inpenden=independ_vars)
                    (top_value,top_label,res_r2) = find_largest_p_value(res_temp)
                    outcome="continues"
                    if top_value>0.1:
                        pass
                    else:
                        break

                else:
                    outcome="all fail"
                    break


        (res_temp,independ_vars)=write_formula_and_do_ols_test(dataset=all_test,all_var_list=all_var_list,Samematrix = 0,all_list_depen=dependent_var,all_list_inpenden=independ_vars,printout=0)
        print res_temp.summary()
        """








            #print (top_value,top_label)

    #x=equation[11]





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
