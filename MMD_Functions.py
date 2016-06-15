# -*- coding: utf-8 -*-
"""
Created on Tue Oct 13 13:33:25 2015

@author: ny1j035
"""



import pyodbc
import datetime
import pandas as pd
import numpy as np
import scipy as sp
import scipy.optimize
import matplotlib.pyplot as plt

cnxn = pyodbc.connect('DSN=pachp;UID=ny1j035;PWD=monday2')


cursor = cnxn.cursor()
aa = cursor.execute("""select * from ipgadm.fe_mmd_rates a
                        where a.effdate in (select max(b.effdate) from ipgadm.fe_mmd_rates b)""")

qryMmd = """select * from ipgadm.fe_mmd_rates a
                        where a.effdate in (select max(b.effdate) from ipgadm.fe_mmd_rates b)"""

#mmdRates = DataFrame(aa.fetchall())
mmdRates = pd.read_sql(qryMmd, cnxn)

colNew = [w.replace('YR_', '') for w in mmdRates.columns[1:]]
maturities = np.asarray([float(w) for w in colNew])

ratesDf = pd.DataFrame(np.column_stack((maturities, mmdRates.values[0,1:])), columns=['Maturities','Rates'])

pd.date_range(mmdRates.EFFDATE[0], periods=4, freq='6M')

# Assume semi-annual compounding
# 

class Parameter:
    'Class for IFRs and other similar parameters'
    def __init__(self, ntimes=0):
        self.paramVec = np.zeros(ntimes)
        self.timeVec = np.zeros(ntimes)
        self.assignedVec = np.zeros(ntimes, dtype=bool)
    
    def integrateParam(self, t1, t2):
        sp.integrate.quad(lambda x: self.getParamValue(x), t1, t2)
        
    def getParamValue(self, tt):
        if(self.assignedVec.sum() == 0):
            return 0
        else:
            indTmp = np.where(self.assignedVec == True)
            paramTmp = self.paramVec[indTmp]
            timeTmp = self.timeVec[indTmp]
            return np.interp(tt, timeTmp, paramTmp) # May need to use scipy interp for extrapolation



class YieldCurve:
    
    def __init__(self, ratesDf_):
        self.ratesDf = ratesDf_
        self.ratesDf['IFR'] = 0
        
        self.discountFactors = pd.DataFrame(np.arange(0, max(self.ratesDf.Maturities), 0.5)+0.5, columns=['Yearfrac'])
        self.discountFactors['DF'] = 0
        
        self.Bootstrap()
#    def BondPrice(self):
        
        
    def Bootstrap(self):
        # Assumes equally spaced years, semi-annual bonds, piecewise linear IFR
        
        for ii in range(len(ratesDf)):
            dfInd = np.where(self.discountFactors.Yearfrac <= (self.ratesDf.Maturities[ii] - 1))
            dfLoop = self.discountFactors.iloc[dfInd]
            cLoop = self.ratesDf.Rates[ii]/100/2    # Semi-annual
            sumDfLoop = dfLoop.DF.sum()
            if(dfLoop.empty):
                lastDf = 1
            else:
                lastDf = float(dfLoop.DF.tail(1))
                
            def loopFun(r_):
                return cLoop*sumDfLoop - 1 + cLoop*lastDf*(np.exp(-r_*0.5)) + (1+cLoop)*lastDf*(np.exp(-r_*1))
            
            self.ratesDf.loc[ii,'IFR'] = sp.optimize.newton(loopFun, cLoop)
            self.discountFactors.loc[2*ii, 'DF'] = lastDf*np.exp(-self.ratesDf.IFR[ii]*0.5)
            self.discountFactors.loc[2*ii+1, 'DF'] = lastDf*np.exp(-self.ratesDf.IFR[ii]*1)
    
    def GetDfSingle(self, t1):
        # First get precalculated DF
        indIfr = np.where(self.ratesDf.Maturities <= t1)
        if(len(indIfr[0]) == 0):
            return sp.exp(-self.ratesDf.IFR.iloc[0]*t1)
#            indIfr = len(self.ratesDf) - 1  # Since index begins at 0
#            ifrLoop = self.ratesDf.IFR.iloc[-1] # Last element: -1
        elif(max(indIfr[0]) == len(self.ratesDf)):
            ifrLoop = self.ratesDf.IFR.iloc[-1] # Last element: -1
        else:
            ifrLoop = self.ratesDf.IFR.iloc[max(indIfr[0])+1]
        
        indDf = np.where(self.discountFactors.Yearfrac <= t1)[0]
        if(len(indDf) == 0):
            dfLoop = 1
            tLoop = 0
        else:
            dfTmp = self.discountFactors.loc[max(indDf), ['Yearfrac','DF']]
#            dfLoop = self.discountFactors.DF.iloc[max(indDf)]
#            tLoop = self.discountFactors.Yearfrac.iloc[max(indDf)]
            dfLoop = dfTmp.DF
            tLoop = dfTmp.Yearfrac
        
        return(dfLoop*sp.exp(-ifrLoop*(t1 - tLoop)))
    
    def GetDf(self, t1, t2):
        return (self.GetDfSingle(t2)/self.GetDfSingle(t1))
    
mmdCurve = YieldCurve(ratesDf)

print(mmdCurve.GetDf(0, 1.8))








    