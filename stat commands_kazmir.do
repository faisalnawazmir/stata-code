use "C:\Users\AZAN Laptop Store\Desktop\thesis data.dta" 
ren data1 ASK PRICE
ren data1 ASK_PRICE
ren data2 BID_PRICE
ren data3 CASH
ren data4 CASH_GENERIC
ren data5 Environment_Pillar_Score
ren data6 Governce_Pillar_Score
ren data7 LONG_TERM_DEBT
ren data8 MARKET_VALUE
ren data9 NET_INCOME 
ren data9 NET_INCOME
ren data10 NET_SALES_OR_REVENUES
ren data11 NET_TANGIBLE_ASSET
ren data12 RECEIVABLES(NET)
ren data12 RECEIVABLES_NET
ren data13 RETURN_ON_ASSETS
ren data14 Social_Pillar_Score
ren data15 TOTAL_ASSETS
ren data16 TOTAL_DEBT
ren data17 TOTAL_INTANGIBLE_ASSETS
ren data18 TOTAL_INVESTMENTS
ren var Age
ren var EPU
gen TotalInv=(NET_TANGIBLE_ASSET+TOTAL_INTANGIBLE_ASSETS)/l.TOTAL_ASSETS
gen SG=( NET_SALES_OR_REVENUES-l.NET_SALES_OR_REVENUES)/l.NET_SALES_OR_REVENUES
regress TotalInv SG
predict residual,residuals
gen Inveff_abs=abs(residual)
gen IE=(Inveff_abs)*-1
gen CSRP=(Environment_Pillar_Score+Governce_Pillar_Score+Social_Pillar_Score)/3
gen FC=(−0.737Size + 0.043Size2 − 0.040Age)
gen IAdiff=(ASK_PRICE-BID_PRICE)
gen IAavg=(ASK_PRICE+BID_PRICE)/2
gen Asym=IAdiff/IAavg
gen ROA= NET_INCOME/TOTAL_ASSETS
gen Size=ln(TOTAL_ASSETS)
gen Size2=(Size)^2
gen cash= CASH/l.TOTAL_ASSETS
gen TobinQ= MARKET_VALUE/ TOTAL_ASSETS
gen STD= TOTAL_DEBT- LONG_TERM_DEBT
gen Leverage= STD+LONG_TERM_DEBT/TOTAL_ASSETS
gen Leve= (TOTAL_DEBT-LONG_TERM_DEBT+ LONG_TERM_DEBT) /TOTAL_ASSETS
gen changeAR= RECEIVABLES_NET-l.RECEIVABLES_NET
gen changeReven= NET_SALES_OR_REVENUES-l.NET_SALES_OR_REVENUES
regress changeAR changeReven
predict Residual,residuals
gen frq_abs=abs(Residual)
gen FRQ=(frq_abs)*-1
gen changeaccrecv=ln(changeAR)
gen changeRevenue=ln(changeReven)
regress changeaccrecv changeRevenue
predict RESIDUAL, Residual
gen AbsFrq
gen Financial_RQ=(AbsFrq)*-1 
winsor2 EPU cuts(1 99) by(year)
gen Epu=ln(EPU)
winsor2 IE cuts(1 99) by(year)
gen Csrp=ln(CSRP)
gen Fc=ln(FC)
gen asym=ln(Asym)
gen SIZE=ln(Size)
summarize Epu IE_w Csrp Fc asym SIZE cash ROA TobinQ Leve Financial_RQ 
asdoc summarize Epu IE_w Csrp Fc asym SIZE cash ROA TobinQ Leve Financial_RQ
xtunitroot fisher Epu, dfuller lags(0)
xtunitroot fisher Epu, dfuller lags(1)
xtunitroot fisher IE_w, dfuller lags(0)
xtunitroot fisher Csrp, dfuller lags(0)
xtunitroot fisher Fc, dfuller lags(0)
xtunitroot fisher asym, dfuller lags(0)
xtunitroot fisher SIZE, dfuller lags(0)
xtunitroot fisher cash, dfuller lags(0)
xtunitroot fisher ROA, dfuller lags(0)
xtunitroot fisher TobinQ, dfuller lags(0)
xtunitroot fisher Leve, dfuller lags(0)
xtunitroot fisher FRQ, dfuller lags(0)
pwcorr Epu IE_w Csrp asym Fc SIZE cash ROA TobinQ Leve Financial_RQ, sig
asdoc pwcorr Epu IE_w Csrp asym Fc SIZE cash ROA TobinQ Leve Financial_RQ, sig
regress IE_w Epu Size cash ROA TobinQ Leve Financial_RQ
vif
regress IE_w Epu SIZE cash ROA TobinQ Leve Financial_RQ
hettest, rhs fstat
regress Csrp Epu SIZE cash ROA TobinQ Leve Financial_RQ
hettest, rhs fstat
regress IE_w Csrp Epu SIZE cash ROA TobinQ Leve Financial_RQ
hettest, rhs fstat
regress Fc Epu SIZE cash ROA TobinQ Leve Financial_RQ
hettest, rhs fstat
regress IE_w Fc Epu SIZE cash ROA TobinQ Leve Financial_RQ
hettest, rhs fstat
regress asym Epu SIZE cash ROA TobinQ Leve Financial_RQ
hettest, rhs fstat
regress  IE_w asym Epu SIZE cash ROA TobinQ Leve Financial_RQ
hettest, rhs fstat
xtserial IE_w Epu SIZE cash ROA TobinQ Leve Financial_RQ
xtserial Csrp Epu SIZE cash ROA TobinQ Leve Financial_RQ
xtserial IE_w Csrp Epu SIZE cash ROA TobinQ Leve Financial_RQ
xtserial Fc Epu SIZE cash ROA TobinQ Leve Financial_RQ
xtserial IE_w Fc Epu SIZE cash ROA TobinQ Leve Financial_RQ
xtserial asym Epu SIZE cash ROA TobinQ Leve Financial_RQ
xtserial IE_w asym Epu SIZE cash ROA TobinQ Leve Financial_RQ
regress IE_w Epu Size cash ROA TobinQ Leve Financial_RQ
sgmediation2 IE_w, iv(Epu) mv(Csrp) cv(SIZE cash ROA TobinQ Leve Financial_RQ)
sgmediation2 IE_w, iv(Epu) mv(Fc) cv(SIZE cash ROA TobinQ Leve Financial_RQ)
sgmediation2 IE_w, iv(Epu) mv(asym) cv(SIZE cash ROA TobinQ Leve Financial_RQ)
