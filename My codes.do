use "D:\MS-3\Data file\Analysis\USA Data excl of 2022.dta", clear
log using "D:\MS-3\Data file\Analysis\FINAL.smcl"

gen CSR=ln(esg_combined_score)
gen FS=ln(Firmsize)
gen LEV=ln(leverage)
gen RoA=ln(ROA)

ssc install asdoc
asdoc sum CSR COVID19 Eco_Innovation_Score FS LEV RoA Sale_growth Tobin_Q Genderdiversity
asdoc pwcorr CSR COVID19 Eco_Innovation_Score FS LEV RoA Sale_growth Tobin_Q Genderdiversity , sig


reg CSR COVID19 FS LEV RoA Sale_growth Tobin_Q Genderdiversity d2008 d2009 d2010 d2011 d2012 d2013 d2014 d2015 d2016 d2017 d2018 d2019 d2020 d2021 doilgaspro dbasicmaterial dindustries dconsumer dhealthcare dconservice dtelecommunication dutilities dtechnology dinvestinsurance
gen inter1=COVID19*Eco_Innovation_Score
reg CSR COVID19 Eco_Innovation_Score inter1 FS LEV RoA Sale_growth Tobin_Q Genderdiversity d2008 d2009 d2010 d2011 d2012 d2013 d2014 d2015 d2016 d2017 d2018 d2019 d2020 d2021 doilgaspro dbasicmaterial dindustries dconsumer dhealthcare dconservice dtelecommunication dutilities dtechnology dinvestinsurance


ssc install outreg2
reg CSR COVID19 FS LEV RoA Sale_growth Tobin_Q Genderdiversity d2008 d2009 d2010 d2011 d2012 d2013 d2014 d2015 d2016 d2017 d2018 d2019 d2020 d2021 doilgaspro dbasicmaterial dindustries dconsumer dhealthcare dconservice dtelecommunication dutilities dtechnology dinvestinsurance
outreg2 using CEI,replace word dec(3)
shellout using `"CEI.rtf"'
seeout using "CEI.txt"
outreg2 using CEI,replace word dec(3)
seeout using "CEI.txt"
shellout using `"CEI.rtf"'
reg CSR COVID19 FS LEV RoA Sale_growth Tobin_Q Genderdiversity d2008 d2009 d2010 d2011 d2012 d2013 d2014 d2015 d2016 d2017 d2018 d2019 d2020 d2021 doilgaspro dbasicmaterial dindustries dconsumer dhealthcare dconservice dtelecommunication dutilities dtechnology dinvestinsurance
outreg2 using CEII,replace word dec(3)
shellout using `"CEII.rtf"'
reg CSR COVID19 Eco_Innovation_Score inter1 FS LEV RoA Sale_growth Tobin_Q Genderdiversity d2008 d2009 d2010 d2011 d2012 d2013 d2014 d2015 d2016 d2017 d2018 d2019 d2020 d2021 doilgaspro dbasicmaterial dindustries dconsumer dhealthcare dconservice dtelecommunication dutilities dtechnology dinvestinsurance
outreg2 using CEII,append word dec(3)
shellout using `"CEII.rtf"'


xtunitroot fisher CSR ,dfuller trend demean lag(0)
xtunitroot fisher Eco_Innovation_Score ,dfuller trend demean lag(0)
xtunitroot fisher FS ,dfuller trend demean lag(0)
xtunitroot fisher LEV ,dfuller trend demean lag(0)
xtunitroot fisher RoA ,dfuller trend demean lag(0)
xtunitroot fisher Sale_growth ,dfuller trend demean lag(0)
xtunitroot fisher Tobin_Q ,dfuller trend demean lag(0)
xtunitroot fisher Genderdiversity ,dfuller trend demean lag(0)


gen INTER= COVID19* dummyecoinnovation_score
reg CSR COVID19 dummyecoinnovation_score INTER FS LEV RoA Sale_growth Tobin_Q Genderdiversity d2008 d2009 d2010 d2011 d2012 d2013 d2014 d2015 d2016 d2017 d2018 d2019 d2020 d2021 doilgaspro dbasicmaterial dindustries dconsumer dhealthcare dconservice dtelecommunication dutilities dtechnology dinvestinsurance

reg CSR COVID19 Eco_Innovation_Score INTER FS LEV RoA Sale_growth Tobin_Q Genderdiversity d2008 d2009 d2010 d2011 d2012 d2013 d2014 d2015 d2016 d2017 d2018 d2019 d2020 d2021 doilgaspro dbasicmaterial dindustries dconsumer dhealthcare dconservice dtelecommunication dutilities dtechnology dinvestinsurance
reg CSR COVID19 Eco_Innovation_Score INTER FS LEV RoA Sale_growth Tobin_Q Genderdiversity d2008 d2009 d2010 d2011 d2012 d2013 d2014 d2015 d2016 d2017 d2018 d2019 d2020 d2021 doilgaspro dbasicmaterial dindustries dconsumer dhealthcare dconservice dtelecommunication dutilities dtechnology dinvestinsurance , cluster( c_id)
