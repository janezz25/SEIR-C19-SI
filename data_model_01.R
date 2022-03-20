
###################################################################
### Age groups ####
###################################################################

sdata = read.csv("data/demographics_si.csv", sep = ";")



N1 =sum( sdata[1:25])
N2 =sum( sdata[26:45])
N3 =sum( sdata[46:65])
N4 =sum( sdata[66:75])
N5 =sum( sdata[76:86])

sN = N1+N2+N3+N4+N5

w1 =N1 / sN
w2 =N2 / sN
w3 =N3 / sN
w4 =N4 / sN
w5 =N5 / sN



w = c(w1, w2, w3, w4, w5)


# n population
Npop = N1 + N2 + N3 + N4 + N5




###################################################################
### Vaccination ####
###################################################################

vaccdat = read.csv("data/vacccination_data_age-grps_si.csv")

