data1 = read.csv("Dataset/Dengue.csv")
data2 = read.csv("Dataset/Singapore_weather.csv")
data3 = read.csv("Dataset/Singapore_gai.csv")
data4 = read.csv("Dataset/SouthAmerica_case.csv")
data5 = read.csv("Dataset/Thai_case.csv")
data6 = read.csv("Dataset/hongkong_resp.csv")

den = data1$X17[731:868]
gai = data3$number[1:138]
FOD_kendall(gai,den,23,6,3,500) # FOD, FOD_dcov

deng_sg = data1$X17[734:1045]
preccum = cumsum(data2$rain)
prec21 = preccum[27:2554] - preccum[6:2533]
prec_sg = prec21[(0:311)*7+1]
tempcum = cumsum(data2$temp)
temp21 = tempcum[27:2554] - tempcum[6:2533]
temp_sg = temp21[(0:311)*7+1]
FOD_kendall(prec_sg,deng_sg,52,6,4,500) # FOD, FOD_docv
FOD_kendall(temp_sg,deng_sg,52,6,4,500)

deng_tai = data5$bk[4:315]
deng_mexico = data4$Mexico[4:315]
FOD(deng_sg,deng_tai,52,6,5,500) # FOD_kendall/FOD_dcov
FOD(deng_sg,deng_mexico,52,6,5,500)
FOD(deng_mexico,deng_tai,52,6,5,500)

resp = data6$resp[1:1460]
temp = data6$temp[1:1460]
pm10 = data6$rsp[1:1460]
no2 = data6$no2[1:1460]
so2 = data6$so2[1:1460]
o3 = data6$o3[1:1460]
hum = data6$hum[1:1460]
FOD(resp,temp,365,4,10,500) # FOD_kendall/FOD_dcov
FOD(resp,pm10,365,4,10,500)
FOD(resp,no2,365,4,10,500)
FOD(resp,so2,365,4,10,500)
FOD(resp,o3,365,4,10,500)
FOD(resp,hum,365,4,10,500)