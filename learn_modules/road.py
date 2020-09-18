road = [50,10,30,5,90,20,40,2,25,10,8,0]

troad = list(zip(road[0::3],road[1::3],road[2::3]))
for i in troad: 
	print(i)