# script to rewrite binary data (Presence-Absence data) to locus organized data
# Written by M.Thorstensen
import csv
from random import randint

reader = csv.reader(open('66Genotypes_NoLiz.csv'))

data = []

# When using a different input, you'll need to change which columns the different loci are in. Also, this is for a 13 locus data set- with more or fewer loci,
# you can add or delete them as needed.
def listmaker(file):
	for row in file:
		individuals = []
		everyallele = list(row[0:])
		#Actm110
		locus1 = everyallele[0:9]
		locus1.sort(reverse=True)
		locus1 = locus1[0:8]
		#Actm177
		locus2 = everyallele[9:13]
		locus2.sort(reverse=True)	
		locus2 = locus2[0:4]		
		#As015
		locus3 = everyallele[13:19]
		locus3.sort(reverse=True)
		locus3 = locus3[0:8]
		#Actm35
		locus4 = everyallele[19:30]
		locus4.sort(reverse=True)
		locus4 = locus4[0:8]
		#Atr1101
		locus5 = everyallele[30:33]
		locus5.sort(reverse=True)
		locus5 = locus5 [0:4]
		#Atr109
		locus6 = everyallele[33:51]
		locus6.sort(reverse=True)
		locus6 = locus6[0:8]
		#Atr117
		locus7 = everyallele[51:61]
		locus7.sort(reverse=True)
		locus7 = locus7[0:8]
		#Actm2
		locus8 = everyallele[61:66]
		locus8.sort(reverse=True)
		locus8 = locus8[0:4]
		#Actm52
		locus9 = everyallele[66:81]
		locus9.sort(reverse=True)
		locus9 = locus9[0:8]	
		#Actm53
		locus10 = everyallele[81:86]
		locus10.sort(reverse=True)
		locus10 = locus10[0:4]
		#Atr105
		locus11 = everyallele[86:92]
		locus11.sort(reverse=True)
		locus11 = locus11[0:4]
		#Atr107
		locus12 = everyallele[92:103]
		locus12.sort(reverse=True)
		locus12 = locus12[0:8]
		#Atr1173
		locus13 = everyallele[103:112]
		locus13.sort(reverse=True)
		locus13 = locus13[0:8]		
		individuals.append(locus2)	
		individuals.append(locus5)	
		individuals.append(locus8)	
		individuals.append(locus10)
		individuals.append(locus11)		
		individuals.append(locus1)
		individuals.append(locus3)
		individuals.append(locus4)
		individuals.append(locus6)
		individuals.append(locus7)
		individuals.append(locus9)
		individuals.append(locus12)
		individuals.append(locus13)		
		data.append(individuals)

	
	
listmaker(reader)	

#print(data)


# A little script for randomly sampling form the whole data set. 

def random_indivs(list, number):
	while len(randomsample) < number:
		randomsample.append(list.pop(randint(0, len(list) - 1)))  
		
#random_indivs(data, 100)


# writing a csv file with output data
# still need to remove brackets and quotes in excel with find+replace	
def csvwriter(datalist):	
	with open("output.csv", 'w', newline = '') as output:
		writer = csv.writer(output, lineterminator = '\n', delimiter = ' ', 
		quotechar = ']', escapechar = ' ', quoting = csv.QUOTE_ALL, dialect = 'excel')
		for val in datalist:
			writer.writerow([val])

csvwriter(data)	