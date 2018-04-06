# A script to change alleles per locus data to allele presence-absence data
# Written by M.Thorstensen


import csv
from random import randint

#headerreader = csv.reader(open('AlleleHeader.csv'))

header = []

# making a single list of all alleles possible in population, from a separate file of the different allele possibilities.
def headermaker(file):
	global header
	for row in file:
		individuals = []
		everyallele = list(row[0:])
		locus1 = everyallele[0:22]
		locus2 = everyallele[22:44]
		locus3 = everyallele[44:63]
		locus4 = everyallele[63:92]
		locus5 = everyallele[92:122]
		locus6 = everyallele[122:147]
		locus7 = everyallele[147:178]
		locus8 = everyallele[178:199]
		individuals.append(locus1)
		individuals.append(locus2)
		individuals.append(locus3)
		individuals.append(locus4)
		individuals.append(locus5)
		individuals.append(locus6)
		individuals.append(locus7)
		individuals.append(locus8)
	header = list(individuals)
	return header
	
#headermaker(headerreader)


reader = csv.reader(open('try2.csv'))
reader2 = csv.reader(open('try2.csv'))

# This function is similar in nature to the header reader function in that it creates an index of possible alleles at a locus. 
# However, it instead takes the data set and looks at the alleles present, as opposed to needing a list of possible alleles.
def headerset(file):
	global header
	actm177 = []
	atr1101 = []
	actm2 = []
	actm53 = []
	atr105 = []
	actm110 = []
	as015 = []
	actm35 = []
	atr109 = []
	atr117 = []
	actm52 = []
	atr107 = []
	atr1173 = []
	individuals = []	
	for row in file:	
		everyallele = list(row[0:])
		for i in everyallele[0:4]:
			actm177.append(i)
		for i in everyallele[4:8]:
			atr1101.append(i)
		for i in everyallele[8:12]:
			actm2.append(i)
		for i in everyallele[12:16]:
			actm53.append(i)
		for i in everyallele[16:20]:
			atr105.append(i)
		for i in everyallele[20:28]:
			actm110.append(i)
		for i in everyallele[28:36]:
			as015.append(i)
		for i in everyallele[36:44]:
			actm35.append(i)
		for i in everyallele[44:52]:
			atr109.append(i)
		for i in everyallele[52:60]:
			atr117.append(i)
		for i in everyallele[60:68]:
			actm52.append(i)
		for i in everyallele[68:76]:
			atr107.append(i)
		for i in everyallele[76:84]:
			atr1173.append(i)
	individuals.append(list(set(actm177)))
	individuals.append(list(set(atr1101)))
	individuals.append(list(set(actm2)))
	individuals.append(list(set(actm53)))
	individuals.append(list(set(atr105)))
	individuals.append(list(set(actm110)))
	individuals.append(list(set(as015)))
	individuals.append(list(set(actm35)))	
	individuals.append(list(set(atr109)))			
	individuals.append(list(set(atr117)))	
	individuals.append(list(set(actm52)))	
	individuals.append(list(set(atr107)))	
	individuals.append(list(set(atr1173)))	
	header =  list(individuals)	
	return header
	
		
data = []  # empty list for individuals pre bottleneck

# create a list called data, of lists of alleles called individuals. 

def listdata(file):
	global data
	for row in file:
		individuals = []			# individuals empties every iteration
		everyallele = list(row[0:]) # reading all alleles into a single list		
		locus1 = everyallele[0:4]
		locus2 = everyallele[4:8]
		locus3 = everyallele[8:12]
		locus4 = everyallele[12:16]
		locus5 = everyallele[16:20]	
		locus6 = everyallele[20:28]		
		locus7 = everyallele[28:36]
		locus8 = everyallele[36:44]
		locus9 = everyallele[44:52]
		locus10 = everyallele[52:60]
		locus11 = everyallele[60:68]
		locus12 = everyallele[68:76]
		locus13 = everyallele[76:84]
		individuals.append(locus1)   # appending individuals with each locus
		individuals.append(locus2)
		individuals.append(locus3)
		individuals.append(locus4)
		individuals.append(locus5)
		individuals.append(locus6)
		individuals.append(locus7)
		individuals.append(locus8)
		individuals.append(locus9)
		individuals.append(locus10)
		individuals.append(locus11)
		individuals.append(locus12)
		individuals.append(locus13)
		data.append(individuals)
	return data	


reformatlist = []

# reformatting all data from locus organized to Presence-Absence formatted, using the header reference and the locus data.
def datareformat(heading, alleles):
	global reformatlist
	reformatlist.append(heading)    # adding the header to track alleles
	for individual in alleles:        
		tempindividual = []		# temp individual reset each iteration
		for index, locus in enumerate(individual):
			for place, value in enumerate(heading):
				# matching the loci in both the data set and header data
				# if the loci match, then add information
				if place == index:
					# a temporary locus, so that duplicates may be added
					# across the whole data set
					templocus = []
					for allele in value:
						if allele in locus:
							# if/else to avoid duplicate alleles in 1 locus
							if allele not in templocus:
							# type allele if non-binary, '1' if binary
								templocus.append(allele)  
							else:
								pass
						# adding the '-' if the allele is not present		
						if allele not in locus:
							templocus.append('-')
					tempindividual.append(templocus)		
				else:
					pass
		reformatlist.append(tempindividual)				
	return reformatlist


	
# writing a csv file with output data
# still need to remove brackets and quotes in excel with find+replace	
def csvwriter(datalist, name):	
	with open(name, 'w', newline = '') as output:
		writer = csv.writer(output, lineterminator = '\n', delimiter = ' ', 
		quotechar = ']', escapechar = ' ', quoting = csv.QUOTE_ALL, dialect = 'excel')
		for val in datalist:
			writer.writerow([val])

			
headerset(reader)
listdata(reader2)
datareformat(header, data)	

csvwriter(reformatlist, "crosses_reformat1.csv")	