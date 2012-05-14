import sys,random

fd = open(sys.argv[1],'r')
urllist = fd.readlines()
fd1 = open(sys.argv[2],'w')

random.seed()

for i in range(0,int(sys.argv[3])):
	url = urllist[i]
	if url != '\n' and url != '0':
		fd1.write(url[0:-1]+str(random.randint(0,1234567))+'\n')
	
fd.close()
fd1.close()	
