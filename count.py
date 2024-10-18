def countsocial(filename,skip=0,verbose=1):
    import string
    largeBoth = 0
    largeY = 0
    largeN = 0
    nonworkBoth = 0
    nonworkY = 0
    nonworkN = 0
    smallBoth = 0
    smallY = 0
    smallN = 0
    nonworksmallBoth = 0
    nonworksmallY = 0
    nonworksmallN = 0
    
    # read first file
    f1=open(filename,'r')
    for i in range(skip+1):
        inp=f1.readline() # skip lines before reading first line 

    print inp
    while not(inp==""): # repeat until end of file has been reached
##        outp = map(string.strip,string.split(inp)) # parse each line by using space separators
        largeBoth=largeBoth + inp.count("LGW1")
        largeY=largeY + inp.count("LGW2")
        largeN=largeN + inp.count("LGW3")

        nonworkBoth=nonworkBoth + inp.count("LG1")
        nonworkY=nonworkY + inp.count("LG2")
        nonworkN=nonworkN + inp.count("LG3")

        smallBoth=smallBoth + inp.count("SGW1")
        smallY=smallY + inp.count("SGW2")
        smallN=smallN + inp.count("SGW3")

        nonworksmallBoth=nonworksmallBoth + inp.count("SG1")
        nonworksmallY=nonworksmallY + inp.count("SG2")
        nonworksmallN=nonworksmallN + inp.count("SG3")

        inp=f1.readline() # read next line
    f1.close()

    totalBoth = largeBoth+nonworkBoth+smallBoth+nonworksmallBoth
    totalY = largeY+nonworkY+smallY+nonworksmallY
    
    if (verbose==1):
        print "large gathering(work), Both:"+str(largeBoth)
        print "large gathering(work), Murali only:"+str(largeY)
        print "large gathering(work), neither:"+str(largeN)
        
        print "large gathering, Both:"+str(nonworkBoth)
        print "large gathering, Murali only:"+str(nonworkY)
        print "large gathering, neither:"+str(nonworkN)
        
        print "small gathering(work), Both:"+str(smallBoth)
        print "small gathering(work), Murali only:"+str(smallY)
        print "small gathering(work), neither:"+str(smallN)
        
        print "small gathering, Both:"+str(nonworksmallBoth)
        print "small gathering, Murali only:"+str(nonworksmallY)
        print "small gathering, neither:"+str(nonworksmallN)

    print "large gathering proportion attended:"+str(1.0*(largeBoth+nonworkBoth)/(largeBoth+largeY+nonworkBoth+nonworkY))+" (total="+str(largeBoth+largeY+nonworkBoth+nonworkY)+")"
    print "small gathering proportion attended:"+str(1.0*(smallBoth+nonworksmallBoth)/(smallBoth+smallY+nonworksmallBoth+nonworksmallY))+" (total="+str(smallBoth+smallY+nonworksmallBoth+nonworksmallY)+")"

    print "overall proportion attended:"+str(1.0*totalBoth/(totalBoth+totalY)),"(total=",totalBoth+totalY,")"

#countsocial("oldcalendar.2005",skip=283)
