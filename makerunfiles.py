# input: file1, file2, colnames, colindex1, colindex2, mergefile,
# colindex1, colindex2 are the indices corresponding to the common colnames (in the same order)
# inorder=TRUE implies that the columns of file1 are already in the right order
# output (write to files): mergedfile, mergedrownames, mergedcolnames
def makerunfiles(seedlist,namepref,seqlist):
    if (len(seedlist)!=len(seqlist)):
        print "ERROR: seedlist not compatible with seqlist"
        return 1
    
    j=0
    for i in seqlist:
        filename =  namepref + str(i) + '.R'
##        filename = 'mcseconsis.runJ' + str(i) + '.R'
        print filename
        f=open(filename,'w')
        ## write out files
        f.write('source("mcseconsis.final.R")')
        f.write('\n')
        f.write('mcseconslips('+str(seedlist[j])+',"'+str(i)+'")')
        f.write('\n')
        f.close()
        j=j+1
## makerunfiles([533395,656423,990791,764340,792000,438363,151348,916842,59584,117408,820413,107631,713205,127011, 672233,  728510,  95703,  905362,  330499,  968134],'mcseconsis.final',range(20))
## makerunfiles([825184,  180195,  292595,  928746,  404547,  957562,318528,138337,965587,662871,326158,124106,242755,651219,35912,359317,105832,5985,96006,543458],'mcseconsis.final',range(20,40))
        
##nameeg= ['lB1','lB2','lB4','lB5','lB6','lB7','lB10','lB11','lB12','lB13','lB15','lB18','lB19']
##nameeg= ['lC1','lC2','lC4','lC5','lC6','lC7','lC10','lC11','lC12','lC13','lC15','lC18','lC19']
##nameeg= ['lD1','lD2','lD4','lD5','lD6','lD7','lD10','lD11','lD12','lD13','lD15','lD18','lD19']
##> library(rlecuyer)
##>>> execfile('makerunfiles.py')
execfile('findreplace.py') ## load in appropriate functions

## input: fromFile, toFile (filename strings) and findreplace, a vector of pairs of strings
## to find and replace
## output: toFile, a file with all find-replaces done
## eg.: replaceStringNewfile("runmcse.script0","runmcse.scriptBOO",[('final0.R','finalBOO.R')])
def replaceStringNewfile(fromFile,toFile,findreplace):
    "find-replace in fromFile to create new file toFile"
    input = open(fromFile)
    output = open(toFile,'w')
    s=input.read()
    for couple in findreplace:
        outtext=s.replace(couple[0],couple[1])
        s=outtext
    output.write(outtext)
    output.close()
    input.close()

def replaceStringInFile(filePath,findreplace):
   "replaces all findStr by repStr in file filePath"
   print filePath
   tempName=filePath+'~~~'
   input = open(filePath)
   output = open(tempName,'w')
   s=input.read()
   for couple in findreplace:
       outtext=s.replace(couple[0],couple[1])
       s=outtext
   output.write(outtext)
   output.close()
   input.close()
   os.rename(tempName,filePath)

## function to edit lionxm script runmcse.script0 to run mcseconsis.final0.R by creating 20 new scripts
## to now run mcseconsis.final20.R through mcseconsis.final39.R

##     "replaces all mcseconsis.final0.R by mcseconsis.final??.R, creating new lionxm scripts for each"

def lionxmedit():
    for i in range(20,40):
        replaceStringNewfile("runmcse.script0","runmcse.script"+str(i),[('final0.R','final'+str(i)+'.R')])
        print 'created runmcse.script'+str(i)
        

