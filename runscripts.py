# input: file1, file2, colnames, colindex1, colindex2, mergefile,
# colindex1, colindex2 are the indices corresponding to the common colnames (in the same order)
# inorder=TRUE implies that the columns of file1 are already in the right order
# output (write to files): mergedfile, mergedrownames, mergedcolnames
# ASSUMPTION: FOR EVERY FILE, FIRST COL IS LIST OF ROW NAMES
import os
def runscripts(prefname="runmcse.script",seqlist=range(20)):
    print 'hello'
    for i in seqlist:
        filename = prefname + str(i)
        print 'submit ' + filename
#        os.system('ls '+ filename)
        os.system('qsub '+ filename)

##seedeg= [127011, 672233,  728510,  95703,  905362,  330499,  968134,  825184,  180195,  292595,  928746,  404547,  957562]
##nameeg= ['lB1','lB2','lB4','lB5','lB6','lB7','lB10','lB11','lB12','lB13','lB15','lB18','lB19']
## seedeg=[127011,543458,728510,95703,905362,330499,968134,825184,180195,292595,928746,404547,957562]
##nameeg= ['lC1','lC2','lC4','lC5','lC6','lC7','lC10','lC11','lC12','lC13','lC15','lC18','lC19']
        
## seedeg=[318528,138337,965587,662871,326158,124106,242755,651219,35912,359317,105832,5985,96006]
##nameeg= ['lD1','lD2','lD4','lD5','lD6','lD7','lD10','lD11','lD12','lD13','lD15','lD18','lD19']
##> library(rlecuyer)
##>>> execfile('makerunfiles.py')

## for running on local linux machines
def runlocal(machname):
    os.system('ssh ' + machname)
    os.system('cd multreg')
    os.system('nohup nice R --no-save < mcseconsis.final20.R')
    os.system('exit')
    print 'done!'

## use 'Expect' from within python to log-on to several machines

