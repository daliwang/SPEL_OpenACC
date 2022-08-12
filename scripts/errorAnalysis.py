from numpy import require


def loadData(file):
   """
   This function loads the data from a verfication 
   module generated file
   """
   import re 
   var_name_regex = re.compile('\w+(%)\w+')
   varnames = []
   vardict = {}
   for line in file:
      match_varname = var_name_regex.search(line)
      if(match_varname):
         var = match_varname.group()
         varnames.append(var) 
         continue 
      data = line.strip().split()
      ##########################
      for x in data:
         vardict.setdefault(var,[]).append(float(x))

   return vardict 
   

def errorVerification(cpufn,gpufn):
   """
   This function performs BFB statistical analysis on 
   a cpu and gpu run.  
   """
   import numpy as np 
   import sys
   # Read files to compare 
   print(f"CPU file {cpufn} ::: : ::: GPU file {gpufn}")
   file = open(cpufn, 'r')
   cpufile = file.readlines()
   file.close() 
   #
   file = open(gpufn,'r') 
   gpufile = file.readlines() 
   file.close() 

   cpudata = loadData(cpufile)
   gpudata = loadData(gpufile)
   for var in cpudata.keys():
      info = [] 
      # print(f"Analyzing {var}")
      cpu_arr = np.array(cpudata[var])
      gpu_arr = np.array(gpudata[var]) 
      if(len(cpu_arr) != len(gpu_arr)): 
         print("Error: gpu and cpu arrays aren't same length ")
         sys.exit()
      compare =  cpu_arr - gpu_arr
      compare = np.abs(compare)
      for i in range(0,len(cpu_arr)):
         if(compare[i] > 0.0):
            info.append([cpu_arr[i],gpu_arr[i],compare[i]])
      if(info):
         print(var)
         for el in info:
            print(el)
      
def findDataFiles(unittest): 
   import subprocess as sp

   #get the cpu files:
   output = sp.getoutput(f"ls -t cpu_{unittest}*.txt")
   temp = output.split()
   temp.reverse() 
   cpu_files = temp.copy()

   #get the gpu files :
   output = sp.getoutput(f'ls -t gpu_{unittest}*.txt')
   temp = output.split()
   temp.reverse() 
   gpu_files = temp.copy()
   return cpu_files, gpu_files

if (__name__ == '__main__'):
   import argparse
   import sys 
   parser = argparse.ArgumentParser(prog='errorAnalysis',description="BFB analysis for cpu vs gpu")
   parser.add_argument('-c',action='store',required=False,dest='cpufn',help='cpu data filename',default='')
   parser.add_argument('-g',action='store',required=False,dest='gpufn',help='gpu data filename',default='')
   parser.add_argument('-b',action='store',required=False,dest='unittest',help="Analyze all files found for this unit-test",default='')
   args = parser.parse_args()
   if(args.cpufn and args.gpufn):
      errorVerification(cpufn=args.cpufn, gpufn=args.gpufn)
   elif(args.unittest):
      print(f"performing batch for UnitTest {args.unittest}")
      cpu_files, gpu_files = findDataFiles(args.unittest)
      if(len(cpu_files) != len(gpu_files)):
         sys.exit("CPU and GPU data files do not match!")
      for n in range(0,len(cpu_files)):
         errorVerification(cpufn=cpu_files[n],gpufn=gpu_files[n])



