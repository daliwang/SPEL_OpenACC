"""
Module that holds the Loop Class 
that is used to parse subroutines
"""
import re  
import sys 

def lineContinuationAdjustment(lines,ln,verbose=False): 
   """
   This function returns takes a string that
   accounts for line continuations 

   Could be simpler/easier to read without using enumerate 
   in calling loop. 
   """
   l = lines[ln];
   l = l.split('!')[0]
   l = l.strip().lower()
   l = l.rstrip('\n')

   lines_to_skip = 0
   while(l.endswith('&')): 
      lines_to_skip+=1
      ct = ln+lines_to_skip
      newline = lines[ct].split('!')[0]
      newline = newline.strip().lower() 
      l = l[:-1] + newline.strip().rstrip('\n')

   if(verbose and lines_to_skip > 0): 
      print("Started with line: ")
      print(lines[ln]) 
      print("Returning line:")
      print(l)
      print(f"Skipped {lines_to_skip} lines")


   return l, lines_to_skip;

def exportVariableDependency(subname, var_list, DoLoops):
    """
    This function takes list of all collected DoLoops of a 
    subroutine (parent) and creates a readable table with
    the variables.  
    """
    #Initialize dictionary for variables across all Loops 
    num_loops = len(DoLoops)
    varsForAllLoops = {v : [" - "]*num_loops for v in var_list }
    current_name = subname
    old_name = subname
    header = []
    count = 0 
    for n, loop in enumerate(DoLoops):
        current_name = loop.subcall
        if(current_name is old_name):
            count += 1
            for v in var_list:
                varsForAllLoops[v][n] = f"{'-':4}"
        else:
            for v in var_list:
                varsForAllLoops[v][n] = f"{'|-':4}"
            width = 4*count
            if(count == 0): continue 
            x = '|'+old_name[0:width-2]
            string  = f"{x:{width}}"
            header.append(string)
            old_name = loop.subcall
            count = 1
        #end of Loops
        if(n == len(DoLoops)-1):
            x = '|'+current_name[0:width-2]            
            string = f"{x:{width}}"
            header.append(string)

    
    header_string = "".join(header)
    print(f'{" ":<20} {header_string}')
    ofile = open(f"{subname}AllLoopVariables.dat",'w')
    ofile.write(f'{" ":<20} {header_string}'+'\n')
    #Update formatted status for each var
    current_name = subname
    old_name = subname
    for n, loop in enumerate(DoLoops):
        current_name = loop.subcall
        if(current_name is not old_name):
            #Loops are in a different subroutine 
            for v, status in loop.vars.items():
                if(v not in var_list): continue 
                status = "|"+status 
                varsForAllLoops[v][n] = f"{status:4}"
            old_name = loop.subcall
        else:
             for v, status in loop.vars.items():
                if(v not in var_list): continue 
                varsForAllLoops[v][n] = f"{status:4}"
    
    for key in varsForAllLoops.keys():
        string = ''.join(varsForAllLoops[key])
        print(f"{key: <20} {string}")
        ofile.write(f"{key: <20} {string}"+'\n')

    ofile.close()
    
    return 


class Loop(object):
    def __init__(self, start,index,ifile,subname):
        self.start = [start]
        self.end = [0]
        self.index = [index]
        self.nested = 0
        self.innerloops = [] 
        self.vars = {} 
        self.reduction = False
        self.reduce_vars = []
        self.subcall = subname
        self.scalar_vars = {}
        self.lines = []
        self.file = ifile 
        self.filter = []

    
    def printLoop(self,substartline=0,long=True):
        """
        Function to print a Loop instance
        """
        lstart = self.start[0]
        lend = self.end[0]

        print(f"Loop at {lstart}-{lend} of {self.file}::{self.subcall}")
        slice = self.lines[:]
        if(long):
            print(f"It is nested {self.nested} times with indices {self.index}")
            for ln,line in enumerate(slice):
                print(lstart+ln,line.strip('\n'))
    
    def removeArraysAsIndices(self,vdict,line,arrays,verbose): 
        """
        Removes arrays such as snl(c) used as indices and sets them as read-only 
        """
        regex_var = re.compile(r'\w+')
        temp = line 
        keep_removing = False 
        regex_indices = re.compile(r'(?<=\()(.+)(?=\))')

        for var in arrays:
            # only get var name 
            v = regex_var.search(var).group()
            regex_check_index = re.compile(f"\w+\([,a-z0-9*-]*(({v})\(.\))[,a-z+0-9-]*\)",re.IGNORECASE)
            match = regex_check_index.search(temp)
            if(match):
                if(verbose): print(f"found array {v} used as index")
                # Add to dict as read-only: 
                vdict.setdefault(v,[]).append('r')
                # substitute from the entire line 
                i = regex_indices.search(var).group() 
                i = f"\({i}\)"
                temp = re.sub(f"{v}{i}",v,temp)
                if(verbose):
                    print(var,v) 
                    print("new line is: ",temp)
                keep_removing = True

        return temp, keep_removing

    def parseVariablesinLoop(self,verbose=False):
        """
        Goes through loop line by line and
        returns the self.vars dictionary that 
        holds the variables modified by this Loop
        """
        lstart = self.start[0]
        lend = self.end[0]
        cc = "[a-zA-Z\,0-9]"; 

        # non-greedy capture
        # ng_regex_array = re.compile(f'\w+?\({cc}+?\)')
        ng_regex_array = re.compile("\w+\([,\w+\*-]+\)",re.IGNORECASE)

        regex_if = re.compile(r'^(if|else if)')
        regex_cond = re.compile(r'\((.+)\)')
        regex_subcall = re.compile(r'^(call)')
        # 
        #regex to match code that should be ignored 
        regex_skip = re.compile(r'^(write)')
        regex_dowhile = re.compile(f'\s*(do while)',re.IGNORECASE)


        # Initialize dictionary that will 
        # hold array variables used in the loop
        variable_dict = {}

        # slice = lines[lstart:lend+1]
        slice = self.lines[:]
        lines_to_skip = 0
        reprint=True
        for ln,line in enumerate(slice):
            if(lines_to_skip > 0):
               lines_to_skip -= 1 
               continue 
            l, lines_to_skip = lineContinuationAdjustment(slice,ln,verbose)

            #match any functions or statements to be ignored
            match_skip = regex_skip.search(l)
            if(match_skip): continue 
            # match if statements 
            match_if = regex_if.search(l);
            match_dowhile = regex_dowhile.search(l)
            if(match_if):
                m = regex_cond.findall(l);
                if("then" not in l):
                    if(verbose): print("single line if statement")
                else:
                    if(verbose): print(ln,m)
            elif(not match_dowhile):
                # Currently ignore subroutines called inside loops 
                match_subcall = regex_subcall.search(l)
                if(match_subcall): continue

                #Find all array variables in the line
                m_arr = ng_regex_array.findall(l);
                if(m_arr): 
                    lnew = l
                    # assume that there are some arrays used as indices
                    removing = True  
                    while(removing):
                        temp, removing = self.removeArraysAsIndices(vdict=variable_dict, line=lnew,arrays=m_arr,verbose=verbose)
                        lnew = temp 
                        m_arr = ng_regex_array.findall(lnew)
                        if(verbose): print("New findall is: ",m_arr)
                    
                    if(m_arr):
                        print(ln,l)
                        variable_dict, reprint = self._getArrayVariables(ln,l,m_arr,variable_dict,reprint=reprint,verbose=verbose)
        
        if(self.reduction): 
            print("This Loop contains a reduction for the variables \n",self.reduce_vars)

        #clarify read/write status of each variable in the loop
        for var, status in variable_dict.items():
            #remove duplicates
            status = list(set(status))
            #set read-only, write-only, rw status:
            if('r' in status and 'w' not in status):
                variable_dict[var] = 'ro' #read only 
            elif('w' in status and 'r' not in status):
                variable_dict[var] = 'wo' #write only
            elif('w' in status and 'r' in status):
                variable_dict[var] = 'rw' #read-write

        self.vars = variable_dict.copy()
        return 

    def addOpenACCFlags(self, lines_adjusted, subline_adjust,verbose= False):
        """
        Function that will add openACC directives to loop
        
        lines_adjusted is a dictionary to keep track of line insertions into a subroutine
        to appropriately adjust where the loops start.
        """
        from mod_config import elm_files

        
        # tightly_nested = [ False for loop in range(0,self.nested+1) ]
        parallelism = ["gang", "worker", "vector", "seq"]
        total_loops = self.nested + 1 
        ifile = open(f"{elm_files}{self.file}",'r')
        mod_lines = ifile.readlines() 
        ifile.close() 
        
        outer_lstart = self.start[0] + subline_adjust + lines_adjusted[self.subcall]
        # First check if OpenACC flags have already been added.
        if("!$acc" in mod_lines[outer_lstart-2]): 
            print("Loop already has OpenACC flags, Skipping")
            return

        tightly_nested = 1
        for loop in range(0,total_loops):
            # Loop through every loop that isn't this one and test to
            # see if they are nested. 
            # Increment counter for the collapse clause
            if(tightly_nested >1): 
                tightly_nested -= 1 
                continue  
            acc_parallel_directive = f" "
            for innerloops in range(loop,total_loops):
                diff = self.start[innerloops] - self.start[loop] 
                if(diff==tightly_nested): 
                    tightly_nested += 1
            
            if(loop == 0):
                # only put data clause on outermost loop 
                acc_parallel_directive = "!$acc parallel loop independent gang vector default(present)"
            else : 
                # for inner loops just default to sequential for now
                # and allow the developer to increase parallelism based on profiling results 
                acc_parallel_directive = "!$acc loop seq"
            if(tightly_nested> 1):
                acc_parallel_directive = acc_parallel_directive + f" collapse({tightly_nested}) "
            acc_parallel_directive = acc_parallel_directive + '\n'
            lstart = self.start[loop]-1+lines_adjusted[self.subcall]+subline_adjust
            line = mod_lines[lstart]
            padding = " "*(len(line) - len(line.lstrip()))
            acc_parallel_directive = padding+acc_parallel_directive
            print(f"Inserting :\n {acc_parallel_directive}")
            mod_lines.insert(lstart,acc_parallel_directive)
            lines_adjusted[self.subcall] += 1 
        
        # lstart = self.start[0]-1+lines_adjusted[self.subcall]
        lend = self.end[0] + lines_adjusted[self.subcall] + subline_adjust
        for ln in range(outer_lstart-1,lend): 
            print(ln,mod_lines[ln].strip('\n'))
        overwrite = input(f"Overwrite {self.file}?")
        if(overwrite == 'y' ): 
            with open(f"{elm_files}{self.file}",'w') as ofile:
                ofile.writelines(mod_lines)
        else:
            sys.exit()



    def _getArrayVariables(self,ln,l,m_arr,variable_dict,reprint=True,verbose=False,interact=False):
        """
        This function takes a given line in of a Loop 
        and identifies the read/write status of each array 
        variable present
        """
        
        # split the about the assignment
        assignment = l.split("=")
        if(len(assignment)>2):
            print(l) 
            sys.exit("getArrayVariables::Too many equals in this case!")
        lhs = assignment[0] 
        rhs = assignment[1]
        #
        regex_indices = re.compile(r'(?<=\()(.+)(?=\))')
        regex_var = re.compile(r'\w+')

        vars_already_examined = variable_dict.keys() 
        for var in m_arr:
            
            # This means only the first instance of the variable is catalogued as 
            # write, read or reduction. 
            #get subgrid index from filter 
            if("filter" in var): 
                findx = lhs.strip()
                fvar = regex_var.search(rhs).group() 
                self.filter = [fvar,findx]
                #print(f"Filter index is {findx}")

            if(var in vars_already_examined): continue 
            #check if var is only on assigned
            if(var in lhs and var not in rhs): 
                #print(ln,var,"is output")
                varname = regex_var.search(var).group() 
                variable_dict.setdefault(varname,[]).append('w')
                            
            elif(var in rhs and var not in lhs): 
                #print(ln,var,"is input")
                varname = regex_var.search(var).group() 
                variable_dict.setdefault(varname,[]).append('r')

            # Now check if variable appears on both sides
            # Requires additional checking for reduction operation
            elif(var in lhs and var in rhs):
                m_indices = regex_indices.search(var).group()
                indices = m_indices.split(',')
                # variable appears on both sides and is missing at least one loop index 
                # Need to ask if the variable is being reduced
                if(verbose): print(indices,self.index)
                # Since inner loops may not be tightly nested, we need to 
                # find out how many loops this line of code is inside. 
                loopcount = 0 
                for n, loopstart in enumerate(self.start):
                    loopend = self.end[n]
                    linecount = ln + self.start[0]
                    print(linecount, loopstart, loopend)
                    if(linecount > loopstart and linecount < loopend):
                        loopcount += 1 
                if(verbose) : print(f"This line {ln} is inside {loopcount} Loops!")
                if(loopcount < 1): 
                    print(l)
                    for n, loopstart in enumerate(self.start):
                        print(loopstart, self.end[n])
                    sys.exit("Error: Not in a Loop!?")


                if(len(indices) < loopcount):
                    if(reprint and interact):
                        self.printLoop()
                        reprint = False
                    if(var in self.reduce_vars): continue 
                    if(interact):
                        reduction = input(f"is {var} at line {ln} being reduced in this loop?")
                    else:
                        reduction='y'
                    if(reduction=='y'):
                        self.reduction = True
                        self.reduce_vars.append(var)
                        print(f"Setting {var} as being reduced in this Loop")
                
                varname = regex_var.search(var).group()
                # variable_dict.setdefault(varname,[]).append('rw')
                variable_dict.setdefault(varname,[]).extend(['r','w'])

        
        return variable_dict, reprint
