"""
Python Module that collects functions that 
have broad utility for several modules in SPEL 
"""
import sys 
import re 
import subprocess as sp
from typing import Optional
from mod_config import elm_files, _bc


class Variable(object):
    """
    Class to hold information on the variable
    declarations in a subroutine 
        * self.type -> data type of variable 
        * self.name -> name of variable 
        * self.subgrid -> subgrid level used for allocation 
        * self.ln -> line number of declaration
    """
    def __init__(self, type, name,subgrid,ln,dim,optional=False):
        self.type = type
        self.name = name
        self.subgrid = subgrid
        self.ln = ln 
        self.dim = dim
        self.optional = optional
    
    def printVariable(self): 
        print(f"Variable:\n {self.type} {self.name} subgrid: {self.subgrid} {self.dim}-D")

def removeBounds(line,verbose=False):
    """
    This function matches (beg:end) style phrases in
    subroutine calls.  They must be removed to use !$acc routine

    """
    cc = "[ a-zA-Z0-9%\-\+]*?:[ a-zA-Z0-9%\-\+]*?"
    non_greedy1D = re.compile(f"\({cc}\)")
    non_greedy2D = re.compile(f"\({cc},{cc}\)")
    non_greedy3D = re.compile(f"\({cc},{cc},{cc}\)")
    non_greedy4D = re.compile(f"\({cc},{cc},{cc},{cc}\)")
    newline = line

    m1D = non_greedy1D.findall(newline)
    for bound in m1D: 
        newline = newline.replace(bound,'')

    m2D = non_greedy2D.findall(newline)
    for bound in m2D:
        newline = newline.replace(bound,'')

    m3D = non_greedy3D.findall(newline)
    for bound in m3D: 
        newline = newline.replace(bound,'')

    m4D = non_greedy4D.findall(newline)
    for bound in m4D: 
        newline = newline.replace(bound,'')
    
    return newline


def getArguments(l,verbose=False):
    """
    Function that takes a subroutine call 
    as an argument and returns the variables 
    passed as arguments. 

    Will be used to compare with the subroutines 
    argument list.
    
    This is neccessary to change variable allocations 
    and resolve ambiguities from interfaces 
    """
    if(verbose): print(_bc.WARNING+f"getArguments:: Processing {l}\n\n"+_bc.ENDC)
    par = re.compile("(?<=\().+(?=\))")
    m = par.search(l)
    if(not m): 
        print("Subroutine has no arguments",l)
        args = []
        return args;
    
    args = m.group()
    newargs = removeBounds(args,verbose)

    args = newargs.split(',')
    args = [x.strip() for x in args]
    
    return args


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

def find_file_for_subroutine(name,fn=''):
    """
    finds file, start and end line numbers for subroutines
    find file and start of interface for interfaces
    """
    interface_list = get_interface_list()
    
    if(not fn):
        search_file = f"{elm_files}*.F90"
    else:
        search_file = f"{elm_files}{fn}"
    
    if(name not in interface_list):
        cmd = f'grep -in -E "^[[:space:]]*(subroutine {name})\\b" {search_file} | head -1'
        cmd_end = f'grep -in -E "^[[:space:]]*(end subroutine {name})\\b" {search_file} | head -1'
    else:
        cmd = f'grep -in -E "^[[:space:]]+(interface {name})" {search_file} | head -1'
        cmd_end = ''

    output = sp.getoutput(cmd)
    if(not fn):
        file = output.split(':')[0]
        file = file.replace(elm_files,'')
        startline = int(output.split(':')[1])
        if(cmd_end!=''):
            output = sp.getoutput(cmd_end)
            endline = int(output.split(':')[1]) 
        else:
            endline = 0
    else:
        file = fn
        startline = int(output.split(':')[0])
        if(cmd_end!=''):
            output = sp.getoutput(cmd_end)
            endline = int(output.split(':')[0]) 
        else:
            endline = 0

    if(file == 'grep'):
        print(f"ERROR FILE FOR {name} NOT PRESENT")
        sys.exit(1)

    return file, startline, endline

def get_interface_list():
    """
    returns a list of all interfaces
    """
    cmd = f'grep -in -E "^[[:space:]]+(interface)" {elm_files}*.F90'
    output = sp.getoutput(cmd)
    output = output.split('\n')

    interface_list = []
    for el in output:
        el = el.split()
        interface = el[2]
        interface_list.append(interface.lower())

    return interface_list

def getLocalVariables(sub,verbose=False,class_var=False ):
    """
    this function will retrieve  the local variables from
    the subroutine at startline, endline in the given file

    Note: currently only looks at type declaration for class object
    """
    import subprocess as sp 
    filename = elm_files+sub.filepath
    subname = sub.name 
    file = open(filename,'r');
    lines = file.readlines()
    file.close()
    startline = sub.startline; endline=sub.endline;
    
    if(verbose): print(f"getLocalVariables::{filename},{subname} at L{startline}-{endline}")
    # Doesn't actaully get rid of associate (can though) should rename .
    cc = "." 
    # non-greedy capture
    ng_regex_array = re.compile(f'\w+?\s*\({cc}+?\)')
    
    find_arg = re.compile(r'(intent)',re.IGNORECASE)
    find_this = re.compile(r'^(this|type)',re.IGNORECASE)
    find_type = re.compile(r'(?<=\()\w+(?=\))')
    find_variables = re.compile('^(class\(|type\(|integer|real|logical|character)',re.IGNORECASE)
    regex_var = re.compile(r'\w+')
    regex_subgrid_index = re.compile('(?<=bounds\%beg)[a-z]',re.IGNORECASE)
    # test for intrinsic data types or user defined
    intrinsic_type = re.compile(r'^(integer|real|logical|character)', re.IGNORECASE)
    user_type = re.compile(r'^(class\(|type\()',re.IGNORECASE)
    #
    found_this = False
    
    for ln in range(startline,endline):
        line = lines[ln].split("!")[0]
        line = line.strip()
        line = line.strip("\n")
        if(not(line)): continue
        if(not class_var): # NOTE: This is a clunky way of doing this 
            match_variable = find_variables.search(line)
            if(match_variable):
                temp_decl = line.split("::")[0]
                temp_vars = line.split("::")[1]
                match_arg = find_arg.search(temp_decl)

                # Get data type first 
                m_type = intrinsic_type.search(temp_decl)
                if(m_type):
                    data_type = m_type.group() 
                else: #user-defined type 
                    m_type = user_type.search(temp_decl)
                    if(not m_type): 
                        sys.exit(f"Error: Can't Identify Data Type for {line}")
                    data_type = find_type.search(temp_decl).group()
                
                #
                # Go through and replace all arrays first
                match_arrays = ng_regex_array.findall(temp_vars)
                if(match_arrays): 
                    for arr in match_arrays:
                        varname = regex_var.search(arr).group()
                        index = regex_subgrid_index.search(arr)
                        if(index):
                            subgrid = index.group() 
                        else:
                            if(verbose): print(f"{arr} Not allocated by bounds")
                            subgrid = '?'
                        # Storing line number of declaration 
                        dim = arr.count(',')+1
                        if(verbose): print(f"var = {varname}; subgrid = {subgrid}; {dim}-D array")
                        if(match_arg):
                            optional = False 
                            if('optional' in temp_decl):
                                optional = True
                            sub.Arguments.append(Variable(data_type,varname,subgrid,ln,dim,optional))
                        else: 
                            sub.LocalVariables['arrays'].append(Variable(data_type,varname,subgrid,ln,dim))
                        # This removes the array from the list of variables
                        temp_vars = temp_vars.replace(arr,'')
                # Get the scalar arguments  
                temp_vars = temp_vars.split(',')
                temp_vars = [x.strip() for x in temp_vars if x.strip()]
                for var in temp_vars:
                    if(match_arg):
                        optional = False 
                        if('optional' in temp_decl):
                            optional = True
                        sub.Arguments.append(Variable(data_type,var,'',ln,dim=0,optional=optional))
                    else:
                        parameter = bool("parameter" in temp_decl.lower())
                        if(parameter): continue
                        if("=" in var): var = var.split("=")[0]
                        sub.LocalVariables['scalars'].append(Variable(data_type,var,'',ln,dim=0))
                    
        else: #class var
            m_this = find_this.search(line.lower())
            m_type = find_type.findall(line.lower())
            if(m_this and m_type):
                # the derived type should always be 1st
                if(verbose): print(f"found {m_type} for data type for this")
                var_type = m_type[0]
                found_this = True 
                cmd = f'grep -E "^[[:space:]]+(type\({var_type}\))" {elm_files}*.F90 | grep -v "intent"'
                output = sp.getoutput(cmd) 
                print(f"cmd: {cmd}")
                print(f"OUTPUT: {output}")
                output = output.split('::')
                varname = output[1].strip()
                return var_type, varname
            
    if(class_var and not found_this):
        print(f"Error: Couldn't find declaration for class variable in {sub.name} {sub.filepath}")
        sys.exit()
    return

def convertAssociateDict(associate_vars, varlist):
    dtypes = []
    for vars in associate_vars.values():
        for v in vars: 
            _type, field = v.split("%")
            if(_type not in dtypes):
                dtypes.append(_type)
    
    # Analyze derived type if not already
    for _type in dtypes:
        for var in varlist: 
            if(_type == var.name and not var.analyzed):
                var.analyzeDerivedType() 

    return

def adjust_array_access_and_allocation(local_arrs,passed_to_sub,sub,verbose=False):
    """
    Function edits ELM FORTRAN files to reduce memory.
    Replaces statements like "arr(bounds%begc:bounds%endc)" -> "arr(1:num_filterc)" 
    and accesses like "arr(c)" -> "arr(fc)" 

    * local_arrs    : list of local array Variables
    * passed_to_sub : dictionary that matches "arr" to subroutines that take them as arguments
    * sub           : Subroutine that calls this function and declares the local_arrs
    """
    import re
    from mod_config import _bc, elm_files,home_dir
    import os.path 

    # Get lines of this file:
    ifile = open(elm_files+sub.filepath,'r')
    print(f"First Time modifying {sub.filepath} for memory adjustments")
    
    lines = ifile.readlines() # read entire file
    ifile.close()
    track_changes = [] 
    arg_list = [v.name for v in sub.Arguments]
    scalar_list = [v.name for v in sub.LocalVariables['scalars'] ]

    print(_bc.BOLD+_bc.FAIL+f"arguments for {sub.name} are",arg_list,_bc.ENDC)
    print(_bc.BOLD+_bc.FAIL+f"scalars for {sub.name} are",scalar_list,_bc.ENDC)

    # replace declarations first
    for arr in local_arrs:
        var = arr[0]; filter_used = arr[1]
        ln = var.ln; #line number of declaration
        subgrid = var.subgrid # subgrid index 

        print(_bc.BOLD+_bc.WARNING+f"Adjusting {var.name}"+_bc.ENDC)
        filter_used = filter_used + subgrid
        
        # Check that the corresponding num_filter exists
        num_filter = "num_"+filter_used.replace("filter_","")
        if(num_filter not in arg_list):
            print(num_filter,"doesn't exist!")
            sys.exit()
        lold = lines[ln]
        print(_bc.FAIL+lold.strip('\n')+_bc.ENDC)
        _str = f"bounds%beg{subgrid}:bounds%end{subgrid}"

        replace_str = f"1:{num_filter}"
        lnew = lines[ln].replace(_str,replace_str)
        print(_bc.OKGREEN+lnew.strip('\n')+_bc.ENDC)
        lines[ln] = lnew 
        track_changes.append(lnew)

    # Go through all loops and make replacements for filter index 
    ng_regex_array = re.compile("\w+\s*\([,\w+\*-]+\)",re.IGNORECASE)
    regex_var = re.compile(r'\w+')
    regex_indices = re.compile(r'(?<=\()(.+)(?=\))')
    
    # Make list for quick check if var should be adjusted.
    list_of_var_names = [v[0].name for v in local_arrs]
    for loop in sub.DoLoops:
        lstart = loop.start[0]; lend = loop.end[0]
        if(loop.subcall.name != sub.name): continue
        for n in range(lstart,lend):
            l = lines[n].split("!")[0]
            l = l.strip() 
            if(not l): continue
            m_arr = ng_regex_array.findall(lines[n])
            lold = lines[n]
            lnew = lines[n]

            replaced = False 
            temp_line = lold
            
            removing = True
            while(removing):
                # set removing to be False unless there is a match
                removing = False 
                
                for arr in m_arr:
                    v = regex_var.search(arr).group()

                    # min and max functions are special cases since they take two argumnets (fix?)
                    if(v in ["min","max"]): 
                        temp_line = temp_line.replace(arr,v)
                        removing = True
                        continue
                    # Check if var is 
                    if(v in list_of_var_names):
                        loc_ = list_of_var_names.index(v)
                        local_var = local_arrs[loc_]
                        var = local_var[0]
                        filter_used = local_var[1]+var.subgrid

                        # Consistency check for filter.
                        # TODO: allow scripts to insert reverse filter (eg., "fc = col_to_filter(c)" )
                        if(filter_used != loop.filter[0]):
                            print(f"Filter Mismatch: loops uses {loop.filter[0]}, var needs {filter_used}")
                            sys.exit()
                        # Make replacement in line: (assumes subgrid is first index!!)
                        # lnew = lnew.replace(f"{v}({subgrid}",f"{v}(f{subgrid}")
                        lnew = re.sub(f"{v}\s*\({subgrid}",f"{v}(f{subgrid}",lnew)
                        replaced = True 

                    regex_check_index = re.compile(f"\w+\([,a-z0-9*-]*(({v})\(.\))[,a-z+0-9-]*\)",re.IGNORECASE)
                    match = regex_check_index.search(temp_line)
                    if(match): # array {v} is being used as an index
                        # substitute from the entire line 
                        i = regex_indices.search(arr).group()
                        i = f"\({i}\)"
                        temp_line = re.sub(f"{v}{i}",v,temp_line)
                        removing = True
                m_arr = ng_regex_array.findall(temp_line)
                
            if(replaced and verbose):
                print(_bc.FAIL + lold.strip('\n') + _bc.ENDC)
                print(_bc.OKGREEN+lnew.strip('\n')+_bc.ENDC)
                print("\n")
                lines[n] = lnew
                track_changes.append(lnew)

    
    # Check if subroutine calls need to be adjusted
    for arr in passed_to_sub:
        childsub = passed_to_sub[arr][0] 
        ln = passed_to_sub[arr][1]
        argnum = passed_to_sub[arr][2] 
        
        lold = lines[ln].strip('\n')
        
        # get the Variable instance for "arr"
        # If index fails, then there is an inconsistency 
        # in examineLoops 
        loc_ = list_of_var_names.index(arr)
        local_var = local_arrs[loc_]
        var = local_var[0]
        filter_used = local_var[1]+var.subgrid
        
        # bounds string:
        bounds_string = f"\s*bounds%beg{var.subgrid}\s*:\s*bounds%end{var.subgrid}\s*"
        # string to replace bounds with
        num_filter = "num_"+filter_used.replace("filter_","")
        num_filter = f"1:{num_filter}"
        replaced = False 
        regex_array_arg = re.compile(f"{arr}\s*\({bounds_string}")
        match = regex_array_arg.search(lold)
        if(match):
            lnew = regex_array_arg.sub(f"{arr}({num_filter}",lines[ln])
            lines[ln] = lnew 
            replaced = True 
            track_changes.append(lnew)


        while(lold.endswith('&') and not replaced):
            ln += 1
            lold = lines[ln].strip('\n')
            match = regex_array_arg.search(lold)
            if(match): 
                lnew = regex_array_arg.sub(f"{arr}({num_filter}",lines[ln])
                lines[ln] = lnew
                replaced = True 
                track_changes.append(lnew)

        
        if(replaced):
            print(_bc.FAIL   +lold.strip('\n')+_bc.ENDC)
            print(_bc.OKGREEN+lnew.strip('\n')+_bc.ENDC)
            print("\n")
        else:
            sys.exit(f"Error: couldn't replace argument {arr}")
    
        # Modify dummy arguments for child subs if needed 
        # print(arr,"matches:",childsub.dummy_args_list[argnum])
        darg = Variable(var.type,childsub.dummy_args_list[argnum],var.subgrid,0,var.dim,var.optional)
        adjust_child_sub_arguments(childsub,darg)

    # Save changes:
    if(track_changes):
        if(not os.path.exists(home_dir+f"modified-files/{sub.filepath}")):
            print(_bc.BOLD+_bc.WARNING+"Writing to file ",home_dir+f"modified-files/{sub.filepath}"+_bc.ENDC)
            ofile = open(home_dir+f"modified-files/{sub.filepath}",'w')
            ofile.writelines(lines) 
            ofile.close()
        else: 
            print(_bc.BOLD+_bc.WARNING+"Writing to file ",elm_files+sub.filepath+_bc.ENDC)
            ofile = open(elm_files+sub.filepath,'w')
            ofile.writelines(lines) 
            ofile.close()

def adjust_child_sub_arguments(childsub,arg):
    """
    Function that checks and modifies bounds accesses 
    for subroutine arguments 
        * childsub : Subroutine instance for child subroutine 
        * arg : Variable instance for dummy arg name
    """
    ifile = open(elm_files+childsub.filepath,'r') 
    lines = ifile.readlines() 
    ifile.close() 

    startline = childsub.startline
    endline = childsub.endline 
    regex_arg_type = re.compile(f"{arg.type}")
    regex_arg_name = re.compile(f"{arg.name}\s*\(")
    regex_bounds = re.compile(f"bounds%beg{arg.subgrid}")

    for ct in range(startline-1,endline):
        line = lines[ct]
        line = line.split("!")[0] 
        line = line.strip()
        # Match type and name for arg 
        match_type = regex_arg_type.search(line) 
        match_name = regex_arg_name.search(line)
        if(match_type and match_name): 
            match_bounds = regex_bounds.search(line) 
            if(match_bounds):
                print(f"Need to replace {line} and all accesses")
                sys.exit()
            else:
                print(f"{line} -- No Action Needed")
                return 
    
    