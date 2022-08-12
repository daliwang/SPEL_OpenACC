import re
import sys
from process_associate import get_rid_of_associate
import subprocess as sp
from mod_config import elm_files,home_dir
from LoopConstructs import Loop, exportVariableDependency 

def debug_print(str_debug, verbose):
    if (verbose <= DEBUG_PRINT_VERBOSE):
        print(str_debug)

def error_print(str_error, verbose):
    if (verbose <= DEBUG_PRINT_VERBOSE):
        print(str_error)

def test_bit(int_type, offset):
    mask = 1 << offset
    return(int_type & mask)

def replace_key(key):

    if(key == 'vegcf'):
        return 'veg_cf'
    elif(key == 'colcf'):
        return 'col_cf'
    elif(key == 'colcs'):
        return 'col_cs'
    elif(key == 'vegcs'):
        return 'veg_cs'
    elif(key == 'vegpf'):
        return 'veg_pf'
    elif(key == 'colpf'):
        return 'col_pf'
    elif(key == 'colps'):
        return 'col_ps'
    elif(key == 'vegps'):
        return 'veg_ps'
    elif(key == 'vegnf'):
        return 'veg_nf'
    elif(key == 'colnf'):
        return 'col_nf'
    elif(key == 'colns'):
        return 'col_ns'
    elif(key == 'vegns'):
        return 'veg_ns'
    else:
        return key    
    

def find_file_for_subroutine(name):
    """
    finds file, start and end line numbers for subroutines
    find file and start of interface for interfaces
    """
    interface_list = get_interface_list()
    if(name not in interface_list):
        cmd = f'grep -in -E "^[[:space:]]*(subroutine {name})\\b" {elm_files}*.F90 | head -1'
        cmd_end = f'grep -in -E "^[[:space:]]*(end subroutine {name})\\b" {elm_files}*.F90 | head -1'
    else:
        cmd = f'grep -in -E "^[[:space:]]+(interface {name})" {elm_files}*.F90 | head -1'
        cmd_end = ''

    output = sp.getoutput(cmd)
    file = output.split(':')[0]
    file = file.replace(elm_files,'')
    startline = int(output.split(':')[1])
    if(cmd_end!=''):
        output = sp.getoutput(cmd_end)
        endline = int(output.split(':')[1])
        
    else:
        endline = 0

    if(file == 'grep'):
        print(f"ERROR FILE FOR {name} NOT PRESENT")
        sys.exit(1)

    return file, startline, endline

def determine_level_in_tree(branch,tree_to_write):
    """
    will be called recursively
    branch is a list containing names of subroutines
    ordered by level in call_tree
    """
    for j in range(0,len(branch)):
        sub_el = branch[j]
        islist = bool(type(sub_el) is list)
        if(not islist):
            #if(sub_el not in added):
            #    added.append(sub_el)
            if(j+1 == len(branch)):
                tree_to_write.append([sub_el,j-1])
            elif(type(branch[j+1]) is list):
                tree_to_write.append([sub_el,j-1])

        if(islist):
            tree_to_write = determine_level_in_tree(sub_el,tree_to_write)

    return tree_to_write

def add_acc_routine_info(sub):
    """
    This function will add the !$acc routine directive to subroutine
    """
    filename = sub.filepath

    file = open(elm_files+filename,'r')
    lines = file.readlines() # read entire file
    file.close()

    first_use = 0
    ct = sub.startline
    while(ct < sub.endline):
        line = lines[ct]
        l = line.split('!')[0]
        if(not l.strip()): ct+=1; continue; #line is just a commment

        if (first_use == 0):
            m = re.search(f'[\s]+(use)',line)
            if(m): first_use = ct;

            match_implicit_none = re.search(r'[\s]+(implicit none)',line)
            if(match_implicit_none): first_use = ct
            match_type = re.search(r'[\s]+(type|real|integer|logical|character)',line)
            if(match_type): first_use = ct

        ct+=1
    print(f'first_use = {first_use}')
    lines.insert(first_use,'      !$acc routine seq \n')
    print(f"Added !$acc to {sub.name} in {filename}")
    with open(elm_files+filename,'w') as ofile:
        ofile.writelines(lines)

def get_interface_list():
    """
    returns a list of all interfaces!
    """
    cmd = f'grep -in -E "^[[:space:]]+(interface)" {elm_files}*.F90'
    output = sp.getoutput(cmd)
    output = output.split('\n')

    interface_list = []
    for el in output:
        el = el.split()
        interface = el[2]
        interface_list.append(interface)

    return interface_list

def remove_bounds(line):
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
    for bound in m1D: newline = newline.replace(bound,'')

    m2D = non_greedy2D.findall(newline)
    for bound in m2D:
        print(bound)
        newline = newline.replace(bound,'')

    m3D = non_greedy3D.findall(newline)
    for bound in m3D: newline = newline.replace(bound,'')

    m4D = non_greedy4D.findall(newline)
    for bound in m4D: newline = newline.replace(bound,'')
    return newline

def get_args(line):
    """
    This function returns the arguments used to call a subroutine
    Needed to resolve interfaces
    """
    par = re.compile("(?<=\().+(?=\))")
    m = par.search(line)
    args = m.group()
    print("==============================")
    print(args)

    newargs = remove_bounds(args)

    print("done taking out bounds ")
    print(newargs)
    args = newargs.split(',')
    return args

def getLocalVariables(sub,verbose=False,class_var=False ):
    """
    this function will retrieve  the local variables from
    the subroutine at startline, endline in the given file

    Note: currently only looks at type declaration for class object
    """
    filename = elm_files+sub.filepath
    subname = sub.name 
    file = open(filename,'r');
    lines = file.readlines()
    startline = sub.startline; endline=sub.endline;
    
    if(verbose): print(f"getLocalVariables::{filename},{subname} at L{startline}-{endline}")
    #Doesn't actaully get rid of associate (can though) should rename .
    sub.associate_vars, sub.status, associate_start, associate_end = get_rid_of_associate(sub)
    
    #initialize global_vars 
    global_vars = [key for key in sub.associate_vars.keys()]
    local_vars = {}
    cc = "." 
    # non-greedy capture
    ng_regex_array = re.compile(f'\w+?\({cc}+?\)')
    find_arg = re.compile(r'(intent)',re.IGNORECASE)
    find_this = re.compile(r'(this)$',re.IGNORECASE)
    find_type = re.compile(r'(?<=\()\w+(?=\))')
    find_variables = re.compile('(type\(|integer|real|logical|character)',re.IGNORECASE)
    regex_var = re.compile(r'\w+')
    regex_subgrid_index = re.compile('(?<=bounds\%beg)[a-z]',re.IGNORECASE)

    found_this = False
    for i in range(startline,associate_start):
        line = lines[i].split("!")[0]
        line = line.strip()
        line = line.strip("\n")
        if(not(line)): continue
        match_variable = find_variables.search(line.lower())
        if(match_variable):
            temp_vars = line.split("::")[1]
            temp_decl = line.split("::")[0]
            match_arg = find_arg.search(temp_decl)
            if(match_arg): continue 
            
            match_arrays = ng_regex_array.findall(temp_vars) 
            if(match_arrays): 
                varname = regex_var.search(temp_vars).group()
   
                print(temp_vars,match_arrays)
                for v in match_arrays:
                    varname = regex_var.search(v).group()
                    index = regex_subgrid_index.search(v)
                    if(not index): 
                        if(verbose): print(f"{v} Not allocated by bounds")
                        local_vars[varname] = '' 
                    else:
                        if(verbose): print(f"var = {varname}; subgrid = {index.group()}")
                        local_vars[varname] = [index.group(), i]


        if(class_var):
            m_this = find_this.search(line.lower())
            m_type = find_type.findall(line.lower())
            if(m_this and m_type):
                # the derived type should always be 1st
                var_type = m_type[0]
                found_this = True 
                return var_type

    if(class_var and not found_this):
        print("Error: Couldn't not find declaration for class variable (this)")
        sys.exit()
    return local_vars

def replaceArrayBounds(sub,var,Loops):
        """
        search through all loops and declarations that use var
        and replace the declarations with num_filter and the loop access 
        with the filter index 
        """
        


class Subroutine(object):
    def __init__(self, name,file,calltree):
        self.name = name
        self.filepath = file
        self.subroutine_init = None
        self.clmtype_r = {}
        self.clmtype_w = {}
        self.clmtype_rw = {}
        self.child_clmtype_r = {}
        self.child_clmtype_w = {}
        self.child_clmtype_rw = {}
        self.child_subroutine_list = []
        self.child_Subroutine = {}
        self.status = False
        self.startline = 0
        self.endline = 0
        self.associate_vars = []
        self.local_vars = []
        self.acc_status = False
        self.calltree = list(calltree)
        self.calltree.append(name)
        self.clmtypes = []
        self.DoLoops = []

    def _get_global_constants(self,constants):
        """
        This function will loop through an ELM F90 file,
        collecting all constants -- derived types must be included in the
        usemod.txt file manually
        """
        const_mods = ['elm_varcon','elm_varpar',
                      'landunit_varcon','column_varcon','pftvarcon',
                      'elm_varctl']
        file = open(elm_files+self.filepath,'r')
        lines = file.readlines()
        print(f"opened file {self.filepath}")
        ct = 0
        while (ct < len(lines)):
            line = lines[ct]
            l = line.strip()
            match_use = re.search(r'^use ',l.lower())
            if(match_use):
                l = l.replace(',',' ').replace(':', ' ').replace('only','').split()
                m = l[1]
                if(m in const_mods):
                    for c in l[2:]:
                        if c.lower() not in constants[m]: constants[m].append(c.lower())

            ct+=1
        print(constants)



    def _determine_subroutine_from_interface(self,args,name):
        ## USED FOR INTERFACES!!
        ## opens file containing subroutine and returns
        ## the start line and end line of subroutine
        ## and stores to be reused in processing.
        file,startline,endline = find_file_for_subroutine(name)
        iofile = open('../'+file, 'r')
        lines = iofile.readlines()
        iofile.close()
        ct = startline
        #logical that tracks being inside the interface
        interface = True
        procedures = []
        while (interface ):
            line = lines[ct]

            mod_proc = re.compile(f"^[\s]+(module procedure)")
            end_interface = re.compile(f"^[\s]+(end interface)")

            if(mod_proc.search(line.lower()) ):
                temp = line.replace('module procedure', '')
                temp = temp.split(',')
                for proc in temp: procedures.append(proc.rstrip('\n'))
            if(end_interface.search(line.lower() ) ): interface = False
            ct = ct + 1

        print(procedures)


        return name

    def _preprocess_file(self):
        """
        This function will find child subroutines and variables used
        in the associate clause to be used for parsing for ro and wr derived types
        """
        rewrite = False

        interface_list = get_interface_list()
        #Doesn't actaully get rid of associate (can though) should rename .
        self.associate_vars, self.status, associate_start, associate_end = get_rid_of_associate(self)

        self._check_acc_status()
        if(not self.acc_status):
            print(f"Must Add Acc Routine Directive to {self.name}")
            add_acc_routine_info(self)

        file = open(elm_files+self.filepath,'r')
        lines = file.readlines()
        file.close()

        ct = self.startline
        while(ct < self.endline):
            line = lines[ct]
            match_call = re.search(r'^call ',line.strip())
            if(match_call) :
                method = False
                x = line.strip().replace('(',' ').replace(')',' ').split()
                child_sub_name = ''
                ignore = bool(x[1].lower() in ['cpu_time','get_curr_date'] or 'update_vars' in x[1].lower())
                if(not ignore):
                    child_sub_name = x[1]

                    if('%' in child_sub_name):
                        print(f"WARNING CALLING CLASS METHOD at LINE {ct}!")
                        print(line)
                        method = True

                    child_subs = self.child_subroutine_list

                    if(child_sub_name not in child_subs and not method):
                        self.child_subroutine_list.append(child_sub_name)

            ct+=1

        if(rewrite):
            with open(elm_files+self.filepath,'w') as ofile:
                ofile.writelines(lines)

        #cover case where no derived types but still has child
        #subroutines to analyze
        if(self.child_subroutine_list):
            self.status = False

        return associate_end

    def _check_acc_status(self):
        """
        checks if subroutine already has !$acc routine directives
        """
        filename = self.filepath
        file = open(elm_files+filename,'r')
        lines = file.readlines() # read entire file
        for ct in range(self.startline, self.endline):
            line = lines[ct]
            # checks for any !$acc pragmas to determine if 
            # !$acc routine seq is needed.
            match_acc_routine = re.search(r'[\s]+(\!\$acc)',line)
            if(match_acc_routine): self.acc_status = True; return

    def _analyze_variables(self,associate_end,vars, class_routine_info):
        """
        function used to determine read and write variables
        If var is written to first, then rest of use is ignored.
        Vars determined to be read are read-in from a file generated by
        full E3SM run.
        Vars that are determined to be written to must be checked to verify
        results of unit-testing.
        """
        #dictionary to be returned
        file = open(elm_files+self.filepath,'r')
        lines = file.readlines()
        file.close()

        if(class_routine_info["bool"]):
            print(f"In class routine ",class_routine_info["var"])

        vardict = {} # dictionary where key is the variable and values are read/write status.
        c = '[^a-zA-Z0-9_%]'  #character class for regex needed to ignore '%'
        for v in vars:
            variable_used = False
            ct = associate_end
            first_match = True
            while (ct < self.endline):
                line = lines[ct]
                #get rid of comments
                line = line.split('!')[0]

                #take into account line continuations
                line = line.rstrip('\n')
                while line.endswith('&'):
                    ct+=1
                    line = line[:-1] + lines[ct].strip().rstrip('\n')

                if(class_routine_info["bool"]):
                    vname = class_routine_info["var"]
                    line = re.sub("this%",vname+"%",line)

                #search line for variable:
                matchv     =  re.search(f'(?<={c}){v}(?=\W)',line) #match variable
                matcheq    =  re.search(r'[^<>=/]=[^><=]', line) #match assignment
                match_doif = re.search(r'^\s*(do |if|else if).',line) #match do and if statements
                match_call = re.search(r'^\s*call ',line)
                if((not match_doif) and matchv and matcheq and not(match_call)):
                    variable_used = True
                    l = line.replace('(',' ').replace(',',' ').replace(')',' ').replace('=',' = ')
                    split_line = l.split('=')
                    if (len(split_line) > 2 ):
                        print(line);
                        split_line[1] = split_line[1]+"=="+split_line[3]
                        print(split_line)
                        if(len(split_line) > 4): sys.exit("too many equals")
                    # if on left-hand side of '=' it's an output variable
                    # ignore rest for now -- USING clmtype_rw for NOW
                    lhs = split_line[0]
                    rhs = split_line[1]
                    match_lhs = re.search(f'(?<={c}){v}(?=\W)', lhs)
                    if(match_lhs):
                        vardict.setdefault(v,[]).append('w')
                        #if the first_match is write then it doesn't
                        # need to be read in from file
                        if(first_match): first_match = False; ct = self.endline

                    match_rhs = re.search(f'(?<={c}){v}(?=\W)', rhs)
                    if(match_rhs):
                        if(first_match): first_match = False;
                        vardict.setdefault(v,[]).append('r')

                    if(not(match_lhs or match_rhs)):
                        print(self.name, ct, v); print(line)
                        print(lhs, rhs)
                        sys.exit('Must be rhs or lhs')

                #count if and do statements as being read variables
                if(match_doif and matchv):
                    variable_used = True
                    if(first_match): first_match = False;
                    vardict.setdefault(v,[]).append('r')

                if(matchv and match_call): #assume read and write
                    variable_used = True
                    vardict.setdefault(v,[]).append('r');  vardict.setdefault(v,[]).append('w')

                ct += 1

        ##clean up vardict:
        for key in vardict.keys():  vardict[key] = list(set(vardict[key]))
        return vardict

    def parse_subroutine(self,clmvar,verbose=False):
            """
            This function parses temp.F90 to find which variables are ro,wo,rw
            clmvars is a list of derived_type variables.
            This function is called in main() after process_for_unit_test
            currently varlist holds all derived types known to be used in ELM.
            """
            ignoreList = ['this','self','colpf','colnf','colps','colcf','colns',
                          'vegcf','vegcs','vegps','vegpf','vegns','vegnf','bounds',
                          'spm','spm_d','spm_p','isocol_cs', 'isoveg_cs','colcs',
                          'isocol_cf', 'isoveg_cf','cs','cf','col_cf_input']

            associate_end   = self._preprocess_file()
            associated_list = [ key for key in self.associate_vars.keys() ]
            temp_dtype_names = []
            dtype_var_list = []
            class_routine_info = {"bool":False, "var" : ""}
            for val in self.associate_vars.values():
                for x in val:
                    dtype = x.split('%')[0]
                    temp_dtype_names.append(dtype)
            #Create list that holds the names of the derived types only
            elm_var_names = [v.name for v in clmvar]
            if(associate_end == 0):
                ct = self.startline
            else:
                ct = associate_end

            fname = elm_files+self.filepath
            file = open(elm_files+self.filepath,'r')
            lines = file.readlines()
            file.close()
            class_routine = False
            class_routine_info["bool"] = False
            while( ct < self.endline ):
                line = lines[ct]
                #get rid of comments
                line = line.strip().split('!')[0]
                #take into account line continuations
                line = line.rstrip('\n')
                while line.endswith('&'):
                    ct+=1
                    line = line[:-1] + lines[ct].strip().rstrip('\n')

                match = bool('%' in line and not('call' in line))
                if(match):
                    match_var = re.findall(r"\w+%\w+",line)
                    for dtype in match_var:
                        typename = dtype.split('%')[0]
                        if(typename.lower() in ['this'] and not class_routine ):
                            if(verbose ): print(f"Inside 'class' routine {self.name}")
                            class_routine = True
                            class_routine_info["bool"] = True
                            #
                            var_type = getLocalVariables(self,class_var=True)
                            err = True
                            for vname in clmvar:
                                if(vname.dtype == var_type):
                                    err = False
                                    if(verbose): print(f"{typename} is {vname.name}")
                                    typename = vname.name
                                    dtype = re.sub("this%",vname.name +"%",dtype)
                                    if(verbose): print(f"var is {dtype}")
                                    class_routine_info["var"] = vname.name

                            if(err): sys.exit("Couldn't find var_type from list")


                        if(typename.lower() not in ignoreList):
                            temp_dtype_names.append(typename)
                            dtype_var_list.append(dtype)
                #end of if(match)
                ct+=1
            dtype_var_list = list(set(dtype_var_list))
            self.clmtypes  = list(set(temp_dtype_names))

            if(verbose): print(f"analyzing associate list variables for sub {self.name}")
            vardict_associate = self._analyze_variables(associate_end,associated_list, class_routine_info)
            #Need to match read/write status of associate pointer to the derived type:
            for key, values in vardict_associate.items():
                dtypes_li = self.associate_vars[key] #list of derivedtypes pointed to by key
                for var in dtypes_li:
                    varname   = var.split('%')[0]
                    varname = replace_key(varname)
                    if(varname == 'vegcf'): varname = 'veg_cf'
                    if(varname == 'col_cf_input'): varname = 'col_cf'
                    try:
                        component = var.split('%')[1]
                    except:
                        print('Error:',var)

                    if( varname.lower() in ['this']):
                        if(verbose): print(f"ajusting class procedure associate list")
                        for vname in elm_var_names:
                            match_class_var = re.search(f'{vname}',self.name)
                            if (match_class_var):
                                if(verbose): print(f"{varname} is {vname}")
                                varname = vname

                    if('r' in values): self.clmtype_r.setdefault(varname,[]).append(component)
                    if('w' in values): self.clmtype_w.setdefault(varname,[]).append(component)


            print(f"analyzing other list variables for sub {self.name}")
            vardict_dtypes = self._analyze_variables(associate_end, dtype_var_list,class_routine_info)
            for key, values in vardict_dtypes.items():
                varname   = key.split('%')[0]
                component = key.split('%')[1]
                if('r' in values): self.clmtype_r.setdefault(varname,[]).append(component)
                if('w' in values): self.clmtype_w.setdefault(varname,[]).append(component)

            #clean up!
            for key in self.clmtype_r.keys(): self.clmtype_r[key] = list(set(self.clmtype_r[key]))
            for key in self.clmtype_w.keys(): self.clmtype_w[key] = list(set(self.clmtype_w[key]))
            

    def child_subroutines_analysis(self,clmvar):
        """
        This function handles parsing child_subroutines and merging
        variable dictionaries...
        """
        #print(self.clmtype_ro)
        interface_list = get_interface_list()
        for child_sub in self.child_subroutine_list:

            file,startline,endline = find_file_for_subroutine(child_sub)
            self.child_Subroutine[child_sub] = Subroutine(child_sub,file,self.calltree)
            sub = self.child_Subroutine[child_sub]
            sub.startline = startline; sub.endline = endline
            if(child_sub in interface_list):  continue
            self.child_Subroutine[child_sub].parse_subroutine(clmvar)

            if(sub.child_subroutine_list):
                sub.child_subroutines_analysis(clmvar)

            #add needed clmtypes to parent subroutine
            self.clmtypes.extend(sub.clmtypes)
            self.clmtypes = list(set(self.clmtypes))

            for key, values in sub.clmtype_r.items():
                for var in values:
                    self.clmtype_r.setdefault(key,[]).append(var)
                self.clmtype_r[key] = list(set(self.clmtype_r[key]))
            for key, values in sub.clmtype_w.items():
                for var in values:
                    self.clmtype_w.setdefault(key,[]).append(var)
                self.clmtype_w[key] = list(set(self.clmtype_w[key]))
                

        for s in self.child_Subroutine.values():
            self.calltree.append(s.calltree)
            if(child_sub in interface_list): continue

        self.status = True

    def analyze_calltree(self,tree):
        """
        returns unraveled calltree
        """
        tree_to_write = [[self.name,0]]
        #added = ['clm_drv',self.name]
        for i in range(0,len(tree)):
            el = tree[i]
            tree_to_write = determine_level_in_tree(branch=el,tree_to_write=tree_to_write)

        ofile = open(f"{self.name}CallTree.dat",'w')
        for branch in tree_to_write:
            level=branch[1];sub = branch[0]
            print(level*"|---->"+sub)
            ofile.write(level*"|---->"+sub+'\n')
        ofile.close()

    def generate_update_directives(self, clmvars_dict):
        """
        This function will create .F90 routine to execute the
        update directives to check the results of the subroutine
        """
        ofile = open(f"{home_dir}scripts/script-output/update_vars_{self.name}.F90",'w')

        spaces ="     "
        ofile.write("subroutine update_vars_{}(gpu,desc)\n".format(self.name))
        replace_inst = ['soilstate_inst','waterflux_inst','canopystate_inst','atm2lnd_inst','surfalb_inst',
                        'solarabs_inst','photosyns_inst','soilhydrology_inst','urbanparams_inst']
        for v in self.clmtype_w.keys():
            if(v in replace_inst): v = v.replace('_inst','_vars')

            mod = clmvars_dict[v].declaration
            ofile.write(spaces + f"use {mod}, only : {v} \n")

        ofile.write(spaces+"implicit none \n")
        ofile.write(spaces+"integer, intent(in) :: gpu\n")
        ofile.write(spaces+"character(len=*), optional, intent(in) :: desc\n")
        ofile.write(spaces+"character(len=256) :: fn\n")
        ofile.write(spaces+"if(gpu) then\n")
        ofile.write(spaces+spaces+f'fn="gpu_{self.name}"\n')
        ofile.write(spaces+"else\n")
        ofile.write(spaces+spaces+f"fn='cpu_{self.name}'\n")
        ofile.write(spaces+'end if\n')
        ofile.write(spaces+"if(present(desc)) then\n")
        ofile.write(spaces+spaces+"fn = trim(fn) // desc\n")
        ofile.write(spaces+"end if\n")
        ofile.write(spaces+'fn = trim(fn) // ".txt"\n')
        ofile.write(spaces+'print *, "Verfication File is :",fn\n')
        ofile.write(spaces+"open(UNIT=10, STATUS='REPLACE', FILE=fn)\n")

        ofile.write(spaces+"if(gpu) then\n")
        acc = "!$acc "

        for v,comp_list in self.clmtype_w.items():
            if(v in replace_inst): v = v.replace('_inst','_vars')
            ofile.write(spaces+acc+"update self(& \n")
            i = 0
            for c in comp_list:
                i +=1
                if i == len(comp_list) :
                    name = f"{v}%{c}"
                    c13c14 = bool('c13' in name or 'c14' in name)
                    if(c13c14):
                        ofile.write(spaces+acc+f")\n")
                    else:
                        ofile.write(spaces+acc+f"{name} )\n")
                else:
                    name = f"{v}%{c}"
                    c13c14 = bool('c13' in name or 'c14' in name)
                    if(c13c14): continue
                    ofile.write(spaces+acc+f"{name}, & \n")


        ofile.write(spaces+"end if \n")
        ofile.write(spaces+"!! CPU print statements !! \n")
        ## generate cpu print statements
        for v,comp_list in self.clmtype_w.items():
            if(v in replace_inst): v = v.replace('_inst','_vars')
            for c in comp_list:
                name = f"{v}%{c}"
                c13c14 = bool('c13' in name or 'c14' in name)
                if(c13c14): continue
                ofile.write(spaces+f"write(10,*) '{name}' \n")
                ofile.write(spaces+f"write(10,*) {name}\n")

        ofile.write(spaces+"close(10)\n")
        ofile.write("end subroutine ")
        ofile.close()
    
    
    def examineLoops(self,global_vars,add_acc=False,subcall=False,verbose=False,adjust_allocation=False):
        """
        Function that will parse the loop structure of a subroutine
        Add loop parallel directives if desired
        """
        from mod_config import _bc
        if(not subcall):
            self.associate_vars, self.status, associate_start, associate_end = get_rid_of_associate(self)
            associate_keys = self.associate_vars.keys()
            temp_global_vars = [key for key in associate_keys]
            global_vars = temp_global_vars[:]
        
        if(verbose): print(f"Opening file {elm_files+self.filepath} ")
        ifile = open(elm_files+self.filepath,'r')
        lines = ifile.readlines() # read entire file
        ifile.close()
    
        loop_start = 0 
        loop_end = 0
        #regex_do= re.compile(r'^\s*(do)\s+[a-z].') #match do
        regex_do= re.compile(r'\s*(do)\s+\w+\s*(?=[=])',re.IGNORECASE)
        regex_dowhile = re.compile(f'\s*(do while)',re.IGNORECASE)
        regex_enddo = re.compile(r'^\s*(end)\s*(do)')
        regex_subcall = re.compile(r'^(call)')

        # Starting parsing lines at the subroutine start
        sublines = lines[self.startline-1:self.endline]
        loopcount = 0
        print(f"Examing Loops for {self.name}")
        dowhile = False 
        for n,line in enumerate(sublines):
            #get rid of comments 
            l = line.split('!')[0]
            l = l.strip()
            # Use RegEx
            m_call = regex_subcall.search(l.lower()) 
            m_do = regex_do.search(l.lower())
            m_enddo = regex_enddo.search(l.lower())
            m_dowhile = regex_dowhile.search(l.lower())
            if(m_dowhile): 
                dowhile = True  #Inside do while loop 
                print(n,line)
                if(verbose): print("Inside do while loop")
            # if we match subroutine call 
            # we should analyze it now so that the 
            # DoLoops array has the Loops in order of use
            if(m_call):
                x = l.replace('(',' ').replace(')',' ').split()
                child_sub_name = ''
                if(x[1].lower() not in ['cpu_time','get_curr_date'] and 'update_vars' not in x[1].lower()):
                    child_sub_name = x[1]
                    file,startline,endline = find_file_for_subroutine(child_sub_name)
                    childsub = Subroutine(child_sub_name,file,[self.name])
                    childsub.startline = startline; childsub.endline = endline;
                    # get global variable aliases 
                    childsub.associate_vars, childsub.status, associate_start, associate_end = get_rid_of_associate(childsub)
                    associate_keys = childsub.associate_vars.keys()
                    temp_global_vars = [key for key in associate_keys]
                    global_vars.extend(temp_global_vars)
                    global_vars = list(dict.fromkeys(global_vars).keys())
                    # Examine Do loops in child subs
                    if(verbose): 
                        print(f"Instantiated new Subroutine {child_sub_name}\n in file {file} L{startline}-{endline}")
                    childloops = childsub.examineLoops(global_vars=global_vars, subcall=True,verbose=verbose)
                    self.DoLoops.extend(childloops)

            if(m_do): 
                #get index 
                index = m_do.group().split()[1]
                #outer most loop
                if(loopcount == 0):
                    if(verbose): print(f"============{self.name}===============")
                    if(verbose): print("New loop at line ",n+self.startline)
                    loop_start = n+self.startline
                    newloop = Loop(loop_start,index,self.filepath,self.name)
                #Found an inner loop
                else :
                    newloop.index.append(index)
                    loop_start = n+self.startline; newloop.nested += 1;
                    newloop.start.append(loop_start)
                    newloop.end.append(0)
                loopcount += 1
            
            if(m_enddo): 
                if(dowhile): 
                    dowhile = False
                    if(verbose): 
                        print(_bc.WARNING + f"Do while loop in {self.name} ends at {n+self.startline}"+_bc.ENDC) 
                    continue 
                loopcount -= 1
                if(loopcount == 0) :
                    if(verbose): print("end of outer loop at ",n+self.startline)
                    loop_end = n+self.startline
                    # append loop object:
                    newloop.end[loopcount] = loop_end
                    lstart = newloop.start[0] - self.startline 
                    lend = loop_end - self.startline
                    
                    newloop.lines = sublines[lstart:lend+1]
                    self.DoLoops.append(newloop)
                    #if(verbose): 
                    #    print(f"Appending new Loop in {self.name}")
                    #    print(f"Loop Starts at {lstart+self.startline} - {lend+self.startline}")
                    #    newloop.printLoop()
                else:
                    loop_end = n+self.endline
                    newloop.end[loopcount] = loop_end 
        
        #Parse variables used in Loop
        
        print(f"Parsing variables for Loops in {self.name}")
        for n, loop in enumerate(self.DoLoops):
            if(loop.subcall is self.name):
                # if(verbose): loop.printLoop(substartline=self.startline,long=False)
                # loops in child subroutines have already been Parsed!
                loop.parseVariablesinLoop(verbose=verbose)
        
        if(adjust_allocation):
            local_vars = getLocalVariables(self,verbose=verbose)
            if(verbose): print("Checking if variables can be allocated by filter")
            for lcl_arr, vals in local_vars.items():
                filter_used = ''
                indx, ln = vals
                for loop in self.DoLoops: 
                    if(lcl_arr in loop.vars):
                        fvar, fidx = loop.filter
                        #Case where no filter is used 
                        if(not fvar): 
                            if(indx not in loop.index):
                                print("Can't replace?")
                                print(f"{lcl_arr} {indx} used in {loop.subcall}:L{loop.start[0]} {fvar},{fidx}{loop.index}")
                                sys.exit() 
                        if(not filter_used):
                            # Get rid of the subgrid info
                            filter_used = fvar[:-1]
                        elif(filter_used != fvar[:-1]):
                            print("Incorrect filter?")
                            print(f"{lcl_arr} {indx} used in {loop.subcall}:L{loop.start[0]} {fvar},{fidx}{loop.index}")
                            sys.exit()
                # If make it through the loops then time to replace?


            sys.exit()

        if(subcall):
            return self.DoLoops

        if(add_acc): 
            # Create dictionary to hold subroutines found so far 
            # and their initial startlines to help adjust for line insertions later.
            sub_dict = {}
            # dictionary of the loops to avoid adding OpenACC directives to the same 
            # Loop more than once 
            loop_dict = {} 
            for loop in self.DoLoops: 
                loopkey = f"{loop.subcall}:L{loop.start[0]}"
                if(loopkey not in loop_dict):
                    loop_dict[loopkey] = loop
                    loop.printLoop(long=False)
                if(loop.subcall not in sub_dict):
                    file,startline,endline = find_file_for_subroutine(loop.subcall)
                    sub_dict[loop.subcall] = startline

            lines_adjusted = {}
            for key, loop in loop_dict.items(): 
                if(not loop.reduction):
                    loop.printLoop(long=False)
                    if(loop.subcall not in lines_adjusted):
                        # This keeps track of the number of line adjustments
                        # inside a given subroutine.  The start of the subroutine 
                        # is rechecked inside addOpenACCFlags function
                        lines_adjusted[loop.subcall] = 0
                    file,startline,endline = find_file_for_subroutine(loop.subcall)
                    subline_adjust = startline - sub_dict[loop.subcall]

                    loop.addOpenACCFlags(lines_adjusted,subline_adjust)

            # Report which loops weren't auto converted (ie. ones with reduction operations)
            for keys, loop in loop_dict.items():
                if(loop.reduction): 
                    print(_bc.WARNING + f"Reduction in {loop.file}::{keys} for:\n {loop.reduce_vars}"+_bc.ENDC)

        #Note: Can't use List Comprehension here with class attributes
        var_list = []       
        for loop in self.DoLoops:
            for key in loop.vars.keys():
                var_list.append(key) 
        var_list = list(dict.fromkeys(var_list).keys())

        # print only the global variable list:
        global_loop_vars = [] 
        for v in var_list:
            if(v in global_vars or "filter" in v):
                global_loop_vars.append(v)
        
        exportVariableDependency(self.name,var_list,self.DoLoops[0:10])

        return

    def exportReadWriteVariables(self):
        """
        Writes the variables for read and write to a separate data file
        """
        spaces = "     "
        read_flat = [] 
        write_flat = []
        all_flat = [] 
        maxlen = 0
        #read variables
        print("exportReadWriteVariables ===================") 
        print(self.clmtype_r)
        for varname, components in self.clmtype_r.items():
            for c in components:
                var = f"{varname}%{c}"
                read_flat.append(var)
                all_flat.append(var)
                if(len(var) > maxlen): maxlen = len(var)

        for varname, components in self.clmtype_w.items():
            for c in components:
                var = f"{varname}%{c}"
                write_flat.append(var)
                if(len(var) > maxlen): maxlen = len(var)
                if(var not in all_flat): all_flat.append(var)
                
        
        output_list = [] 
        #header 
        ofile = open(f"{self.name}-ReadWriteVars.dat",'w')
        header = f"{'Variable':<{maxlen}} {'Status':5}"
        output_list.append(header)
        ofile.write(header+"\n")
        for var in all_flat:
            status = ''
            if(var in read_flat):
                status += 'r'
            if(var in write_flat):
                status +='w'
            if(len(status)<2): status +='o'
            string = f"{var:<{maxlen}} {status:5}\n"
            ofile.write(string)
        ofile.close()                 

        return 


    # def analyze_local_arrays(self):
    #     """
    #     This function will look at the local variables of the subroutine
    #     and find the automatic arrays.  Then, the function will determine if
    #     it's safe to replace automatic arrays with scalar.
    #     """
    #     print("file:",self.filepath,f"Subroutine {self.name}")
    #
    #
    #     file = open(source_dir+self.filepath,'r')
    #     lines = file.readlines()
    #
    #     ct = self.startline
    #     while (ct < self.endline ):
    #         ct+=1
    #         line = lines[ct]
    #         #get rid of comments
    #         line = line.strip().split('!')[0]
    #
    #         #take into account line continuations
    #         line = line.rstrip('\n')
    #         while line.endswith('&'):
    #             ct+=1
    #             line = line[:-1] + lines[ct].strip().rstrip('\n')
    #
    #         match_auto_arr = re.search(r'^(real|integer|logical)',line)
    #
    #         if(match_auto_arr and "bounds" in line and "intent" not in line):
    #             l    = line.split('::')[1].strip()
    #             type = line.split('::')[0].strip()
    #             if('begp' in l or 'endp' in l): dim = 'p'
    #             if('begc' in l or 'endc' in l): dim = 'c'
    #             if('begt' in l or 'endt' in l): dim = 't'
    #             if('begg' in l or 'endg' in l): dim = 'g'
    #             var = l.split('(')[0].strip()
    #
    #             if var not in self.local_vars:
    #                 print(type,var,dim)
    #                 self.local_vars.append([type,var,dim])
    #
    #     if(not self.local_vars):
    #         print("No local auto arrays found")
    #         for child_Sub in self.child_Subroutine.values():
    #             child_Sub.analyze_local_arrays()
    #         return
    #     file.close()
    #     #go back through file and identify where each auto array is used
    #     outname = self.filepath+".bak"
    #
    #     for el in self.local_vars:
    #         file = open(source_dir+self.filepath,'r')
    #         lines = file.readlines()
    #         ofile = open(f"{outname}",'w')
    #         type, var, dim = el
    #         #print("searching for var:",type,var,dim)
    #         ct = 0
    #         for line in lines:
    #             ct += 1
    #             if (ct > self.startline and ct < self.endline):
    #                 declaration = re.compile(f'^\s*(real|integer|logical)\s*::')
    #                 match_var = re.compile(f'{var}')
    #                 m_var = match_var.search(line)
    #                 m_dec = declaration.match(line)
    #                 repl = re.compile(f"{var}\([^\)]*\)")
    #                 if(m_dec and m_var): line = repl.sub(f'{var}',line)
    #                 if(m_var and not m_dec): line = repl.sub(f'{var}',line)
    #             ofile.write(line)
    #         ofile.close()
    #         file.close()
    #         #os.system('mv '+outname+' '+self.filepath)
    #
    #     for child_Sub in self.child_Subroutine.values():
    #         child_Sub.analyze_local_arrays()
