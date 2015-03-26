""" Parse fortran 90 code and return a list of modules
"""
from __future__ import print_function
import re
from .ioparams import Variable, Group, Module

# Regular expression to find modules and type:
# ...first form: use with findall to get a tuple of all modules
# ...template form: format to find a specific module or group by name
# (the template form is the most useful one here)
TYPE_RE = r'type( +| *:: *)(?P<name>) *\n(?P<defs>.*?)\n *end *type'
TYPE_RE_TEMPLATE = r'type( +| *:: *){name} *\n(?P<defs>.*?)\n *end *type'

def findall_modules(source):
    " return a list of tuples (name, defs, body) "
    module_re = r' *module +(?P<name>\w+) *\n(?P<defs>.*?)contains *\n(?P<body>.*?)end *module'
    return re.findall(module_re, source, re.DOTALL)

def search_module(name, source):
    " return a dict with keys 'defs' and 'body'"
    module_re_template = r' *module +{name} *\n(?P<defs>.*?)contains *\n(?P<body>.*?)end *module'
    m = re.search(module_re_template.format(name=name), source, re.DOTALL)
    if m is None:
        raise ValueError("Module "+name+" was not found")
    return m.groupdict()

def search_type(name, source):
    """ search for a specific type definitions 
    return type body (as raw text)
    """
    m = re.search(TYPE_RE_TEMPLATE.format(name=name), source, re.DOTALL)
    if m is None:
        raise ValueError("Type "+name+" was not found")
    return m.groupdict()["defs"]

def _clean_code(string):
    lines = []
    for line in string.splitlines():
        if '!' in line:
            line = line[:line.index('!')]
        line = line.strip()
        if line == "":
            continue
        lines.append(line)
    return "\n".join(lines)

def parse_vartype(string):
    """
    >>> parse_vartype("real(dp), dimension(31)")
    ('real', 'dp', 31)
    >>> parse_vartype("real (kind=8)")
    ('real', 'kind=8', None)
    >>> parse_vartype("real")
    ('real', None, None)
    >>> parse_vartype("real, dimension(3)")
    ('real', None, 3)
    """
    # left-hand side of "::"
    type_re = "(?P<dtype>\w+)(\((?P<attrs>.*?)\))*(,dimension\((?P<size>.+?)\))*"
    m = re.search(type_re, string.replace(" ",""))
    if m is None:
        raise ValueError("Failed to parse var type : "+string)
    d = m.groupdict()
    if d["size"] is None:
        size = None
    else:
        size = int(d["size"])
    return d["dtype"], d["attrs"], size

def parse_varnames(string):
    """ right-hand size of "::" 
    returns a list of dict with keys "name", "value", "size"
    >>> parse_varname("a(36)=45, b")
    [{'name': 'a', 'value': '45', 'size': '36'}, {'name': 'b', 'value': None, 'size': None}]
    """
    name_re = "(?P<name>\w+)(\((?P<size>\d+)\))*(=(?P<value>\w+))*(!(?P<help>))*"
    variables = []
    for m in re.finditer(name_re, string.replace(" ","")):
        variables.append(m.groupdict())
    return variables

def parse_line(string):
    """
    Retrieve following:
    type1(...=...), dimension(N) :: var1 = val1, var2 = val2 ! comment
    """
    # comments
    if "!" in string:
        i = string.index("!")
        comment = string[i+1:]
        string = string[:i]
    else:
        comment = None

    # type :: variables
    try:
        typedef, namesdef = string.split("::")
    except:
        print(string)
        raise ValueError("Error when parsing source code. `::` missing in variable definition")

    # Un-build fortran type
    dtype, attrs, size = parse_vartype(typedef)

    # var1 = val1
    variables = []
    for v in parse_varnames(namesdef):
        vsize = size or v['size'] 
        var = Variable(name=v['name'], value=v['value'], dtype=dtype, attrs=attrs, size=vsize)
        variables.append(var) 
    return variables

def parse_type(string, type_name, group_name=None, mod_name=None):
    """ Define a Group object from source code

    Parameters
    ----------
    type_name : type name to search
    mod_name : optional
        module name where the type belongs
        (will be searched in all modules if not provided)
    group_name : optional
        'name' attribute of the returned Group
        (can be set later)

    string : code to parse

    Returns
    -------
    Group object
    """
    if mod_name is None:
        # search type in all modules
        type_content = None
        searched_modules = []
        for mod_name, defs, body in findall_modules(string):
            try:
                type_content = search_type(type_name, defs)
                break
            except:
                pass # not found
            searched_modules.append(mod_name)
        if type_content is None:
            raise ValueError("Type "+type_name+" not found in modules "+", ".join(searched_modules))
        print("type",type_name,"found in module",mod_name)

    else:
        # search for specific type
        d = search_module(mod_name, string)
        type_content = search_type(type_name, d["defs"])

    # Create Group instance
    group = Group(name=group_name, mod_name=mod_name, type_name=type_name)

    # Fill up the variables
    for line in type_content.splitlines():
        if line.strip() == "": continue
        if line.strip().startswith("!"): continue
        for v in parse_line(line):
            v.group = group_name
            group.append_variable(v)

    return group

def main():
    import argparse
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("sourcefile")
    args = parser.parse_args()

    with open(args.sourcefile) as f:
        code = f.read()

    group1 = parse_type(code, group_name="group1", type_name="group1_t")
    group2 = parse_type(code, group_name="group2", type_name="group2_t")

    print(group1.format())
    print(group2.format())

if __name__ == "__main__":
    main()
