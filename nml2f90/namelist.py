""" Utilities to read namelist file

Originally adapted from 
https://github.com/leifdenby/namelist_python
"""
from __future__ import print_function
from collections import OrderedDict as odict
import re
import warnings
from itertools import groupby

class Param(object):
    def __init__(self, name="", value=None, group="", help="", units="", **kwargs):
        self.name = name
        self.value = value
        self.group = group
        self.help = help  # .e.g. "blabla ({units})"
        self.units = units
        if (len(kwargs) > 0):
            warnings.warn("unknown parameters to Param were ignored: "+", ".join(kwargs.keys()))
        # self.__dict__.update(kwargs)

    @property
    def key(self):
        " unique ID "
        return (self.group, self.name)

    def __eq__(self, other):
        return self.key == other.key

    def __repr__(self):
        return "Param(name=%r, value=%r, group=%r)" % (self.name, self.value, self.group)

class Params(list):
    """ list of parameters
    """
    def __init__(self, *args):
        # make sure we have a list
        list.__init__(self, *args)
        for p in self:
            if not isinstance(p, Param):
                print(type(p),":",p)
                raise TypeError("Params can only contain Param instances")

    def append(self, param):
        if not isinstance(param, Param):
            raise TypeError("Params can only contain Param instances")
        list.append(self, param)

    def to_nml(self):
        return Namelist(self)

    def to_json(self, **kwargs):
        import json
        return json.dumps([vars(p) for p in self], **kwargs)

    @classmethod
    def from_json(cls, string):
        import json
        return cls([Param(**p) for p in json.loads(string)])

    # generic method to be overloaded, default to json
    parse = from_json
    format = to_json

    def write(self, filename, mode='w', **kwargs):
        with open(filename, mode) as f:
            f.write(self.format(**kwargs))

    @classmethod
    def read(cls, filename):
        with open(filename) as f:
            params = cls.parse(f.read())
        return params

    def __repr__(self):
        return "{cls}({list})".format(cls=self.__class__.__name__, list=list.__repr__(self))

class Namelist(Params):
    """ Parse / Format method specific to Namelist
    """
    @classmethod
    def parse(cls, string):
        try:
            params = _parse_nml(string)
        except:
            warnings.warn("some characters in the comments (likely / or \) namelist parsing, discard all comments")
            params = _parse_nml(string)
        return cls(params)

    def format(self):
        return _format_nml(self)

#
# Work for Namelist parsing and conversion
#
def _parse_nml(string, ignore_comments=False):
    """ parse a string namelist, and returns a list of params
    with four keys: name, value, help, group
    """
    group_re = re.compile(r'&([^&]+)/', re.DOTALL)  # allow blocks to span multiple lines
    array_re = re.compile(r'(\w+)\((\d+)\)')
    # string_re = re.compile(r"\'\s*\w[^']*\'")
    string_re = re.compile(r"[\'\"]*[\'\"]")
    # self._complex_re = re.compile(r'^\((\d+.?\d*),(\d+.?\d*)\)$')

    # list of parameters
    params = Params()
    # groups = odict()

    filtered_lines = []
    for line in string.split('\n'):
        line = line.strip()
        if line == "":
            continue
        # remove comments, since they may have forward-slashes
        # set ignore_comments to True is you want to keep them.
        if line.startswith('!'):
            continue  
        if ignore_comments and '!' in line:
            line = line[:line.index('!')]

        filtered_lines.append(line)

    group_blocks = re.findall(group_re, "\n".join(filtered_lines))

    for i, group_block in enumerate(group_blocks):
        group_lines = group_block.split('\n')
        group_name = group_lines.pop(0).strip()
        # check for comments
        if "!" in group_name:
            i = group_name.index("!")
            group_name = group_name[:i].strip()
            group_help = group_name[i+1:].strip()

        # some lines are continuation of previous lines: filter
        joined_lines = []
        for line in group_lines:
            line = line.strip()
            if '=' in line:
                joined_lines.append(line)
            else:
                # continuation of previous line
                joined_lines[-1] += line
        group_lines = joined_lines

        for line in group_lines:
            name, value, comment = _parse_line(line)

            param = {
                "name": name,
                "value": value,
                "help": comment,
                "group": group_name,
            }
            param = Param(**param)
            # group[variable_name] = parsed_value
            params.append(param)

        # groups[group_name] = group
    return params

def _parse_line(line):
    "parse a line within a block"
    # commas at the end of lines seem to be optional
    comment = ""
    if '!' in line:
        sep = line.index("!")
        comment = line[sep+1:].strip()
        line = line[:sep].strip()

    if line.endswith(','):
        line = line[:-1]

    k, v = line.split('=')
    name = k.strip()
    value = _parse_value(v.strip())
    return name, value, comment

def _parse_value(variable_value):
    """
    Tries to parse a single value, raises an exception if no single value is matched
    """
    try:
        parsed_value = int(variable_value)
    except ValueError:
        try:
            parsed_value = float(variable_value)
        except ValueError:
            if variable_value.lower() in ['.true.', 't', 'true']:
                # boolean
                parsed_value = True
            elif variable_value.lower() in ['.false.', 'f', 'false']:
                parsed_value = False
            elif variable_value.startswith("'") \
                and variable_value.endswith("'") \
                and variable_value.count("'") == 2 \
            or variable_value.startswith('"') \
                and variable_value.endswith('"') \
                and variable_value.count('"') == 2:
                parsed_value = variable_value[1:-1]
            elif variable_value.startswith("/") and variable_value.endswith("/"):
                # array /3,4,5/
                parsed_value = _parse_array(variable_value[1:-1].split(','))
            elif "," in variable_value:
                # array 3, 4, 5
                parsed_value = _parse_array(variable_value.split(','))
            elif '*' in variable_value:
                # 3*4  means [4, 4, 4, 4] ==> this is handled in _parse_array
                parsed_value = _parse_array([variable_value])
            elif len(variable_value.split()) > 1:
                # array 3 4 5
                parsed_value = _parse_array(variable_value.split())
            else:
                print("Parsing ERROR: >>>{}<<<".format(variable_value))
                raise ValueError(variable_value)
    return parsed_value

def _parse_array(values):
    """ parse a list of (string) values representing a fortran array
    and return a python list
    """
    assert type(values) is list
    parsed_value = []
    for v in values:
        if '*' in v:
            # 3* "a" === "a", "a", "a"
            mult, val = v.split('*')
            parsed_value.extend(int(mult) * [ _parse_value(val.strip())  ])
        else:
            parsed_value.append(_parse_value(v))
    return parsed_value

def _format_nml(params):
    """ format a flat parameter list to be written in the namelist
    """
    lines = []
    for group_name, group_params in groupby(params, lambda x: x.group):
        if group_name == "":
            print(list(group_params))
            raise ValueError("Group not defined. Cannot write to namelist.")
        lines.append("&%s" % group_name)
        for param in group_params:
            if isinstance(param.value, list):
                line = "  %s = %s" % (param.name, " ".join([_format_value(v) for v in param.value]))
            else:
                line = "  %s = %s" % (param.name, _format_value(param.value))
            if param.help:
                line += '! '+param.help
            lines.append(line)
        lines.append("/\n")
    return "\n".join(lines)

def _format_value(value):
    """ Format a value into fortran's namelist format (return a string)
    """
    if isinstance(value, bool):
        return value and '.true.' or '.false.'
    elif isinstance(value, int):
        return "%d" % value
    elif isinstance(value, float):
        # return "{:.3e}".format(value) # use exp. notation after 3 digits
        return "{}".format(value) # use exp. notation after 3 digits
    elif isinstance(value, basestring):
        return "'%s'" % value
    elif isinstance(value, complex):
        return "(%s,%s)" % (_format_value(value.real), _format_value(value.imag))
    else:
        raise Exception("Variable type not understood: %s" % type(value))
