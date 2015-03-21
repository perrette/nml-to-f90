""" Utilities to read namelist file

Originally adapted from 
https://github.com/leifdenby/namelist_python
"""
from collections import OrderedDict
import re

def read_namelist_file(filename):
    return Namelist(open(filename, 'r').read())

class AttributeMapper():
    """
    Simple mapper to access dictionary items as attributes
    """

    def __init__(self, obj):
        self.__dict__['data'] = obj

    def __getattr__(self, attr):
        if attr in self.data:
            found_attr = self.data[attr]
            if isinstance(found_attr, dict):
                return AttributeMapper(found_attr)
            else:
                return found_attr
        else:
            raise AttributeError

    def __setattr__(self, attr, value):
        if attr in self.data:
            self.data[attr] = value
        else:
            raise NotImplementedError

    def __dir__(self):
        return self.data.keys()

class Namelist():
    """
    Parses namelist files in Fortran 90 format, recognised groups are
    available through 'groups' attribute.
    """

    def __init__(self, input_str=None):
        self.groups = OrderedDict()
        if input_str is None:
            return 

        group_re = re.compile(r'&([^&]+)/', re.DOTALL)  # allow blocks to span multiple lines
        array_re = re.compile(r'(\w+)\((\d+)\)')
        # string_re = re.compile(r"\'\s*\w[^']*\'")
        string_re = re.compile(r"[\'\"]*[\'\"]")
        # self._complex_re = re.compile(r'^\((\d+.?\d*),(\d+.?\d*)\)$')

        # remove all comments, since they may have forward-slashes
        # TODO: store position of comments so that they can be re-inserted when
        # we eventually save
        filtered_lines = []
        for line in input_str.split('\n'):
            if '!' in line:
                line = line[:line.index('!')]
            if line.strip() == "":
                continue
            filtered_lines.append(line)

        group_blocks = re.findall(group_re, "\n".join(filtered_lines))

        for i, group_block in enumerate(group_blocks):
            block_lines = group_block.split('\n')
            group_name = block_lines.pop(0).strip()

            # some lines are continuation of previous lines: filter
            clean_lines = []
            for line in block_lines:
                line = line.strip()
                if line == "":
                    continue
                if line.startswith('!'):
                    continue
                if '=' in line:
                    clean_lines.append(line)
                else:
                    # continuation of previous line
                    clean_lines[-1] += line

            group = OrderedDict()

            for line in clean_lines:

                # commas at the end of lines seem to be optional
                if line.endswith(','):
                    line = line[:-1]

                k, v = line.split('=')
                variable_name = k.strip()
                variable_value = v.strip()

                parsed_value = _parse_value(variable_value)
                group[variable_name] = parsed_value

            self.groups[group_name] = group

    def dump(self, array_inline=True):
        lines = []
        for group_name, group_variables in self.groups.items():
            lines.append("&%s" % group_name)
            for variable_name, variable_value in group_variables.items():
                if isinstance(variable_value, list):
                    if array_inline:
                        lines.append("  %s = %s" % (variable_name, " ".join([_format_value(v) for v in variable_value])))
                    else:
                        for n, v in enumerate(variable_value):
                            lines.append("  %s(%d) = %s" % (variable_name, n+1, _format_value(v)))
                else:
                    lines.append("  %s = %s" % (variable_name, _format_value(variable_value)))
            lines.append("/\n")

        return "\n".join(lines)

    @property
    def data(self):
        return AttributeMapper(self.groups)

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
                print "Parsing ERROR: >>>{}<<<".format(variable_value)
                raise ValueError(variable_value)
    return parsed_value

def _parse_array(values):
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

def _format_value(value):
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

