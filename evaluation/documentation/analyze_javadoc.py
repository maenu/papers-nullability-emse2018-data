import sys
import os
import subprocess
import re
import traceback
import csv
import random
import requests
import pprint
from bs4 import BeautifulSoup, Comment

if len(sys.argv) != 4:
    print 'requires doc root path, input and output csv argument'
    exit(1)

DOC = sys.argv[1]
IN = sys.argv[2]
OUT = sys.argv[3]
BYTECODE_PRIMITIVE_TYPES = {
    'B': 'byte',
    'C': 'char',
    'D': 'double',
    'F': 'float',
    'I': 'int',
    'J': 'long',
    'S': 'short',
    'Z': 'boolean',
    'V': 'void',
}

class JavadocParser(object):
    
    def index(self, soup):
        return {
            'fields': self.index_fields(soup),
            'constructors': self.index_constructors(soup),
            'methods': self.index_methods(soup)
        }
    
    def index_fields(self, soup):
        return {}
    
    def index_constructors(self, soup):
        return {}
    
    def index_methods(self, soup):
        return {}

class JavadocParser5(JavadocParser):
    
    field_detail_name = 'field_detail'
    method_detail_name = 'method_detail'
    constructor_detail_name = 'constructor_detail'
    
    def normalize_method_key(self, method_key):
        return method_key.replace(' ', '')
    
    def index_fields(self, soup):
        detail = soup.find('a', {
            'name': self.field_detail_name
        })
        if detail is None:
            return {}
        index = {}
        anchors = detail.find_next_siblings('a')
        for anchor in anchors:
            if isinstance(anchor.previous_element, Comment):
                break
            field_key = anchor['name']
            details = anchor.find_next_sibling('dl')
            description = ''
            if details:
                description = details.get_text()
            index[field_key] = {
                'description': re.sub(r'\s+', ' ', description).strip()
            }
        return index
    
    def index_methods(self, soup):
        detail = soup.find('a', {
            'name': self.method_detail_name
        })
        if detail is None:
            return {}
        return self.index_methods_details_(soup, detail)
    
    def index_constructors(self, soup):
        detail = soup.find('a', {
            'name': self.constructor_detail_name
        })
        if detail is None:
            return {}
        return self.index_methods_details_(soup, detail)
    
    def index_methods_details_(self, soup, detail):
        index = {}
        anchors = detail.find_next_siblings('a')
        for anchor in anchors:
            if isinstance(anchor.previous_element, Comment):
                break
            method_key = self.normalize_method_key(anchor['name'])
            details = anchor.find_next_sibling('dl')
            dds = details.find_all('dd', recursive=False)
            description = ''
            if len(dds) > 0:
                description = dds[0].get_text()
            labels = details.select('dl > dt > b')
            parameter_labels = filter(lambda x: x.get_text().strip() == 'Parameters:', labels)
            parameters = []
            if parameter_labels:
                parameter_current = parameter_labels[0].parent
                while parameter_current.next_sibling and parameter_current.next_sibling.name == 'dd':
                    parameter_current = parameter_current.next_sibling
                    parameters.append(parameter_current.get_text())
            return_labels = filter(lambda x: x.get_text().strip() == 'Returns:', labels)
            return_text = ''
            if return_labels:
                return_text = return_labels[0].parent.find_next_sibling('dd').get_text()
            index[method_key] = {
                'description': re.sub(r'\s+', ' ', description).strip(),
                'parameters': [re.sub(r'\s+', ' ', parameter).strip() for parameter in parameters],
                'return': re.sub(r'\s+', ' ', return_text).strip()
            }
        return index

class JavadocParser6(JavadocParser5):
    pass

class JavadocParser7(JavadocParser6):
    
    def index_fields(self, soup):
        detail = soup.find('a', {
            'name': self.field_detail_name
        })
        if detail is None:
            return {}
        index = {}
        anchors = detail.find_next_siblings('a')
        for anchor in anchors:
            field_key = anchor['name']
            details = anchor.find_next_sibling('ul')
            descriptions = details.li.find_all('div', recursive=False)
            description = ''
            if descriptions:
                description = '\n'.join(map(lambda x: x.get_text(), descriptions))
            index[field_key] = {
                'description': re.sub(r'\s+', ' ', description).strip()
            }
        return index
    
    def index_methods_details_(self, soup, detail):
        index = {}
        anchors = detail.find_next_siblings('a')
        for anchor in anchors:
            method_key = self.normalize_method_key(anchor['name'])
            details = anchor.find_next_sibling('ul')
            descriptions = details.li.find_all('div', recursive=False)
            description = ''
            if descriptions:
                description = '\n'.join(map(lambda x: x.get_text(), descriptions))
            parameters = []
            parameter_label = details.find('span', {
                'class': 'paramLabel'
            })
            if parameter_label:
                parameter_root = parameter_label.parent
                parameter_current = parameter_label.parent.find_next_sibling('dd')
                while parameter_current and parameter_current.find_previous_sibling('dt') is parameter_root:
                    # param description starts with name and type
                    parameter_text = re.sub(r'^\s*\S+\s+\S+', '', parameter_current.get_text())
                    parameters.append(parameter_text)
                    parameter_current = parameter_current.find_next_sibling('dd')
            return_label = details.find('span', {
                'class': 'returnLabel'
            })
            return_text = ''
            if return_label:
                return_text = return_label.parent.find_next_sibling('dd').get_text()
            index[method_key] = {
                'description': re.sub(r'\s+', ' ', description).strip(),
                'parameters': [re.sub(r'\s+', ' ', parameter).strip() for parameter in parameters],
                'return': re.sub(r'\s+', ' ', return_text).strip()
            }
        return index

class JavadocParser8(JavadocParser7):
    
    field_detail_name = 'field.detail'
    method_detail_name = 'method.detail'
    constructor_detail_name = 'constructor.detail'
    
    def normalize_method_key(self, method_key):
        match = re.match(r'^([^\-]+)-(.*)-$', method_key)
        method_name = match.group(1)
        parameter_types = match.group(2).split('-')
        parameter_types = [t.replace(':A', '[]') for t in parameter_types]
        return '{}({})'.format(method_name, ','.join(parameter_types))

def read_javadoc(class_key):
    parts = class_key.split('.')
    parts[-1] = parts[-1].replace('$', '.')
    p = '{}/{}.html'.format(DOC, '/'.join(parts))
    with open(p, 'rb') as f:
        return f.read()

def index_javadoc(javadoc):
    soup = BeautifulSoup(javadoc, 'html.parser')
    comment = soup.head.find_all(string=lambda text:isinstance(text, Comment))[0]
    match = re.search(r'\(.*?\d\.(\d)\..*\)', comment)
    version = 8
    if match:
        version = int(match.group(1))
    parser = JavadocParser8()
    if version == 7:
        parser = JavadocParser7()
    elif version == 6:
        parser = JavadocParser6()
    elif version == 5:
        parser = JavadocParser5()
    return parser.index(soup)

def normalize_bytecode_type(t):
    suffix = ''
    arrays = len(re.search(r'^\[*', t).group(0))
    for i in range(arrays):
        suffix = suffix +'[]'
        t = t[1:]
    if t[0] == 'L':
        t = t[1:-1].replace('/', '.')
    else:
        t = BYTECODE_PRIMITIVE_TYPES[t]
    return t + suffix

def normalize_bytecode_signature(class_name, signature):
    match = re.match(r'^([^\(]+)\((.*)\)', signature)
    name = match.group(1)
    if name == '<init>':
        name = class_name
    types = match.group(2)
    types = re.findall(r'(?:\[)*(?:V|B|C|D|F|I|J|S|Z|L[^;]+;)', types)
    types = [normalize_bytecode_type(t) for t in types]
    return '{}({})'.format(name, ','.join(types))

def print_progress(successes, not_found, errors):
    sys.stdout.write('r: {}|{}|{}\r'.format(successes, not_found, errors))
    sys.stdout.flush()

indexed_classes = {}

successes = 0
not_found = 0
errors = 0
with open(IN, 'rb') as f:
    reader = csv.DictReader(f)
    with open(OUT, 'wb') as g:
        writer = csv.DictWriter(g, fieldnames=reader.fieldnames + ['constructor', 'documentation'])
        writer.writeheader()
        for row in reader:
            class_key = ''
            signature = ''
            try:
                class_key = row['class']
                if class_key in indexed_classes:
                    index = indexed_classes[class_key]
                else:
                    javadoc = read_javadoc(class_key)
                    index = index_javadoc(javadoc)
                    indexed_classes[class_key] = index
                index_type = 'methods'
                row['constructor'] = False
                if row['name'].startswith('<init>'):
                    index_type = 'constructors'
                    row['constructor'] = True
                signature = normalize_bytecode_signature(class_key.split('.')[-1], row['name'])
                type_index = int(row['index'])
                if signature in index[index_type]:
                    documentation = index[index_type][signature]
                    if type_index == -1:
                        if 'return' in documentation and 'null' in documentation['return'].lower():
                            row['documentation'] = 'MENTIONED'
                        else:
                            if 'null' in documentation['description']:
                                row['documentation'] = 'MENTIONED_IN_GENERAL'
                            else:
                                row['documentation'] = 'NOT_MENTIONED'
                    else:
                        if 'parameters' in documentation and type_index in documentation['parameters'] and 'null' in documentation['parameters'][type_index].lower():
                            row['documentation'] = 'MENTIONED'
                        else:
                            if 'null' in documentation['description']:
                                row['documentation'] = 'MENTIONED_IN_GENERAL'
                            else:
                                row['documentation'] = 'NOT_MENTIONED'
                    successes = successes + 1
                else:
                    row['documentation'] = 'METHOD_NOT_FOUND'
                    not_found = not_found + 1
            except IOError as error:
                row['documentation'] = 'CLASS_NOT_FOUND'
                not_found = not_found + 1
            except Exception as exception:
                row['documentation'] = 'ERROR'
                errors = errors + 1
                print >> sys.stderr, 'ERROR', exception, class_key, signature
                traceback.print_exc(file=sys.stderr)
            writer.writerow(row)
            print_progress(successes, not_found, errors)
print ''
