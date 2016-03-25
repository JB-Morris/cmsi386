# Do not import anything other than sys and re!
import sys
import re

# this function removes punctuation from a string.
# you should use this before adding a word to the index,
# so that "asdf." and "asdf" and "asdf," are considered to be
# the same word.

def remove_punctuation(s):
  return re.sub(r'[^\w\s]', '', s)

assert(remove_punctuation('asdf.') == 'asdf')
assert(remove_punctuation('asdf,') == 'asdf')
assert(remove_punctuation("?;'.!") == '')

# index is a global dictionary. The keys should be words, and the
# values should be tuples of (filename, line number, position in line).

index = {}

def build_index(args):
  global index
  for f in args:
    filename = f
    line_number = 1
    position_in_line = 0
    text = open(f, 'r')
    for line in text:
      line = line[:-1]
      line = remove_punctuation(line)
      line = line.lower()
      line = line.split(' ')
      for word in line:
        index.setdefault(word, [])
        index[word].append([filename, line_number, position_in_line])
        position_in_line += len(word) + 1
      line_number += 1
      position_in_line = 0
    print(index)

build_index(['lorem.txt', 'ipsum.txt'])


# commands

def words(args):
  start = args[0]
  start.lower()
  keys = index.keys()
  keys = list(keys)
  keys.sort()
  for key in keys:
    if key.startswith(start):
      print(key)

def occurrences(args):
  occurrence = 0
  for i in index[args[0]]:
    print('(' , occurrence , ')' , 'File' , i[0] , 'Line' , i[1] , 'Character' , i[2])
    occurrence += 1
    
def context(args):
  key = args[0]
  occurrence = int(args[1])
  line_number = index[key][occurrence][1]
  filename = index[key][occurrence][0]
  text = open(filename, 'r')
  counter = 1
  for line in text:
    if(counter == line_number):
      print(line)
      print(str(' ' * (index[key][occurrence][2] + 1)) + str('^' * len(key)))
      return
    counter += 1

def output(args):
  keys = index.keys()
  keys = list(keys)
  keys.sort()
  for key in keys:
    print(key)
    occurrence = 0
    for i in index[key]:
      print('(' , occurrence , ')' , 'File' , i[0] , 'Line' , i[1] , 'Character' , i[2])
      occurrence += 1
    
cmds = {
  'words' : words,
  'occurrences' : occurrences,
  'context' : context,
  'output' : output,
  }

def interact():
  # print the prompt line
  print('> ', end='', flush=True)
  
  for ln in sys.stdin:
    ln = ln.strip().split(' ')
    if ln[0] == 'quit':
      return
    else:
      cmds[ln[0]](ln[1:])

    # print the next prompt line
    print('> ', end='', flush=True)

interact()
