import re

def load(filename, last=''):
    vectors = []

    with open(filename, 'r') as f:
        vector = {}

        for line in f:
            m = re.match('^(\w+)\s*\=\s*(.*)$', line)
            if m:
                vector[m.group(1)] = m.group(2).strip()

                if m.group(1) == last:
                    vectors.append(vector)
                    vector = {}
            else:
                if len(vector) > 0:
                    vectors.append(vector)
                    vector = {}

        if len(vector) > 0:
            vectors.append(vector)

    return vectors