PARSED =
    { 'chars': [0] * BIG,
      'num_chars': 0,
      'verbatim_ends': [0] * SMALL,
      'num_verbatim_ends': 0,
      'binds': [],
      'num_binds': 0
    }

def parse(raw_bytes):
    

def format(unformatted):
    raw_bytes = unformatted.encode("utf8")
    parsed = parse(raw_bytes)
    return to_string(parsed)
