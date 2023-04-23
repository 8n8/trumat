import re

top_level_bind_unformatted = fr'(\n[a-z] =\n) +{expression_unformatted}'

top_level_bind_formatted = r'\1    \2'

def format(unformatted):
    return re.sub(
        top_level_bind_unformatted,
        top_level_bind_formatted,
        unformatted)
