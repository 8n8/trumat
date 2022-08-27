
# Types
STRING_LITERAL_TYPE = 0


class InvalidElm(ValueError):
    pass


def get_new_string_id(ast):
    if ast['positioned_id'].buffer_info()[1] == 0:
        return 0

    max_id = 0
    for i, positioned_id in enumerate(ast['positioned_id']):
        if ast['positioned_type'][i] == STRING_LITERAL_TYPE:
            max_id = positioned_id

    return max_id


def parse_triple_quoted_string(src, i, module_id, ast):
    try:
        if src[i] == ord('"') and src[i+1] == ord('"') and src[i+2] == ord('"'):
            pass

        else:
            return i, None
    except IndexError:
        return i, None
    
    i += 3
    ast['string_start'].append(i)

    try:
        while True:
            if src[i] == ord('\\'):
                i += 2
                continue

            if src[i] == ord('"') and src[i+1] == ord('"') and src[i+2] == ord('"'):
                ast['string_end'].append(i)
                return i+3, None

            i += 1

    except IndexError:
        pass
            
    raise InvalidElm("I was parsing a triple-quoted string, but got the end of the input without finding the closing quote marks.")


def parse_ordinary_string(src, i, ast):
    try:
        if src[i] != ord('"'):
            return i
    except IndexError:
        return i

    i += 1
    ast['string_start'].append(i)

    try:
        while True:
            if src[i] == ord('\\'):
                i += 2
                continue

            if src[i] == ord('"'):
                ast['string_end'].append(i)
                return i+1

            i += 1
    except IndexError:
        pass

    raise InvalidElm("I was parsing an ordinary string, but got the end of the input without finding the closing quote mark.")
