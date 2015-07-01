


def subsets(s):
    if not s:
        return [[]]
    else:
        rest = subsets(s[1:])
        return rest + map(lambda x : [s[0]] + x, rest)