
def insert_dot_for_millards(data):
    parts = []
    while True:
        if not data:
            break
        parts.append(data[-3:])
        data = data[:-3]
        print(data)
        print(parts)
    return ".".join(parts[::-1])


def format_number(data):
    if not data.isdigit():
        if "%" in data:
            if not "(" in data:
                return "(" + data.replace(".", ",") + ")"
            return data.replace(".", ",")
        print(f"[{data}]")
        return data
    return insert_dot_for_millards(data)


def format_numbers(text):
    words = set(text.split())
    d = {
        w: format_number(w)
        for w in words
    }
    d = [(k, v) for k, v in d.items() if k != v]
    d = sorted(d, reverse=True, key=lambda x: len(x[0]))
    for a, b in d:
        text = text.replace(a, b)
    return text


text = """
Fontes sem DOI com citação única\t90203\t73076\t3234
\t96,99%\t77,65%\t5,03%
Fontes com DOI com citação única\t656\t11425\t31
\t0,71%\t12,14%\t0,05%
Fontes sem DOI com citações múltiplas\t2132\t6532\t60187
\t2,29%\t6,94%\t93,61%
Fontes com DOI com citações múltiplas\t7\t3072\t843
\t0,01%\t3,26%\t1,31%
Total de fontes\t92998\t94105\t64295"""

print(format_numbers(text))
        