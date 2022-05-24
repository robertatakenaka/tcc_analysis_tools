
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
labels\tC
Total de artigos registrados\t9468
Artigos sem resumo em inglês\t5943 (62.77%)
Artigos com resumo somente em inglês\t3525 (37.23%)
Artigos com resumos em inglês + outros idiomas\t  0 (0.00%)
Total de referências bibliográficas\t306079
Total de fontes criadas a partir de referências bibliográficas\t64295
Total de conexões dos artigos (feitas pelas referências bibliográficas)\t114843
Artigos sem recomendações\t162 (1.71%)
Artigos vinculados pelas referências bibliográficas\t9306 (98.29%)
Artigos cuja pontuação mais alta está entre 1-59\t151 (1.62%)
Artigos cuja pontuação mais alta está entre 60-69\t406 (4.36%)
Artigos cuja pontuação mais alta está entre 70-79\t1585 (17.03%)
Artigos cuja pontuação mais alta está entre 80-89\t3388 (36.41%)
Artigos cuja pontuação mais alta está entre 90-100\t3776 (40.58%)
NA\t162 (1.74%)
Referências bibliográficas com DOI\t3987 (1.30%)
Fontes citadas por mais de 1 artigo\t61030 (94.92%)
Fontes citadas por um artigo\t3265 (5.08%)
Fontes sem DOI citadas > 1 vez\t60187 (93.61%)
Fontes sem DOI citadas uma vez\t3234 (5.03%)
Fontes com DOI citadas > 1 vez\t843 (1.31%)
Fontes com DOI citadas uma vez\t 31 (0.05%)
Conexões pelas referências bibliográficas, mas cortados na seleção de candidatos\t35558 (30.96%)
Conexões candidatas à similaridade\t79285 (69.04%)
Conexões por similaridade semântica >= 70%\t22402 (28.26%)
Conexões por similaridade semântica >= 90%\t8941 (11.28%)
Conexões por similaridade semântica 80-89%\t8286 (10.45%)
Conexões por similaridade semântica 70-79%\t5175 (6.53%)
Conexões por similaridade semântica 60-69%\t12573 (15.86%)
Conexões por similaridade semântica 1-59%\t44308 (55.88%)
Conexões por similaridade semântica < 1%\t  2 (0.00%)
"""

print(format_numbers(text))
        