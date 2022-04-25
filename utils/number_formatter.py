
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
Total de artigos registrados\t4908\t4170\t9467
Artigos sem resumo em inglês\t4895 (99.74%)\t 16 (0.38%)\t5947 (62.82%)
Artigos com resumo somente em inglês\t  0 (0.00%)\t735 (17.63%)\t3520 (37.18%)
Artigos com resumos em inglês + outros idiomas\t 13 (0.26%)\t3419 (81.99%)\t  0 (0.00%)
Total de referências bibliográficas\t110028\t140052\t307008
Total de fontes criadas a partir de referências bibliográficas\t92998\t94105\t105932
Total de conexões dos artigos (feitas pelas referências bibliográficas)\t2660\t126850\t80239
Artigos sem recomendações\t3530 (71.92%)\t740 (17.75%)\t160 (1.69%)
Artigos vinculados pelas referências bibliográficas\t1378 (28.08%)\t3430 (82.25%)\t9307 (98.31%)
Artigos cuja pontuação mais alta está entre 1-59\t639 (46.37%)\t703 (20.50%)\t307 (3.30%)
Artigos cuja pontuação mais alta está entre 60-69\t371 (26.92%)\t1163 (33.91%)\t413 (4.44%)
Artigos cuja pontuação mais alta está entre 70-79\t259 (18.80%)\t1352 (39.42%)\t1586 (17.04%)
Artigos cuja pontuação mais alta está entre 80-89\t 74 (5.37%)\t192 (5.60%)\t3388 (36.40%)
Artigos cuja pontuação mais alta está entre 90-100\t 35 (2.54%)\t 20 (0.58%)\t3613 (38.82%)
NA\t3530 (256.17%)\t740 (21.57%)\t160 (1.72%)
Referências bibliográficas com DOI\t698 (0.63%)\t20400 (14.57%)\t3987 (1.30%)
Fontes citadas por mais de 1 artigo\t2139 (2.30%)\t9604 (10.21%)\t104247 (98.41%)
Fontes citadas por um artigo\t90859 (97.70%)\t84501 (89.79%)\t1685 (1.59%)
Fontes sem DOI citadas uma vez\t90203 (96.99%)\t73076 (77.65%)\t1685 (1.59%)
Fontes sem DOI citadas > 1 vez\t2132 (2.29%)\t6532 (6.94%)\t102850 (97.09%)
Fontes com DOI citadas uma vez\t656 (0.71%)\t11425 (12.14%)\tNA
Fontes com DOI citadas > 1 vez\t  7 (0.01%)\t3072 (3.26%)\t1397 (1.32%)
Conexões pelas referências bibliográficas, mas cortados na seleção de candidatos\t  0 (0.00%)\t86981 (68.57%)\t16903 (21.07%)
Conexões candidatas à similaridade\t2660 (100.00%)\t39869 (31.43%)\t63336 (78.93%)
Conexões por similaridade semântica >= 70%\t484 (18.20%)\t5030 (12.62%)\t20018 (31.61%)
Conexões por similaridade semântica >= 90%\t 38 (1.43%)\t 20 (0.05%)\t8252 (13.03%)
Conexões por similaridade semântica 80-89%\t103 (3.87%)\t264 (0.66%)\t7458 (11.78%)
Conexões por similaridade semântica 70-79%\t343 (12.89%)\t4746 (11.90%)\t4308 (6.80%)
Conexões por similaridade semântica 60-69%\t638 (23.98%)\t14660 (36.77%)\t8719 (13.77%)
Conexões por similaridade semântica 1-59%\t1538 (57.82%)\t20179 (50.61%)\t34599 (54.63%)
Conexões por similaridade semântica < 1%\t  0 (0.00%)\t  0 (0.00%)\t  0 (0.00%)





"""

print(format_numbers(text))
        