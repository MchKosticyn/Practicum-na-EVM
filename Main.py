import sys
from difflib import SequenceMatcher

""" List[list] -> list[otrezok] """
str1 = "alpha bravo charlie delta echofoxtrot".split()  # подрярд, перестановка
str2 = "alpha bravo delta foxtrot golf india kilo".split()
str3 = "bravo delta india oscar uniform".split()
# str4 = "bravo delta india oscar uniform".split()
""" Algorithm """


def simplify(a, b, sz, str):
    offset = len(str.lstrip(" "))
    b += sz - offset
    a += sz - offset
    sz = offset + len(str.rstrip(" ")) - sz
    return (a, b, sz)


def BiDiff(text1, text2):
    text1 = " ".join(text1)
    text2 = " ".join(text2)
    match = SequenceMatcher(lambda x: x == " ", text1, text2)
    list2 = []
    for (fst, snd, lsize) in match.get_matching_blocks():
        line = text1[fst: fst + lsize]
        (fst, snd, lsize) = simplify(fst, snd, lsize, line)
        words = line.split()
        indx = 0
        for word in words:
            a = [word, len(word)]
            render1 = text1[0: fst + indx + 1].split()
            render2 = text2[0: snd + indx + 1].split()
            a.append([(len(render1) - 1, len(render1[-1]) - 1),  # Что это?
                      (len(render2) - 1, len(render2[-1]) - 1)])
            list2.append(a)
            indx += len(word) + 1
    return list2


def modifyRes(res, newdiff):
    for newWord in newdiff:
        for oldWord in res:
            if newWord[0] in oldWord[0]:
                offsets = [(x, y + newWord[2][1][1]) for (x, y) in oldWord[2]]
                yield [newWord[0], newWord[1], offsets]
                break


def Diff(documents):
    result = BiDiff(documents[0], documents[1])
    #print(result)
    documents[1] = map(lambda x: x[0], result)
    for i in range(1, len(documents) - 1):
        curdiff = BiDiff(documents[i], documents[i + 1])
        result = list(modifyRes(result, curdiff))  # нужен "list"?
        for (ii, item) in enumerate(result):
            item[2].append(curdiff[ii][2][1])
        documents[i + 1] = map(lambda x: x[0], result)
        #print(result)
    return result


document = [str1, str2, str3]
print()
print()
print(Diff(document))
print()
