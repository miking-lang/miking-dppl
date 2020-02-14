import numpy as np
import pandas as pd
import gensim
import nltk
from gensim.utils import simple_preprocess
from gensim.parsing.preprocessing import STOPWORDS
from nltk.stem import WordNetLemmatizer, SnowballStemmer
from nltk.stem.porter import *

stemmer = SnowballStemmer('english')
nltk.download('wordnet')

documents = pd.read_csv('papers2017.csv', error_bad_lines=False)


# Pre-process the data

# lemmatizing and stemming
def lem_stem(text):
    return stemmer.stem(WordNetLemmatizer().lemmatize(text, pos='v'))


def preprocess(text):
    result = []
    for token in gensim.utils.simple_preprocess(text):
        if token not in STOPWORDS and len(token) >= 3:
            result.append(lem_stem(token))
    return result


def writeToFile(docs):
    f = open("processedDocuments.txt", "w+")
    for doc in docs:
        strTowrite = ""
        for word in doc:
            strTowrite += str(word) + ","
        strTowrite = strTowrite[:-1]
        f.write(strTowrite + "\n")


# Process all of the abstracts
processed_docs = documents['abstract'].map(preprocess)

# Create a dictionary
dictionary = gensim.corpora.Dictionary(processed_docs)
dictionary.filter_extremes(no_below=10, no_above=0.5)
bow_corpus = [dictionary.doc2bow(doc) for doc in processed_docs]
print(bow_corpus)
print(dictionary)
print(len(dictionary))
print(len(processed_docs))
print(len(processed_docs[0]))
print(len(processed_docs[1]))
print(processed_docs[0])
lengths = [len(doc) for doc in processed_docs]
print(lengths)
idxDocs = [dictionary.doc2idx(doc) for doc in processed_docs]
maxLength = np.max([len(doc) for doc in idxDocs])
avgLen = np.mean([len(doc) for doc in idxDocs])

for doc in idxDocs:
    numToFill = maxLength - len(doc)
    for i in range(numToFill):
        doc.append(-1)

minLength = np.min([len(doc) for doc in idxDocs])
print("maxL:", maxLength)
print("minL:", minLength)
print("idxDocs", len(idxDocs[92]))
print(idxDocs)
'''
idxsByTopic = [
    [698, 5, 106, 370, 397, 606, 375, 117, 738, 447],
    [854, 223, 403, 429, 204, 354, 183, 442, 217, 641],
    [761, 840, 310, 334, 109, 521, 155, 433, 119, 532],
    [259, 537, 513, 487, 467, 378, 405, 77, 246, 586],
    [440, 679, 245, 395, 59, 230, 157, 471, 570, 543],
    [198, 854, 715, 757, 75, 462, 351, 64, 273, 452],
    [839, 479, 526, 596, 594, 533, 816, 412, 203, 532],
    [355, 714, 554, 302, 408, 536, 732, 849, 376, 228],
    [385, 732, 469, 759, 10, 810, 77, 800, 627, 447],
    [174, 21, 785, 111, 318, 516, 461, 555, 171, 665]
]
'''
idxsByTopic = [
    [570, 255, 157, 650, 751, 211, 617, 732, 552, 829],
    [85, 655, 776, 642, 698, 334, 785, 54, 71, 86],
    [446, 844, 180, 489, 513, 837, 258, 263, 449, 420],
    [737, 412, 783, 627, 213, 223, 465, 767, 281, 658],
    [349, 603, 573, 830, 111, 200, 277, 576, 237, 72],
    [318, 386, 660, 450, 427, 13, 224, 317, 63, 264],
    [98, 469, 424, 855, 393, 507, 584, 477, 824, 146],
    [345, 608, 444, 661, 518, 458, 651, 606, 78, 462],
    [565, 412, 275, 416, 164, 540, 351, 131, 544, 788],
    [363, 259, 352, 533, 279, 101, 383, 790, 470, 643]
]

for k, idxs in enumerate(idxsByTopic):
    wordsTopic = [dictionary[i] for i in idxs]
    print("Topic[", k, "]:", wordsTopic[0:5])
# wordsTopic1 = [dictionary[i] for i in idxTopic1]
# wordsTopic2 = [dictionary[i] for i in idxTopic2]
# print("Topic 1:", wordsTopic1)
# print("Topic 2:", wordsTopic2)
# writeToFile(processed_docs)
