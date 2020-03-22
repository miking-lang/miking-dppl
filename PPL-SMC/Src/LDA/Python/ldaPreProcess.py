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

# documents = pd.read_csv('Python/papers2017.csv', error_bad_lines=False)


def getDataSet():
    np.random.seed(13)
    K = 3
    D = 10
    N = 100

    '''
    topics = [
        ['apple', 'banana', 'pineapple', 'kiwi', 'grape'],  # fruits
        ['football', 'basketball', 'tennis', 'swimming', 'boxing'],  # sports
        ['computer', 'car', 'processor', 'boat', 'airplane'],  # hardware stuff
        ['dog', 'cat', 'horse', 'pig', 'monkey']  # animals
    ]
    '''

    topics = [
        ['111a', '111b', '111c', '111d', '111e'],  # fruits
        ['222a', '222b', '222c', '222d', '222e'],  # sports
        ['333a', '333b', '333c', '333d', '333e'],  # hardware stuff
        ['444a', '444b', '444c', '444d', '444e']  # animals
    ]

    topics = topics[0:K]
    topics = [t[0:3] for t in topics]

    print(topics)

    voc = []
    for t in topics:
        voc += t

    V = len(voc)
    print(voc)

    wordsPerTopic = int(V / K)
    eta = np.ones(wordsPerTopic) * 10
    alpha = np.ones(K) * 1
    beta = np.ones((K, V)) * 0.001
    theta = np.zeros((D, K))
    for i in range(K):
        beta[i][i * wordsPerTopic: (i + 1) * wordsPerTopic] = np.random.dirichlet(eta)
        beta[i] /= np.sum(beta[i])
    # print(beta)

    for d in range(D):
        theta[d] = np.random.dirichlet(alpha)

    corpIndices = np.ones((D, N), dtype=int) * -1
    corp = []

    for d in range(D):
        doc = []
        for n in range(N):
            z = np.random.choice(np.arange(K), p=theta[d])
            w = np.random.choice(np.arange(V), p=beta[z])
            corpIndices[d][n] = w
            doc.append(voc[w])
        corp.append(doc)

    # print(corpIndices)
    print("Beta:")
    print(np.round(beta, 3))
    print("Theta:")
    print(np.round(theta, 3))
    # print(corp)
    writeToFile(corpIndices)
    return voc


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
    f = open("simpleDataIdx.txt", "w+")
    for doc in docs:
        strTowrite = ""
        for word in doc:
            strTowrite += str(word) + ","
        strTowrite = strTowrite[:-1]
        f.write(strTowrite + "\n")


def preProcess(documents):
    # Process all of the abstracts
    processed_docs = documents['abstract'].map(preprocess)

    # Create a dictionary
    dictionary = gensim.corpora.Dictionary(processed_docs)
    dictionary.filter_extremes(no_below=10, no_above=0.5)
    bow_corpus = [dictionary.doc2bow(doc) for doc in processed_docs]
    # print(bow_corpus)
    # print(dictionary)
    # print(len(dictionary))
    # print(len(processed_docs))
    # print(len(processed_docs[0]))
    # print(len(processed_docs[1]))
    # print(processed_docs[0])
    # lengths = [len(doc) for doc in processed_docs]
    # print(lengths)
    idxDocs = [dictionary.doc2idx(doc) for doc in processed_docs]
    maxLength = np.max([len(doc) for doc in idxDocs])
    avgLen = np.mean([len(doc) for doc in idxDocs])

    for doc in idxDocs:
        numToFill = maxLength - len(doc)
        for i in range(numToFill):
            doc.append(-1)

    minLength = np.min([len(doc) for doc in idxDocs])
    # print("maxL:", maxLength)
    # print("minL:", minLength)
    # print("idxDocs", len(idxDocs[92]))
    # print(idxDocs)
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
        [281, 128, 834, 10, 361, 637, 69, 2, 777, 214],
        [847, 276, 389, 613, 654, 713, 329, 410, 374, 225],
        [840, 512, 775, 397, 611, 823, 539, 541, 296, 609],
        [748, 46, 567, 563, 183, 621, 273, 549, 177, 328],
        [818, 801, 713, 500, 780, 229, 737, 822, 707, 290],
        [529, 447, 638, 824, 27, 220, 480, 159, 479, 678],
        [727, 611, 590, 528, 559, 594, 308, 491, 136, 800],
        [255, 171, 196, 97, 87, 121, 271, 646, 671, 729],
        [35, 56, 54, 300, 38, 201, 736, 74, 397, 353],
        [501, 772, 303, 404, 714, 268, 521, 200, 473, 619]
    ]

    # for k, idxs in enumerate(idxsByTopic):
    # wordsTopic = [dictionary[i] for i in idxs]
    # print("Topic[", k, "]:", wordsTopic[0:5])
    # wordsTopic1 = [dictionary[i] for i in idxTopic1]
    # wordsTopic2 = [dictionary[i] for i in idxTopic2]
    # print("Topic 1:", wordsTopic1)
    # print("Topic 2:", wordsTopic2)
    # writeToFile(processed_docs)
    return dictionary, idxDocs


def printWords(idxsByTopic, dictionary):
    for k, idxs in enumerate(idxsByTopic):
        wordsTopic = [dictionary[i] for i in idxs]
        print("Topic[", k, "]:", wordsTopic[0:5])


def plotCorrRatio(numParams, ratios):
    import matplotlib.pyplot as plt
    # for i in range(len(numParams)):
    #     plt.plot(numParams[i], ratios[i], label=labels[i])
    plt.plot(numParams, ratios, color="green")

    # plt.legend()
    # plt.title("Metric on how many correctly grouped words by topic (sloppy measure of beta quality)")
    plt.title("Metric on beta quality, 10.000 particles")
    plt.xlabel("#Estimated Params")
    plt.ylabel("Accuracy")
    plt.show()


if __name__ == '__main__':
    voc = getDataSet()
    print(np.mean(
        [
            0.2222, 0.5556, 0.6667, 0.4444, 0.2222, 0.1111, 0.6667, 0.4444, 0.5556, 0.4444, 0.6667, 0.5556, 0.4444, 0.4444,
            0.6667, 0.4444, 0.4444, 0.3333, 0.3333, 0.4444, 0.4444, 0.4444, 0.3333, 0.7778, 0.6667, 0.3333, 0.2222, 0.6667,
            0.4444, 0.5556, 0.2222, 0.5556, 0.5556, 0.7778, 0.3333, 0.3333, 0.4444, 0.6667, 0.6667, 0.7778, 0.5556, 0.4444,
            0.6667, 0.4444, 0.4444, 0.5556, 0.2222, 0.4444, 0.3333, 0.4444, 0.2222, 0.5556, 0.6667, 0.4444, 0.3333, 0.5556,
            0.5556, 0.5556, 0.6667, 0.3333, 0.3333, 0.6667, 0.5556, 0.4444, 0.3333, 0.4444, 0.4444, 0.4444, 0.3333, 0.5556,
            0.3333, 0.4444, 0.3333, 0.2222, 0.7778, 0.3333, 0.3333, 0.3333, 0.4444, 0.8889, 0.5556, 0.4444, 0.4444, 0.2222,
            0.2222, 0.6667, 0.3333, 0.4444, 0.6667, 0.4444, 0.3333, 0.4444, 0.2222, 0.4444, 0.6667, 0.2222, 0.5556, 0.3333,
            0.5556, 0.4444
        ]
    ))

    numParams = [20, 32, 63, 104, 155, 180, 230, 380, 630]  # , 34, 40, 80]
    numParamsInit = [10, 22, 48, 84, 130, 130, 130, 130, 130]  # , 14, 20, 40]
    ratios = [0.99, 0.613, 0.3595, 0.303, 0.2496, 0.2684, 0.2612, 0.248, 0.2592]  # , 0.8539, 0.4545, 0.3694]
    labels = [
        "K=2, V=4, 10k particles",
        "K=2, V=10, 10k particles",
        "K=3, V=15, 10k particles",
        "K=4, V=20, 10k particles",
        "K=5, V=25, 10k particles",
        "D=10 instead of 5",
        "D=20",
        "D=50",
        "D=100",
        "K=2, V=3*K",
        "K=3, V=3*K",
        "K=4, V=3*K",
        "K=5, V=3*K"
    ]

    sortedTuples = [(numParams[i], numParamsInit[i], ratios[i]) for i in range(len(numParams))]
    # print(sortedTuples)
    sortedTuples.sort(key=lambda x: x[1])
    # print(sortedTuples)
    sortedTuples = np.array(sortedTuples)
    # print(sortedTuples)

    # plotCorrRatio(sortedTuples[:, 1], sortedTuples[:, 2])
    # plotCorrRatio(numParamsInit, ratios)

    documents = pd.read_csv('Python/papers2017.csv', error_bad_lines=False)
    voc, corpus = preProcess(documents)

    # printWords(idxsByTopic, voc)

    # 0.98 1 per resample
    # 0.98 10 per resample
    # 0.99 99 per resample
