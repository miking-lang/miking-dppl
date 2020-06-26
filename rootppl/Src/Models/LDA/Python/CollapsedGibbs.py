import numpy as np
import pandas as pd

from Python.ldaPreProcess import preProcess

K = 10
GIBBS_ITERATIONS = 50


def normalizeMatrix(mat):
    return mat / np.expand_dims(np.sum(mat, axis=1), 1)


def getTFIDF(beta):
    prod = np.prod(beta, axis=0)
    products = np.power(prod, (1.0 / K))
    return beta * np.log(beta / products)


def getMostPopularWordsPerTopic(beta, vocab):
    betaNorm = normalizeMatrix(beta)
    tfidf = getTFIDF(betaNorm)
    # tfidf = betaNorm
    V = len(tfidf[0])

    tuples = []
    for i in range(K):
        tuples.append([(tfidf[i][j], vocab[j]) for j in range(V)])

    for i in range(K):
        tuples[i] = sorted(tuples[i], key=lambda x: float(x[0]))
        tuples[i] = np.array(tuples[i])

    for i in range(K):
        print(tuples[i][-5:, 1])


def getMostPopularTopicsPerDocument(theta, M, numTopicsToShow):
    for d in range(M):
        print(np.argmax(theta[d]))


def approxBeta(cTilde, eta, V):
    beta = np.zeros((K, V))
    etaSum = np.sum(eta)
    for k in range(K):
        cTildeSum = np.sum(cTilde[k])
        for v in range(V):
            beta[k, v] = (eta[v] + cTilde[k, v]) / (etaSum + cTildeSum)

    return beta


def approxTheta(c, alpha, M):
    theta = np.zeros((M, K))
    alphaSum = np.sum(alpha)

    for d in range(M):
        cSum = np.sum(c[d])
        for k in range(K):
            theta[d, k] = (alpha[k] + c[d, k]) / (alphaSum + cSum)

    return theta


def gibbsInference(docs, vocab):
    M = len(docs)
    V = len(vocab)

    alpha = np.ones(K)
    eta = np.ones(V)
    alphaSum = np.sum(alpha)
    etaSum = np.sum(eta)
    topics = np.arange(K)

    z = []
    c = np.zeros((M, K))
    cTilde = np.zeros((K, V))
    for d, doc in enumerate(docs):
        z.append(np.random.randint(0, K, len(doc)))
        # k = z[d]
        # print(k)
        # c[d][k] += 1
        for n, w in enumerate(doc):
            k = z[d][n]
            c[d, k] += 1
            cTilde[k, w] += 1

    for i in range(GIBBS_ITERATIONS):
        print("Epoch:", i)
        for d, doc in enumerate(docs):
            # print("docIdx:", d)
            for n, w in enumerate(doc):
                k = z[d][n]

                c[d, k] -= 1
                cTilde[k, w] -= 1

                # distr = []
                cSum = np.sum(c[d])
                cTildeSums = np.sum(cTilde, axis=1)
                factor1 = (alpha + c[d]) / (alphaSum + cSum)
                factor2 = (eta[w] + cTilde[:, w]) / (etaSum + cTildeSums)
                distr = factor1 * factor2

                '''
                for kDist in range(K):
                    cTildeSum = np.sum(cTilde[kDist])
                    factor1 = (alpha[kDist] + c[d, kDist]) / (alphaSum + cSum)
                    factor2 = (eta[kDist] + cTilde[kDist, w]) / (etaSum + cTildeSum)
                    distr.append(factor1 * factor2)
                '''

                k = np.random.choice(topics, p=distr / np.sum(distr))
                z[d][n] = k
                c[d, k] += 1
                cTilde[k, w] += 1

    beta = approxBeta(cTilde, eta, V)
    theta = approxTheta(c, alpha, M)
    return z, c, cTilde, beta, theta


def doTest():
    documents = pd.read_csv('papers2017.csv', error_bad_lines=False)
    vocabulary, corpus = preProcess(documents)
    for idx, d in enumerate(corpus):
        corpus[idx] = [w for w in d if w != -1]
    V = len(vocabulary)
    M = len(corpus)
    print("Vocab Size:", V)
    print("Number of docs:", M)
    z, c, cTilde, beta, theta = gibbsInference(corpus, vocabulary)
    getMostPopularWordsPerTopic(beta, vocabulary)
    getMostPopularTopicsPerDocument(theta, 10, numTopicsToShow=1)


if __name__ == '__main__':
    # np.random.seed(13)
    doTest()
