import numpy as np
import matplotlib.pyplot as plt

# np.random.seed(28)


def genCluster(n):
    # n: an integer
    # Generates a cluster of n data points with a normal distribution
    # of randomly chosen mean (mu) and sd (sigma)
    mu = np.random.uniform(0, 100)
    sigma = np.random.uniform(0, 10)
    x = np.random.normal(mu, sigma, n)
    mu = np.random.uniform(0, 100)
    sigma = np.random.uniform(0, 10)
    y = np.random.normal(mu, sigma, n)
    return x, y

def makeClusters(k, n):
    # k: an integer
    # n: an integer
    # Makes k clusters of n data points
    clusters = []
    for i in range(k):
        clusters.append(genCluster(n))
    return clusters

def plotClusters(numClusters, clusters, number_of_subplots, next_subplot):
    # numClusters: an integer
    # clusters: a list
    # Plots clusters on one set of axes
    # ax1 = fig.add_subplot(number_of_subplots/3, 3, next_subplot)

    try:
        for i in range(numClusters):
            ax1.scatter(clusters[i][0], clusters[i][1])
            ax1.set_title(next_subplot, color="red")
            ax1.set_xlim(0 - 20, 100 + 20)
            ax1.set_ylim(0 - 20, 100 + 20)
    except:
        pass


def makeAllPoints(clusters, numClusters):
    # clusters: an array
    # numClusters: an integer
    # Creates separate arrays of all X values combined and all Y values combined
    allXPoints = clusters[0][0]
    allYPoints = clusters[0][1]
    for i in range(1, numClusters):
        allXPoints = np.concatenate((allXPoints, clusters[i][0]))
        allYPoints = np.concatenate((allYPoints, clusters[i][1]))
    return allXPoints, allYPoints

def genRandomClusterCenters(numClusters):
    #numClusters: an integer
    # Generates 'numClusters' random centers as guesses for the clusters
    xCenters = np.random.uniform(0, 100, numClusters)
    yCenters = np.random.uniform(0, 100, numClusters)
    return xCenters, yCenters

def euclidDist(centerX, centerY, pointX, pointY):
    # centerX/Y, pointX/Y: floats
    return np.sqrt((centerX - pointX)**2 + (centerY - pointY)**2)

def findClosestCenter(numClusters, centerGuess, allPoints):
    # numClusters: an integer
    # centerGuess: an array
    # allPoints: an array
    # Creates 'k' new clusters based on which cluster center each point is closest to
    # and returns these new clusters
    closestList = []
    for i in range(numClusters):
        closestList.append([])
    for index in range(len(allPoints[0])):
        minDist = np.nan
        minInd = 0
        for centerIndex in range(len(centerGuess[0])):
            distPointToCenter = euclidDist(centerGuess[0][centerIndex],
                                           centerGuess[1][centerIndex],
                                           allPoints[0][index], allPoints[1][index])
            if np.isnan(minDist) == True:
                minDist = distPointToCenter
            elif distPointToCenter < minDist:
                minDist = distPointToCenter
                minInd = centerIndex
        closestList[minInd].append((allPoints[0][index], allPoints[1][index]))
    return closestList

def unzipCluster(zippedClusters):
    # zippedClusters: a list of (x, y) tuples
    xPoint, yPoint = zip(*zippedClusters)
    return np.array(xPoint), np.array(yPoint)

def convertClusterToPlotForm(temp):
    # temp: a list of which points are closest to each cluster center
    # converts the list of points to a form compatible with the plotClusters function
    newClusterList = []
    for i in range(len(temp)):
        if len(temp[i]) > 0:
            a, b = unzipCluster(temp[i])
            c = (a, b)
            newClusterList.append(c)
    return newClusterList

def findNewCenters(plotData):
    # plotData: the closest point to each cluster centerGuess
    # recalculates centerGuess of each cluster based on the outcome of
    # which points were closest in the previous iteration
    plotLen = len(plotData)
    newXCenters = np.empty(plotLen)
    newYCenters = np.empty(plotLen)
    for i in range(plotLen):
        newXCenters[i] = np.mean(plotData[i][0])
        newYCenters[i] = np.mean(plotData[i][1])
    return newXCenters, newYCenters

# Set number of clusters and how many points in teach cluster
numClusters = 5
clusterSize = 20
clusters = makeClusters(numClusters, clusterSize)

fig = plt.figure(figsize=(16, 8))
number_of_subplots = 1 + 8
next_subplot = 1


# Create an array of all points (without regard to clusters) and generate
# random centers as a guess
allPoints = makeAllPoints(clusters, numClusters)
centerGuess = genRandomClusterCenters(numClusters)


temp = findClosestCenter(numClusters, centerGuess, allPoints)
plotData = convertClusterToPlotForm(temp)

# Create subplot of data points and first guess at centers
ax1 = fig.add_subplot(number_of_subplots/3, 3, next_subplot)
ax1.scatter(centerGuess[0], centerGuess[1], s=150, marker="s", color="blue")
ax1.set_title(next_subplot, color="red")
for i in range(numClusters):
    ax1.scatter(clusters[i][0], clusters[i][1], color="black")
ax1.set_xlim(0 - 20, 100 + 20)
ax1.set_ylim(0 - 20, 100 + 20)
next_subplot += 1
# plotClusters(numClusters, plotData, number_of_subplots, next_subplot)



newCenters = findNewCenters(plotData)


for i in range(number_of_subplots - 1):
    temp = findClosestCenter(numClusters, newCenters, allPoints)
    plotData = convertClusterToPlotForm(temp)
    newCenters = findNewCenters(plotData)
    ax1 = fig.add_subplot(number_of_subplots / 3, 3, next_subplot)
    ax1.scatter(newCenters[0], newCenters[1], s=150, marker="s", color="blue")
    plotClusters(numClusters, plotData, number_of_subplots, next_subplot)
    next_subplot += 1

plt.show()
