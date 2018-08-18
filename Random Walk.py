import random
import math
import matplotlib.pyplot as plt

#simulates a random walk of k steps, outputs the distance from origin
def randWalk(k):
    position = [0, 0]
    for steps in range(k):
        step = random.randint(1, 4)
        if step == 1:
            position[0] += 1
        elif step == 2:
            position[0] -= 1
        elif step == 3:
            position[1] += 1
        else:
            position[1] -= 1

    return math.sqrt(position[0]**2 + position[1]**2)


# simulates a random walk of k steps a number of times n
# stores distances in a dictionary, returns a dictionary of the distances and frequencies
def simRandWalk(k, n):

    distances = {}

    for walks in range(n):
        trial = randWalk(k)
        distances[trial] = distances.get(trial, 0) + 1

    #for k in sorted(distances):
    #    print(k, distances[k]/n)

    return (distances, k, n)

#calculates the average distance travelled from a dictionary of distances and their frequency
def aveDist(dictionary):
    distances = dictionary[0]
    n = dictionary[2]

    average = 0
    for k in distances:
        average += k * distances[k]/n

    return average


#turns dictionary of (distances, frequencies) into a list of all data points
def dictToList(dictionary):
    data = list(dictionary[0].items())
    dataFull = []
    for i in data:
        for j in range(i[1]):
            dataFull.append(i[0])

    return dataFull


#plots a histogram of the data
# input the output dictionary, n tuple from simRandWalk and output a histogram
def plotDistribution(dictionary):

    data = dictToList(dictionary)
    k = dictionary[1]
    n = dictionary[2]

    num, bins, patches = plt.hist(data, 10, normed=1, facecolor='green')

    plt.xlabel("Distance")
    plt.ylabel("Frequency")
    plt.title("Simulation of {} Random Walks of {} Steps".format(n, k))
    plt.axis()
    plt.grid()

    plt.show()



#a = simRandWalk(1000, 10000)
#print(aveDist(a))
#plotDistribution(a)

class Location(object):
    def __init__(self, x, y):
        """x and y are floats """
        self.x = x
        self.y = y

    def move(self, deltaX, deltaY):
        """deltaX and deltaY are floats"""
        return Location(self.x + deltaX, self.y + deltaY)

    def getX(self):
        return self.x

    def getY(self):
        return self.y

    def distFrom(self, other):
        ox = other.x
        oy = other.y
        xDist = self.x - ox
        yDist = self.y - oy
        return (xDist**2 + yDist**2)**0.5

    def __str__(self):
        return '<' + str(self.x) + ", " + str(self.y) + '>'


def takeStep():
    stepChoices = [(0.0, 1.0),(0.0, -1.0),(1.0, 0.0),(-1.0, 0.0)]
    return random.choice(stepChoices)

def trip(k):
    #input an integer k number of steps to take
    #output a list of all points visited on the grid
    place = Location(0.0, 0.0)
    trips = [place]
    for steps in range(k):
        xDist, yDist = takeStep()
        place = place.move(xDist, yDist)
        trips.append(place)

    return trips, k
    
def getPoints(trip, k):
    # Creates a list of points (xVals, yVals) from the random walk data
    xValues = []
    yValues = []
    for point in trip:
        xValues.append(point.getX())
        yValues.append(point.getY())
        #print(xValues[-1], yValues[-1])

    return xValues, yValues, k

def plotPath(xVals, yVals, k):
    #creates a plot of one path

    colors = (random.random(), random.random(), random.random(), random.random())
    plt.plot(xVals, yVals, c = colors)
   

origin = Location(0, 0)
walkDist = []
#plot n random walks
for i in range(1000):
    k=1000
    a = trip(k)
    walkDist.append(a[0][-1].distFrom(origin))
    b = getPoints(a[0], a[1])
    plotPath(b[0], b[1], b[2])


z = 2.5
plt.title("Random Walk of {} Steps".format(k))
plt.axis([-z*math.sqrt(k), z*math.sqrt(k), -z*math.sqrt(k), z*math.sqrt(k)])
plt.show()

average = sum(walkDist) / float(len(walkDist))
print(average)






