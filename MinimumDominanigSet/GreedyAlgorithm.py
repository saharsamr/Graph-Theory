import igraph

NOT_CHECKED = False
CHECKED = True

class MinDomainSet:

    def __init__ (self, filePath):
        self.graph = igraph.read(filePath)
        self.degree = {}
        self.__map_degrees_to_vertex__(self.graph.degree())

    def __map_degrees_to_vertex__ (self, degree):
        self.degree = {}
        for i in range(len(degree)):
            self.degree[i] = (degree[i], NOT_CHECKED)


if __name__ == "__main__":
    g = MinDomainSet("./../data/got.graphml")
