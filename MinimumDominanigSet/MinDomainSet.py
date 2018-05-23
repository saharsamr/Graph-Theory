import igraph

NOT_CHECKED = False
CHECKED = True

NUM_OF_UNCOVERED_VERTICES = 0
CHECK_STATUS = 1

class MinDomainSet:
    def __init__ (self, filePath):
        self.graph = igraph.read(filePath)
        self.vertices_status = {}
        self.__initial_status__(self.graph.degree())
        self.min_domain_set = []
        self.sorted_vertices = []

    def solve_greedy (self):
        self.sorted_vertices = self.__sort_by_num_of_uncovered_neighbors__()
        while(True):
            vertex_index = self.__find_first_unchecked__()
            if (vertex_index >= 0):
                self.__add_to_domain_set__(vertex_index)
            else:
                break
        return self.min_domain_set

    def __initial_status__ (self, degree):
        for i in range(len(degree)):
            self.vertices_status[i] = [degree[i], NOT_CHECKED]

    def __find_first_unchecked__ (self):
        for i in range(len(self.sorted_vertices)-1, -1, -1):
            if (not self.vertices_status[self.sorted_vertices[i]][CHECK_STATUS]):
                return self.sorted_vertices[i]
        return -1

    def __add_to_domain_set__ (self, vertex_index):
        self.min_domain_set.append(vertex_index)
        self.__update_status__(vertex_index)
        self.sorted_vertices = self.__sort_by_num_of_uncovered_neighbors__()

    def __update_status__ (self, vertex_index):
        self.vertices_status[vertex_index][CHECK_STATUS] = CHECKED
        for neighbor in self.graph.neighbors(vertex_index):
            if(not self.vertices_status[neighbor][CHECK_STATUS]):
                self.__decrease_uncovered_num_of_neighbors__(neighbor)
                self.vertices_status[neighbor][CHECK_STATUS] = CHECKED

    def __decrease_uncovered_num_of_neighbors__ (self, vertex_index):
        for neighbor in self.graph.neighbors(vertex_index):
            if (not self.vertices_status[neighbor][CHECK_STATUS] \
            and self.vertices_status[neighbor][NUM_OF_UNCOVERED_VERTICES] > 0):
                self.vertices_status[neighbor][NUM_OF_UNCOVERED_VERTICES] -= 1

    def __sort_by_num_of_uncovered_neighbors__ (self):
        result = []
        for key, value in sorted(self.vertices_status.iteritems(), key=lambda (k,v): (v,k)):
            if(not self.vertices_status[key][CHECK_STATUS]):
                result.append(key)
        return result



if __name__ == "__main__":
    g = MinDomainSet("./../data/got.graphml")
    print (g.solve_greedy())
