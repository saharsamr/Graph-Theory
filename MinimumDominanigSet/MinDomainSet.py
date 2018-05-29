import igraph

NOT_CHECKED = False
CHECKED = True

NUM_OF_UNCOVERED_VERTICES = 0
CHECK_STATUS = 1

class MinDomainSet:
    def __init__ (self, graph):
        self.graph = graph
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

def make_queen_chess_cover_graph ():
    graph = igraph.Graph(12*12)
    for i in range(12):
        for j in range(12):
            graph.add_edges([(i*12+j, k*12+j) for k in range(i+1, 12)])
            graph.add_edges([(i*12+j, i*12+k) for k in range(j+1, 12)])
            min_ = min(i, 12-j-1)
            max_ = max(i, j)
            graph.add_edges([(i*12+j, (i+i1)*12+(j+i1)) for i1 in range(1, 12-max_)])
            graph.add_edges([(i*12+j, (i-i1)*12+(j-i1)) for i1 in range(1, min_)])

    print graph.degree()
    return graph

if __name__ == "__main__":
    graph = make_queen_chess_cover_graph()
    dominating_set = MinDomainSet(graph).solve_greedy()
    print dominating_set
