import igraph

NO_COLOUR = -1
DEGREE = 0
COLOUR_NUMBER = 1
USED = True

class Coloring:
    def __init__ (self, graph):
        self.graph = graph
        self.vertices_status = {}
        self.__initial_status__(self.graph.degree())
        self.sorted_vertices = self.__sort_by_degree__()
        self.last_colored = 0;

    def greedy_coloring (self):
        while self.last_colored < len(self.sorted_vertices):
            self.__colour_vertex__(self.sorted_vertices[-self.last_colored - 1])
            self.last_colored += 1

    def __initial_status__ (self, degree):
        for i in range(len(degree)):
            self.vertices_status[i] = [degree[i], NO_COLOUR]

    def __colour_vertex__ (self, vertex_index):
        neighbours_colours = {}
        for neighbor in self.graph.neighbors(vertex_index):
            if(self.vertices_status[neighbor][COLOUR_NUMBER] > NO_COLOUR):
                neighbours_colours[self.vertices_status[neighbor][COLOUR_NUMBER]] = USED;
        for i in range(len(neighbours_colours)+1):
            if not neighbours_colours.has_key(i):
                self.vertices_status[vertex_index][COLOUR_NUMBER] = i
                return

    def __sort_by_degree__ (self):
        result = []
        for key, value in sorted(self.vertices_status.iteritems(), key=lambda (k,v): (v,k)):
            result.append(key)
        return result

    def __str__(self):
        max = 0
        for vertex in self.vertices_status:
            if self.vertices_status[vertex][COLOUR_NUMBER] > max:
                max = self.vertices_status[vertex][COLOUR_NUMBER]
        return str(max)

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
    return graph

if __name__ == "__main__":
    graph = make_queen_chess_cover_graph()
    colored_graph = Coloring(graph)
    colored_graph.greedy_coloring()
    print colored_graph
