import networkx as nx
import manim
from manim import *

config.background_color = WHITE

def generate_network(graph_type,N):
    if graph_type == "complete":
        G = nx.complete_graph(N)
    elif graph_type == "random_regular":
        G = nx.random_regular_graph(int(N/4), N, seed=None)
    elif graph_type == "random_erdos":
        G = nx.gnp_random_graph(N, 0.26087, seed=None, directed=False) # khat=p(n-1)
        while not nx.is_connected(G):
            G = nx.gnp_random_graph(N, 0.26087, seed=None, directed=False)
    elif graph_type == "random_barabasi":
        G = nx.barabasi_albert_graph(N, 3, seed=None) #khat=2m
    elif graph_type == "random_small_world":
        G = nx.connected_watts_strogatz_graph(N, int(N/4), 0, tries=200, seed=None)
    elif graph_type == "custom_adj_list":
        G = nx.read_adjlist(custom_adj_list_filename)
    else:
        print("Incorrect graph name!")

    return G

class ImportNetworkxGraph(Scene):
    def construct(self):
        graph_type = "random_erdos"
        N=24
        G = generate_network(graph_type,N)
        g = Graph.from_networkx(G, layout="spring", layout_scale=2, labels=False, vertex_config={"color": BLACK, "radius": 0.1}, edge_config={"color": GREY, "stroke_width": 1})
        #a = Text("Random Regular", line_spacing=1)
        self.add(g)

scene = ImportNetworkxGraph()
scene.render()
