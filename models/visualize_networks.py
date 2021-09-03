import networkx as nx
import manim
from manim import *

config.background_color = WHITE

def generate_network(graph_type,N):
    if graph_type == "complete":
        G = nx.complete_graph(N)
    elif graph_type == "random_regular":
        G = nx.random_regular_graph(int(N/2), N, seed=None)
    elif graph_type == "random_erdos":
        G = nx.gnp_random_graph(N, 0.5333, seed=None, directed=False)
        while not nx.is_connected(G):
            G = nx.gnp_random_graph(N, 0.5333, seed=None, directed=False)
    elif graph_type == "random_barabasi":
        G = nx.barabasi_albert_graph(N, 4, seed=None)
    elif graph_type == "random_small_world":
        G = nx.connected_watts_strogatz_graph(N, int(N/2), 0.01, tries=200, seed=None)
    else:
        print("Incorrect graph name!")

    return G

class ImportNetworkxGraph(Scene):
    def construct(self):
        graph_type = "random_regular"
        N=16
        G = generate_network(graph_type,N)
        g = Graph.from_networkx(G, layout="circular", layout_scale=2, labels=True)
        a = Text("Random Regular", line_spacing=1)
        self.add(a,g)

scene = ImportNetworkxGraph()
scene.render()
