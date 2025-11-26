#pragma once


#include <string>
#include <vector>
#include <graphviz/types.h>
#include <graphviz/gvc.h>
#include <graphviz/cgraph.h>

#include "GraphNode.hpp"
#include "GraphEdge.hpp"


class Graph {
    
  public:
    double width;
    double height;
    std::vector<GraphNode> nodes;
    std::vector<GraphEdge> edges;

    Graph(std::string filename = "");
    std::vector<WatchPoint*> asWatchPoints();

};
