#include "Graph.hpp"


#include <cstdint>
#include <iostream>


Graph::Graph(std::string filename) {
  GVC_t* gvc = gvContext();
  
  FILE* fp = fopen(filename.c_str(), "r");
  if (!fp) {
    std::cerr << "Failed to open file: " << filename << std::endl;
  }

  Agraph_t* g = agread(fp, nullptr);
  fclose(fp);
  if (!g) {
    std::cerr << "Failed to parse graph: " << filename << std::endl;
  }

  if (gvLayout(gvc, g, "dot") != 0) {
    std::cerr << "Graphviz layout failed\n";
    agclose(g);
    gvFreeContext(gvc);
  }

  boxf bb = GD_bb(g);
  width = bb.UR.x-bb.LL.x;
  height = bb.UR.y-bb.LL.y;

  bool first = true;
  // --- Extract nodes ---
  for (Agnode_t* n = agfstnode(g); n; n = agnxtnode(g, n)) {
    uint64_t lowerAddress = 0;
    uint64_t upperAddress = 1;
    nodes.push_back(GraphNode(std::string(agnameof(n)), lowerAddress, upperAddress, ND_coord(n).x, ND_coord(n).y, ND_width(n)*72/2, ND_height(n)*72/2));
    if (first) {
      nodes.back().setAsCurrent();
      first = false;
    }
  }

  // --- Extract edges ---
  for (Agnode_t* n = agfstnode(g); n; n = agnxtnode(g, n)) {
    int tail_index = -1;
    for (size_t i = 0; i < nodes.size(); ++i)
      if (nodes[i].name == agnameof(n))
        tail_index = i;

    for (Agedge_t* e = agfstout(g, n); e; e = agnxtout(g, e)) {
      const char* style = agget(e, (char*)"style");
      if (style && strcmp(style, "invis") == 0)
        continue;

      int head_index;
      for (size_t i = 0; i < nodes.size(); ++i)
        if (nodes[i].name == agnameof(aghead(e)))
          head_index = i;
      edges.push_back(GraphEdge(tail_index, head_index));

      splines* spl = ED_spl(e);
      if (spl && spl->list) {
        for (int i = 0; i < spl->size; ++i) {
          bezier bz = spl->list[i];
          for (int j = 0; j < bz.size; ++j) {
            edges.back().points.emplace_back(bz.list[j].x, bz.list[j].y);
          }
        }
      }
    }
  }

  gvFreeLayout(gvc, g);
  agclose(g);
  gvFreeContext(gvc);
}

std::vector<WatchPoint*> Graph::asWatchPoints() {
  std::vector<WatchPoint*> res;
  for (GraphNode& node : nodes) {
    res.push_back(&node);
  }
  return res;
}
