#pragma once


#include <vector>


class GraphEdge {

  public:
    int tail;
    int head;
    // For spline coordinates
    std::vector<std::pair<double, double>> points;

    GraphEdge(int tail = 0, int head = 0);
};
