#pragma once


#include <cstdint>


#include "WatchPoint.hpp"


class GraphNode: public WatchPoint {

  public:
    double x;
    double y;
    double width;
    double height;
    
    GraphNode(std::string name = "", uint64_t address = 0, double x = 0.0, double y = 0.0, double width = 1.0, double height = 1.0);
};
