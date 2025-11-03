#pragma once


#include "Event.hpp"


class GraphNode: public Event {

  public:
    double x;
    double y;
    double width;
    double height;
    
    GraphNode(std::string name = "", double x = 0.0, double y = 0.0, double width = 1.0, double height = 1.0);
};
