#include "GraphNode.hpp"


GraphNode::GraphNode(std::string name, uint64_t lower, uint64_t upper, double x, double y, double width, double height): WatchPoint(name, lower, upper), x(x), y(y), width(width), height(height) {};
