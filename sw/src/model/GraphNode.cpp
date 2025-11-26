#include "GraphNode.hpp"


GraphNode::GraphNode(std::string name, uint64_t address, double x, double y, double width, double height): WatchPoint(name, address), x(x), y(y), width(width), height(height) {};
