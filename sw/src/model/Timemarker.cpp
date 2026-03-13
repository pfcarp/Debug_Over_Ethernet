#include "Timemarker.hpp"


Timemarker::Timemarker(uint64_t time, Color color, std::string name): time(time), color(color), name(name) {}


const uint64_t& Timemarker::getTime() const {
  return time;
}


const Color& Timemarker::getColor() const {
  return color;
}


const std::string& Timemarker::getName() const {
  return name;
}


bool Timemarker::operator<(const Timemarker& other) const {
  return time < other.time;
}


bool Timemarker::operator<(const uint64_t& other) const {
  return time < other;
}
