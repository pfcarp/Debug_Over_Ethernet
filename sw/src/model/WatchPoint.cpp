#include "WatchPoint.hpp"


WatchPoint::WatchPoint(std::string name, uint64_t address): Event(name), address(address) {}

bool WatchPoint::matches(uint64_t attempt) {
  return address == attempt;
}
