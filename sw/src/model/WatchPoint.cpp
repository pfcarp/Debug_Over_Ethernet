#include "WatchPoint.hpp"


WatchPoint::WatchPoint(std::string name, uint64_t lower, uint64_t upper): Event(name), lower(lower), upper(upper) {}

bool WatchPoint::matches(uint64_t attempt) {
  return (lower <= attempt) && (attempt < upper);
}
