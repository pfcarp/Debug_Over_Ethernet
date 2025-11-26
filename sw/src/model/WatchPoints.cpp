#include "WatchPoints.hpp"


#include <cstdlib>


#include "HistogramBuffer.hpp"


WatchPoints::WatchPoints(std::vector<WatchPoint*> watchpoints): Collection() {
  for (WatchPoint*& watchpoint : watchpoints) {
    add(new HistogramBuffer(watchpoint));
  }
}

WatchPoints::~WatchPoints() {
  for (size_t i = 0; i < buffers.size(); i++) {
    delete buffers[i];
  }
}

Buffer* WatchPoints::operator[](size_t index) {
  return buffers[index];
}
