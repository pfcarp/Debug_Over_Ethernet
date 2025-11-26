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

void WatchPoints::push(int index, Packet::Base& packet) {}

void WatchPoints::push(int index, Packet::ShortAddress& packet) {
  for (size_t i = 0; i < buffers.size(); i++) {
    if (buffers[i]->event->matches(packet.getAddress())) {
      TimedData res = {.time = 1, .value = 0};
      buffers[i]->add(res);
    }
  }
}

void WatchPoints::push(int index, Packet::LongAddress& packet) {
  for (size_t i = 0; i < buffers.size(); i++) {
    if (buffers[i]->event->matches(packet.getAddress())) {
      TimedData res = {.time = 1, .value = 0};
      buffers[i]->add(res);
    }
  }
}
