#include "Events.hpp"


#include <cstddef>
#include <iostream>


#include "DataBuffer.hpp"


Events::Events(std::vector<Event*> events): Collection() {
  for (Event*& event : events) {
    add(new DataBuffer(event, "-"));
  }
  if (events.size() > 4) {
    std::cerr << "Warning: only up to 4 events can be displayed!" << std::endl;
  }
}

Events::~Events() {
  for (size_t i = 0; i < buffers.size(); i++) {
    delete buffers[i];
  }
}

Buffer* Events::operator[](size_t index) {
  return buffers[index];
}

void Events::push(int source, Packet::Base& packet) {}

void Events::push(int source, Packet::Event& packet) {
  for (size_t i = 0; i < 4; i++) {
    if (packet.hasEvent(i)) {
      TimedData res = {.time = 1, .value = 0};
      buffers[i]->add(res);
    }
  }
}
