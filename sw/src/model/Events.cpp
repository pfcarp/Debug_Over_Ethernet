#include "Events.hpp"


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

