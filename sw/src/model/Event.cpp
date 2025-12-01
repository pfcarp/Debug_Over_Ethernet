#include "Event.hpp"


Event::Event(std::string name, Color color, bool current): name(name), color(color), current(current) {}

bool Event::matches(uint64_t attempt) {
  return true;
}

void Event::setAsCurrent() {
  current = true;
}

void Event::setAsNotCurrent() {
  current = false;
}
