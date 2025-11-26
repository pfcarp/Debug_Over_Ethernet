#include "Event.hpp"


Event::Event(std::string name, Color color): name(name), color(color) {}

bool Event::matches(uint64_t attempt) {
  return true;
}
