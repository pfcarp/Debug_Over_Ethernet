#include "Points.hpp"
#include <cstdlib>


#include "Color.hpp"
#include "Point.hpp"
#include "DataBuffer.hpp"


Points::Points(Event* current): Collection() {
  // Building roofline (TODO: depart from hardcoded values!)
  add(new DataBuffer(new Event("Roofline", Color(0.0, 0.0, 0.0, 1.0))));
  TimedData coord = {.time = 0, .value = 0};
  buffers.back()->add(coord);
  coord = {.time = 0.710755600, .value = 1.333};
  buffers.back()->add(coord);
  coord = {.time = 50, .value = 1.333};
  buffers.back()->add(coord);
  // Current perf tracker
  add(new Point(current));
  // Archive
  add(new DataBuffer(new Event(current->name, Color(current->color.red, current->color.green, current->color.blue, 0.25)), "."));
}

Points::~Points() {
  delete buffers[0]->event;
  delete buffers[0];
  delete buffers[1];
  delete buffers[2]->event;
  delete buffers[2];
}

Buffer* Points::operator[](size_t index) {
  if (std::rand()%100 == 0)
    archives();
  return buffers[index];
}

void Points::archives() {
  buffers[2]->add(buffers[1]->at(0));
}
