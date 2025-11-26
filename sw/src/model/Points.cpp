#include "Points.hpp"


#include <cstdlib>
#include <iostream>


#include "Color.hpp"
#include "Point.hpp"
#include "DataBuffer.hpp"


Points::Points(): Collection() {
  // Building roofline (TODO: depart from hardcoded values!)
  add(new DataBuffer(new Event("Roofline", Color(0.0, 0.0, 0.0, 1.0))));
  TimedData coord = {.time = 0, .value = 0};
  buffers.back()->add(coord);
  coord = {.time = 0.710755600, .value = 1.333};
  buffers.back()->add(coord);
  coord = {.time = 50, .value = 1.333};
  buffers.back()->add(coord);
  // Current perf tracker
  add(new Point(new Event("Roofline")));
  // Archive
  add(new DataBuffer(new Event(buffers.back()->event->name, Color(buffers.back()->event->color.red, buffers.back()->event->color.green, buffers.back()->event->color.blue, 0.25)), "."));
}

Points::~Points() {
  delete buffers[0]->event;
  delete buffers[0];
  delete buffers[1]->event;
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

void Points::clear() {
  buffers[1]->clear();
  buffers[2]->clear();
}
