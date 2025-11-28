#include "Points.hpp"


#include <cstdlib>
#include <iostream>


#include "Color.hpp"
#include "Point.hpp"
#include "DataBuffer.hpp"


Points::Points(std::vector<WatchPoint*> watchpoints): Collection(), watchpoints(watchpoints) {
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
  return buffers[index];
}

void Points::archives() {
  buffers[2]->add(buffers[1]->at(0));
}

void Points::clear() {
  buffers[1]->clear();
  buffers[2]->clear();
}

void Points::push(int source, Packet::Base& packet) {}

void Points::push(int source, Packet::ShortAddress& packet) {
  for (size_t i = 0; i < watchpoints.size(); i++) {
    if (watchpoints[i]->matches(packet.getAddress())) {
      if (watchpoints[i] != buffers[2]->event) {
        archives();
        buffers[2]->event = watchpoints[i];
      }
    }
  }
}

void Points::push(int source, Packet::LongAddress& packet) {
  for (size_t i = 0; i < watchpoints.size(); i++) {
    if (watchpoints[i]->matches(packet.getAddress())) {
      if (watchpoints[i] != buffers[2]->event) {
        archives();
        buffers[2]->event = watchpoints[i];
      }
    }
  }
}

void Points::push(int source, Packet::AddressWithContext& packet) {
  for (size_t i = 0; i < watchpoints.size(); i++) {
    if (watchpoints[i]->matches(packet.getAddress())) {
      if (watchpoints[i] != buffers[2]->event) {
        archives();
        buffers[2]->event = watchpoints[i];
      }
    }
  }
}

void Points::push(int source, Packet::Event& packet) {
  instructions += packet.hasEvent(0);
  cacheline_refill += packet.hasEvent(2);
  time += 1.0; //packet.delta;
  TimedData res = {.time = instructions/cacheline_refill, .value = instructions/time};
  buffers[2]->add(res);
}
