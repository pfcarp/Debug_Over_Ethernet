#include "Dispatcher.hpp"


Dispatcher::Dispatcher(WatchPoints& watchpoints, Events& events): watchpoints(watchpoints), events(events) {}

void Dispatcher::push(int source, Packet::Base packet) {}

void Dispatcher::push(int source, Packet::ShortAddress packet) {
  for (size_t i = 0; i < watchpoints.amount(); i++) {
    if (watchpoints[i]->event->matches(packet.getAddress())) {
      TimedData item = {.time = i, .value=0};
      watchpoints[i]->add(item);
    }
  }
  i++;
}

void Dispatcher::push(int source, Packet::LongAddress packet) {
  for (size_t i = 0; i < watchpoints.amount(); i++) {
    if (watchpoints[i]->event->matches(packet.getAddress())) {
      TimedData item = {.time = i, .value=0};
      watchpoints[i]->add(item);
    }
  }
  i++;
}

void Dispatcher::push(int source, Packet::Event packet) {
  for (size_t i = 0; i < events.amount(); i++) {
    if (packet.hasEvent(i)) {
      TimedData item = {.time = i, .value=1};
      events[i]->add(item);
    }
  }
  i++;
}
