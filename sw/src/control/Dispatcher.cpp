#include "Dispatcher.hpp"


Dispatcher::Dispatcher(WatchPoints& watchpoints, Events& events, Points& points): watchpoints(watchpoints), events(events), points(points) {}

void Dispatcher::push(int source, Packet::Base packet) {
  watchpoints.push(source, packet);
  events.push(source, packet);
  points.push(source, packet);
}
