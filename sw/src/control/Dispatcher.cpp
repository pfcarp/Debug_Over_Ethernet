#include "Dispatcher.hpp"
#include <iostream>

Dispatcher::Dispatcher(WatchPoints& watchpoints, Events& events, Points& points): watchpoints(watchpoints), events(events), points(points) {}

void Dispatcher::push(int source, Packet::Base packet) {
  // std::cout<<packet.asString()<<"b!"<<typeid(events).name()<<std::endl;
 
  // watchpoints.push(source, packet);
  // events.push(source, packet);
  // points.push(source, packet);
}

void Dispatcher::push(int source, Packet::Event packet) {
  // std::cout<<packet.asString()<<"b!"<<typeid(events).name()<<std::endl;
  
  events.push(source, packet);
  points.push(source, packet);
}
