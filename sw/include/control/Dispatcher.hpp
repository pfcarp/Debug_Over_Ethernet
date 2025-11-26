#pragma once


#include "Packet.hpp"
#include "WatchPoints.hpp"
#include "Events.hpp"


class Dispatcher {

  private:
    double i = 0.0; // TEMPORARY!
    // Attributes
    Events& events;
    WatchPoints& watchpoints;

  public:
    // Methods
    Dispatcher(WatchPoints& watchpoints, Events& events);
    void push(int source, Packet::Base packet);
    void push(int source, Packet::ShortAddress packet);
    void push(int source, Packet::LongAddress packet);
    void push(int source, Packet::Event packet);
    
};
