#pragma once


#include "Packet.hpp"
#include "WatchPoints.hpp"
#include "Events.hpp"
#include "Points.hpp"


class Dispatcher {

  private:
    // Attributes
    Events& events;
    WatchPoints& watchpoints;
    Points& points;

  public:
    // Methods
    Dispatcher(WatchPoints& watchpoints, Events& events, Points& points);
    void push(int source, Packet::Base packet);
    
};
