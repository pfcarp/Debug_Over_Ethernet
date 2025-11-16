#pragma once


#include "Event.hpp"
#include "Collection.hpp"


class Dispatcher {

  protected:
    // Attributes
    Collection events = Collection();
    Collection watchpoints = Collection();

  public:
    // Methods
    void add(Packet::Event event)
    
};
