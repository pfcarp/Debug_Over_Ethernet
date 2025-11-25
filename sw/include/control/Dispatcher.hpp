#pragma once


#include "Packet.hpp"
#include "Collection.hpp"


class Dispatcher {

  protected:
    // Attributes
    Collection events = Collection();
    Collection watchpoints = Collection();

  public:
    // Methods
    void push(int source, Packet::Base packet);
    
};
