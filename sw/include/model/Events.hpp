#pragma once


#include <cstdlib>
#include <vector>


#include "Collection.hpp"


class Events: public Collection {


  public:
    // Attributes
    std::vector<Event*> events;
    // Methods
    Events(std::vector<Event*> events);
    ~Events();
    Buffer* operator[](size_t index) override;
    void push(int source, Packet::Base& packet) override;
    void push(int source, Packet::Event& packet) override;

};
