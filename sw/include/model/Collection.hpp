#pragma once


#include <vector>


#include "Packet.hpp"
#include "Buffer.hpp"


class Collection {

  public:
    // Attributes
    std::vector<Buffer*> buffers;
    // methods
    double xmin();
    double xmax();
    double ymin();
    double ymax();
    void add(Buffer* buffer);
    unsigned amount();
    virtual Buffer* operator[](size_t index);
    virtual void clear();
    virtual void push(int source, Packet::Base& packet) {};
    virtual void push(int source, Packet::ShortAddress& packet) {};
    virtual void push(int source, Packet::LongAddress& packet) {};
    virtual void push(int source, Packet::Event& packet) {};

};
