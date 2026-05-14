#pragma once


#include "Collection.hpp"
#include "WatchPoint.hpp"
#include "Packet.hpp"


class WatchPoints: public Collection {


  public:
    WatchPoints(std::vector<WatchPoint*> watchpoints);
    ~WatchPoints();
    Buffer* operator[](size_t index) override;
    void push(int source, Packet::Base& packet) override;
    void push(int source, Packet::ShortAddress& packet) override;
    void push(int source, Packet::LongAddress& packet) override;
    void push(int source, Packet::AddressWithContext& packet) override;

};
