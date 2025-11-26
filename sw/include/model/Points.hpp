#pragma once


#include <vector>


#include "Collection.hpp"
#include "WatchPoint.hpp"


class Points: public Collection {

  private:
    // Attributes    
    std::vector<WatchPoint*>& watchpoints;
    double instructions = 0.0;
    double cacheline_refill = 0.0;
    double time = 0.0;
    // Methods
    void archives();

  public:
    Points(std::vector<WatchPoint*> watchpoints);
    ~Points();
    Buffer* operator[](size_t index) override;
    void clear() override;
    void push(int source, Packet::Base& packet) override;
    void push(int source, Packet::ShortAddress& packet) override;
    void push(int source, Packet::LongAddress& packet) override;
    void push(int source, Packet::Event& packet) override;

};
