#pragma once


#include <cstdint>
#include <string>


#include "Color.hpp"


class Event {

  public:
    bool current = false;
    uint32_t factor;
    std::string name;
    Color color;

    Event(std::string name, Color color = Color(), bool current = false);
    virtual bool matches(uint64_t attempt);
    void setAsCurrent();
    void setAsNotCurrent();
    void setFactor(uint32_t f);

};
