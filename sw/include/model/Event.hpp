#pragma once


#include <cstdint>
#include <string>


#include "Color.hpp"


class Event {

  public:
    bool current = false;
    std::string name;
    Color color;

    Event(std::string name, Color color = Color(), bool current = false);
    virtual bool matches(uint64_t attempt);
    void setAsCurrent();
    void setAsNotCurrent();

};
