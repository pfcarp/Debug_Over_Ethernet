#pragma once


#include <cstdint>
#include <string>


#include "Color.hpp"


class Event {

  public:
    std::string name;
    Color color;

    Event(std::string name, Color color = Color());
    virtual bool matches(uint64_t attempt);

};
