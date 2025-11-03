#pragma once


#include <string>


#include "Color.hpp"


class Event {

  public:
    std::string name;
    Color color;

    Event(std::string name, Color color = Color());
};
