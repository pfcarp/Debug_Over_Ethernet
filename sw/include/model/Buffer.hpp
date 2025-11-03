#pragma once


#include <string>


#include "TimedData.hpp"
#include "Event.hpp"


class Buffer {

  public:
    Event* event = NULL; 
    std::string style = "-";
    bool show = false;
    bool cumulative = false;
    std::string name = "";

    Buffer(Event* event, std::string style = "-"): event(event), style(style) {
      show = true;
    }
    virtual ~Buffer() = default;
    virtual TimedData at(size_t index) const = 0;
    virtual void add(TimedData item) = 0;
    virtual double ymin() const = 0;
    virtual double ymax() const = 0;
    virtual double xmin() const = 0;
    virtual double xmax() const = 0;
    virtual size_t size() const = 0;
    virtual void clear() = 0;
};
