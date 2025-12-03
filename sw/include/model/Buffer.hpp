#pragma once


#include <string>
#include <mutex>


#include "TimedData.hpp"
#include "Event.hpp"


class Buffer {

  protected:
    std::mutex m;

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
    virtual TimedData at(size_t index) = 0;
    virtual void add(TimedData item) = 0;
    virtual double ymin() = 0;
    virtual double ymax() = 0;
    virtual double xmin() = 0;
    virtual double xmax() = 0;
    virtual size_t size() = 0;
    virtual void clear() = 0;
};
