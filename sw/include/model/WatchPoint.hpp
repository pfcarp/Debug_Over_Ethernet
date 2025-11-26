#pragma once


#include <cstdint>
#include <string>


#include "Event.hpp"


class WatchPoint: public Event {

  private:
    uint64_t address = 0;

  public:
    WatchPoint(std::string name, uint64_t address);
    virtual bool matches(uint64_t attempt) override;
  
};
