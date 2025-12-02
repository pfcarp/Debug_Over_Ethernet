#pragma once


#include <cstdint>
#include <string>


#include "Event.hpp"


class WatchPoint: public Event {

  private:
    uint64_t lower = 0;
    uint64_t upper = 0;

  public:
    WatchPoint(std::string name, uint64_t lower, uint64_t upper);
    virtual bool matches(uint64_t attempt) override;
  
};
