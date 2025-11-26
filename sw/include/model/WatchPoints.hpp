#pragma once


#include "Collection.hpp"
#include "WatchPoint.hpp"


class WatchPoints: public Collection {


  public:
    WatchPoints(std::vector<WatchPoint*> watchpoints);
    ~WatchPoints();
    Buffer* operator[](size_t index) override;

};
