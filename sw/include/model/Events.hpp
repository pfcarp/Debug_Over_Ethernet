#pragma once


#include <cstdlib>
#include <vector>


#include "Collection.hpp"


class Events: public Collection {


  public:
    Events(std::vector<Event*> events);
    ~Events();
    Buffer* operator[](size_t index) override;

};
