#pragma once


#include "Collection.hpp"


class Points: public Collection {


  public:
    Points(Event* current);
    ~Points();
    Buffer* operator[](size_t index) override;
    void archives();
};
