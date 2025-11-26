#pragma once


#include "Collection.hpp"


class Points: public Collection {


  public:
    Points();
    ~Points();
    Buffer* operator[](size_t index) override;
    void clear() override;
    void archives();

};
