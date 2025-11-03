#pragma once


#include <vector>


#include "Buffer.hpp"


class Collection {

  protected:
    std::vector<Buffer*> buffers;

  public:
    
    double xmin();
    double xmax();
    double ymin();
    double ymax();
    void add(Buffer* buffer);
    unsigned amount();
    virtual Buffer* operator[](size_t index);
    virtual void clear();

};
