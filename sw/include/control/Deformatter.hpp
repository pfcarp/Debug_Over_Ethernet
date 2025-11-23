#pragma once


#include <vector>
#include <cstdint>


#include "Stream.hpp"


class Deformatter {

  private:
    // Attributes
    uint8_t counter = 0;
    std::vector<uint8_t> frame = std::vector<uint8_t>(16);
    
  public:
    // Attributes
    bool insertInPrevious = false;
    uint8_t current = 0;
    uint8_t previous = 0;
    std::vector<Stream> streams = std::vector<Stream>(4);
    // Methods
    void insert(uint8_t byte);
    void format();
    void clean();

};

