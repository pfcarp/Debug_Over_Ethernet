#pragma once


#include <vector>
#include <cstdint>


#include "Stream.hpp"


class Deformatter {

  private:
    // Attributes
    bool insertInPrevious = false;
    uint8_t current = 0;
    uint8_t previous = 0;
    uint8_t counter = 0;
    // Methods
    void format();
    void clean();
    
  public:
    // Attributes
    std::vector<Stream> streams = std::vector<Stream>(4);
    std::vector<uint8_t> frame = std::vector<uint8_t>(16); // Needed for tests...
    // Methods
    bool insert(uint8_t byte);
    bool toInsertInPrevious(const uint8_t aux, const uint8_t offset) const;

};

