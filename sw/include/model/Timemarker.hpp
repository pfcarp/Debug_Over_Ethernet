#pragma once


#include <cstdint>
#include <string>


#include "Color.hpp"


class Timemarker {
  
  private:
    // Attributes
    uint64_t time;
    Color color;
    std::string name;
    // Methods

  public:
    // Attributes
    // Methods
    Timemarker(uint64_t time, Color color, std::string name);
    const uint64_t& getTime() const;
    const Color& getColor() const;
    const std::string& getName() const;
    bool operator<(const Timemarker& other) const;
    bool operator<(const uint64_t& other) const;

};
