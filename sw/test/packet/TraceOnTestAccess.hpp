#pragma once


#include <cstdint>


#include "Packet.hpp"


struct TraceOnTestAccess {

  static uint8_t iterator(Packet::TraceOn& obj) {
    return obj.getIterator();
  }

};
