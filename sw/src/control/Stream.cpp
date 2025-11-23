#include "Stream.hpp"


void Stream::insert(uint8_t byte) {
  if (factory.insert(byte)) {
    packets.push_back(factory.get());
  }
}
