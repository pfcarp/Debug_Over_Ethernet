#include "Stream.hpp"


#include <iostream>


void Stream::insert(uint8_t byte) {
  if (factory.insert(byte)) {
    packets.push_back(factory.get());
  }
}

Stream::~Stream() {
  std::cout << "STREAM -----------------------" << std::endl;
  for (const auto& packet : packets)
    std::cout << packet->asString() << std::endl;
}
