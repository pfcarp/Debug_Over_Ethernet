#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("ShortAddress packet: 1 beat") {
  // Encoding
  std::vector<uint8_t> encoding = {0x7B};
  // Packet under construction
  Packet::ShortAddress obj(0b10010110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("ShortAddress packet: 2 beats") {
  // Encoding
  std::vector<uint8_t> encoding = {0xAB, 0xDB};
  // Packet under construction
  Packet::ShortAddress obj(0b10010110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}
