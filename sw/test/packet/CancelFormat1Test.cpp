#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("CancelFormat1 packet: N steps") {
  // Encoding
  std::vector<uint8_t> encoding = {0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x0A};
  // Packet under construction
  Packet::CancelFormat1 obj(0b00101110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("CancelFormat1 packet: 1 step") {
  // Encoding
  std::vector<uint8_t> encoding = {0x0A};
  // Packet under construction
  Packet::CancelFormat1 obj(0b00101110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

