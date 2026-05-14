#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("CycleCountFormat2 packets are 'done' right after construction") {
  // Encoding
  std::vector<uint8_t> encoding = {0xAB};
  // Packet under construction
  Packet::CycleCountFormat2 obj(0x0);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("CycleCountFormat2 packets are always 'done'") {
  // Encoding
  std::vector<uint8_t> encoding = {0xAB};
  // Packet under construction
  Packet::CycleCountFormat2 obj(0x1);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}
