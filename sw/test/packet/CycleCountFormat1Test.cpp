#include <doctest/doctest.h>
#include <cstdint>
#include <vector>


#include "Packet.hpp"


TEST_CASE("CycleCountFormat1 packet: unknown (i.e., no COUNT)") {
  // Encoding
  std::vector<uint8_t> encoding = {0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x0A};
  // Packet under construction
  Packet::CycleCountFormat1 obj(0b00001110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("CycleCountFormat1 packet: known (i.e., with partial COUNT)") {
  // Encoding
  std::vector<uint8_t> encoding = {0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x0A, 0x9A, 0x0B};
  // Packet under construction
  Packet::CycleCountFormat1 obj(0b00001111);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("CycleCountFormat1 packet: known (i.e., with full COUNT)") {
  // Encoding
  std::vector<uint8_t> encoding = {0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x0A, 0x9A, 0x9B, 0x3C};
  // Packet under construction
  Packet::CycleCountFormat1 obj(0b00001111);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}
