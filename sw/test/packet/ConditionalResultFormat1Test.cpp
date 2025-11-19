#include <doctest/doctest.h>
#include <cstdint>
#include <vector>


#include "Packet.hpp"


TEST_CASE("ConditionalResultFormat1 packet: just KEY0") {
  // Encoding
  std::vector<uint8_t> encoding = {0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x0A};
  // Packet under construction
  Packet::ConditionalResultFormat1 obj(0b01101110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("ConditionalResultFormat1 packet: KEY0 and KEY1") {
  // Encoding
  std::vector<uint8_t> encoding = {0x80, 0x81, 0x82, 0x83, 0x84, 0x05, 0x86, 0x87, 0x88, 0x89, 0x0A};
  // Packet under construction
  Packet::ConditionalResultFormat1 obj(0b01101010);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("ConditionalResultFormat1 packet: KEY0 and KEY1 with short step") {
  // Encoding
  std::vector<uint8_t> encoding = {0x65, 0x7A};
  // Packet under construction
  Packet::ConditionalResultFormat1 obj(0b01101010);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}
