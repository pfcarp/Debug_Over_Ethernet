#include <doctest/doctest.h>
#include <cstdint>
#include <vector>


#include "Packet.hpp"


TEST_CASE("ASync packet length is 1+11 (i.e., 0-11 -> false; 12 -> true)") {
  // Encoding
  std::vector<uint8_t> encoding = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80};
  // Packet under construction
  Packet::Extension obj(0b00000000);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Discard") {
  // Encoding
  std::vector<uint8_t> encoding = {0x3};
  // Packet under construction
  Packet::Extension obj(0b00000000);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Overflow") {
  // Encoding
  std::vector<uint8_t> encoding = {0x5};
  // Packet under construction
  Packet::Extension obj(0b00000000);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Branch future flush") {
  // Encoding
  std::vector<uint8_t> encoding = {0x7};
  // Packet under construction
  Packet::Extension obj(0b00000000);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

