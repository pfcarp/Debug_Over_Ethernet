#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("LongAddress packet: IS0 4 beat") {
  // Encoding
  std::vector<uint8_t> encoding = {0x80, 0x81, 0x82, 0x83};
  // Packet under construction
  Packet::LongAddress obj(0b10011010);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());

  // Check address
  CHECK(obj.getAddress() == 0x0000000083820200);
}

TEST_CASE("LongAddress packet: IS1 4 beat") {
  // Encoding
  std::vector<uint8_t> encoding = {0x80, 0x81, 0x82, 0x83};
  // Packet under construction
  Packet::LongAddress obj(0b10011011);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());

  // Check address
  CHECK(obj.getAddress() == 0x0000000083828100);
}

TEST_CASE("LongAddress packet: IS0 8 beats") {
  // Encoding
  std::vector<uint8_t> encoding = {0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87};
  // Packet under construction
  Packet::LongAddress obj(0b10011101);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());

  // Check address
  CHECK(obj.getAddress() == 0x8786858483820200);
}

TEST_CASE("LongAddress packet: IS1 8 beats") {
  // Encoding
  std::vector<uint8_t> encoding = {0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87};
  // Packet under construction
  Packet::LongAddress obj(0b10011110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());

  // Check address
  CHECK(obj.getAddress() == 0x8786858483828100);
}
