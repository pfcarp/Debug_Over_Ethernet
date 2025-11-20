#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("ShortAddress packet: IS0 1 beat") {
  // Encoding
  std::vector<uint8_t> encoding = {0x7B};
  // Packet under construction
  Packet::ShortAddress obj(0b10010101);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());

  // Check address
  CHECK(obj.getAddress() == 0x000001EC);
}

TEST_CASE("ShortAddress packet: IS0 2 beats") {
  // Encoding
  std::vector<uint8_t> encoding = {0xFB, 0xDB};
  // Packet under construction
  Packet::ShortAddress obj(0b10010101);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());

  // Check address
  CHECK(obj.getAddress() == 0x0001B7EC);
}

TEST_CASE("ShortAddress packet: IS1 1 beat") {
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

  // Check address
  CHECK(obj.getAddress() == 0x000000F6);
}

TEST_CASE("ShortAddress packet: IS1 2 beats") {
  // Encoding
  std::vector<uint8_t> encoding = {0xFB, 0xDB};
  // Packet under construction
  Packet::ShortAddress obj(0b10010110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());

  // Check address
  CHECK(obj.getAddress() == 0x0000DBF6);
}
