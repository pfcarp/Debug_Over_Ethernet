#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("Exception packet: header E0 1 beat") {
  // Encoding
  std::vector<uint8_t> encoding = {0x3F};
  // Packet under construction
  Packet::Exception obj(0b00000110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
  CHECK(obj.getType() == 31);
  CHECK(obj.getE0() == 1);
  CHECK(obj.getE1() == 0);
  CHECK(obj.getP() == 0xff);
}

TEST_CASE("Exception packet: header E1 1 beat") {
  // Encoding
  std::vector<uint8_t> encoding = {0x7E};
  // Packet under construction
  Packet::Exception obj(0b00000110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Exception packet: header E0 2 beats") {
  // Encoding
  std::vector<uint8_t> encoding = {0xBF, 0x37};
  // Packet under construction
  Packet::Exception obj(0b00000110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Exception packet: header E1 2 beats") {
  // Encoding
  std::vector<uint8_t> encoding = {0xFE, 0x37};
  // Packet under construction
  Packet::Exception obj(0b00000110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Exception packet: header E0 1 beat") {
  // Encoding
  std::vector<uint8_t> encoding = {0x3F};
  // Packet under construction
  Packet::Exception obj(0b00000110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Exception packet: header E1 1 beat") {
  // Encoding
  std::vector<uint8_t> encoding = {0x7E};
  // Packet under construction
  Packet::Exception obj(0b00000110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Exception packet: header E0 2 beats") {
  // Encoding
  std::vector<uint8_t> encoding = {0xBF, 0x37};
  // Packet under construction
  Packet::Exception obj(0b00000110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Exception packet: header E1 2 beats") {
  // Encoding
  std::vector<uint8_t> encoding = {0xFE, 0x37};
  // Packet under construction
  Packet::Exception obj(0b00000110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}
