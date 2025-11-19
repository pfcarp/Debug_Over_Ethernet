#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("Exception packet: header E0 1 beat, short address 1 beat") {
  // Encoding
  std::vector<uint8_t> encoding = {0x3F, 0x95, 0x7B};
  // Packet under construction
  Packet::Exception obj;

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Exception packet: header E1 1 beat, short address 2 beats") {
  // Encoding
  std::vector<uint8_t> encoding = {0x7E, 0x95, 0xAB, 0xDB};
  // Packet under construction
  Packet::Exception obj;

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Exception packet: header E0 2 beats, short address 1 beat") {
  // Encoding
  std::vector<uint8_t> encoding = {0xBF, 0x37, 0x95, 0x7B};
  // Packet under construction
  Packet::Exception obj;

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Exception packet: header E1 2 beats, short address 2 beats") {
  // Encoding
  std::vector<uint8_t> encoding = {0xFE, 0x37, 0x95, 0xAB, 0xDB};
  // Packet under construction
  Packet::Exception obj;

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Exception packet: header E0 1 beat, long address 4 beats") {
  // Encoding
  std::vector<uint8_t> encoding = {0x3F, 0x9A, 0x80, 0x81, 0x82, 0x83};
  // Packet under construction
  Packet::Exception obj;

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Exception packet: header E1 1 beat, long address 4 beats") {
  // Encoding
  std::vector<uint8_t> encoding = {0x7E, 0x9A, 0x80, 0x81, 0x82, 0x83};
  // Packet under construction
  Packet::Exception obj;

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Exception packet: header E0 2 beats, long address 8 beat") {
  // Encoding
  std::vector<uint8_t> encoding = {0xBF, 0x37, 0x9D, 0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87};
  // Packet under construction
  Packet::Exception obj;

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Exception packet: header E1 2 beats, long address 8 beats") {
  // Encoding
  std::vector<uint8_t> encoding = {0xFE, 0x37, 0x9D, 0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87};
  // Packet under construction
  Packet::Exception obj;

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}
