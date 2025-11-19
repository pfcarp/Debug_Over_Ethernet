#include <doctest/doctest.h>
#include <vector>
#include <cstdint>


#include "Packet.hpp"


TEST_CASE("Q packet: no address and no count") {
  // Packet under construction
  Packet::Q obj(0b10101111);

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Q packet: no address and count (v1)") {
  // Encoding
  std::vector<uint8_t> encoding = {0xAB, 0xDB, 0x79};
  // Packet under construction
  Packet::Q obj(0b10100000);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Q packet: no address and count (v2)") {
  // Encoding
  std::vector<uint8_t> encoding = {0xAB, 0xDB, 0x80, 0x87, 0x79};
  // Packet under construction
  Packet::Q obj(0b10100001);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Q packet: no address and count (v3)") {
  // Encoding
  std::vector<uint8_t> encoding = {0xAB, 0xDB, 0x80, 0x87, 0x85, 0x9A, 0x79};
  // Packet under construction
  Packet::Q obj(0b10100010);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Q packet: short address (1 beat) and count (v1)") {
  // Encoding
  std::vector<uint8_t> encoding = {0x6B, 0xAB, 0xDB, 0x79};
  // Packet under construction
  Packet::Q obj(0b10100101);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Q packet: short address (2 beat) and count (v1)") {
  // Encoding
  std::vector<uint8_t> encoding = {0x7B, 0xDF, 0xAB, 0xDB, 0x79};
  // Packet under construction
  Packet::Q obj(0b10100101);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Q packet: short address (1 beat) and count (v2)") {
  // Encoding
  std::vector<uint8_t> encoding = {0x6B, 0xAB, 0xDB, 0x80, 0x87, 0x79};
  // Packet under construction
  Packet::Q obj(0b10100101);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Q packet: short address (2 beat) and count (v2)") {
  // Encoding
  std::vector<uint8_t> encoding = {0x7B, 0xDF, 0xAB, 0xDB, 0x80, 0x87, 0x79};
  // Packet under construction
  Packet::Q obj(0b10100101);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Q packet: short address (1 beat) and count (v3)") {
  // Encoding
  std::vector<uint8_t> encoding = {0x6B, 0xAB, 0xDB, 0x80, 0x87, 0x85, 0x9A, 0x79};
  // Packet under construction
  Packet::Q obj(0b10100101);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Q packet: short address (2 beat) and count (v3)") {
  // Encoding
  std::vector<uint8_t> encoding = {0x7B, 0xDF, 0xAB, 0xDB, 0x80, 0x87, 0x85, 0x9A, 0x79};
  // Packet under construction
  Packet::Q obj(0b10100101);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Q packet: long address and count (v1)") {
  // Encoding
  std::vector<uint8_t> encoding = {0x6B, 0x7C, 0x8D, 0x9E, 0xAB, 0xDB, 0x79};
  // Packet under construction
  Packet::Q obj(0b10101010);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Q packet: long address and count (v2)") {
  // Encoding
  std::vector<uint8_t> encoding = {0x6B, 0x7C, 0x8D, 0x9E, 0xAB, 0xDB, 0x80, 0x87, 0x79};
  // Packet under construction
  Packet::Q obj(0b10101010);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Q packet: long address and count (v3)") {
  // Encoding
  std::vector<uint8_t> encoding = {0x6B, 0x7C, 0x8D, 0x9E, 0xAB, 0xDB, 0x80, 0x87, 0x85, 0x9A, 0x79};
  // Packet under construction
  Packet::Q obj(0b10101010);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

