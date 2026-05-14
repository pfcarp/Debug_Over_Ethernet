#include <doctest/doctest.h>
#include <cstdint>
#include <vector>


#include "Packet.hpp"


TEST_CASE("Timestamp: no count + 1 stamp step") {
  // Encoding
  std::vector<uint8_t> encoding = {0x0A};
  // Packet under construction
  Packet::Timestamp obj(0b00000010);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Timestamp: no count + N stamp steps") {
  // Encoding
  std::vector<uint8_t> encoding = {0x8A, 0x80, 0x8C, 0x89, 0x85, 0x07};
  // Packet under construction
  Packet::Timestamp obj(0b00000010);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Timestamp: no count + 8 stamp steps") {
  // Encoding
  std::vector<uint8_t> encoding = {0x8A, 0x84, 0x82, 0x80, 0x8C, 0x89, 0x85, 0x07};
  // Packet under construction
  Packet::Timestamp obj(0b00000010);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Timestamp: with full count + 1 stamp step") {
  // Encoding
  std::vector<uint8_t> encoding = {0x0A, 0x80, 0x8C, 0x07};
  // Packet under construction
  Packet::Timestamp obj(0b00000011);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Timestamp: with full count + N stamp steps") {
  // Encoding
  std::vector<uint8_t> encoding = {0x8A, 0x80, 0x8C, 0x89, 0x85, 0x07, 0x80, 0x8C, 0x07};
  // Packet under construction
  Packet::Timestamp obj(0b00000011);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Timestamp: with partial count + 1 stamp step") {
  // Encoding
  std::vector<uint8_t> encoding = {0x0A, 0x07};
  // Packet under construction
  Packet::Timestamp obj(0b00000011);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Timestamp: with partial count + N stamp steps") {
  // Encoding
  std::vector<uint8_t> encoding = {0x8A, 0x80, 0x8C, 0x89, 0x85, 0x07, 0x80, 0x07};
  // Packet under construction
  Packet::Timestamp obj(0b00000011);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

