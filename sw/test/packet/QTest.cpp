#include <doctest/doctest.h>
#include <vector>
#include <cstdint>


#include "Packet.hpp"


TEST_CASE("Q packet: no address and no count") {
  // Packet under construction
  Packet::Q obj(0b10101111);

  // Finally ready
  CHECK(obj.isDone());
  // Compare address
  CHECK(obj.getAddress() == 0x0000000000000000);
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
  // Compare address
  CHECK(obj.getAddress() == 0x0000000000000000);
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
  // Compare address
  CHECK(obj.getAddress() == 0x0000000000000000);
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
  // Compare address
  CHECK(obj.getAddress() == 0x0000000000000000);
}

TEST_CASE("Q packet: short address IS0 (1 beat) and count (v1)") {
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
  // Compare address
  CHECK(obj.getAddress() == 0x00000000000001AC);
}

TEST_CASE("Q packet: short address IS0 (2 beat) and count (v1)") {
  // Encoding
  std::vector<uint8_t> encoding = {0xFB, 0xDF, 0xAB, 0xDB, 0x79};
  // Packet under construction
  Packet::Q obj(0b10100101);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
  // Compare address
  CHECK(obj.getOffset() == 2);
  CHECK(obj.getAddress() == 0x000000000001BFEC);
}

TEST_CASE("Q packet: short address IS0 (1 beat) and count (v2)") {
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
  // Compare address
  CHECK(obj.getAddress() == 0x00000000000001AC);
}

TEST_CASE("Q packet: short address IS0 (2 beat) and count (v2)") {
  // Encoding
  std::vector<uint8_t> encoding = {0xFB, 0xDF, 0xAB, 0xDB, 0x80, 0x87, 0x79};
  // Packet under construction
  Packet::Q obj(0b10100101);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
  // Compare address
  CHECK(obj.getAddress() == 0x000000000001BFEC);
}

TEST_CASE("Q packet: short address IS0 (1 beat) and count (v3)") {
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
  // Compare address
  CHECK(obj.getAddress() == 0x00000000000001AC);
}

TEST_CASE("Q packet: short address IS0 (2 beat) and count (v3)") {
  // Encoding
  std::vector<uint8_t> encoding = {0xFB, 0xDF, 0xAB, 0xDB, 0x80, 0x87, 0x85, 0x9A, 0x79};
  // Packet under construction
  Packet::Q obj(0b10100101);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
  // Compare address
  CHECK(obj.getAddress() == 0x000000000001BFEC);
}

TEST_CASE("Q packet: short address IS1 (1 beat) and count (v1)") {
  // Encoding
  std::vector<uint8_t> encoding = {0x6B, 0xAB, 0xDB, 0x79};
  // Packet under construction
  Packet::Q obj(0b10100110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
  // Compare address
  CHECK(obj.getAddress() == 0x00000000000000D6);
}

TEST_CASE("Q packet: short address IS1 (2 beat) and count (v1)") {
  // Encoding
  std::vector<uint8_t> encoding = {0xFB, 0xDF, 0xAB, 0xDB, 0x79};
  // Packet under construction
  Packet::Q obj(0b10100110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
  // Compare address
  CHECK(obj.getAddress() == 0x000000000000DFF6);
}

TEST_CASE("Q packet: short address IS1 (1 beat) and count (v2)") {
  // Encoding
  std::vector<uint8_t> encoding = {0x6B, 0xAB, 0xDB, 0x80, 0x87, 0x79};
  // Packet under construction
  Packet::Q obj(0b10100110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
  // Compare address
  CHECK(obj.getAddress() == 0x00000000000000D6);
}

TEST_CASE("Q packet: short address IS1 (2 beat) and count (v2)") {
  // Encoding
  std::vector<uint8_t> encoding = {0xFB, 0xDF, 0xAB, 0xDB, 0x80, 0x87, 0x79};
  // Packet under construction
  Packet::Q obj(0b10100110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
  // Compare address
  CHECK(obj.getAddress() == 0x000000000000DFF6);
}

TEST_CASE("Q packet: short address IS1 (1 beat) and count (v3)") {
  // Encoding
  std::vector<uint8_t> encoding = {0x6B, 0xAB, 0xDB, 0x80, 0x87, 0x85, 0x9A, 0x79};
  // Packet under construction
  Packet::Q obj(0b10100110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
  // Compare address
  CHECK(obj.getAddress() == 0x00000000000000D6);
}

TEST_CASE("Q packet: short address IS1 (2 beat) and count (v3)") {
  // Encoding
  std::vector<uint8_t> encoding = {0xFB, 0xDF, 0xAB, 0xDB, 0x80, 0x87, 0x85, 0x9A, 0x79};
  // Packet under construction
  Packet::Q obj(0b10100110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
  // Compare address
  CHECK(obj.getAddress() == 0x000000000000DFF6);
}

TEST_CASE("Q packet: long address IS0 and count (v1)") {
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
  // Compare address
  CHECK(obj.getAddress() == 0x000000009E8DF9AC);
}

TEST_CASE("Q packet: long address IS0 and count (v2)") {
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
  // Compare address
  CHECK(obj.getAddress() == 0x000000009E8DF9AC);
}

TEST_CASE("Q packet: long address IS0 and count (v3)") {
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
  // Compare address
  CHECK(obj.getAddress() == 0x000000009E8DF9AC);
}

TEST_CASE("Q packet: long address IS1 and count (v1)") {
  // Encoding
  std::vector<uint8_t> encoding = {0x6B, 0x7C, 0x8D, 0x9E, 0xAB, 0xDB, 0x79};
  // Packet under construction
  Packet::Q obj(0b10101011);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
  // Compare address
  CHECK(obj.getAddress() == 0x000000009E8D7CD6);
}

TEST_CASE("Q packet: long address IS1 and count (v2)") {
  // Encoding
  std::vector<uint8_t> encoding = {0x6B, 0x7C, 0x8D, 0x9E, 0xAB, 0xDB, 0x80, 0x87, 0x79};
  // Packet under construction
  Packet::Q obj(0b10101011);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
  // Compare address
  CHECK(obj.getAddress() == 0x000000009E8D7CD6);
}

TEST_CASE("Q packet: long address IS1 and count (v3)") {
  // Encoding
  std::vector<uint8_t> encoding = {0x6B, 0x7C, 0x8D, 0x9E, 0xAB, 0xDB, 0x80, 0x87, 0x85, 0x9A, 0x79};
  // Packet under construction
  Packet::Q obj(0b10101011);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
  // Compare address
  CHECK(obj.getAddress() == 0x000000009E8D7CD6);
}

