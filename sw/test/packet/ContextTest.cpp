#include <doctest/doctest.h>
#include <cstdint>
#include <vector>


#include "Packet.hpp"


TEST_CASE("Context packet: no payload") {
  // Packet under construction
  Packet::Context obj(0b10000000);

  // Done/ready upon creation
  CHECK(obj.isDone());
}

TEST_CASE("Context packet: with VMID and CONTEXTID") {
  // Encoding
  std::vector<uint8_t> encoding = {0xCF, 0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87};
  // Packet under construction
  Packet::Context obj(0b10000001);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Context packet: no VMID and CONTEXTID") {
  // Encoding
  std::vector<uint8_t> encoding = {0x0F};
  // Packet under construction
  Packet::Context obj(0b10000001);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Context packet: with VMID and no CONTEXTID") {
  // Encoding
  std::vector<uint8_t> encoding = {0x4F, 0x80, 0x81, 0x82, 0x83};
  // Packet under construction
  Packet::Context obj(0b10000001);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Context packet: no VMID and with CONTEXTID") {
  // Encoding
  std::vector<uint8_t> encoding = {0x8F, 0x84, 0x85, 0x86, 0x87};
  // Packet under construction
  Packet::Context obj(0b10000001);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

