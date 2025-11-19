#include <doctest/doctest.h>
#include <cstdint>
#include <vector>


#include "Packet.hpp"


TEST_CASE("AddressWithContext packet: short address and no payload") {
  // Encoding
  std::vector<uint8_t> encoding = {0x11, 0x22, 0x33, 0x44, 0x0F};
  // Packet under construction
  Packet::AddressWithContext obj(0b10000011);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("AddressWithContext packet: short address, with VMID and CONTEXTID") {
  // Encoding
  std::vector<uint8_t> encoding = {0x11, 0x22, 0x33, 0x44, 0xCF, 0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87};
  // Packet under construction
  Packet::AddressWithContext obj(0b10000011);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("AddressWithContext packet: short address, with no VMID and no CONTEXTID") {
  // Encoding
  std::vector<uint8_t> encoding = {0x11, 0x22, 0x33, 0x44, 0x0F};
  // Packet under construction
  Packet::AddressWithContext obj(0b10000011);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("AddressWithContext packet: short address, with VMID and no CONTEXTID") {
  // Encoding
  std::vector<uint8_t> encoding = {0x11, 0x22, 0x33, 0x44, 0x4F, 0x80, 0x81, 0x82, 0x83};
  // Packet under construction
  Packet::AddressWithContext obj(0b10000011);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("AddressWithContext packet: short address, no VMID and with CONTEXTID") {
  // Encoding
  std::vector<uint8_t> encoding = {0x11, 0x22, 0x33, 0x44, 0x8F, 0x84, 0x85, 0x86, 0x87};
  // Packet under construction
  Packet::AddressWithContext obj(0b10000011);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("AddressWithContext packet: long address and no payload") {
  // Encoding
  std::vector<uint8_t> encoding = {0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x0F};
  // Packet under construction
  Packet::AddressWithContext obj(0b10000110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("AddressWithContext packet: long address, with VMID and CONTEXTID") {
  // Encoding
  std::vector<uint8_t> encoding = {0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0xCF, 0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87};
  // Packet under construction
  Packet::AddressWithContext obj(0b10000110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("AddressWithContext packet: long address, with no VMID and no CONTEXTID") {
  // Encoding
  std::vector<uint8_t> encoding = {0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x0F};
  // Packet under construction
  Packet::AddressWithContext obj(0b10000110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("AddressWithContext packet: long address, with VMID and no CONTEXTID") {
  // Encoding
  std::vector<uint8_t> encoding = {0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x4F, 0x80, 0x81, 0x82, 0x83};
  // Packet under construction
  Packet::AddressWithContext obj(0b10000110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("AddressWithContext packet: long address, no VMID and with CONTEXTID") {
  // Encoding
  std::vector<uint8_t> encoding = {0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x8F, 0x84, 0x85, 0x86, 0x87};
  // Packet under construction
  Packet::AddressWithContext obj(0b10000110);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}
