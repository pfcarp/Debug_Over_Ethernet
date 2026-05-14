#include <doctest/doctest.h>
#include <cstdint>
#include <vector>


#include "Packet.hpp"


TEST_CASE("Trace info (!info, !key, !spec, !cyct): no payload after PLCTL") {
  // Encoding
  std::vector<uint8_t> encoding = {0b00000000};
  // Packet under construction
  Packet::TraceInfo obj(0b00000001);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
  CHECK(!obj.hasInfo());
  CHECK(!obj.hasKey());
  CHECK(!obj.hasSpec());
  CHECK(!obj.hasCyct());
}

TEST_CASE("Trace info (!info, !key, !spec, !cyct): no payload after PLCTL, several PLCTL steps") {
  // Encoding
  std::vector<uint8_t> encoding = {0b10000000, 0b10000000, 0b00000000};
  // Packet under construction
  Packet::TraceInfo obj(0b00000001);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
  CHECK(!obj.hasInfo());
  CHECK(!obj.hasKey());
  CHECK(!obj.hasSpec());
  CHECK(!obj.hasCyct());
}

TEST_CASE("Trace info (info, !key, !spec, !cyct)") {
  // Encoding
  std::vector<uint8_t> encoding = {0b00000001, 0x80, 0x84, 0x88, 0x8C, 0x10};
  // Packet under construction
  Packet::TraceInfo obj(0b00000001);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Trace info (!info, key, !spec, !cyct)") {
  // Encoding
  std::vector<uint8_t> encoding = {0b00000010, 0x81, 0x85, 0x89, 0x8D, 0x11};
  // Packet under construction
  Packet::TraceInfo obj(0b00000001);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Trace info (info, key, !spec, !cyct)") {
  // Encoding
  std::vector<uint8_t> encoding = {0b00000011, 0x80, 0x84, 0x88, 0x8C, 0x10, 0x81, 0x85, 0x89, 0x8D, 0x11};
  // Packet under construction
  Packet::TraceInfo obj(0b00000001);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Trace info (!info, !key, spec, !cyct)") {
  // Encoding
  std::vector<uint8_t> encoding = {0b00000100, 0x82, 0x86, 0x8A, 0x8D, 0x12};
  // Packet under construction
  Packet::TraceInfo obj(0b00000001);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Trace info (info, !key, spec, !cyct)") {
  // Encoding
  std::vector<uint8_t> encoding = {0b00000101, 0x80, 0x84, 0x88, 0x8C, 0x10, 0x82, 0x86, 0x8A, 0x8D, 0x12};
  // Packet under construction
  Packet::TraceInfo obj(0b00000001);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Trace info (!info, key, !spec, !cyct)") {
  // Encoding
  std::vector<uint8_t> encoding = {0b00000110, 0x81, 0x85, 0x89, 0x8D, 0x11, 0x82, 0x86, 0x8A, 0x8D, 0x12};
  // Packet under construction
  Packet::TraceInfo obj(0b00000001);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Trace info (info, key, spec, !cyct)") {
  // Encoding
  std::vector<uint8_t> encoding = {0b00000111, 0x80, 0x84, 0x88, 0x8C, 0x10, 0x81, 0x85, 0x89, 0x8D, 0x11, 0x82, 0x86, 0x8A, 0x8D, 0x12};
  // Packet under construction
  Packet::TraceInfo obj(0b00000001);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Trace info (!info, !key, !spec, cyct)") {
  // Encoding
  std::vector<uint8_t> encoding = {0b00001000, 0x83, 0x87, 0x8B, 0x8F, 0x13};
  // Packet under construction
  Packet::TraceInfo obj(0b00000001);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Trace info (!info, key, !spec, cyct)") {
  // Encoding
  std::vector<uint8_t> encoding = {0b00001010, 0x81, 0x85, 0x89, 0x8D, 0x11, 0x83, 0x87, 0x8B, 0x8F, 0x13};
  // Packet under construction
  Packet::TraceInfo obj(0b00000001);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Trace info (info, key, !spec, cyct)") {
  // Encoding
  std::vector<uint8_t> encoding = {0b00001011, 0x80, 0x84, 0x88, 0x8C, 0x10, 0x81, 0x85, 0x89, 0x8D, 0x11, 0x83, 0x87, 0x8B, 0x8F, 0x13};
  // Packet under construction
  Packet::TraceInfo obj(0b00000001);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Trace info (!info, !key, spec, cyct)") {
  // Encoding
  std::vector<uint8_t> encoding = {0b00001100, 0x82, 0x86, 0x8A, 0x8D, 0x12, 0x83, 0x87, 0x8B, 0x8F, 0x13};
  // Packet under construction
  Packet::TraceInfo obj(0b00000001);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Trace info (info, !key, spec, cyct)") {
  // Encoding
  std::vector<uint8_t> encoding = {0b00001101, 0x80, 0x84, 0x88, 0x8C, 0x10, 0x82, 0x86, 0x8A, 0x8D, 0x12, 0x83, 0x87, 0x8B, 0x8F, 0x13};
  // Packet under construction
  Packet::TraceInfo obj(0b00000001);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Trace info (!info, key, spec, cyct)") {
  // Encoding
  std::vector<uint8_t> encoding = {0b00001110, 0x81, 0x85, 0x89, 0x8D, 0x11, 0x82, 0x86, 0x8A, 0x8D, 0x12, 0x83, 0x87, 0x8B, 0x8F, 0x13};
  // Packet under construction
  Packet::TraceInfo obj(0b00000001);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}

TEST_CASE("Trace info (info, key, spec, cyct)") {
  // Encoding
  std::vector<uint8_t> encoding = {0b00001111, 0x80, 0x84, 0x88, 0x8C, 0x10, 0x81, 0x85, 0x89, 0x8D, 0x11, 0x82, 0x86, 0x8A, 0x8D, 0x12, 0x83, 0x87, 0x8B, 0x8F, 0x13};
  // Packet under construction
  Packet::TraceInfo obj(0b00000001);

  // During insertion
  for (uint8_t byte : encoding) {
    CHECK(!obj.isDone());
    obj.insert(byte);
  }

  // Finally ready
  CHECK(obj.isDone());
}
