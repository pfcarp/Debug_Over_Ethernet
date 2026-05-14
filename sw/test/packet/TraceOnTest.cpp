#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("TraceOn packets are 'done' right after construction") {
  Packet::TraceOn obj(0b00000100);

  // Right after creaion
  CHECK(obj.isDone());
}

TEST_CASE("TraceOn packets are always 'done'") {
  Packet::TraceOn obj(0b00000100);

  for (int i = 0; i < 5; i++) {
    obj.insert(0);
    CHECK(obj.isDone());
  }
}
