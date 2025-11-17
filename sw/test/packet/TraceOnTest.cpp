#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("TraceOn packets are 'done' right after construction") {
  Packet::TraceOn obj;

  // Right after creaion
  CHECK(obj.isDone());
}

TEST_CASE("TraceOn packets are always 'done'") {
  Packet::TraceOn obj;

  for (int i = 0; i < 5; i++) {
    obj.insert(0);
    CHECK(obj.isDone());
  }
}
