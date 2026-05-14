#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("TimestampMarker packets are 'done' right after construction") {
  Packet::TimestampMarker obj(0b10001000);

  // Right after creaion
  CHECK(obj.isDone());
}

TEST_CASE("TimestampMarker packets are always 'done'") {
  Packet::TimestampMarker obj(0b10001000);

  for (int i = 0; i < 5; i++) {
    obj.insert(0);
    CHECK(obj.isDone());
  }
}
