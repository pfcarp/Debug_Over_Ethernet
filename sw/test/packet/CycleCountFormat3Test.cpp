#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("CycleCountFormat3 packets are 'done' right after construction") {
  Packet::CycleCountFormat3 obj(0x09);

  // Right after creaion
  CHECK(obj.isDone());
}

TEST_CASE("CycleCountFormat3 packets are always 'done'") {
  Packet::CycleCountFormat3 obj(0x09);

  for (int i = 0; i < 5; i++) {
    obj.insert(0);
    CHECK(obj.isDone());
  }
}
