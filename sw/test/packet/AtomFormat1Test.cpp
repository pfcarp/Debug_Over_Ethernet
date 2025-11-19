#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("AtomFormat1 packets are 'done' right after construction") {
  Packet::AtomFormat1 obj(0b11110110);

  // Right after creaion
  CHECK(obj.isDone());
}

TEST_CASE("AtomFormat1 packets are always 'done'") {
  Packet::AtomFormat1 obj(0b11110110);

  for (int i = 0; i < 5; i++) {
    obj.insert(0);
    CHECK(obj.isDone());
  }
}
