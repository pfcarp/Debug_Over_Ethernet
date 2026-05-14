#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("AtomFormat3 packets are 'done' right after construction") {
  Packet::AtomFormat3 obj(0b11111000);

  // Right after creaion
  CHECK(obj.isDone());
}

TEST_CASE("AtomFormat3 packets are always 'done'") {
  Packet::AtomFormat3 obj(0b11111000);

  for (int i = 0; i < 5; i++) {
    obj.insert(0);
    CHECK(obj.isDone());
  }
}
