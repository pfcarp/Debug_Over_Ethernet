#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("AtomFormat5 packets are 'done' right after construction") {
  Packet::AtomFormat5 obj(0b11110101);

  // Right after creaion
  CHECK(obj.isDone());
}

TEST_CASE("AtomFormat5 packets are always 'done'") {
  Packet::AtomFormat5 obj(0b11110101);

  for (int i = 0; i < 5; i++) {
    obj.insert(0);
    CHECK(obj.isDone());
  }
}
