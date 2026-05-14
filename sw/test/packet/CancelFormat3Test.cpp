#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("CancelFormat3 packets are 'done' right after construction") {
  Packet::CancelFormat3 obj(0x07);

  // Right after creaion
  CHECK(obj.isDone());
}

TEST_CASE("CancelFormat3 packets are always 'done'") {
  Packet::CancelFormat3 obj(0x07);

  for (int i = 0; i < 5; i++) {
    obj.insert(0);
    CHECK(obj.isDone());
  }
}
