#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("ConditionalResultFormat2 packets are 'done' right after construction") {
  Packet::ConditionalResultFormat2 obj(0x07);

  // Right after creaion
  CHECK(obj.isDone());
}

TEST_CASE("ConditionalResultFormat2 packets are always 'done'") {
  Packet::ConditionalResultFormat2 obj(0x07);

  for (int i = 0; i < 5; i++) {
    obj.insert(0);
    CHECK(obj.isDone());
  }
}
