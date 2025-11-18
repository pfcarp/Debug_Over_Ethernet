#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("ConditionalResultFormat4 packets are 'done' right after construction") {
  Packet::ConditionalResultFormat4 obj(0x03);

  // Right after creaion
  CHECK(obj.isDone());
}

TEST_CASE("ConditionalResultFormat4 packets are always 'done'") {
  Packet::ConditionalResultFormat4 obj(0x03);

  for (int i = 0; i < 5; i++) {
    obj.insert(0);
    CHECK(obj.isDone());
  }
}
