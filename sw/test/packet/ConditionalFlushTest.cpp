#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("ConditionalFlush packets are 'done' right after construction") {
  Packet::ConditionalFlush obj(0b01000011);

  // Right after creaion
  CHECK(obj.isDone());
}

TEST_CASE("ConditionalFlush packets are always 'done'") {
  Packet::ConditionalFlush obj(0b01000011);

  for (int i = 0; i < 5; i++) {
    obj.insert(0);
    CHECK(obj.isDone());
  }
}
