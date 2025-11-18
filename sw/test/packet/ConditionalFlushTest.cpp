#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("ConditionalFlush packets are 'done' right after construction") {
  Packet::ConditionalFlush obj;

  // Right after creaion
  CHECK(obj.isDone());
}

TEST_CASE("ConditionalFlush packets are always 'done'") {
  Packet::ConditionalFlush obj;

  for (int i = 0; i < 5; i++) {
    obj.insert(0);
    CHECK(obj.isDone());
  }
}
