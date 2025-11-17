#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("Resynchronization packets are 'done' right after construction") {
  Packet::Resynchronization obj;

  // Right after creaion
  CHECK(obj.isDone());
}

TEST_CASE("Resynchronization packets are always 'done'") {
  Packet::Resynchronization obj;

  for (int i = 0; i < 5; i++) {
    obj.insert(0);
    CHECK(obj.isDone());
  }
}
