#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("ExceptionReturn packets are 'done' right after construction") {
  Packet::ExceptionReturn obj;

  // Right after creaion
  CHECK(obj.isDone());
}

TEST_CASE("ExceptionReturn packets are always 'done'") {
  Packet::ExceptionReturn obj;

  for (int i = 0; i < 5; i++) {
    obj.insert(0);
    CHECK(obj.isDone());
  }
}
