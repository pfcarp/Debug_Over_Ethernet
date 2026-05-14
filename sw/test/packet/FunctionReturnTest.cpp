#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("Function return packets are 'done' right after construction") {
  Packet::FunctionReturn obj(0b00000101);

  // Right after creaion
  CHECK(obj.isDone());
}

TEST_CASE("Function return packets are always 'done'") {
  Packet::FunctionReturn obj(0b00000101);

  for (int i = 0; i < 5; i++) {
    obj.insert(0);
    CHECK(obj.isDone());
  }
}
