#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("ExactMatchAddress packets are 'done' right after construction") {
  Packet::ExactMatchAddress obj(0b10010011);

  // Right after creaion
  CHECK(obj.isDone());
}

TEST_CASE("ExactMatchAddress packets are always 'done'") {
  Packet::ExactMatchAddress obj(0b10010011);

  for (int i = 0; i < 5; i++) {
    obj.insert(0);
    CHECK(obj.isDone());
  }
}
