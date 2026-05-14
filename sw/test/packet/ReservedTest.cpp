#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("Reserved packets are 'done' right after construction") {
  Packet::Reserved obj(0b00001001);

  // Right after creaion
  CHECK(obj.isDone());
}

TEST_CASE("Reserved packets are always 'done'") {
  Packet::Reserved obj(0b00001001);

  for (int i = 0; i < 5; i++) {
    obj.insert(0);
    CHECK(obj.isDone());
  }
}
