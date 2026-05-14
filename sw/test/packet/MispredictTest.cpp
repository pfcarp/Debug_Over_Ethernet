#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("Mispredict packets are 'done' right after construction") {
  Packet::Mispredict obj(0x03);

  // Right after creaion
  CHECK(obj.isDone());
}

TEST_CASE("Mispredict packets are always 'done'") {
  Packet::Mispredict obj(0x03);

  for (int i = 0; i < 5; i++) {
    obj.insert(0);
    CHECK(obj.isDone());
  }
}
