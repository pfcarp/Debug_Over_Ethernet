#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("Event packets are 'done' right after construction") {
  Packet::Event obj(0b01111111);

  // Right after creaion
  CHECK(obj.isDone());
}
