#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("Ignore packets are 'done' right after construction") {
  Packet::Ignore obj(0b01110000);

  // Right after creaion
  CHECK(obj.isDone());
}
