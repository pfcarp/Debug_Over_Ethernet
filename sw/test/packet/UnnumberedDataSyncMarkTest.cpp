#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("UnnumberedDataSyncMark packets are 'done' right after construction") {
  Packet::UnnumberedDataSyncMark obj(0b00101011);

  // Right after creaion
  CHECK(obj.isDone());
}

TEST_CASE("UnnumberedDataSyncMark packets are always 'done'") {
  Packet::UnnumberedDataSyncMark obj(0b00101100);

  for (int i = 0; i < 5; i++) {
    obj.insert(0);
    CHECK(obj.isDone());
  }
}
