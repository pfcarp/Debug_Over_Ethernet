from generate import TPIU, Packet
import random


def test_format_0():
    input = bytearray(0)
    input.extend(bytes([0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71]))
    answer = bytearray(0)
    answer.extend(bytes([0x70, 0x71, 0x70, 0x71, 0x70, 0x71, 0x70, 0x71, 0x70, 0x71, 0x70, 0x71, 0x70, 0x71, 0x70, 0xff]))
    assert(answer == TPIU.format(input))


def test_format_1():
    input = bytearray(0)
    input.extend(bytes([0x70, 0x70, 0x70, 0x70, 0x70, 0x70, 0x70, 0x70, 0x70, 0x70, 0x70, 0x70, 0x70, 0x70, 0x70]))
    answer = bytearray(0)
    answer.extend(bytes([0x70, 0x70, 0x70, 0x70, 0x70, 0x70, 0x70, 0x70, 0x70, 0x70, 0x70, 0x70, 0x70, 0x70, 0x70, 0x00]))
    assert(answer == TPIU.format(input))


def test_FixedSeries_0():
    for _ in range(64):
        n = random.randrange(64)
        series = Packet.generateFixedSeries(n)
        assert(len(series) == n)


def test_UnboundedCSeries_0():
    for _ in range(64):
        series = Packet.generateUnboundedCSeries()
        assert(len(series) < Packet.unboundedMaximum)
        for i, b in enumerate(series):
            if (i == len(series)-1): # if last
                assert(b < 128)
            else:
                assert(b >= 128)


def test_BoundedCSeries_0():
    for _ in range(64):
        series = Packet.generateBoundedCSeries(1, 20)
        print(" ".join(f"0x{b:02x} ({i})" for i, b in enumerate(series)))
        assert(len(series) <= 20)
        for i, b in enumerate(series):
            if (i == len(series)-1): # if last
                if (i == 20-1):
                    assert(b < 256)
                else:
                    assert(b < 128)
            else:
                assert(b >= 128)


