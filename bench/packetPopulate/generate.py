"""
Usage:
    generate.py --packet <packet> --amount <amount>
    generate.py (-h | --help)

Options:
    --packet <packet>   Packet type
    --amount <amount>   Amount of packets to be generated int he trace (padded with Ignore packets is needed)
"""
from docopt import docopt
import random


class Packet():

    unboundedMaximum = 20

    @staticmethod
    def generateFixedSeries(maximum):
        res = bytearray(0)
        res.extend(bytes([random.randrange(2**8) for _ in range(maximum)]))
        return res

    @staticmethod
    def generateUnboundedCSeries(minimum=1):
        res = bytearray(0)
        steps = random.randrange(minimum, Packet.unboundedMaximum)
        res.extend(bytes([(2**7)+(random.randrange(2**7)) for _ in range(steps-1)]))
        res.extend(bytes([random.randrange(2**7)]))
        return res

    @staticmethod
    def generateBoundedCSeries(minimum, maximum):
        res = bytearray(0)
        steps = random.randrange(minimum, maximum+1)
        res.extend(bytes([(2**7)+(random.randrange(2**7)) for _ in range(steps-1)]))
        if (steps == maximum): # Bound reached last element can be in [0, 256[
            res.extend(bytes([0xff]))#[random.randrange(2**8)]))
        else:
            res.extend(bytes([random.randrange(2**7)]))
        return res


class ASync():

    @staticmethod
    def generate():
        return bytes([0x00]*12) # 12 = header+payload (=1+11)


class Discard():

    @staticmethod
    def generate():
        return bytes([0x00, 0x03])


class Overflow():

    @staticmethod
    def generate():
        return bytes([0x00, 0x05])


class BranchFutureFlush():

    @staticmethod
    def generate():
        return bytes([0x00, 0x07])


class TraceInfo():

    @staticmethod
    def generate():
        res = bytearray(0)
        res.extend(bytes([0x01]))
        # PLCTL
        hasInfo = random.randrange(2)
        hasKey  = random.randrange(2)
        hasSpec = random.randrange(2)
        hasCyct = random.randrange(2)
        res.extend(bytes([0x00 | (hasCyct << 3) | (hasSpec << 2) | (hasKey << 1) | hasInfo]))
        # Payloads
        if (hasInfo):
            res.extend(Packet.generateUnboundedCSeries())
        if (hasKey):
            res.extend(Packet.generateUnboundedCSeries())
        if (hasSpec):
            res.extend(Packet.generateUnboundedCSeries())
        if (hasCyct):
            res.extend(Packet.generateUnboundedCSeries())
        return res


class Timestamp():

    @staticmethod
    def generate():
        res = bytearray(0)
        hasCount = random.randrange(2)
        res.extend(bytes([0x02+hasCount]))
        res.extend(Packet.generateBoundedCSeries(1, 8))
        if (hasCount):
            res.extend(Packet.generateBoundedCSeries(1, 3))
        return res


class TraceOn():

    @staticmethod
    def generate():
        return bytes([0x04])


class FunctionReturn():

    @staticmethod
    def generate():
        return bytes([0x05])


class Exception():

    @staticmethod
    def generate():
        res = bytearray(0)
        res.extend(bytes([0x06]))
        res.extend(Packet.generateBoundedCSeries(1, 2))
        res.extend(random.choice((ShortAddress, LongAddress, ExactMatchAddress)).generate())
        return res


class ExceptionReturn():

    @staticmethod
    def generate():
        return bytes([0x07])


class Resynchronization():

    @staticmethod
    def generate():
        return bytes([0x08])


class CycleCountFormat2():

    @staticmethod
    def generate():
        return bytes([0x0C+random.randrange(2), random.randrange(2**8)])


class CycleCountFormat1():

    @staticmethod
    def generate():
        res = bytearray(0)
        hasCount = random.randrange(2)
        res.extend(bytes([0x0E+hasCount]))
        res.extend(Packet.generateUnboundedCSeries())
        if (hasCount):
            res.extend(Packet.generateBoundedCSeries(1, 3))
        return res


class CycleCountFormat3():

    @staticmethod
    def generate():
        return bytes([0x10+random.randrange(16)])


class NumberedDataSynchronizationMark():

    @staticmethod
    def generate():
        return bytes([0x20+random.randrange(8)])


class UnnumberedDataSynchronizationMark():

    @staticmethod
    def generate():
        return bytes([0x28+random.randrange(8)])


class Commit():

    @staticmethod
    def generate():
        res = bytearray(0)
        res.extend(bytes([0x2D]))
        res.extend(Packet.generateUnboundedCSeries())
        return res


class CancelFormat1():

    @staticmethod
    def generate():
        res = bytearray(0)
        res.extend(bytes([0x2E+(random.randrange(2))]))
        res.extend(Packet.generateUnboundedCSeries())
        return res


class Mispredict():

    @staticmethod
    def generate():
        return bytes([0x30+random.randrange(4)])


class CancelFormat2():

    @staticmethod
    def generate():
        return bytes([0x34+random.randrange(4)])


class CancelFormat3():

    @staticmethod
    def generate():
        return bytes([0x38+random.randrange(8)])


class ConditionalInstructionFormat2():

    @staticmethod
    def generate():
        return bytes([0x40+random.randrange(4)])


class ConditionalFlush():

    @staticmethod
    def generate():
        return bytes([0x43])


class ConditionalResultFormat4():

    @staticmethod
    def generate():
        return bytes([0x44+random.randrange(4)])


class ConditionalResultFormat2():

    @staticmethod
    def generate():
        return bytes([0x48+random.randrange(8)])


class ConditionalResultFormat3():

    @staticmethod
    def generate():
        return bytes([0x50+random.randrange(16), random.randrange(256)])


class ConditionalResultFormat1():

    @staticmethod
    def generate():
        res = bytearray(0)
        withCI1 = random.randrange(2)
        res.extend(bytes([0x6E if (withCI1) else 0x6B]))
        res.extend(Packet.generateUnboundedCSeries())
        if (withCI1):
            res.extend(Packet.generateUnboundedCSeries())
        return res


class ConditionalInstructionFormat1():

    @staticmethod
    def generate():
        res = bytearray(0)
        res.extend(bytes([0x6C]))
        res.extend(Packet.generateUnboundedCSeries())
        return res


class ConditionalInstructionFormat3():

    @staticmethod
    def generate():
        return bytes([0x6D, random.randrange(128)])


class Ignore():

    @staticmethod
    def generate():
        return bytes([0x70])


class Event():

    @staticmethod
    def generate():
        return bytes([0x71+random.randrange(15)])


class Context():

    @staticmethod
    def generate():
        res = bytearray(0)
        hasPayload = random.randrange(2)
        res.extend(bytes([0x80+hasPayload]))
        if (hasPayload):
            hasVMID = random.randrange(2)
            hasContextID = random.randrange(2)
            res.extend(bytes([(hasVMID << 6)+(hasContextID << 7)+random.randrange(6)]))
            if (hasVMID):
                res.extend(bytes([random.randrange(256) for _ in range(4)]))
            if (hasContextID):
                res.extend(bytes([random.randrange(256) for _ in range(4)]))
        return res


class AddressWithContext():

    @staticmethod
    def generate():
        res = bytearray(0)
        isIS0 = random.randrange(2)
        is32b = random.randrange(2)
        offset = (3, 2, 5, 6)[(isIS0*2)+is32b]
        res.extend(bytes([0x80+offset]))
        # Address
        res.extend(bytes([random.randrange(256) for _ in range(4 if (is32b) else 8)]))
        # Context
        hasVMID = random.randrange(2)
        hasContextID = random.randrange(2)
        res.extend(bytes([(hasVMID << 6)+(hasContextID << 7)+random.randrange(6)]))
        if (hasVMID):
            res.extend(bytes([random.randrange(256) for _ in range(4)]))
        if (hasContextID):
            res.extend(bytes([random.randrange(256) for _ in range(4)]))
        return res


class TimestampMarker():

    @staticmethod
    def generate():
        return bytes([0x88])


class ExactMatchAddress():

    @staticmethod
    def generate():
        return bytes([0x90+(random.randrange(4))])


class ShortAddress():

    @staticmethod
    def generate():
        res = bytearray(0)
        res.extend(bytes([0x95+(random.randrange(2))]))
        res.extend(Packet.generateBoundedCSeries(1, 2))
        return res


class LongAddress():

    @staticmethod
    def generate():
        is32b = random.randrange(2)
        res = bytearray(0)
        res.extend(bytes([0x98+(2 if (is32b) else 5)]))
        res.extend(Packet.generateBoundedCSeries(1, 4 if (is32b) else 8))
        return res


class Q():

    @staticmethod
    def generate():
        res = bytearray(0)
        typeField = random.choice((0, 1, 2, 5, 6, 10, 11, 12, 15))
        hasCount = False
        res.extend(bytes([0xA0+typeField]))
        if (typeField in (0, 1, 2)):
            res.extend(bytes([0x90+(typeField%3)]))
            hasCount = True
        elif (typeField in (5, 6)):
            res.extend(Packet.generateBoundedCSeries(1, 2))
            hasCount = True
        elif (typeField in (10, 11)):
            is32b = random.randrange(2)
            res.extend(Packet.generateBoundedCSeries(1, 4 if (is32b) else 8))
            hasCount = True
        elif (typeField  == 12):
            hasCount = True
        # Count
        if (hasCount):
            res.extend(Packet.generateUnboundedCSeries())
        return res


class AtomFormatX():

    @staticmethod
    def generate():
        return bytes([0xC0+(random.randrange(64))])


class TPIU():

    @staticmethod
    def addAux(buffer):
        data = bytearray(0)
        for packet in range(0, len(buffer), 15):
            auxiliary = 0x00
            for frame in range(15):
                if ((frame%2) == 0):
                    data.extend(bytes([buffer[packet+frame] & 0xfe]))
                    auxiliary |= (buffer[packet+frame] & 0x01) << (frame//2)
                else:
                    data.extend(bytes([buffer[packet+frame]]))
            data.extend(bytes([auxiliary]))
        return data

    @staticmethod
    def addTimestamp(buffer):
        timestamp = b'\x01\x00\x00\x00' # reversed for endianess
        frameWidth = 16
        data = bytearray(0)
        for i in range(0, len(buffer), frameWidth):
            #print(len(data))
            frame = buffer[i : i+frameWidth]
            if (len(frame) == frameWidth):
                data.extend(timestamp)
            data.extend(frame)
        return data

    @staticmethod
    def format(buffer):
        return TPIU.addTimestamp(TPIU.addAux(buffer))


class ETM():

    get = {
        "ASync"                             : ASync,
        "Discard"                           : Discard,
        "Overflow"                          : Overflow,
        "BranchFutureFlush"                 : BranchFutureFlush,
        "TraceInfo"                         : TraceInfo,
        "Timestamp"                         : Timestamp,
        "TraceOn"                           : TraceOn,
        "FunctionReturn"                    : FunctionReturn,
        "Exception"                         : Exception,
        "ExceptionReturn"                   : ExceptionReturn,
        "Resynchronization"                 : Resynchronization,
        "CycleCountFormat2"                 : CycleCountFormat2,
        "CycleCountFormat1"                 : CycleCountFormat1,
        "CycleCountFormat3"                 : CycleCountFormat3,
        "NumberedDataSynchronizationMark"   : NumberedDataSynchronizationMark,
        "UnnumberedDataSynchronizationMark" : UnnumberedDataSynchronizationMark,
        "Commit"                            : Commit,
        "CancelFormat1"                     : CancelFormat1,
        "Mispredict"                        : Mispredict,
        "CancelFormat2"                     : CancelFormat2,
        "CancelFormat3"                     : CancelFormat3,
        "ConditionalInstructionFormat2"     : ConditionalInstructionFormat2,
        "ConditionalFlush"                  : ConditionalFlush,
        "ConditionalResultFormat4"          : ConditionalResultFormat4,
        "ConditionalResultFormat2"          : ConditionalResultFormat2,
        "ConditionalResultFormat3"          : ConditionalResultFormat3,
        "ConditionalResultFormat1"          : ConditionalResultFormat1,
        "ConditionalInstructionFormat1"     : ConditionalInstructionFormat1,
        "ConditionalInstructionFormat3"     : ConditionalInstructionFormat3,
        "Ignore"                            : Ignore,
        "Event"                             : Event,
        "Context"                           : Context,
        "AddressWithContext"                : AddressWithContext,
        "TimestampMarker"                   : TimestampMarker,
        "ExactMatchAddress"                 : ExactMatchAddress,
        "ShortAddress"                      : ShortAddress,
        "LongAddress"                       : LongAddress,
        "Q"                                 : Q,
        "AtomFormatX"                       : AtomFormatX
    }


class Binary():

    def __init__(self, filename):
        self.filename = filename
        self.buffer = bytearray(0)

    def add(self, data):
        self.buffer.extend(data)

    def write(self):
        # pad with IGNORE packets such that it is a multiple of 15 (i.e., 16-1)
        padding = 15-(len(self.buffer)%15)
        print(f"[WARNING] Add {padding} `Ignore` packets.")
        for _ in range(padding):
            self.add(Ignore.generate())
        # format
        self.buffer = TPIU.format(self.buffer)
        # dump binary
        with open(self.filename, "wb") as f:
            f.write(self.buffer)


if (__name__ == "__main__"):
    args = docopt(__doc__)

    repetitions = int(args["--amount"])
    packetType  = args["--packet"]
    filename    = f"{packetType}.bin"

    binary = Binary(filename)
    for _ in range(repetitions):
        binary.add(ETM.get[packetType].generate())
    binary.write()
