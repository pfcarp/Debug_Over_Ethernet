#!/usr/bin/env bash

packets=(
  ASync
  Discard
  Overflow
  BranchFutureFlush
  TraceInfo
  Timestamp
  TraceOn
  FunctionReturn
  Exception
  ExceptionReturn
  Resynchronization
  CycleCountFormat2
  CycleCountFormat1
  CycleCountFormat3
  NumberedDataSynchronizationMark
  UnnumberedDataSynchronizationMark
  Commit
  CancelFormat1
  Mispredict
  CancelFormat2
  CancelFormat3
  ConditionalInstructionFormat2
  ConditionalFlush
  ConditionalResultFormat4
  ConditionalResultFormat2
  ConditionalResultFormat3
  ConditionalResultFormat1
  ConditionalInstructionFormat1
  ConditionalInstructionFormat3
  Ignore
  Event
  Context
  AddressWithContext
  TimestampMarker
  ExactMatchAddress
  ShortAddress
  LongAddress
  Q
  AtomFormatX
)

mkdir -p results/

for packet in "${packets[@]}"; do
  echo "[${packet}]"
  python3 generate.py --packet ${packet} --amount 1048576
  ../../sw/bench.out ${packet}.bin >> results/${packet}.csv
  rm ${packet}.bin
done

