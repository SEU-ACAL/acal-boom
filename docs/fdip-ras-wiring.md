# FDIP RAS wiring

`BoomRAS` in `Ras.scala` wraps `BoomRasStack` and translates prediction,
redirect, and FTQ retirement into stack operations.

## Prediction and checkpoints

RAS speculation occurs in S2, the final prediction stage. An accepted, taken
CFI uses `attribute.hasPush` and `attribute.hasPop`, including `PopAndPush`.
S1 replay does not update the stack. S2 correction discards younger S1 work;
the S2 packet still applies its own operation once.

The S2 return target and checkpoint both come from the stack before that
operation. `RasMeta` contains `ssp`, `sctr`, `tosr`, `tosw`, `nos`, and
`topRetAddr`. The wrap bit accompanies each speculative pointer. FTQ writes
this entire checkpoint into `redirect_mem` using the last prediction stage's
FTQ index. The S1 metadata is provisional; only S2 writes the recovery RAM.

CFI position denotes the terminal halfword of an instruction. The push address
is therefore `fetchAlign(startPc) + (cfiPosition << 1) + 2` for both compressed
and uncompressed instructions.

## Redirect and retirement

Every IFU or backend redirect returns the checkpoint, including conditional
branches and backend flushes. Redirect first restores the checkpoint and
then applies the resolved RAS action if the CFI is valid and taken. The backend
action comes from the corrected CFI entry, rather than the old `pd_mem` value.
Redirect wins over a simultaneous speculative update.

`ras_commitInfo` is independent of branch-predictor training valid. Each
retired FTQ entry returns its actual CFI, start PC, and `RasCommitMeta(ssp,
tosw)` with the same one-cycle RAM-read timing as its commit valid. Entries
without CFIs still deliver queue reclamation information. Backend/IFU redirect
and a new FTQ retirement read share a port, so redirect pauses that read.

The committed stack and speculative stack apply pop before push for
`PopAndPush`. The speculative queue uses register writes; the following
cycle sees those entries directly. Invalid predecessor pointers are normalized
outside the live interval so that repeated ring wraps cannot revive an old
link. Commit advances `bos` to reclaim speculative entries.

Near overflow freezes speculative push/pop, as in the intended queue policy;
prediction can lose accuracy while frozen. Redirect restoration and commit
remain active. This integration does not establish benchmark performance or
the architectural handling of every exception/underflow case.

## Reproducible checks

From `soc-generator`:

```sh
sbt 'boom/Test/runMain boom.fdip.ifu.bpd.ras.RasWiringCheck'
```

The executable checks call/return state retention, non-RAS redirect recovery,
redirect priority, taken gating, recursive calls, PopAndPush, repeated pointer
wrap, overflow freezing and reclamation, FTQ checkpoint round trips, resolved
backend actions, and retirement without a CFI. It also generates SystemVerilog
for a BPU + FTQ + RAS harness with the default 16-entry committed stack and
32-entry speculative queue, and prints the generated artifact path.

These are module simulations and integration RTL generation. A full core
workload run remains a separate validation step.
