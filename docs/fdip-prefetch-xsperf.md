# FDIP 预取与 FTQ 背压计数器

启用 `WithBoomXSPerf` 后，以下计数器通过现有 XSPERF endpoint 输出。
`FDIPMegaBoomV3CosimPerfDBConfig` 已启用该配置。

原有 `icache_prefetch` 统计预取 MSHR 发出的 TileLink A 请求。它不统计
FTQ 查询，也不统计 ICache 命中的预取。判断是否发出预取，应沿以下路径查看：

```text
FTQ -> PrefetchPipe S0 -> ITLB/meta -> WayLookup -> MainPipe -> IFU
                                \-> S2 miss -> MSHR -> TileLink -> refill
```

## FTQ 是否提供请求、指针是否推进

| 计数器 | 含义 |
|---|---|
| `ftq_prefetch_valid` | FTQ 提供有效预取请求的周期 |
| `ftq_prefetch_fire` | 请求被预取流水线接收的次数 |
| `ftq_prefetch_stall` | 有请求但下游不接收的周期 |
| `ftq_prefetch_ptr_advance` | 接收请求且实际推进 pf_ptr 的次数 |
| `ftq_prefetch_repeat` | 接收请求，但 pf_ptr 已领先 ifu_ptr，指针保持的次数 |

`ftq_prefetch_fire` 不等于独立 FTQ 项数；`repeat` 可揭示同一指针的重复查询。

在 `ftq_full` 周期内，`ftq_full_prefetch_*` 给出互斥分类：

| 后缀 | 含义 |
|---|---|
| `empty` | pf_ptr 已追平 bpu_ptr，无待查询项 |
| `redirect` | 有待查询项，但被 backend/IFU 或影响该指针的 F2 redirect 抑制 |
| `stall` | 提供请求，但下游不接收 |
| `advance` | 接收请求并推进 pf_ptr |
| `repeat` | 接收请求但不推进 pf_ptr |

上述五项相加应等于 `ftq_full`。`ftq_full_fetch_*` 是并行观察项，不能相加：

- `valid/fire/stall`：FTQ 到 ICache 主取指入口的请求、接收和等待。
- `ifu_caught_up`：ifu_ptr 已追平 bpu_ptr。
- `commit_wait`：FTQ 回收指针 commit_ptr 已追平后端 deq_ptr，无新的后端提交项可回收；不能单独判定为 ROB 停顿。
- `commit_advance`：本周期进行 FTQ 提交回收。

## 预取流水线停在哪里

`prefetch_s0_*` 在入口有请求时给出互斥结果：

- `fire`：接收成功。
- `flush_blocked`：被 flush 阻止。
- `stall_s1`：S1 有效但本周期不能完成，或空闲状态尚未回到 Idle。
- `stall_itlb`：S1 可接收，但 ITLB 请求不 ready。
- `stall_meta`：S1 和 ITLB 可接收，但 metadata 端口不 ready。

S0 可以在 S1 为空或本周期完成时接收请求，支持 S1 出队与新 S0 接收同时发生。
正常命中且下游 ready 时，不再因 S1 有效而插入一个空周期。
`stall_s1` 较大时需结合 S1 各等待原因判断，不能直接解释为流水线卡死。

`prefetch_s1_*` 在 S1 有效且无 flush 时给出互斥结果：

- `advance`：完成 S1 并进入 S2。
- `wait_itlb`：等待地址翻译完成。
- `wait_meta`：翻译已完成，等待 metadata 响应。
- `wait_waylookup`：结果齐备，但 WayLookup 不接收。
- `wait_s2`：WayLookup 已写入或本周期写入，但 S2 不接收。
- `wait_control`：其余重发/状态切换周期。

其它预取流水线计数：

| 计数器 | 含义 |
|---|---|
| `prefetch_itlb_req/miss` | ITLB 请求握手次数及其中返回 miss 的次数；包括重发 |
| `prefetch_waylookup_hit/miss` | 写入 WayLookup 的结果中，waymask 非零/为零的次数 |
| `prefetch_s2_hit` | S2 缓存命中完成次数 |
| `prefetch_s2_miss_valid` | S2 提供 miss 请求的周期 |
| `prefetch_s2_miss_fire` | miss 请求被 MissUnit 接收的次数 |
| `prefetch_s2_miss_stall` | miss 请求等待 MissUnit 的周期 |
| `prefetch_flush_s1/s2` | flush 清除有效 S1/S2 事务的次数 |

`icache_prefetch_meta_stall_refill` 表示 metadata 读请求被 refill 阻塞。
当前阻塞条件覆盖 outstanding MSHR 的整个存续期间，包括等待下层返回的时间。
同组 `flush/invalidate` 用于区分另两个原因，三个原因互斥。

## WayLookup 为什么不消费

| 计数器 | 含义 |
|---|---|
| `waylookup_enq/deq` | 队列写入/主取指消费次数 |
| `waylookup_full/empty` | 满/空周期，不包含 flush 周期 |
| `waylookup_head_update_stall` | 队首因 refill 更新而暂不能消费的周期 |
| `waylookup_consumer_stall` | 队首可提供，但 MainPipe 不接收的周期 |

`icache_main_req_*` 在主取指入口有请求时，以 `fire`、`flush_blocked`、
`refill_blocked`、`pipe_blocked` 四项互斥分类。

`icache_main_s0_*` 在 MainPipe 收到请求且无 flush 时，以 `fire`、
`wait_waylookup`、`wait_s1`、`wait_data_port` 四项互斥分类。

`icache_main_s1_*` 在 MainPipe S1 有效且无 flush 时，以 `advance`、
`wait_refill`、`wait_ifu` 三项互斥分类。`wait_ifu` 表示取指数据已经准备好，
但 IFU 尚不能接收，是区分预取供给不足与下游消费受阻的关键指标。

## FetchBuffer 空间检查与消费背压

`fb_enq_*`、`fb_deq_*` 和占用计数均排除 `clear` 周期。`clear` 周期单独
清空队列，输入 ready/valid 不代表正常存储推进。

| 计数器 | 含义 |
|---|---|
| `fb_enq_valid/fire/stall` | 入队请求周期、接收次数、请求被拒绝周期 |
| `fb_enq_stall_mask_fits` | 被拒绝，但当前空槽已能容纳实际输入 mask |
| `fb_enq_stall_deq_fits` | 当前空槽不足，计入本周期完整行出队后能容纳输入 |
| `fb_enq_stall_space` | 即使计入本周期完整行出队，仍无足够空间 |
| `fb_enq_stall_decode_stall` | 入队被拒绝，且有有效出队行但 Decode 不释放该行；是交叉子集 |
| `fb_deq_valid/fire/stall` | 有效出队行周期、出队握手次数、有效行等待周期 |
| `fb_deq_row_release` | 完整行释放并推进存储 head 的次数 |
| `fb_enq_instr` | 成功入队的有效指令数量，累计 PopCount(mask) |
| `fb_enq_size_0`～`fb_enq_size_8` | 实际接收包大小分布；上限随 fetchWidth 配置变化 |
| `fb_occupancy_sum` | 每周期已占用指令槽数的累计值，排除 clear |
| `fb_occupancy_empty` | 占用为 0 |
| `fb_occupancy_room_fetch` | 非空，且空槽至少 fetchWidth 个 |
| `fb_occupancy_tight` | 空槽为 1～fetchWidth-1 个 |
| `fb_occupancy_full` | 无空槽 |

占用由现有 one-hot head/tail 和 maybe_full 计算。Decode 已消费但尚未完整
释放的部分行仍占存储槽；不会把部分 decode 当作可用空间。平均存储占用可用
`fb_occupancy_sum / 四个 occupancy 分类之和` 计算。

```text
fb_enq_valid = fb_enq_fire + fb_enq_stall
fb_enq_stall = mask_fits + deq_fits + space
fb_deq_valid = fb_deq_fire + fb_deq_stall
sum(fb_enq_size_N) = fb_enq_fire
sum(N * fb_enq_size_N) = fb_enq_instr
```

`mask_fits` 和 `deq_fits` 是当前保守 admission 的可优化机会计数，不能直接
当作整机可节省周期；同一包可能连续被拒绝，也可能与下游阻塞重叠。

## Decode 与 Dispatch 的最先阻塞原因

Decode/Dispatch 计数包括 redirect 等控制阻塞周期，通过原因分类单独标识。
这些计数与 FetchBuffer 排除 clear 的口径不同。

| 计数器 | 含义 |
|---|---|
| `decode_packet_valid/release/stall` | 有输入行周期、整行释放次数、行未释放周期 |
| `decode_partial_progress` | 行未释放，但至少一条未完成指令进入 Decode 下一阶段 |
| `decode_no_progress` | 行未释放，且本周期没有 dec_fire |
| `decode_stall_no_hazard` | 行未释放但没有有效指令 hazard，例如等待部分行被后续入队补齐 |
| `decode_uops` | 累计 dec_fire 指令数，已完成 mask 防止重复计数 |
| `decode_block_*` | 对最先发生 hazard 的有效槽位进行互斥原因分类 |
| `dispatch_valid/all_advance/blocked` | 有效指令组周期、无阻塞周期、存在阻塞周期 |
| `dispatch_partial_progress/no_progress` | 阻塞周期内有/无 dis_fire，二者互斥 |
| `dispatch_uops` | 累计 dis_fire 指令数 |
| `dispatch_block_*` | 对最先发生 hazard 的有效槽位进行互斥原因分类 |

首先定位按程序顺序最先发生 hazard 的有效指令，再在该指令上按以下优先级选一个原因：

- Decode：`redirect → rollback → exception → dispatch → branch_tag → other`。
- Dispatch：`redirect → rob → rename → ldq → stq → issue → serialize → rocc → other`。

`issue` 对应 dispatcher 的 ren_uops.ready 为 false；`serialize` 包括等待
空流水线/LSU fencei ready，以及前面的有效槽位为 unique 指令。`rename`
对应现有 ren_stalls，可能来自整数、浮点或 predicate 资源。

优先分类反映同时存在的阻塞条件中选中的一项，不证明其他资源在该周期正常。
后面的槽位即使被另一资源阻塞，也不会取代最先阻塞的有效指令。

```text
decode_packet_valid = decode_packet_release + decode_packet_stall
decode_packet_stall = decode_partial_progress + decode_no_progress
decode_packet_stall = decode_stall_no_hazard + sum(decode_block_*)
dispatch_valid = dispatch_all_advance + dispatch_blocked
dispatch_blocked = dispatch_partial_progress + dispatch_no_progress
dispatch_blocked = sum(dispatch_block_*)
```

`decode_ftq_full_no_input`、`decode_ftq_full_no_progress` 和
`dispatch_ftq_full_blocked` 仅用于交叉观察；它们可能重叠，不能相加。

## WayLookup 等待期间的 FTQ 状态

`ftq_waylookup_wait_total` 使用与 `icache_main_s0_wait_waylookup` 完全相同的
事件，应与其相等。该事件从 ICache 送到 FTQ，与本周期真实指针/握手交叉计数。

| 后缀 | 含义 |
|---|---|
| `prefetch_empty` | pf_ptr 已追平 bpu_ptr |
| `prefetch_redirect` | 有待预取项，但影响预取的 redirect 抑制 valid |
| `prefetch_stall` | 有待预取项，预取入口未接收 |
| `prefetch_advance` | 预取入口接收且指针推进 |
| `prefetch_repeat` | 预取入口接收但指针未推进 |
| `ftq_full` | 同时 FTQ 满；与前五类可能重叠 |
| `bpu_f2_redirect` | 同时发生 F2 redirect；即使 prefetch empty 仍计数 |

前五项互斥，相加应等于 `ftq_waylookup_wait_total`。这些是同周期关联，
F2 redirect 的后续空拍可能发生在下一周期，不能据交叉计数独立证明全部因果关系。

## MissUnit 和下层是否接收

| 计数器 | 含义 |
|---|---|
| `prefetch_mshr_req_valid/req_fire` | MissUnit 收到 miss 请求的周期/接收次数 |
| `prefetch_mshr_merge` | 请求命中已有 demand 或 prefetch MSHR，并被接收的次数 |
| `prefetch_mshr_same_fetch` | 与同周期 demand 请求相同，被接收但未独立分配的次数 |
| `prefetch_mshr_same_fetch_wait` | 上述情况中 demand 自己未握手的次数，作为其子集单独观察 |
| `prefetch_mshr_allocate` | 实际新分配预取 MSHR 的次数 |
| `prefetch_mshr_stall` | miss 请求不能被 MissUnit 接收的周期 |
| `prefetch_mshr_full` | 全部 8 个预取 MSHR 占用的周期 |
| `prefetch_mshr_busy_sum` | 每周期占用预取 MSHR 数的累计值；除以 cycles 得到平均占用 |
| `prefetch_l2_valid/fire` | 预取 MSHR 提供下层请求的周期/实际发送次数 |
| `prefetch_l2_wait_demand` | demand 请求因仲裁优先级占用端口的周期 |
| `prefetch_l2_wait_bus` | 无 demand 抢占，但下层不 ready 的周期 |
| `prefetch_l2_refill` | 预取 source 的完整 refill 返回次数，包括已 flush 的在途请求 |

正常无 flush 的接收路径中：

```text
ftq_prefetch_fire = ftq_prefetch_ptr_advance + ftq_prefetch_repeat
ftq_prefetch_fire = prefetch_s0_fire
prefetch_s2_miss_valid = prefetch_s2_miss_fire + prefetch_s2_miss_stall
prefetch_s2_miss_fire = prefetch_mshr_req_fire
prefetch_mshr_req_fire = merge + same_fetch + allocate
prefetch_l2_valid = prefetch_l2_fire + wait_demand + wait_bus
prefetch_l2_fire = icache_prefetch
```

不要把等待周期当作独立请求数。流水线接收数与完成数还可能相差 flush 丢弃项
以及仿真结束时尚未完成的事务；MSHR 分配与下层发送也可能相差 flush 取消项。

## 判读顺序

1. 用 `ftq_full_prefetch_*` 判断 FTQ 满时是没有预取、预取受阻，还是仍在推进。
2. 受阻时查看 `prefetch_s0_stall_*`，进一步查看 S1、metadata、WayLookup 的等待。
3. WayLookup 满时查看 MainPipe 的 `wait_refill/wait_ifu`，定位消费受阻原因。
4. miss 等待时查看 MSHR 满、下层 demand 仲裁及总线 ready。
5. 若 `prefetch_s0_fire/prefetch_s2_hit` 很多而 `prefetch_l2_fire` 很少，说明大量预取查询命中缓存。
6. 若预取和取指仍持续推进、但 FTQ 满且 `commit_wait` 较多，继续检查后端提交和 FTQ 回收，不能仅根据 FTQ 满认定没有预取。

此前 minver 的计数显示预取入口等待 121711 周期，其中 S1 忙占 119380 周期，
而 S1 各资源等待为 0。旧入口要求 S1 为空，正常服务周期也阻止下一条 S0 进入。
修改后需重新生成 RTL 并运行，比较 `prefetch_s0_stall_s1`、
`icache_main_s0_wait_waylookup`、`ftq_full` 和 IPC；旧 DB 不代表修改后的性能。
