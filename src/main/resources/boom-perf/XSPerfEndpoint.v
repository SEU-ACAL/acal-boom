module XSPerfEndpoint(
  input clock,
  input reset,
  input [63:0] values_0,
  input [63:0] values_1,
  input [63:0] values_2,
  input [63:0] values_3,
  input [63:0] values_4,
  input [63:0] values_5,
  input [63:0] values_6,
  input [63:0] values_7,
  input [63:0] values_8,
  input [63:0] values_9,
  input [63:0] values_10,
  input [63:0] values_11,
  input [63:0] values_12,
  input [63:0] values_13
);
  integer stderr = 32'h80000002;
  final begin
    if ($test$plusargs("xsperf")) begin
      $fdisplay(stderr, "XSPERF cycles=%0d", values_0);
        $fdisplay(stderr, "XSPERF commit_instr=%0d", values_1);
        $fdisplay(stderr, "XSPERF branch_resolved=%0d", values_2);
        $fdisplay(stderr, "XSPERF branch_mispredict=%0d", values_3);
        $fdisplay(stderr, "XSPERF control_flow_target_mispredict=%0d", values_4);
        $fdisplay(stderr, "XSPERF flush=%0d", values_5);
        $fdisplay(stderr, "XSPERF icache_demand_miss=%0d", values_6);
        $fdisplay(stderr, "XSPERF dcache_miss=%0d", values_7);
        $fdisplay(stderr, "XSPERF dtlb_miss=%0d", values_8);
        $fdisplay(stderr, "XSPERF l2tlb_miss=%0d", values_9);
        $fdisplay(stderr, "XSPERF icache_prefetch=%0d", values_10);
        $fdisplay(stderr, "XSPERF ftq_full=%0d", values_11);
        $fdisplay(stderr, "XSPERF ftq_metadata_conflict=%0d", values_12);
      $fdisplay(stderr, "XSPERF ftq_redirect_conflict=%0d", values_13);
    end
  end
endmodule
