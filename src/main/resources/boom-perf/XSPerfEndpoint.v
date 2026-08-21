module XSPerfEndpoint #(
  parameter PERF_NAME = "unnamed"
)(
  input clock,
  input reset,
  input [63:0] value
);
  integer stderr = 32'h80000002;

  // Each endpoint owns one dynamically registered counter.  The Chisel
  // BlackBox parameter supplies the name, so adding a counter does not require
  // extending a fixed values_N interface.
  final begin
    if ($test$plusargs("xsperf")) begin
      $fdisplay(stderr, "XSPERF %m.%s=%0d", PERF_NAME, value);
    end
  end
endmodule
