CODE 0 ;

:procedure _start;
_start:
	2 -> RF_1.1, gcu.ra -> LSU.in2, 4 -> ALU.in2, 16777208 -> ALU.in1t.sub ;
	..., ..., ALU.out1 -> LSU.in1t.stw, ALU.out1 -> RF.0 ;
	..., ..., ..., 10 -> RF.2 ;
	..., ..., RF.2 -> ALU.in2, -1 -> ALU.in1t.add ;
	..., RF_1.1 -> ALU.in2, 2 -> ALU.in1t.add, ALU.out1 -> RF.2 ;
	..., RF.2 -> ALU.in1t.gt, ALU.out1 -> RF_1.1, ALU.out1 -> ALU.in2 ;
	..., ..., ALU.out1 -> BOOL.0, ... ;
	..., ?BOOL.0 RF.2 -> ALU.in2, ?BOOL.0 7 -> gcu.pc.jump, ?BOOL.0 -1 -> ALU.in1t.add ;
	..., ?BOOL.0 2 -> ALU.in1t.add, ?BOOL.0 ALU.out1 -> RF.2, ?BOOL.0 RF_1.1 -> ALU.in2 ;
	..., ?BOOL.0 RF.2 -> ALU.in1t.gt, ?BOOL.0 ALU.out1 -> RF_1.1, ?BOOL.0 ALU.out1 -> ALU.in2 ;
	..., ..., ?BOOL.0 ALU.out1 -> BOOL.0, ... ;
	..., _exit -> gcu.pc.call, RF_1.1 -> ALU.in1t.add, RF.2 -> ALU.in2 ;
	..., ..., 8 -> LSU.in1t.stw, ALU.out1 -> LSU.in2 ;
	..., ..., ..., ... ;
	..., ..., ..., ... ;
:procedure _exit;
_exit:
	..., ..., ..., 4 -> RF_1.1 ;
	..., 16 -> gcu.pc.jump, RF_1.1 -> LSU.in1t.stw, 0 -> LSU.in2 ;
	..., ..., ..., ... ;
	..., ..., ..., ... ;
	..., ..., ..., ... ;


DATA data 4 ;

DA 9 ;

