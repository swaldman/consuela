package com.mchange.sc.v1.consuela.ethereum.specification;

// from yellow paper Appendix G
object Fees {
  object G {
    val zero           =     0;
    val base           =     2;
    val verylow        =     3;
    val low            =     5;
    val mid            =     8;
    val high           =    10;
    val ext            =    20;
    val sload          =    50;
    val jumpdest       =     1;
    val sset           = 20000;
    val sreset         =  5000;
    val sclear         = 15000;
    val suicide        = 24000;
    val create         = 32000;
    val codedeposit    =   200;
    val call           =    40;
    val callvalue      =  9000;
    val callstipend    =  2300;
    val callnewaccount = 25000;
    val exp            =    10;
    val expbyte        =    10;
    val memory         =     3;
    val txdatazero     =     4;
    val txdatanonzero  =    68;
    val transaction    = 21000;
    val log            =   375;
    val logdata        =     8;
    val logtopic       =   375;
    val sha3           =    30;
    val sha3word       =     6;
    val copy           =     2;
  }
}
