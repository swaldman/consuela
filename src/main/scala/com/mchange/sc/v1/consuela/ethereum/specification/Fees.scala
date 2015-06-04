package com.mchange.sc.v1.consuela.ethereum.specification;

// from yellow paper Appendix G
object Fees {
  object Int {
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
  object BigInt {
    object G {
      val zero           = scala.BigInt( Fees.Int.G.zero );
      val base           = scala.BigInt( Fees.Int.G.base );
      val verylow        = scala.BigInt( Fees.Int.G.verylow );
      val low            = scala.BigInt( Fees.Int.G.low );
      val mid            = scala.BigInt( Fees.Int.G.mid );
      val high           = scala.BigInt( Fees.Int.G.high );
      val ext            = scala.BigInt( Fees.Int.G.ext );
      val sload          = scala.BigInt( Fees.Int.G.sload );
      val jumpdest       = scala.BigInt( Fees.Int.G.jumpdest );
      val sset           = scala.BigInt( Fees.Int.G.sset );
      val sreset         = scala.BigInt( Fees.Int.G.sreset );
      val sclear         = scala.BigInt( Fees.Int.G.sclear );
      val suicide        = scala.BigInt( Fees.Int.G.suicide );
      val create         = scala.BigInt( Fees.Int.G.create );
      val codedeposit    = scala.BigInt( Fees.Int.G.codedeposit );
      val call           = scala.BigInt( Fees.Int.G.call );
      val callvalue      = scala.BigInt( Fees.Int.G.callvalue );
      val callstipend    = scala.BigInt( Fees.Int.G.callstipend );
      val callnewaccount = scala.BigInt( Fees.Int.G.callnewaccount );
      val exp            = scala.BigInt( Fees.Int.G.exp );
      val expbyte        = scala.BigInt( Fees.Int.G.expbyte );
      val memory         = scala.BigInt( Fees.Int.G.memory );
      val txdatazero     = scala.BigInt( Fees.Int.G.txdatazero );
      val txdatanonzero  = scala.BigInt( Fees.Int.G.txdatanonzero );
      val transaction    = scala.BigInt( Fees.Int.G.transaction );
      val log            = scala.BigInt( Fees.Int.G.log );
      val logdata        = scala.BigInt( Fees.Int.G.logdata );
      val logtopic       = scala.BigInt( Fees.Int.G.logtopic );
      val sha3           = scala.BigInt( Fees.Int.G.sha3 );
      val sha3word       = scala.BigInt( Fees.Int.G.sha3word );
      val copy           = scala.BigInt( Fees.Int.G.copy );
    }
  }
}
