package stellar.rtl

import chisel3._
import chisel3.util._
import chisel3.experimental._

import ChiselUtil._

class SyncMemReadReq[Tag <: Data](addrBits: Int, tagT: Tag, nPaths: Int) extends Bundle {
  val addr = UInt(addrBits.W)
  val tag = getChiselType(tagT)
  val pathId = if (nPaths > 1) Some(UInt(log2Up(nPaths).W)) else None
}

class SyncMemReadResp[T <: Data, Tag <: Data](t: Vec[T], tagT: Tag, nPaths: Int) extends Bundle {
  val data = getChiselType(t)
  val tag = getChiselType(tagT)
  val pathId = if (nPaths > 1) Some(UInt(log2Up(nPaths).W)) else None
}

class SyncMemWriteReq[T <: Data](t: Vec[T], addrBits: Int) extends Bundle {
  val addr = UInt(addrBits.W)
  val data = getChiselType(t)
  val mask = Vec(t.size, Bool())
}

class SyncMemWrapper[T <: Data, Tag <: Data](t: Vec[T], size: Int, tagT: Tag, prevent_simultaneous_access: Boolean = false, nPaths: Int = 1, nBanks: Int = 1, nPorts: Int = 1, independentBanks: Boolean = false, isDummy: Boolean = false) extends Module {
  val addrBits = log2Up(size)

  require(!independentBanks || nPorts == nBanks)

  val io = IO(new Bundle {
    val read_reqs = Vec(nPorts, Flipped(Decoupled(new SyncMemReadReq(addrBits, tagT, nPaths = nPaths))))
    val read_resps = Vec(nPorts, Decoupled(new SyncMemReadResp(t, tagT, nPaths = nPaths)))

    val write_reqs = Vec(nPorts, Flipped(Decoupled(new SyncMemWriteReq(t, addrBits))))
  })

  val bankSize = size / nBanks; require(size % nBanks == 0, s"size=$size | nBanks=$nBanks")
  val useAsyncMem = bankSize == 1 // Vivado replaces single-word SRAMs with registers, but sometimes fails to do so for mysterious reasons, causing the entire synthesis to fail. To help it along, we do Vivado's job for it, and explicitly use registers here for single-entry SRAMs
  val memsOpt = Option.unless(isDummy)(Seq.fill(nBanks)(if (useAsyncMem) Left(Reg(t)) else Right(SyncReadMem(bankSize, t))))

  val read_req_addrs = VecInit(io.read_reqs.map(_.bits.addr / nBanks.U)).suggestName("rra")
  val read_req_banks = if (independentBanks) VecInit.tabulate(nPorts)(_.U(log2Up(nBanks).W)) else VecInit(io.read_reqs.map(_.bits.addr % nBanks.U)).suggestName("rrb")
  // dontTouch(read_req_addrs)
  // dontTouch(read_req_banks)

  val readPortsTaken = VecInit.fill(nPorts)(false.B).suggestName("rt")
  val writePortsTaken = VecInit.fill(nPorts)(false.B).suggestName("wt")

  val read_results = VecInit(memsOpt.map(_.zipWithIndex.map { case (mem, bankId) => noPrefix {
    val takenPortId = if (independentBanks) bankId.U else noPrefix(PriorityEncoder(io.read_reqs.zip(read_req_banks).map { case (r, b) => r.valid && b === bankId.U })).suggestName("tp")

    for (portId <- 0 until nPorts) {
      val bankMatches = if (independentBanks) (portId == bankId).B else (read_req_banks(portId) === bankId.U)
      if (!independentBanks || portId == bankId)
        when (io.read_reqs(portId).valid && bankMatches && read_req_addrs(portId) === read_req_addrs(takenPortId)) {
          readPortsTaken(portId) := true.B
        }
    }

    val ren = io.read_reqs(takenPortId).valid && any(Seq(independentBanks.B, read_req_banks(takenPortId) === bankId.U)) // TODO we should read when "io.read_reqs(takenPortId).fire" rather than when "io.read_reqs(takenPortId).valid", but I'm not sure how to handle that when multiple ports are reading from the same row of the same bank simultaneously
    mem.fold(m => RegNext(m), m => m.read(read_req_addrs(takenPortId), ren))
  }}).getOrElse(Seq.fill(nBanks)(0.U.asTypeOf(t))))

  Seq.tabulate(nBanks) { bankId =>
    val canBeTakens = io.write_reqs.zipWithIndex.map { case (w,i) =>
      val bankMatches = if (independentBanks) (bankId == i).B else (w.bits.addr % nBanks.U === bankId.U)
      all(Seq(w.valid, bankMatches))
    }
    val takenPorts = canBeTakens.zipWithIndex.scanLeft((false.B,0.U)) { case ((alreadyTaken, takenPort), (canBeTaken, canBeTakenPort)) =>
      val taken = alreadyTaken || canBeTaken
      val port = Mux(alreadyTaken, takenPort, canBeTakenPort.U)
      (taken, port)
    }.tail
    val isTakens = takenPorts.zipWithIndex.map { case ((taken, takenPort), portId) => taken && takenPort === portId.U }
    io.write_reqs.zipWithIndex.foreach { case (write_req, portId) =>
      val bankMatches = if (independentBanks) (bankId == portId).B else (write_req.bits.addr % nBanks.U === bankId.U)
      if (!independentBanks || bankId == portId)
        when (all(Seq(bankMatches, !any(isTakens.take(portId))))) {
          writePortsTaken(portId) := true.B
        }
    }
  }

  Seq.tabulate(nPorts)(portId => assert(!io.read_reqs(portId).ready || readPortsTaken(portId)))

  val bank_is_being_written_to = VecInit.fill(nBanks)(false.B).suggestName("bwt")
  val bank_write_addr = VecInit.fill(nBanks)(0.U(log2Up(size / nBanks).W)).suggestName("bwa")
  val bank_write_mask = Wire(Vec(nBanks, getChiselType(io.write_reqs.head.bits.mask))).suggestName("bwm")
  val bank_write_data = Wire(Vec(nBanks, getChiselType(io.write_reqs.head.bits.data))).suggestName("bwd")

  bank_write_mask := DontCare
  bank_write_data := DontCare

  for (portId <- 0 until nPorts) {
    val read_req = io.read_reqs(portId)
    val read_resp = io.read_resps(portId)
    val write_req = io.write_reqs(portId)

    // Handle reads
    val read_req_path = read_req.bits.pathId.getOrElse(0.U)
    val read_req_bank = if (independentBanks) portId.U else read_req_banks(portId)

    assert(!read_req.valid || read_req_path <= nPaths.U, "invalid path selected")

    val read_result_regs = noPrefix(Seq.tabulate(nPaths)(pathId => RegEnable(read_results(RegNext(read_req_bank)), RegNext(read_req.fire) && RegNext(read_req_path) === pathId.U).suggestName("rr")))
    val read_result_tags = noPrefix(Seq.tabulate(nPaths)(pathId => RegEnable(read_req.bits.tag, read_req.fire && read_req_path === pathId.U).suggestName("tag")))

    val read_result_reg_valids = RegInit(VecInit.fill(nPaths)(false.B)).suggestName("rv") // Note: This is a flattened version of a [nBranches][nBanks] array

    val read_resp_path = {
      val reg = RegInit(0.U(log2Up(nPaths).W))
      reg := Mux(reg +& 1.U >= nPaths.U, 0.U, reg +& 1.U)
      Mux(read_result_reg_valids(reg), reg, PriorityEncoder(read_result_reg_valids))
    }

    when (read_resp.fire) {
      read_result_reg_valids(read_resp_path) := false.B
    }

    when (read_req.fire) {
      read_result_reg_valids(read_req_path) := true.B
    }

    read_resp.valid := read_result_reg_valids(read_resp_path)

    Seq.tabulate(nPaths) { pathId =>
      def whenClause(foo: => Unit) = if (pathId == 0) foo else when (read_resp_path === pathId.U) { foo }
      whenClause {
        read_resp.bits.data := read_result_regs(pathId)
        read_resp.bits.tag := read_result_tags(pathId)
      }
    }
    when (RegNext(read_req.fire) && RegNext(read_req_path) === read_resp_path) {
      read_resp.bits.data := read_results(RegNext(read_req_bank))
    }

    read_resp.bits.pathId.foreach(_ := read_resp_path)

    read_req.ready := (read_resp.ready && read_req_path === read_resp_path || !read_result_reg_valids(read_req_path)) && readPortsTaken(portId)
    if (prevent_simultaneous_access) {
      when (any(io.write_reqs.map(write_req => write_req.valid && write_req.bits.addr === read_req.bits.addr))) {
        read_req.ready := false.B
      }
    }

    // Handle writes
    write_req.ready := writePortsTaken(portId)
    when (write_req.fire) {
      val addr = write_req.bits.addr / nBanks.U
      val bank = if (independentBanks) portId.U else (write_req.bits.addr % nBanks.U)
      val data = write_req.bits.data
      val mask = write_req.bits.mask

      bank_is_being_written_to(bank) := true.B
      bank_write_addr(bank) := addr
      bank_write_mask(bank) := mask
      bank_write_data(bank) := data
    }

    assert(!(read_req.fire && write_req.fire && read_req.bits.addr === write_req.bits.addr), "simultaneously writing to and reading from the same SRAM address")
  }

  for (bankId <- 0 until nBanks) {
    when (bank_is_being_written_to(bankId)) {
      memsOpt.foreach(_(bankId).fold(
        _.zip(bank_write_data(bankId)).zip(bank_write_mask(bankId)).foreach { case ((dst, src), mask) => when (mask) { dst := src } },
        _.write(bank_write_addr(bankId), bank_write_data(bankId), bank_write_mask(bankId))))
    }
  }
}
